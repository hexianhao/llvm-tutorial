#include "parser.h"
#include "codegen.h"

//===----------------------------------------------------------------------===//
// Code Generation
//===----------------------------------------------------------------------===//

static std::unique_ptr<llvm::LLVMContext> TheContext;
static std::unique_ptr<llvm::Module> TheModule;
static std::unique_ptr<llvm::IRBuilder<>> Builder;
static std::map<std::string, llvm::AllocaInst *> NamedValues;
static std::unique_ptr<llvm::legacy::FunctionPassManager> TheFPM;

llvm::Value *LogErrorV(const char *Str) {
  LogError(Str);
  return nullptr;
}

llvm::Value *NumberExprAST::codegen() {
  return llvm::ConstantFP::get(*TheContext, llvm::APFloat(Val));
}

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block of
/// the function.  This is used for mutable variables etc.
static llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction,
                                                llvm::StringRef VarName) {
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                          TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(llvm::Type::getDoubleTy(*TheContext), nullptr, VarName);
}

llvm::Value *VariableExprAST::codegen() {
  auto V = NamedValues[Name];
  if (!V)
    return LogErrorV("Unknown variable name");
  
  // Load the value.
  return Builder->CreateLoad(V->getAllocatedType(), V, Name);
}

llvm::Value *UnaryExprAST::codegen() {
  llvm::Value *OpVal = Operand->codegen();
  if (!OpVal)
    return nullptr;
  
  llvm::Function *F = TheModule->getFunction(std::string("unary") + Opcode);
  if (!F)
    return LogErrorV("Unknown unary operator");

  return Builder->CreateCall(F, OpVal, "unop");
}

llvm::Value *BinaryExprAST::codegen() {
  // Special case '=' because we don't want to emit the LHS as an expression.
  if (Op == '=') {
    // This assume we're building without RTTI because LLVM builds that way by
    // default. If you build LLVM with RTTI this can be changed to a
    // dynamic_cast for automatic error checking.
    VariableExprAST *LHSE = static_cast<VariableExprAST*>(LHS.get());
    if (!LHSE)
      return LogErrorV("destination of '=' must be a variable");
    
    // Codegen the RHS.
    llvm::Value *Val = RHS->codegen();
    if (!Val)
      return nullptr;

    // Look up the name.
    llvm::AllocaInst *Alloca = NamedValues[LHSE->getName()];
    if (!Alloca) 
      return LogErrorV("Unknown variable name");

    Builder->CreateStore(Val, Alloca);
    return Val;
  }

  llvm::Value *L = LHS->codegen();
  llvm::Value *R = RHS->codegen();
  if (!L || !R)
    return nullptr;
  
  switch (Op)
  {
  case '+':
    return Builder->CreateFAdd(L, R, "addtmp");
  case '-':
    return Builder->CreateFSub(L, R, "subtmp");
  case '*':
    return Builder->CreateFMul(L, R, "multmp");
  case '<':
    L = Builder->CreateFCmpULT(L, R, "cmptmp");
    return Builder->CreateUIToFP(L, llvm::Type::getDoubleTy(*TheContext),
                                 "booltmp");
  default:
    break;
  }

  // If it wasn't a builtin binary operator, it must be a user defined one. Emit
  // a call to it.
  llvm::Function *F = TheModule->getFunction(std::string("binary") + Op);
  assert(F && "binary operator not found!");

  llvm::Value *Ops[2] = {L, R};
  return Builder->CreateCall(F, Ops, "binop");
}

llvm::Value *CallExprAST::codegen() {
  // Look up the name in the global module table.
  llvm::Function *CalleeF = TheModule->getFunction(Callee);
  if (!CalleeF)
    return LogErrorV("Unkown function referenced");
  
  // If argument mismatch error.
  if (CalleeF->arg_size() != Args.size())
    return LogErrorV("Incorrect # arguments passed");
  
  std::vector<llvm::Value *> ArgsV;
  for (int i = 0; i < Args.size(); i++) {
    ArgsV.push_back(Args[i]->codegen());
    if (!ArgsV.back())
      return nullptr;
  }

  return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

llvm::Function *PrototypeAST::codegen() {
  // Make the function type:  double(double,double) etc.
  std::vector<llvm::Type *> Doubles(Args.size(),
                                    llvm::Type::getDoubleTy(*TheContext));
  llvm::FunctionType *FT = 
      llvm::FunctionType::get(llvm::Type::getDoubleTy(*TheContext), Doubles, false);
  
  llvm::Function *F = 
      llvm::Function::Create(FT, llvm::Function::ExternalLinkage, Name, TheModule.get());
  
  int Idx = 0;
  for (auto &Arg : F->args()) {
    Arg.setName(Args[Idx++]);
  }

  return F;
}

llvm::Function *FunctionAST::codegen() {
  // First, check for an existing function from a previous 'extern' declaration.
  llvm::Function *TheFunction = TheModule->getFunction(Proto->getName());

  if (!TheFunction)
    TheFunction = Proto->codegen();
  
  if (!TheFunction)
    return nullptr;
  
  if (!TheFunction->empty())
    return (llvm::Function*)LogErrorV("Function cannot be redefined.");
  
  if (Proto->isBinaryOp())
    BinopPrecedence[Proto->getOperatorName()] = Proto->getBinaryPrecedence();
  
  // Create a new basic block to start insertion into.
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(*TheContext, "entry", TheFunction);
  Builder->SetInsertPoint(BB);

  // Record the function arguments in the NamedVaues map.
  NamedValues.clear();
  for (auto &Arg : TheFunction->args()) {
    // Create an alloca for this variable.
    llvm::AllocaInst *Alloc = CreateEntryBlockAlloca(TheFunction, Arg.getName());

    // Store the initial value into the alloca.
    Builder->CreateStore(&Arg, Alloc);

    NamedValues[std::string(Arg.getName())] = Alloc;
  }

  if (llvm::Value *RetVal = Body->codegen()) {
    // Finish off the function
    Builder->CreateRet(RetVal);

    // Validate the generated code, checking for consistency.
    llvm::verifyFunction(*TheFunction);

    // Optimize the function.
    TheFPM->run(*TheFunction);

    return TheFunction;
  }

  // Error reading body, remove function
  TheFunction->removeFromParent();
  return nullptr;
}

llvm::Value *IfExprAST::codegen() {
  llvm::Value *CondV = Cond->codegen();
  if (!CondV)
    return nullptr;
  
  // Convert condition to a bool by comparing non-equal to 0.0
  CondV = Builder->CreateFCmpONE(
      CondV, llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0)), "ifcond");
  
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

  // Create blocks for the then and else cases.  Insert the 'then' block at the
  // end of the function
  llvm::BasicBlock *ThenBB = 
        llvm::BasicBlock::Create(*TheContext, "then", TheFunction);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(*TheContext, "else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*TheContext, "ifcont");

  Builder->CreateCondBr(CondV, ThenBB, ElseBB);   // 插入条件跳转的分支

  // Emit then value.
  Builder->SetInsertPoint(ThenBB);

  llvm::Value *ThenV = Then->codegen();
  if (!ThenV)
    return nullptr;

  Builder->CreateBr(MergeBB);   // 在ThenBB中创建一个跳转到MergeBB的分支
  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = Builder->GetInsertBlock();

  // Emit else block
  TheFunction->insert(TheFunction->end(), ElseBB);    // 插入ElseBB块
  Builder->SetInsertPoint(ElseBB);                    // 后续的指令都插入到ElseBB

  llvm::Value *ElseV = Else->codegen();               // Else产生的代码都进入ElseBB块
  if (!ElseV)
    return nullptr;
  
  Builder->CreateBr(MergeBB);
  ElseBB = Builder->GetInsertBlock();

  // Emit merge block
  TheFunction->insert(TheFunction->end(), MergeBB);
  Builder->SetInsertPoint(MergeBB);
  llvm::PHINode *PN = Builder->CreatePHI(llvm::Type::getDoubleTy(*TheContext), 2, "iftmp");

  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);
  return PN;
}

// Output for-loop as:
//   var = alloca double
//   ...
//   start = startexpr
//   store start -> var
//   goto loop
// loop:
//   ...
//   bodyexpr
//   ...
// loopend:
//   step = stepexpr
//   endcond = endexpr
//
//   curvar = load var
//   nextvar = curvar + step
//   store nextvar -> var
//   br endcond, loop, endloop
// outloop:
llvm::Value *ForExprAST::codegen() {
  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

  // Create an alloca for the variable in the entry block.
  llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);

  // Emit the start code first, without 'variable' in scope.
  llvm::Value *StartVal = Start->codegen();
  if (!StartVal)
    return nullptr;
  
  // Store the value into the alloca.
  Builder->CreateStore(StartVal, Alloca);
  
  llvm::BasicBlock *PreheaderBB = Builder->GetInsertBlock();
  llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(*TheContext, "loop", TheFunction);
  llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(*TheContext, "afterloop");

  // Insert an explicit fall through from the current block to the LoopBB.
  Builder->CreateBr(LoopBB);

  // Start insertion in LoopBB
  Builder->SetInsertPoint(LoopBB);
  
  // Within the loop, the variable is defined equal to the PHI node.  If it
  // shadows an existing variable, we have to restore it, so save it now.
  llvm::AllocaInst *OldVal = NamedValues[VarName];
  NamedValues[VarName] = Alloca;

  if (!Body->codegen())
    return nullptr;
  
  llvm::Value *StepVal = nullptr;
  if (Step) {
    StepVal = Step->codegen();
    if (!StepVal)
      return nullptr;
  } else {
    StepVal = llvm::ConstantFP::get(*TheContext, llvm::APFloat(1.0));
  }

  // compute the end condition.
  llvm::Value *EndCond = End->codegen();
  if (!EndCond)
    return nullptr;
  
  // Reload, increment, and restore the alloca. This handles the case where
  // the body of the loop mutates the variable.
  llvm::Value *CurVar = 
            Builder->CreateLoad(Alloca->getAllocatedType(), Alloca, VarName);
  llvm::Value *NextVar = Builder->CreateFAdd(CurVar, StepVal, "nextvar");
  Builder->CreateStore(NextVar, Alloca);


  EndCond = Builder->CreateFCmpONE(
      EndCond, llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0)), "loopcond");
  
  // Insert the condition branch into the end of loop
  Builder->CreateCondBr(EndCond, LoopBB, AfterBB);

  TheFunction->insert(TheFunction->end(), AfterBB);
  Builder->SetInsertPoint(AfterBB);

  // Restore the unshadowed variable.
  if (OldVal)
    NamedValues[VarName] = OldVal;
  else
    NamedValues.erase(VarName);

  // for expr always returns 0.0.
  return llvm::Constant::getNullValue(llvm::Type::getDoubleTy(*TheContext));
}

llvm::Value *VarExprAST::codegen() {
  std::vector<llvm::AllocaInst *> OldBindings;

  llvm::Function *TheFunction = Builder->GetInsertBlock()->getParent();

  for (int i = 0; i < VarNames.size(); i++) {
    std::string VarName = VarNames[i].first;
    ExprAST *Init = VarNames[i].second.get();

    llvm::Value *InitVal;
    if (Init) {
      InitVal = Init->codegen();
      if (!InitVal)
        return nullptr;
    } else {  // If not specified, use 0.0.
      InitVal = llvm::ConstantFP::get(*TheContext, llvm::APFloat(0.0));
    }

    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
    Builder->CreateStore(InitVal, Alloca);

    // Remember the old variable binding so that we can restore the binding when
    // we unrecurse.
    OldBindings.push_back(NamedValues[VarName]);

    // Remember this binding
    NamedValues[VarName] = Alloca;
  }

  llvm::Value *BodyVal = Body->codegen();
  if (!BodyVal)
    return nullptr;
  
  // Pop all our variables from scope.
  for (int i = 0; i < VarNames.size(); i++) {
    NamedValues[VarNames[i].first] = OldBindings[i];
  }

  return BodyVal;
}

void HandleDefinition() {
  if (auto FnAST = ParseDefinition()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Read function definition: ");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleExtern() {
  if (auto ProtoAST = ParseExtern()) {
    if (auto *FnIR = ProtoAST->codegen()) {
      fprintf(stderr, "Read extern: ");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void HandleTopLevelExpression() {
  // Evaluate a top-level expression into an anonymous function.
  if (auto FnAST = ParseTopLevelExpr()) {
    if (auto *FnIR = FnAST->codegen()) {
      fprintf(stderr, "Parsed a top-level expression: ");
      FnIR->print(llvm::errs());
      fprintf(stderr, "\n");

      // Remove the anonymous expression.
      FnIR->eraseFromParent();
    }
  } else {
    // Skip token for error recovery.
    getNextToken();
  }
}

void InitializeModuleAndPassManager() {
  // Open a new context and module
  TheContext = std::make_unique<llvm::LLVMContext>();
  TheModule = std::make_unique<llvm::Module>("my cool jit", *TheContext);

  // create a new builder for the module.
  Builder = std::make_unique<llvm::IRBuilder<>>(*TheContext);

  // Create a new pass manager attached to it.
  TheFPM = std::make_unique<llvm::legacy::FunctionPassManager>(TheModule.get());

  // Promote allocas to registers.
  TheFPM->add(llvm::createPromoteMemoryToRegisterPass());
  // Do simple "peehole" optimizations and bit-twiddling optimzations.
  TheFPM->add(llvm::createInstructionCombiningPass());
  // Reassociate expressions.
  TheFPM->add(llvm::createReassociatePass());
  // Eliminate Common Subexpression.
  TheFPM->add(llvm::createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  TheFPM->add(llvm::createCFGSimplificationPass());

  TheFPM->doInitialization();
}

void ObjCodeGen() {
  // Initialize the target registry etc.
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  auto TargetTriple = llvm::sys::getDefaultTargetTriple();
  TheModule->setTargetTriple(TargetTriple);

  std::string Error;
  auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);

  // Print an error and exit if we couldn't find the requested target.
  // This generally occurs if we've forgotten to initialise the
  // TargetRegistry or we have a bogus target triple.
  if (!Target) {
    llvm::errs() << Error;
    return;
  }

  auto CPU = "generic";
  auto Features = "";
  llvm::TargetOptions opt;
  auto RM = std::optional<llvm::Reloc::Model>();
  auto TheTargetMachine = 
      Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);
  TheModule->setDataLayout(TheTargetMachine->createDataLayout());

  auto Filename = "output.o";
  std::error_code EC;
  llvm::raw_fd_ostream dest(Filename, EC, llvm::sys::fs::OF_None);

  if (EC) {
    llvm::errs() << "Could not open file: " << EC.message();
    return;
  }

  llvm::legacy::PassManager pass;
  auto FileType = llvm::CGFT_ObjectFile;

  if (TheTargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    llvm::errs() << "TheTargetMachine can't emit a file of this type";
    return;
  }

  pass.run(*TheModule);
  dest.flush();

  llvm::outs() << "Wrote " << Filename << '\n';
}