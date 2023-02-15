#include "scanner.h"
#include "parser.h"

int CurTok;
std::map<char, int> BinopPrecedence;

static std::unique_ptr<ExprAST> ParseExpression();

int getNextToken() { return CurTok = gettok(); }

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
  if (!isascii(CurTok))
    return -1;

  // Make sure it's a declared binop.
  int TokPrec = BinopPrecedence[CurTok];
  if (TokPrec <= 0)
    return -1;
  return TokPrec;
}

/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

/// numberexpr ::= number
static std::unique_ptr<NumberExprAST> ParseNumberExpr() {
  auto Result = std::make_unique<NumberExprAST>(NumVal);
  getNextToken(); // consume the number
  return std::move(Result);
}

/// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr() {
  getNextToken(); // eat (.
  auto V = ParseExpression();
  if (!V)
    return nullptr;
  
  if (CurTok != ')')
    return LogError("expected ')'");
  getNextToken(); // eat ).
  return V;
}

/// identifierexpr
///   ::= identifier
///   ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
  std::string IdName = IdentifierStr;

  getNextToken();   // eat identifier

  if (CurTok != '(')  // simple variable ref.
    return std::make_unique<VariableExprAST>(IdName);
  
  // Call.
  getNextToken();
  std::vector<std::unique_ptr<ExprAST>> Args;
  if (CurTok != ')') {
    while (true) {
      if (auto Arg = ParseExpression()) {
        Args.push_back(std::move(Arg));
      } else {
        return nullptr;
      }

      if (CurTok == ')')
        break;
      
      if (CurTok != ',')
        return LogError("Expected ')' or ',' in argument list");
      getNextToken();
    }
  }

  // Eat the ')'.
  getNextToken();

  return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// ifexpr ::= 'if' expression 'then' expression 'else' expression
static std::unique_ptr<ExprAST> ParseIfExpr() {
  getNextToken();   // eat if.

  // condition.
  auto Cond = ParseExpression();
  if (!Cond)
    return nullptr;
  
  if (CurTok != tok_then)
    return LogError("expected then");
  getNextToken();   // eat then.

  // then.
  auto Then = ParseExpression();
  if (!Then)
    return nullptr;
  
  if (CurTok != tok_else)
    return LogError("expected else");
  getNextToken();   // eat else.

  auto Else = ParseExpression();
  if (!Else)
    return nullptr;
  
  return std::make_unique<IfExprAST>(std::move(Cond), std::move(Then), std::move(Else));
}

static std::unique_ptr<ExprAST> ParseForExpr() {
  getNextToken();   // eat for.

  if (CurTok != tok_identifier)
    return LogError("expected identifier after for");
  
  std::string IdName = IdentifierStr;
  getNextToken();   // eat identifier.

  if (CurTok != '=')
    return LogError("expected '=' after for");
  getNextToken();   // eat '='

  auto Start = ParseExpression();
  if (!Start)
    return nullptr;
  if (CurTok != ',')
    return LogError("expected ',' after for start value");
  getNextToken();

  auto End = ParseExpression();
  if (!End)
    return nullptr;
  
  // The step value is optional
  std::unique_ptr<ExprAST> Step;
  if (CurTok == ',') {
    getNextToken();
    Step = ParseExpression();
    if (!Step)
      return nullptr;
  }

  if (CurTok != tok_in)
    return LogError("expected 'in' after for");
  getNextToken();   // eat 'in'.

  auto Body = ParseExpression();
  if (!Body)
    return nullptr;

  return std::make_unique<ForExprAST>(IdName, std::move(Start), 
                                      std::move(End), std::move(Step),
                                      std::move(Body));
}

static std::unique_ptr<ExprAST> ParseVarExpr() {
  getNextToken();   // eat var.

  std::vector<std::pair<std::string, std::unique_ptr<ExprAST>>> VarNames;
  
  // At least one variable name is required.
  if (CurTok != tok_identifier)
    return LogError("expected identifier after var");
  
  while (true) {
    std::string Name = IdentifierStr;
    getNextToken();    // eat identifier.

    // Read the optional identifier
    std::unique_ptr<ExprAST> Init;
    if (CurTok == '=') {
      getNextToken();   // eat '='

      Init = ParseExpression();
      if (!Init) return nullptr;
    }

    VarNames.push_back(std::make_pair(Name, std::move(Init)));

    // End of var list, exit loop
    if (CurTok != ',') break;
    getNextToken();   // eat ','

    if (CurTok != tok_identifier)
      return LogError("expected identifier list after var");
  }

  if (CurTok != tok_in)
    return LogError("expected 'in' keyword after 'var'");
  getNextToken();   // eat 'in'.
  
  auto Body = ParseExpression();
  if (!Body)
    return nullptr;
  
  return std::make_unique<VarExprAST>(std::move(VarNames), std::move(Body));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
///   ::= ifexpr
///   ::= forexpr
///   ::= varexpr
static std::unique_ptr<ExprAST> ParsePrimary() {
  switch (CurTok) {
  case tok_identifier:
    return ParseIdentifierExpr();
  case tok_number:
    return ParseNumberExpr();
  case '(':
    return ParseParenExpr();
  case tok_if:
    return ParseIfExpr();
  case tok_for:
    return ParseForExpr();
  case tok_var:
    return ParseVarExpr();
  default:
    return LogError("unknown token when expecting an expression");
  }
}

/// unary
///   ::= primary
///   ::= '!' unary
static std::unique_ptr<ExprAST> ParseUnary() {
  // If the current token is not an operator, it must be a primary expr.
  if (!isascii(CurTok) || CurTok == '(' || CurTok == ',')
    return ParsePrimary();
  
  // If this is a unary operator, read it.
  int Opc = CurTok;
  getNextToken();
  if (auto Operand = ParseUnary())
    return std::make_unique<UnaryExprAST>(Opc, std::move(Operand));
  return nullptr;
}

static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
                                              std::unique_ptr<ExprAST> LHS) {
  // If this is a binop, find its precedence
  while (true) {
    int TokPrec = GetTokPrecedence();

    // If this is a binop that binds at least as tightly as the current binop,
    // consume it, otherwise we are done.
    if (TokPrec < ExprPrec)
      return LHS;
    
    // Okay, we know this is a binop
    int BinOp = CurTok;
    getNextToken();   // eat binop

    // Parse the primary expression after the binary operator.
    auto RHS = ParseUnary();
    if (!RHS)
      return nullptr;
    
    int NextTokPrec = GetTokPrecedence();
    if (TokPrec < NextTokPrec) {
      RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
      if (!RHS)
        return nullptr;
    }

    // Merge LHS/RHS.
    LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
  }
}

/// expression
///   ::= primary binoprhs
static std::unique_ptr<ExprAST> ParseExpression() {
  auto LHS = ParseUnary();
  if (!LHS)
    return nullptr;
  
  return ParseBinOpRHS(0, std::move(LHS));
}

/// prototype
///   ::= id '(' id* ')'
///   ::= binary LETTER number? (id, id)
static std::unique_ptr<PrototypeAST> ParsePrototype() {
  std::string FnName = IdentifierStr;

  unsigned Kind = 0;    // 0 = identifier, 1 = unary, 2 = binary.
  unsigned BinaryPrecedence = 30;
  
  switch (CurTok)
  {
  case tok_identifier:
    FnName = IdentifierStr;
    Kind = 0;
    getNextToken();
    break;
  case tok_unary:
    getNextToken();
    if (!isascii(CurTok))
      return LogErrorP("Expected unary operator");
    FnName = "unary";
    FnName += (char)CurTok;
    Kind = 1;
    getNextToken();
    break;
  case tok_binary:
    getNextToken();
    if (!isascii(CurTok))
      return LogErrorP("Expected binary operator");
    FnName = "binary";
    FnName += (char)CurTok;
    Kind = 2;
    getNextToken();

    // Read the precedence if present.
    if (CurTok == tok_number) {
      if (NumVal < 1 || NumVal > 100)
        return LogErrorP("Invalid precedence: must be 1..100");
      BinaryPrecedence = (unsigned)NumVal;
      getNextToken();
    }
    break;
  default:
    return LogErrorP("Expected function name in prototype");
  }

  if (CurTok != '(')
    return LogErrorP("Expected '(' in prototype");
  
  // Read the list of argument names.
  std::vector<std::string> ArgNames;
  while (getNextToken() == tok_identifier) {
    ArgNames.push_back(IdentifierStr);
  }

  if (CurTok != ')')
    return LogErrorP("Expected ')' in prototype");

  getNextToken();   // eat ).
  
  return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames), Kind != 0, 
                                          BinaryPrecedence);
}

/// definition ::= 'def' prototype expression
std::unique_ptr<FunctionAST> ParseDefinition() {
  getNextToken();   // eat def.
  auto Proto = ParsePrototype();
  if (!Proto) return nullptr;

  if (auto E = ParseExpression())
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  return nullptr;
}

/// external ::= 'extern' prototype
std::unique_ptr<PrototypeAST> ParseExtern() {
  getNextToken();   // eat extern.
  return ParsePrototype();
}

/// toplevelexpr ::= expression
std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
  if (auto E = ParseExpression()) {
    // Make an anonymous proto.
    auto Proto = std::make_unique<PrototypeAST>("__anon_expr", 
                                                std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
  }
  return nullptr;
}