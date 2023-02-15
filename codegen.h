#ifndef CODEGEN_H_
#define CODEGEN_H_

void InitializeModuleAndPassManager();
void HandleDefinition();
void HandleExtern();
void HandleTopLevelExpression();

void ObjCodeGen();

#endif