#ifndef CODEGEN_H
#define CODEGEN_H

#include <iostream>
#include <stack>
#include <vector>

#include <cppformat/format.h>

#include <llvm-c/Core.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>
#include <llvm-c/Core.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/ExecutionEngine.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Transforms/Scalar.h>
#include <llvm/IR/intrinsics.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/Support/Dwarf.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/Support/Signals.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Transforms/Scalar.h>

#include "node.h"
#include "visitors/visitor.h"

using namespace std;

class CodegenVisitor : public Visitor
{
public:
    LLVMModuleRef mod;
    stack<LLVMBuilderRef> builders;
    stack<LLVMValueRef> currentFunctions;
    stack<LLVMBasicBlockRef> currentScopes;
    stack<bool> is_rvalue;
    bool forcing;

    void visit(Node *n);
    void visitSymbol(Symbol* s);
    void visitConstant(Constant* c);
    void visitDeclaration(Declaration* d);
    void visitParameter(Parameter* p);
    void visitArgument(Argument* a);
    void visitAssignment(Assignment* a);
    void visitTypeFunction(TypeFunction t);
    void visitTypePointer(TypePointer* t);
    void visitApply(Apply* a);
    void visitTypeDefinition(TypeDefinition* t);
    void visitRec(Rec* r);
    void visitFunction(Function* f);
    void visitRet(Ret* r);
    void visitTypeof(Typeof* t);
    void visitDereference(Dereference* d);
    void visitAddressOf(AddressOf* a);
    void visitIf(If* i);
    void visitDo(Do *d);
    void visitWhile(While* w);
    void visitDefer(Defer* d);
    void visitBinOp(BinOp* b);
    void visitValueTuple(ValueTuple* v);
    void visitDot(Dot* d);
    void visitNeg(Neg* n);
    void visitNot(Not* n);
    void visitQuestion(Question *q);
    void visitCast(Cast* c);
    void visitHeap(Heap* h);
    void visitSizeof(Sizeof* s);
    void visitAssert(Assert* a);
    void visitStringLiteral(StringLiteral* s);
    void visitCharacter(Character *c);
    void visitPipe(Pipe* p);
    void visitRequire(Require* r);
    void visitLabel(Label *l);
    void visitJump(Jump *j);

    void visitDeferreds(Scope *s, bool ret = false);

    LLVMTypeRef llvmTypeOf(Node* n);

    CodegenVisitor()
    {
        mod = LLVMModuleCreateWithName("cpi");
        forcing = false;
    }

    LLVMValueRef cg(Node* n, bool rvalue = true, bool force = false);

    // numeric conversions
    LLVMValueRef convertToNumericType(Node* n, ConstantType type);
    LLVMValueRef convertToI8(Node* n);
    LLVMValueRef ConvertToI16(Node* n);
    LLVMValueRef convertToI32(Node* n);
    LLVMValueRef convertToI64(Node* n);
    LLVMValueRef convertToF32(Node* n);
    LLVMValueRef convertToF64(Node* n);

    bool isConstant(Node* n);
};

LLVMTypeRef llvmTypeOf(Node* n);

LLVMValueRef get_array_length(LLVMValueRef c, LLVMBuilderRef b);

#endif
