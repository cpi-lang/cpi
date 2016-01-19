#include "codegen.h"

LLVMOpcode get_op_code(OpType type, LLVMTypeKind type_kind, int is_unsigned)
{
    switch (type)
    {
        case OpType::AND:
        case OpType::BITAND:
            return LLVMAnd;
            break;
        case OpType::OR:
        case OpType::BITOR:
            return LLVMOr;
            break;
        case OpType::XOR:
            return LLVMXor;
            break;
        case OpType::MOD:
            if (type_kind == LLVMIntegerTypeKind)
            {
                if (is_unsigned) { return LLVMURem; }
                return LLVMSRem;
            }
            return LLVMFRem;
            break;
        case OpType::STAR:
            if (type_kind == LLVMIntegerTypeKind)
            {
                return LLVMMul;
            }
            return LLVMFMul;
            break;
        case OpType::FSLASH:
            if (type_kind == LLVMIntegerTypeKind)
            {
                if (is_unsigned) { return LLVMUDiv; }
                return LLVMSDiv;
            }
            return LLVMFDiv;
            break;
        case OpType::PLUS:
            if (type_kind == LLVMIntegerTypeKind)
            {
                return LLVMAdd;
            }
            return LLVMFAdd;
            break;
        case OpType::MINUS:
            if (type_kind == LLVMIntegerTypeKind)
            {
                return LLVMSub;
            }
            return LLVMFSub;
            break;
        case OpType::SHL:
            return LLVMShl;
            break;
        case OpType::SHR:
            return LLVMLShr;
            break;
        case OpType::ASHR:
            return LLVMAShr;
            break;
        default:
            assert(false && "unexpected op here!");
    }

    assert(false && "should have returned before now!");
    return LLVMUnreachable;
}

// Codegen Visitor
void CodegenVisitor::visit(Node* n)
{
    if (n == nullptr)
    {
        return;
    }

    if (visited.find(n) != visited.end())
    {
        // already visited!
        auto found = visited.find(n)->second;
        nodes.top()->metadata.llvm = (LLVMValueRef) found;

        visited[n] = found;

        if (visited[n] != nullptr)
        {
            return;
        }
    }

    visit_base(n);
}

void CodegenVisitor::visitSymbol(Symbol* s)
{
    // if this is a symbol (and NOT a constant), then we really can find the value and load it
    auto ntop = nodes.top();

    if (ntop->metadata.llvm != nullptr)
    {
        return;
    }

    ntop->metadata.llvm = cg(s->resolved, false);
}

void CodegenVisitor::visitConstant(Constant* c)
{
    switch (c->type)
    {
        case ConstantType::TUPLE:
            visitValueTuple(&c->data.tuple);
            break;
        case ConstantType::STRING:
        {
            auto sl = StringLiteral{c->data._string};
            visitStringLiteral(&sl);
        }
            break;
        case ConstantType::CHAR:
            nodes.top()->metadata.llvm = LLVMConstInt(LLVMInt8Type(), (unsigned long long int) c->data._char, 0);
            break;
        case ConstantType::I8:
        case ConstantType::U8:
            nodes.top()->metadata.llvm = LLVMConstInt(LLVMInt8Type(), (unsigned long long int) c->data.intvalue, 0);
            break;
        case ConstantType::I16:
        case ConstantType::U16:
            nodes.top()->metadata.llvm = LLVMConstInt(LLVMInt16Type(), (unsigned long long int) c->data.intvalue, 0);
            break;
        case ConstantType::I32:
        case ConstantType::U32:
            nodes.top()->metadata.llvm = LLVMConstInt(LLVMInt32Type(), (unsigned long long int) c->data.intvalue, 0);
            break;
        case ConstantType::I64:
        case ConstantType::U64:
            nodes.top()->metadata.llvm = LLVMConstInt(LLVMInt64Type(), (unsigned long long int) c->data.intvalue, 0);
            break;
        case ConstantType::F32:
            nodes.top()->metadata.llvm = LLVMConstReal(LLVMFloatType(), c->data.floatvalue);
            break;
        case ConstantType::F64:
            nodes.top()->metadata.llvm = LLVMConstReal(LLVMDoubleType(), c->data.floatvalue);
            break;
        case ConstantType::BOOL:
            nodes.top()->metadata.llvm = LLVMConstInt(LLVMInt1Type(), (unsigned long long int) c->data._bool, 0);
            break;
        case ConstantType::None:
        case ConstantType::NIL:
            nodes.top()->metadata.llvm = LLVMConstPointerNull(LLVMPointerType(LLVMInt8Type(), 0));
            break;
        default:
            assert(false);
    }
}

void CodegenVisitor::visitDeclaration(Declaration* d)
{
    auto top = nodes.top();
    if (top->metadata.llvm != nullptr)
    {
        return;
    }

    if (d->type->type == NodeType::NONE)
    {
        errors.push_back(ParseError{"could not determine type of declaration", nodes.top()->metadata.region});
        return;
    }

    // if it is a symbol, declare to be a copy of whatever the symbol points to
    if (nodes.top()->metadata.scope->isModuleLevel && d->initialValue->type == NodeType::SYMBOL)
    {
        d->initialValue = d->initialValue->data.symbol->resolved;
    }
    if (nodes.top()->metadata.scope->isModuleLevel && d->initialValue->type == NodeType::DECLARATION)
    {
        d->initialValue = d->initialValue->data.declaration->initialValue;
    }
    if (d->initialValue->type == NodeType::FUNCTION && d->declarationType == DeclarationType::IMMUTABLE)
    {
        d->external = true;
        auto fn = LLVMAddFunction(mod, d->name.c_str(), llvmTypeOf(typeOf(d->initialValue)));
        nodes.top()->metadata.llvm = fn;
        currentFunctions.push(fn);
        auto block = LLVMAppendBasicBlock(fn, d->name.c_str());
        auto builder = LLVMCreateBuilder();
        LLVMPositionBuilderAtEnd(builder, block);
        builders.push(builder);
        visitMany(d->initialValue->data.function->block);
        LLVMDisposeBuilder(builder);
        builders.pop();
        currentFunctions.pop();
        return;
    }

    if (d->initialValue->type == NodeType::FUNCTION
        && d->declarationType == DeclarationType::MUTABLE
        && nodes.top()->metadata.scope->isModuleLevel)
    {
        errors.push_back(ParseError{
            "cannot declare mutable function at top level",
            nodes.top()->metadata.region
        });
    }

    if (d->external || nodes.top()->metadata.scope->isModuleLevel)
    {
        if (sanitize(d->type)->type == NodeType::TYPEFUNCTION)
        {
            auto func = LLVMAddFunction(mod, d->name.c_str(), llvmTypeOf(d->type));
            nodes.top()->metadata.llvm = func;
            return;
        }

        auto dtype = sanitizeType(d->type);

        if (dtype->type == NodeType::FUNCTION)
        {
            nodes.top()->metadata.llvm = cg(d->initialValue);
            return;
        }

        if (d->initialValue->type != NodeType::NONE
            && d->declarationType == DeclarationType::IMMUTABLE)
        {
            nodes.top()->metadata.llvm = cg(d->initialValue);
            return;
        }

        // global variable!!
        d->external = true;
        d->global = true;
        auto newGlobal = ((llvm::Module *) mod)->getOrInsertGlobal(d->name, (llvm::Type *) llvmTypeOf(dtype));
        llvm::GlobalVariable *theGlobal = ((llvm::Module *) mod)->getGlobalVariable(newGlobal->getName(), true);
        theGlobal->setLinkage(llvm::GlobalValue::InternalLinkage);
        theGlobal->setInitializer(llvm::UndefValue::get((llvm::Type *) llvmTypeOf(dtype)));

        if (d->initialValue->type != NodeType::NONE)
        {
            errors.push_back(ParseError{
                "initial value will be ignored",
                nodes.top()->metadata.region,
                ParseErrorType::WARNING
            });
        }

        nodes.top()->metadata.llvm = theGlobal;
        return;
    }

    // allocate and store
    auto aType = typeOf(d->type);
    auto allocaType = llvmTypeOf(aType);
    if (LLVMGetTypeKind(allocaType) == LLVMFunctionTypeKind)
    {
        allocaType = LLVMPointerType(allocaType, 0);
    }

    nodes.top()->metadata.llvm = LLVMBuildAlloca(builders.top(), allocaType, d->name.c_str());

    // if type is struct, fill in any default arguments
    auto typ = typeOf(d->type);
    if (typ->type == NodeType::TYPETUPLE)
    {
        for (unsigned int i = 0; i < typ->data.typeTuple->types.size(); i++)
        {
            auto param = typ->data.typeTuple->types[i];
            assert(param->type == NodeType::PARAMETER);
            if (param->data.parameter->initialValue->type != NodeType::NONE)
            {
                auto initptr = LLVMBuildStructGEP(builders.top(),
                                                  (LLVMValueRef) nodes.top()->metadata.llvm,
                                                  i,
                                                  "init_gep");
                LLVMBuildStore(builders.top(), cg(param->data.parameter->initialValue), initptr);
            }
        }
    }

    if (d->initialValue->type != NodeType::NONE)
    {
        auto val = cg(d->initialValue);
        auto ptr = cg(nodes.top(), false);

        auto shouldType = typeOf(d->type);
        if (!typesMatch(typeOf(d->initialValue), shouldType))
        {
            assert(shouldType->type == NodeType::TYPECONSTANT);
            val = convertToNumericType(d->initialValue, shouldType->data.typeConstant->type);
        }

//        LLVMDumpValue(val);
//        LLVMDumpValue(ptr);

        // todo(chad): dangerous?
        ptr = LLVMBuildBitCast(builders.top(), ptr, LLVMPointerType(LLVMTypeOf(val), 0), "assignment_cast");
        LLVMBuildStore(builders.top(), val, ptr);
    }
}

void CodegenVisitor::visitParameter(Parameter* p)
{
    nodes.top()->metadata.llvm = LLVMGetParam(currentFunctions.top(), p->order);
}

void CodegenVisitor::visitArgument(Argument* a)
{
    nodes.top()->metadata.llvm = cg(a->value);
}

void CodegenVisitor::visitAssignment(Assignment* a)
{
    auto lhs = cg(a->lhs, false);
    auto rhs = cg(a->rhs);

//    LLVMDumpValue(lhs);
//    LLVMDumpType(LLVMTypeOf(lhs));
//    LLVMDumpValue(rhs);
//    LLVMDumpType(LLVMTypeOf(rhs));

    // todo(chad): dangerous?
    lhs = LLVMBuildBitCast(builders.top(), lhs, LLVMPointerType(LLVMTypeOf(rhs), 0), "danger_assign_cast");

    LLVMBuildStore(builders.top(), rhs, lhs);
}

void CodegenVisitor::visitTypeFunction(TypeFunction t)
{
    assert(false);
}

void CodegenVisitor::visitTypePointer(TypePointer* t)
{
    assert(false);
}

void CodegenVisitor::visitApply(Apply* a)
{
    // numeric conversion?
    auto realFunc = a->operator_;
    realFunc = sanitize(realFunc);
    if (realFunc->type == NodeType::DECLARATION
        && realFunc->data.declaration->initialValue->type != NodeType::NONE)
    {
        realFunc = sanitize(realFunc->data.declaration->initialValue);
    }

    auto realFuncType = sanitizeType(typeOf(realFunc));
    if (realFuncType->type == NodeType::TYPECONSTANT
        && realFuncType->data.typeConstant->type != ConstantType::STRING
        && realFuncType->data.typeConstant->type != ConstantType::BOOL
        && realFuncType->data.typeConstant->type != ConstantType::CHAR
        && realFuncType->data.typeConstant->type != ConstantType::TUPLE
        && realFuncType->data.typeConstant->type != ConstantType::None
        && realFuncType->data.typeConstant->type != ConstantType::NIL)
    {
        nodes.top()->metadata.llvm = convertToNumericType(a->operands[0],
                                                          realFuncType->data.typeConstant->type);
        return;
    }

    LLVMValueRef func = nullptr;
    func = cg(a->operator_);

    auto parameterCount = (unsigned int) a->operands.size();

    auto realParameterCount = -1;
    if (realFuncType->type == NodeType::TYPEFUNCTION)
    {
        realParameterCount = (unsigned int) realFuncType->data.typeFunction->input.types.size();
    }
    else if (realFuncType->type == NodeType::TYPEPOINTER)
    {
        realParameterCount = 1;
    }
    else if (realFuncType->type == NodeType::TYPETUPLE)
    {
        realParameterCount = (unsigned int) realFuncType->data.typeTuple->types.size();
    }
    else if (realFuncType->type == NodeType::TYPECONSTANT)
    {
        assert(realFuncType->data.typeConstant->type != ConstantType::None
               && realFuncType->data.typeConstant->type != ConstantType::NIL);

        realParameterCount = 1;
    }
    else
    {
        assert(false);
    }

    auto params = (LLVMValueRef*) malloc(realParameterCount * sizeof(LLVMValueRef));
    for (auto i = 0; i < realParameterCount; i++)
    {
        params[i] = nullptr;
    }

    if (realFunc->type == NodeType::FUNCTION)
    {
        assert(realFuncType->type == NodeType::TYPEFUNCTION);

        // if default value, initialize to that first
        for (auto i = 0; i < realFunc->data.function->parameters.size(); i++)
        {
            assert(realFunc->data.function->parameters[i]->type == NodeType::PARAMETER);
            if (realFunc->data.function->parameters[i]->data.parameter->initialValue->type != NodeType::NONE)
            {
                params[i] = cg(realFunc->data.function->parameters[i]->data.parameter->initialValue);
            }
        }
    }
    else if (realFuncType->type == NodeType::TYPETUPLE)
    {
        // allocate space for typeTuple
        auto allocad = LLVMBuildAlloca(builders.top(), llvmTypeOf(realFuncType), "tuple_apply");

        realParameterCount = (unsigned int) realFuncType->data.typeTuple->types.size();
        params = (LLVMValueRef *) malloc(realParameterCount * sizeof(LLVMValueRef));
        for (auto i = 0; i < realParameterCount; i++)
        {
            params[i] = nullptr;
        }

        vector<LLVMValueRef> llvmGivenArgs;
        vector<string> projectedGivenArgs;
        for (auto ap : a->operands)
        {
            assert(ap->type == NodeType::ARGUMENT);

            auto value = ap->data.argument->value;

            if (value->type == NodeType::SPREAD)
            {
                auto projectedType = sanitizeType(typeOf(value->data.spread->value));
                assert(projectedType->type == NodeType::TYPETUPLE);
                for (auto pt = 0; pt < projectedType->data.typeTuple->types.size(); pt++)
                {
                    auto c = cg(value->data.spread->value, true);
                    auto indexed = LLVMBuildExtractValue(builders.top(),
                                                         c,
                                                         (unsigned int) pt,
                                                         "spread_project_extract");

                    projectedGivenArgs.push_back(projectedType->data.typeTuple->types[pt]->data.parameter->name);
                    llvmGivenArgs.push_back(indexed);
                }
            }
            else
            {
                projectedGivenArgs.push_back(ap->data.argument->name);
                llvmGivenArgs.push_back(cg(ap->data.argument->value));
            }
        }

        // any default parameters?
        for (auto i = 0; i < realParameterCount; i++)
        {
            auto param = realFuncType->data.typeTuple->types[i];
            assert(param->type == NodeType::PARAMETER);
            if (param->data.parameter->initialValue != nullptr &&
                param->data.parameter->initialValue->type != NodeType::NONE)
            {
                params[i] = cg(param->data.parameter->initialValue);
            }
        }

        assert(realFuncType->type == NodeType::TYPETUPLE);
        auto expectedTypes = realFuncType->data.typeTuple->types;
        auto givenArgs = a->operands;
        auto ordering = get_ordering(expectedTypes, projectedGivenArgs, &errors, a->operator_->metadata.region);

        for (auto o : ordering)
        {
            params[o.target_index] = llvmGivenArgs[o.given_index];

            if (a->operands.size() > o.given_index
                && a->operands[o.given_index]->data.argument->value->type != NodeType::SPREAD)
            {
                auto targetType = expectedTypes[o.target_index];
                auto givenType = typeOf(a->operands[o.given_index]);
            }
        }

        for (auto i = 0; i < realParameterCount; i++)
        {
            auto member_ptr = LLVMBuildStructGEP(builders.top(), allocad, (unsigned int) i, "struct_gep");

            if (LLVMIsAConstantPointerNull(params[i]))
            {
                llvm::Type *member_ptr_type = (llvm::Type *) LLVMTypeOf(member_ptr);
                auto member_type = member_ptr_type->getContainedType(0);
                params[i] = LLVMBuildBitCast(builders.top(), params[i], (LLVMTypeRef) member_type, "cast_nil");
            }

            LLVMBuildStore(builders.top(), params[i], member_ptr);
        }

        nodes.top()->metadata.llvm = allocad;
        return;
    }
    else if (realFunc->type == NodeType::TYPEDEFINITION)
    {
        auto givenValue = cg(a->operands[0]);
        auto newValue = LLVMBuildAlloca(builders.top(), LLVMTypeOf(givenValue), "alloca apply typedefinition");
        auto store = LLVMBuildStore(builders.top(), givenValue, newValue);
        nodes.top()->metadata.llvm = newValue;
        return;
    }
    else if (realFuncType->type == NodeType::TYPEPOINTER)
    {
        auto index = cg(a->operands[0]);
        LLVMValueRef* indices = (LLVMValueRef*) malloc(sizeof(LLVMValueRef));
        indices[0] = index;
        nodes.top()->metadata.llvm = LLVMBuildGEP(builders.top(), func, indices, 1, "gep_ptr");
        return;
    }

    vector<LLVMValueRef> llvmGivenArgs;
    vector<string> projectedGivenArgs;
    for (auto ap : a->operands)
    {
        assert(ap->type == NodeType::ARGUMENT);

        auto value = ap->data.argument->value;

        if (value->type == NodeType::SPREAD)
        {
            auto projectedType = sanitizeType(typeOf(value->data.spread->value));
            assert(projectedType->type == NodeType::TYPETUPLE);
            for (auto pt = 0; pt < projectedType->data.typeTuple->types.size(); pt++)
            {
                auto c = cg(value->data.spread->value, true);
                auto indexed = LLVMBuildExtractValue(builders.top(),
                                                     c,
                                                     (unsigned int) pt,
                                                     "spread_project_extract");

                projectedGivenArgs.push_back(projectedType->data.typeTuple->types[pt]->data.parameter->name);
                llvmGivenArgs.push_back(indexed);
            }
        }
        else
        {
            projectedGivenArgs.push_back(ap->data.argument->name);
            llvmGivenArgs.push_back(cg(ap->data.argument->value));
        }
    }

    assert(realFuncType->type == NodeType::TYPEFUNCTION);
    auto expectedTypes = realFuncType->data.typeFunction->input.types;
    auto ordering = get_ordering(expectedTypes, projectedGivenArgs, &errors, a->operator_->metadata.region);

    for (auto o : ordering)
    {
        params[o.target_index] = llvmGivenArgs[o.given_index];

        if (a->operands.size() > o.given_index
            && a->operands[o.given_index]->data.argument->value->type != NodeType::SPREAD)
        {
            auto targetType = expectedTypes[o.target_index];
            auto givenType = typeOf(a->operands[o.given_index]);
        }
    }

    auto funcType = (llvm::Type*) LLVMTypeOf(func);
    assert(funcType->isPointerTy());
    funcType = funcType->getContainedType(0);

    auto returnType = LLVMGetReturnType((LLVMTypeRef) funcType);
    bool isVoidFunc = LLVMGetTypeKind(returnType) == LLVMVoidTypeKind;

    auto name = "";
    if (!isVoidFunc)
    {
        name = "apl";
    }

    auto call = LLVMBuildCall(builders.top(),
                              func,
                              params,
                              (unsigned int) realParameterCount,
                              name);

    if (!isVoidFunc)
    {
        auto allocad = LLVMBuildAlloca(builders.top(), LLVMTypeOf(call), "alloca_call");
        nodes.top()->metadata.llvm = allocad;
        LLVMBuildStore(builders.top(), call, allocad);
    }
}

void CodegenVisitor::visitTypeDefinition(TypeDefinition* t)
{
    // should be nothing to do here
//    nodes.top()->metadata.llvm = t->resolved;
}

void CodegenVisitor::visitRec(Rec* r)
{
    nodes.top()->metadata.llvm = currentFunctions.top();
}

void CodegenVisitor::visitFunction(Function* f)
{
    // lambda. get type?
    auto topNode = nodes.top();
    auto fnType = llvmTypeOf(topNode);
    auto fn = LLVMAddFunction(mod, "lambda", fnType);
    nodes.top()->metadata.llvm = fn;
    currentFunctions.push(fn);
    auto block = LLVMAppendBasicBlock(fn, "entry");
    auto builder = LLVMCreateBuilder();
    LLVMPositionBuilderAtEnd(builder, block);
    builders.push(builder);
    visitMany(f->block);
    LLVMDisposeBuilder(builder);
    builders.pop();
    currentFunctions.pop();
}

void CodegenVisitor::visitRet(Ret *r)
{
    auto ll = cg(r->value);
    visitDeferreds(nodes.top()->metadata.scope, true);
    LLVMBuildRet(builders.top(), ll);
}

void CodegenVisitor::visitTypeof(Typeof* t)
{
    assert(false);
}

LLVMTypeRef CodegenVisitor::llvmTypeOf(Node* n)
{
    auto ntype = n->type;

    switch (ntype)
    {
        case NodeType::TYPEFUNCTION:
        {
            auto nParams = n->data.typeFunction->input.types.size();
            LLVMTypeRef paramTypes[nParams];
            for (auto i = 0; i < nParams; i++)
            {
                paramTypes[i] = llvmTypeOf(n->data.typeFunction->input.types[i]);

                // if function type, need a function pointer
                if (LLVMGetTypeKind(paramTypes[i]) == LLVMFunctionTypeKind)
                {
                    paramTypes[i] = LLVMPointerType(paramTypes[i], 0);
                }
            }
            auto outputType = llvmTypeOf(n->data.typeFunction->output);

            // return type should be function pointer, not function
            if (LLVMGetTypeKind(outputType) == LLVMFunctionTypeKind)
            {
                outputType = LLVMPointerType(outputType, 0);
            }

            return LLVMFunctionType(outputType,
                                    paramTypes,
                                    (unsigned int) nParams,
                                    0);
        }
        case NodeType::FUNCTION:
            return llvmTypeOf(n->data.function->type);
        case NodeType::SYMBOL:
            return llvmTypeOf(n->data.symbol->resolved);
        case NodeType::TYPECONSTANT:
        {
            if (n->data.typeConstant->type == ConstantType::I8
                || n->data.typeConstant->type == ConstantType::CHAR
                || n->data.typeConstant->type == ConstantType::U8)
            {
                return LLVMInt8Type();
            }
            if (n->data.typeConstant->type == ConstantType::I16
                || n->data.typeConstant->type == ConstantType::U16)
            {
                return LLVMInt16Type();
            }
            if (n->data.typeConstant->type == ConstantType::I32
                || n->data.typeConstant->type == ConstantType::U32)
            {
                return LLVMInt32Type();
            }
            if (n->data.typeConstant->type == ConstantType::I64
                || n->data.typeConstant->type == ConstantType::U64)
            {
                return LLVMInt64Type();
            }
            if (n->data.typeConstant->type == ConstantType::F32)
            {
                return LLVMFloatType();
            }
            if (n->data.typeConstant->type == ConstantType::F64)
            {
                return LLVMDoubleType();
            }
            if (n->data.typeConstant->type == ConstantType::BOOL)
            {
                return LLVMInt1Type();
            }
            if (n->data.typeConstant->type == ConstantType::STRING)
            {
                LLVMTypeRef elementTypes[] = {
                    LLVMInt32Type(),
                    LLVMPointerType(LLVMInt8Type(), 0)
                };
                return LLVMStructType(elementTypes, 2, 0);
            }
            if (n->data.typeConstant->type == ConstantType::CHAR)
            {
                return LLVMInt8Type();
            }
            assert(false);
        }
        case NodeType::TYPEOF:
            return llvmTypeOf(n->data.typeof_->resolved);
        case NodeType::TYPEPOINTER:
        {
            auto ptype = llvmTypeOf(n->data.typePointer->type);
            if (LLVMGetTypeKind(ptype) == LLVMFunctionTypeKind)
            {
                ptype = LLVMPointerType(ptype, 0);
            }
            return LLVMPointerType(ptype, 0);
        }
        case NodeType::NONE:
            return LLVMVoidType();
        case NodeType::CONSTANT:
        {
            auto ctype = n->data.constant->type;
            switch (ctype)
            {
                case ConstantType::None:
                {
                    return LLVMVoidType();
                }
                    break;
                case ConstantType::I8:
                case ConstantType::CHAR:
                case ConstantType::U8:
                {
                    return LLVMInt8Type();
                }
                    break;
                case ConstantType::I16:
                case ConstantType::U16:
                {
                    return LLVMInt16Type();
                }
                    break;
                case ConstantType::I32:
                case ConstantType::U32:
                {
                    return LLVMInt32Type();
                }
                    break;
                case ConstantType::I64:
                case ConstantType::U64:
                {
                    return LLVMInt64Type();
                }
                    break;
                case ConstantType::F32:
                {
                    return LLVMFloatType();
                }
                    break;
                case ConstantType::F64:
                {
                    return LLVMDoubleType();
                }
                    break;
                case ConstantType::BOOL:
                {
                    return LLVMInt1Type();
                }
                    break;
                case ConstantType::STRING:
                {
                    assert(false);
                    return nullptr;
                }
                    break;
                case ConstantType::NIL:
                {
                    assert(false);
                    return nullptr;
                }
                    break;
                case ConstantType::TUPLE:
                {
                    assert(false);
                    return nullptr;
                }
                    break;
            }
            assert(false);
        }
        case NodeType::DECLARATION:
            return llvmTypeOf(n->data.declaration->type);
        case NodeType::VALUETUPLE:
            assert(false);
        case NodeType::TYPETUPLE:
        {
            if (n->data.typeTuple->isAnonymous)
            {
                auto elementCount = (unsigned int) n->data.typeTuple->types.size();
                auto elementTypes = (LLVMTypeRef*) malloc(n->data.typeTuple->types.size() * sizeof(LLVMTypeRef));
                for (auto i = 0; i < elementCount; i++)
                {
                    elementTypes[i] = llvmTypeOf(n->data.typeTuple->types[i]);
                    if (LLVMGetTypeKind(elementTypes[i]) == LLVMFunctionTypeKind)
                    {
                        elementTypes[i] = LLVMPointerType(elementTypes[i], 0);
                    }
                }

                auto isPacked = 0;
                return LLVMStructType(elementTypes, elementCount, isPacked);
            }

            if (n->data.typeTuple->opaque != nullptr)
            {
                return (LLVMTypeRef) n->data.typeTuple->opaque;
            }

            // create opaque struct type, store in the type tuple element
            LLVMTypeRef opaque = LLVMStructCreateNamed(LLVMGetGlobalContext(), "opaque");
            n->data.typeTuple->opaque = opaque;

            auto elementCount = (unsigned int) n->data.typeTuple->types.size();
            auto elementTypes = (LLVMTypeRef*) malloc(n->data.typeTuple->types.size() * sizeof(LLVMTypeRef));
            for (auto i = 0; i < elementCount; i++)
            {
                elementTypes[i] = llvmTypeOf(n->data.typeTuple->types[i]);

                // if function type, store a function pointer not a function
                if (sanitizeType(n->data.typeTuple->types[i])->type == NodeType::TYPEFUNCTION)
                {
                    elementTypes[i] = LLVMPointerType(elementTypes[i], 0);
                }
            }

            LLVMStructSetBody(opaque,
                              elementTypes,
                              elementCount,
                              0);

            return opaque;
        }
        case NodeType::PARAMETER:
            return llvmTypeOf(n->data.parameter->type);
        case NodeType::TYPEDEFINITION:
        {
            auto underlyingType = sanitize(n);
            if (underlyingType->type == NodeType::TYPETUPLE)
            {
                underlyingType->data.typeTuple->isAnonymous = false;
            }

            if (n->data.typeDefinition->resolved == nullptr)
            {
                n->data.typeDefinition->resolved = llvmTypeOf(n->data.typeDefinition->type);
            }

            if (LLVMGetTypeKind((LLVMTypeRef) n->data.typeDefinition->resolved) == LLVMFunctionTypeKind)
            {
                n->data.typeDefinition->resolved = LLVMPointerType((LLVMTypeRef) n->data.typeDefinition->resolved, 0);
            }

            return (LLVMTypeRef) n->data.typeDefinition->resolved;
        }
        case NodeType::DOT:
        {
            if (n->data.dot->resolved == nullptr || n->data.dot->resolved->type == NodeType::NONE)
            {
                resolveDot(n->data.dot);
            }
            assert(n->data.dot->resolved != nullptr && n->data.dot->resolved->type != NodeType::NONE);
            return llvmTypeOf(n->data.dot->resolved);
        }
        case NodeType::REC:
            return llvmTypeOf(n->data.rec->resolved);
        case NodeType::PARSEERROR:
            reportError(*n->data.parseError);
            exit(-1);
        default:
            assert(false);
    }

    assert(false);
}

LLVMValueRef CodegenVisitor::cg(Node* n, bool rvalue, bool force)
{
    LLVMValueRef ll;

    auto old_forcing = forcing;
    forcing = forcing || force;

    if (n->metadata.llvm == nullptr || forcing)
    {
        is_rvalue.push(rvalue);
        if (forcing)
        {
            forceVisit(n);
        }
        else
        {
            visit(n);
        }
        is_rvalue.pop();
    }

    forcing = old_forcing;

    if (n->metadata.llvm == nullptr)
    {
        return nullptr;
    }

    ll = (LLVMValueRef) n->metadata.llvm;

    auto ic = isConstant(n);
    if (rvalue && !ic)
    {
        ll = LLVMBuildLoad(builders.top(), ll, "ld_non_const");
    }

    return ll;
}

void CodegenVisitor::visitDereference(Dereference* d)
{
    auto target = cg(d->value, false);
    nodes.top()->metadata.llvm = LLVMBuildLoad(builders.top(), target, "deref");
}

void CodegenVisitor::visitAddressOf(AddressOf* a)
{
    auto g = cg(a->value, false);
    auto allocated = LLVMBuildAlloca(builders.top(), LLVMTypeOf(g), "addrof");
    LLVMBuildStore(builders.top(), g, allocated);
    nodes.top()->metadata.llvm = allocated;
}

void CodegenVisitor::visitIf(If* i)
{
    auto thenBlock = LLVMAppendBasicBlock(currentFunctions.top(), "lbl_then");

    LLVMBasicBlockRef elifBlocks[i->elif_branches.size()];
    for (auto j = 0; j < i->elif_branches.size(); j++)
    {
        elifBlocks[j] = LLVMAppendBasicBlock(currentFunctions.top(), "lbl_elif");
    }

    auto elseBlock = LLVMAppendBasicBlock(currentFunctions.top(), "lbl_else");

    // if there is an else, then we need a 'continue' block to come after the else.
    // otherwise, else will function as the continue.
    auto contBlock = LLVMAppendBasicBlock(currentFunctions.top(), "lbl_cont");

    auto g = cg(i->if_branch->condition);

    if (i->elif_branches.empty())
    {
        LLVMBuildCondBr(builders.top(), g, thenBlock, elseBlock);
        LLVMPositionBuilderAtEnd(builders.top(), elseBlock);
    }
    else
    {
        LLVMBuildCondBr(builders.top(), g, thenBlock, elifBlocks[0]);
        LLVMPositionBuilderAtEnd(builders.top(), elifBlocks[0]);
    }

    LLVMPositionBuilderAtEnd(builders.top(), thenBlock);
    for (auto s : i->if_branch->statements)
    {
        visit(s);
    }
    if (!LLVMIsAReturnInst(LLVMGetLastInstruction(thenBlock)))
    {
        visitDeferreds(nodes.top()->metadata.scope);
        LLVMBuildBr(builders.top(), contBlock);
    }

    for (auto j = 0; j < i->elif_branches.size(); j++)
    {
        LLVMBasicBlockRef elifElseBlock;
        if (j < i->elif_branches.size() - 1)
        {
            elifElseBlock = elifBlocks[j + 1];
        }
        else
        {
            elifElseBlock = elseBlock;
        }
        auto elifThenBlock = LLVMInsertBasicBlock(elifElseBlock, "lbl_elif_then");

        LLVMPositionBuilderAtEnd(builders.top(), elifBlocks[j]);
        g = cg(i->elif_branches[j]->condition);
        LLVMBuildCondBr(builders.top(), g, elifThenBlock, elifElseBlock);

        LLVMPositionBuilderAtEnd(builders.top(), elifThenBlock);
        for (auto s : i->elif_branches[j]->statements)
        {
            visit(s);
        }

        // cannot return and then br
        if (!LLVMIsAReturnInst(LLVMGetLastInstruction(elifThenBlock)))
        {
            visitDeferreds(nodes.top()->metadata.scope);
            LLVMBuildBr(builders.top(), contBlock);
        }
    }

    LLVMPositionBuilderAtEnd(builders.top(), elseBlock);
    for (auto s : i->else_branch->statements)
    {
        visit(s);
    }
    if (!LLVMIsAReturnInst(LLVMGetLastInstruction(elseBlock)))
    {
        visitDeferreds(nodes.top()->metadata.scope);
        LLVMBuildBr(builders.top(), contBlock);
    }

    LLVMPositionBuilderAtEnd(builders.top(), contBlock);
}

void CodegenVisitor::visitDo(Do* d)
{
    for (auto s : d->statements)
    {
        visit(s);
    }

    // result
    if (d->identifier != nullptr)
    {
        nodes.top()->metadata.llvm = cg(d->identifier, false);
    }
}

void CodegenVisitor::visitWhile(While* w)
{
    auto g = cg(w->condition);

    auto whileBlock = LLVMAppendBasicBlock(currentFunctions.top(), "while");
    auto contBlock = LLVMAppendBasicBlock(currentFunctions.top(), "cont");

    LLVMBuildCondBr(builders.top(), g, whileBlock, contBlock);

    LLVMPositionBuilderAtEnd(builders.top(), whileBlock);

    currentScopes.push(whileBlock);
    for (auto s : w->statements)
    {
        visit(s);
    }
    if (!LLVMIsAReturnInst(LLVMGetLastInstruction(whileBlock)))
    {
        visitDeferreds(nodes.top()->metadata.scope);

        // at the very end, codegen and possibly repeat
        auto newG = cg(w->condition, true, true);
        LLVMBuildCondBr(builders.top(), newG, whileBlock, contBlock);
    }

    LLVMPositionBuilderAtEnd(builders.top(), contBlock);
    currentScopes.pop();
}

void CodegenVisitor::visitNot(Not *n)
{
    nodes.top()->metadata.llvm = LLVMBuildNot(builders.top(), cg(n->value), "not");
}

void CodegenVisitor::visitQuestion(Question *q)
{
    auto q_type = llvmTypeOf(typeOf(q->value));
    nodes.top()->metadata.llvm = LLVMBuildNot(builders.top(),
                                              LLVMBuildICmp(builders.top(),
                                                            LLVMIntEQ,
                                                            cg(q->value),
                                                            LLVMConstNull(q_type),
                                                            "?"),
                                              "not ?");
}

void CodegenVisitor::visitDefer(Defer* d)
{
    // keep track of the boolean and the defer statement
    nodes.top()->metadata.scope->parent->deferreds.push_back(d);
}

void CodegenVisitor::visitBinOp(BinOp* b)
{
    if (b->type == OpType::PIPE)
    {
        auto rhsType = typeOf(b->rhs);
        if (rhsType->type != NodeType::TYPEFUNCTION)
        {
            // not a function, must be an apply
            rhsType = sanitize(b->rhs);
            assert(rhsType->type == NodeType::APPLY);

            auto firstArg = typeOf(b->lhs);
            auto otherArgs = rhsType->data.apply->operands;
            auto func = rhsType->data.apply->operator_;

            auto funcType = typeOf(func);
            assert(funcType->type == NodeType::TYPEFUNCTION);
            auto expectedArgTypes = funcType->data.typeFunction->input;
            if (typesMatch(expectedArgTypes.types[0], firstArg))
            {
                auto llvmFunc = cg(func);

                auto funcType = (llvm::Type*) LLVMTypeOf(llvmFunc);
                assert(funcType->isPointerTy());
                funcType = funcType->getContainedType(0);
                auto returnType = LLVMGetReturnType((LLVMTypeRef) funcType);
                bool isVoidFunc = LLVMGetTypeKind(returnType) == LLVMVoidTypeKind;

                auto name = "";
                if (!isVoidFunc)
                {
                    name = "apl";
                }

                unsigned int parameterCount = (unsigned int) expectedArgTypes.types.size();
                auto params = (LLVMValueRef*) malloc(parameterCount * sizeof(LLVMValueRef));
                auto llvmParams1 = cg(b->lhs);
                params[0] = llvmParams1;
                for (auto i = 0; i < rhsType->data.apply->operands.size(); i++)
                {
                    params[1 + i] = cg(rhsType->data.apply->operands[i]);
                }

                auto call = LLVMBuildCall(builders.top(),
                                          llvmFunc,
                                          params,
                                          parameterCount,
                                          name);
                nodes.top()->metadata.llvm = call;
            }
            else
            {
                assert(false);
            }
        }
        else
        {
            auto lhsType = typeOf(b->lhs);
            if (lhsType->type != NodeType::TYPETUPLE)
            {
                b->lhs = valueTuple({parameter("", typeOf(b->lhs), b->lhs, 0, Region{})}, Region{});
            }

            auto func = cg(b->rhs);

            auto funcType = (llvm::Type*) LLVMTypeOf(func);
            assert(funcType->isPointerTy());
            funcType = funcType->getContainedType(0);
            auto returnType = LLVMGetReturnType((LLVMTypeRef) funcType);
            bool isVoidFunc = LLVMGetTypeKind(returnType) == LLVMVoidTypeKind;

            auto name = "";
            if (!isVoidFunc)
            {
                name = "apl";
            }

            auto realFuncType = typeOf(b->rhs);
            assert(realFuncType->type == NodeType::TYPEFUNCTION);
            auto expectedArgs = realFuncType->data.typeFunction->input.types;

            unsigned int parameterCount = (unsigned int) expectedArgs.size();
            auto params = (LLVMValueRef*) malloc(parameterCount * sizeof(LLVMValueRef));
            auto llvmParams = cg(b->lhs);

            lhsType = typeOf(b->lhs);
            assert(lhsType->type == NodeType::TYPETUPLE);
            if (typesMatch(expectedArgs[0], lhsType->data.typeTuple->types[0]))
            {
                // types match, destructure
                for (auto i = 0; i < parameterCount; i++)
                {
                    params[i] = LLVMBuildExtractValue(builders.top(), llvmParams, (unsigned int) i, "extract_struct");
                }
            }
            else
            {
                // types do not match, just copy over
                params[0] = llvmParams;
            }

//            LLVMDumpValue(func);
//            for (auto i = 0; i < parameterCount; i++)
//            {
//                LLVMDumpValue(params[i]);
//            }

            auto call = LLVMBuildCall(builders.top(),
                                      func,
                                      params,
                                      parameterCount,
                                      name);

            nodes.top()->metadata.llvm = call;
        }
        return;
    }

    if (b->type == OpType::OR)
    {
        auto storedTruth = LLVMBuildAlloca(builders.top(), LLVMInt1Type(), "short_circuit_truth");

        auto falseBlock = LLVMAppendBasicBlock(currentFunctions.top(), "ss_false");
        auto trueBlock = LLVMAppendBasicBlock(currentFunctions.top(), "ss_true");
        auto contBlock = LLVMAppendBasicBlock(currentFunctions.top(), "ss_cont");

        auto lhs = cg(b->lhs);

        LLVMBuildCondBr(builders.top(), lhs, trueBlock, falseBlock);

        LLVMPositionBuilderAtEnd(builders.top(), trueBlock);
        LLVMBuildStore(builders.top(), LLVMConstInt(LLVMInt1Type(), 1, 0), storedTruth);
        LLVMBuildBr(builders.top(), contBlock);

        LLVMPositionBuilderAtEnd(builders.top(), falseBlock);
        auto rhs = cg(b->rhs);
        LLVMBuildStore(builders.top(), rhs, storedTruth);
        LLVMBuildBr(builders.top(), contBlock);

        LLVMPositionBuilderAtEnd(builders.top(), contBlock);
        nodes.top()->metadata.llvm = LLVMBuildLoad(builders.top(), storedTruth, "loaded_truth");

        return;
    }
    if (b->type == OpType::AND)
    {
        auto storedTruth = LLVMBuildAlloca(builders.top(), LLVMInt1Type(), "short_circuit_truth");

        auto falseBlock = LLVMAppendBasicBlock(currentFunctions.top(), "ss_false");
        auto trueBlock = LLVMAppendBasicBlock(currentFunctions.top(), "ss_true");
        auto contBlock = LLVMAppendBasicBlock(currentFunctions.top(), "ss_cont");

        auto lhs = cg(b->lhs);

        LLVMBuildCondBr(builders.top(), lhs, trueBlock, falseBlock);

        LLVMPositionBuilderAtEnd(builders.top(), trueBlock);
        auto rhs = cg(b->rhs);
        auto andedTruth = LLVMBuildAnd(builders.top(), lhs, rhs, "and");
        LLVMBuildStore(builders.top(), andedTruth, storedTruth);
        LLVMBuildBr(builders.top(), contBlock);

        LLVMPositionBuilderAtEnd(builders.top(), falseBlock);
        LLVMBuildStore(builders.top(), LLVMConstInt(LLVMInt1Type(), 0, 0), storedTruth);
        LLVMBuildBr(builders.top(), contBlock);

        LLVMPositionBuilderAtEnd(builders.top(), contBlock);
        nodes.top()->metadata.llvm = LLVMBuildLoad(builders.top(), storedTruth, "loaded_truth");

        return;
    }

    // after this point, we can safely codegen both sides
    auto lhs = cg(b->lhs);
    auto rhs = cg(b->rhs);

    auto typekind_lhs = LLVMGetTypeKind(LLVMTypeOf(lhs));
    auto typekind_rhs = LLVMGetTypeKind(LLVMTypeOf(rhs));

    auto is_unsigned = false;
    auto lhsType = sanitizeType(typeOf(b->lhs));
    if (lhsType->type == NodeType::TYPECONSTANT)
    {
        auto c_type = lhsType->data.typeConstant->type;
        if (c_type == ConstantType::U8
            || c_type == ConstantType::U16
            || c_type == ConstantType::U32
            || c_type == ConstantType::U64)
        {
            is_unsigned = true;
        }
    }

    if (b->type == OpType::EQEQ)
    {
        if (typekind_lhs == LLVMIntegerTypeKind)
        {
            nodes.top()->metadata.llvm = LLVMBuildICmp(builders.top(), LLVMIntEQ, lhs, rhs, "eqeq");
        }
        else
        {
            nodes.top()->metadata.llvm = LLVMBuildFCmp(builders.top(), LLVMRealOEQ, lhs, rhs, "eqeq");
        }
        return;
    }
    if (b->type == OpType::NEQ)
    {
        if (typekind_lhs == LLVMIntegerTypeKind)
        {
            nodes.top()->metadata.llvm = LLVMBuildNot(builders.top(), LLVMBuildICmp(builders.top(), LLVMIntEQ, lhs, rhs, "eqeq"), "neq");
        }
        else
        {
            nodes.top()->metadata.llvm = LLVMBuildNot(builders.top(), LLVMBuildFCmp(builders.top(), LLVMRealOEQ, lhs, rhs, "eqeq"), "neq");
        }
        return;
    }
    if (b->type == OpType::LT)
    {
        if (typekind_lhs == LLVMIntegerTypeKind)
        {
            if (is_unsigned)
            {
                nodes.top()->metadata.llvm = LLVMBuildICmp(builders.top(), LLVMIntULT, lhs, rhs, "ult");
            }
            else
            {
                nodes.top()->metadata.llvm = LLVMBuildICmp(builders.top(), LLVMIntSLT, lhs, rhs, "slt");
            }
        }
        else
        {
            nodes.top()->metadata.llvm = LLVMBuildFCmp(builders.top(), LLVMRealOLT, lhs, rhs, "olt");
        }
        return;
    }
    if (b->type == OpType::LTE)
    {
        if (typekind_lhs == LLVMIntegerTypeKind)
        {
            if (is_unsigned)
            {
                nodes.top()->metadata.llvm = LLVMBuildICmp(builders.top(), LLVMIntULE, lhs, rhs, "ule");
            }
            else
            {
                nodes.top()->metadata.llvm = LLVMBuildICmp(builders.top(), LLVMIntSLE, lhs, rhs, "sle");
            }
        }
        else
        {
            nodes.top()->metadata.llvm = LLVMBuildFCmp(builders.top(), LLVMRealOLE, lhs, rhs, "ole");
        }
        return;
    }
    if (b->type == OpType::GT)
    {
        if (typekind_lhs == LLVMIntegerTypeKind)
        {
            if (is_unsigned)
            {
                nodes.top()->metadata.llvm = LLVMBuildICmp(builders.top(), LLVMIntUGT, lhs, rhs, "ugt");
            }
            else
            {
                nodes.top()->metadata.llvm = LLVMBuildICmp(builders.top(), LLVMIntSGT, lhs, rhs, "sgt");
            }
        }
        else
        {
            nodes.top()->metadata.llvm = LLVMBuildFCmp(builders.top(), LLVMRealOGT, lhs, rhs, "ogt");
        }
        return;
    }
    if (b->type == OpType::GTE)
    {
        if (typekind_lhs == LLVMIntegerTypeKind)
        {
            if (is_unsigned)
            {
                nodes.top()->metadata.llvm = LLVMBuildICmp(builders.top(), LLVMIntUGE, lhs, rhs, "uge");
            }
            else
            {
                nodes.top()->metadata.llvm = LLVMBuildICmp(builders.top(), LLVMIntSGE, lhs, rhs, "sge");
            }
        }
        else
        {
            nodes.top()->metadata.llvm = LLVMBuildFCmp(builders.top(), LLVMRealOGE, lhs, rhs, "oge");
        }
        return;
    }

    nodes.top()->metadata.llvm = LLVMBuildBinOp(builders.top(),
                   get_op_code(b->type,
                               LLVMGetTypeKind(LLVMTypeOf(lhs)),
                               is_unsigned),
                   lhs,
                   rhs,
                   "binop");
    if (b->is_eq)
    {
        auto lhs_ptr = cg(b->lhs, false);
        auto st = (LLVMValueRef) nodes.top()->metadata.llvm;
        LLVMBuildStore(builders.top(), st, lhs_ptr);
        nodes.top()->metadata.llvm = lhs_ptr;
    }
}

void CodegenVisitor::visitValueTuple(ValueTuple* v)
{
    // allocate
    auto nodeType = typeOf(nodes.top());

    if (nodes.top()->metadata.scope->isModuleLevel)
    {
        auto count = v->values.size();
        auto vals = (LLVMValueRef *) malloc(count * sizeof(LLVMValueRef));
        for (int i = 0; i < count; i++)
        {
            assert(v->values[i]->type == NodeType::PARAMETER);
            vals[i] = cg(v->values[i]->data.parameter->initialValue);
        }

        auto packed = 0;
        nodes.top()->metadata.llvm = LLVMConstStruct(vals, (unsigned int) count, packed);
        return;
    }

    auto alc = LLVMBuildAlloca(builders.top(), llvmTypeOf(nodeType), "alloca_struct");

    // store
    for (auto i = 0; i < v->values.size(); i++)
    {
        assert(v->values[i]->type == NodeType::PARAMETER);
        auto cgv = v->values[i]->data.parameter->initialValue;
        auto c = cg(cgv);
        auto gep = LLVMBuildStructGEP(builders.top(), alc, (unsigned int) i, "struct_gep");
        LLVMBuildStore(builders.top(), c, gep);
    }

    nodes.top()->metadata.llvm = alc;
}

void CodegenVisitor::visitDot(Dot* d)
{
    resolveDot(d);

    if (d->resolved != nullptr && d->resolved->type != NodeType::NONE)
    {
        nodes.top()->metadata.llvm = cg(d->resolved, false);
        if (nodes.top()->metadata.llvm != nullptr)
        {
            return;
        }
    }

    Node* realLhs = nodes.top()->data.dot->lhs;
    if (realLhs->type == NodeType::SYMBOL)
    {
        realLhs = nodes.top()->data.dot->lhs->data.symbol->resolved;
    }
    if (realLhs->type == NodeType::DOT)
    {
        if (realLhs->data.dot->resolved == nullptr)
        {
            nodes.push(realLhs);
            visitDot(realLhs->data.dot);
            nodes.pop();
        }
        assert(realLhs->data.dot->resolved != nullptr);
        realLhs = realLhs->data.dot->resolved;
    }

    if (nodes.top()->data.dot->rhs->type == NodeType::CONSTANT
        && nodes.top()->data.dot->rhs->data.constant->type == ConstantType::I32
        && sanitizeType(typeOf(d->lhs))->type == NodeType::TYPETUPLE)
    {
        int fieldIndex = nodes.top()->data.dot->rhs->data.constant->data.intvalue;

        auto c = cg(d->lhs, false);
        nodes.top()->data.dot->resolved = none(nodes.top()->metadata.region);
        if (LLVMGetTypeKind(LLVMTypeOf(c)) == LLVMPointerTypeKind)
        {
            auto indices = (LLVMValueRef*) malloc(2 * sizeof(LLVMValueRef));
            indices[0] = LLVMConstInt(LLVMInt32Type(), (unsigned long long int) 0, 0);
            indices[1] = LLVMConstInt(LLVMInt32Type(), (unsigned long long int) fieldIndex, 0);

            nodes.top()->metadata.llvm = LLVMBuildGEP(builders.top(), c, indices, 2, "struct_extract");
        }
        else
        {
            auto realValue = LLVMBuildExtractValue(builders.top(), c, (unsigned int) fieldIndex, "struct_extract");
            nodes.top()->metadata.llvm = LLVMBuildAlloca(builders.top(), LLVMTypeOf(realValue), "alloca_dot");
            LLVMBuildStore(builders.top(), realValue, (LLVMValueRef) nodes.top()->metadata.llvm);
        }
        return;
    }

    if (nodes.top()->data.dot->rhs->type != NodeType::SYMBOL)
    {
        errors.push_back(ParseError{
            fmt::format("expected identifier, not {}", typeDescription(typeOf(nodes.top()->data.dot->rhs))),
            nodes.top()->data.dot->rhs->metadata.region
        });
        return;
    }

    auto nameToMatch = nodes.top()->data.dot->rhs->data.symbol->value;

    if (realLhs->type == NodeType::MODULE)
    {
        // find symbol
        for (auto s : realLhs->data.module->statements)
        {
            if (s->type == NodeType::MODULE
                && s->data.module->name == nameToMatch)
            {
                nodes.top()->data.dot->resolved = s;
                break;
            }
            else if (s->type == NodeType::DECLARATION
                && s->data.declaration->name == nameToMatch)
            {
                nodes.top()->data.dot->resolved = s;
                nodes.top()->metadata.llvm = cg(s);
                break;
            }
            else if (s->type == NodeType::TYPEDEFINITION
                && s->data.typeDefinition->name == nameToMatch)
            {
                nodes.top()->data.dot->resolved = s;
                break;
            }
        }

        return;
    }

    auto c = cg(d->lhs, false);

    auto lhsType = typeOf(d->lhs);
    lhsType = sanitizeType(lhsType);

    lhsType = sanitize(lhsType);
    while (lhsType->type == NodeType::TYPEPOINTER)
    {
        lhsType = lhsType->data.typePointer->type;

        // if pointer to pointer, then load
        auto llvmType = (llvm::Type*) LLVMTypeOf(c);
        if (llvmType->isPointerTy() && llvmType->getContainedType(0)->isPointerTy())
        {
            c = LLVMBuildLoad(builders.top(), c, "ld_dot");
        }
    }
    while (lhsType->type == NodeType::SYMBOL && !isNumericType(lhsType->data.symbol->value))
    {
        lhsType = lhsType->data.symbol->resolved;
    }
    while (lhsType->type == NodeType::TYPEDEFINITION)
    {
        lhsType = lhsType->data.typeDefinition->type;
    }
    if (lhsType->type == NodeType::TYPECONSTANT)
    {
        if (lhsType->data.typeConstant->type == ConstantType::STRING)
        {
            if (d->rhs->data.symbol->value == "ptr")
            {
                LLVMValueRef indices[] = {
                    LLVMConstInt(LLVMInt32Type(), 0, 0),
                    LLVMConstInt(LLVMInt32Type(), 1, 0)
                };

                if (LLVMGetTypeKind(LLVMTypeOf(c)) == LLVMPointerTypeKind)
                {
                    nodes.top()->metadata.llvm = LLVMBuildGEP(builders.top(), c, indices, 2, "slice_data_gep");
                }
                else
                {
                    nodes.top()->metadata.llvm = LLVMBuildExtractValue(builders.top(), c, 1, "slice_extract");
                }
                return;
            }
            else if (d->rhs->data.symbol->value == "length")
            {
                nodes.top()->metadata.llvm = get_array_length(c, builders.top());
                return;
            }

            assert(false);
            return;
        }

        assert(false);
        return;
    }

    assert(lhsType->type == NodeType::TYPETUPLE);

    for (auto i = 0; i < lhsType->data.typeTuple->types.size(); i++)
    {
        assert(lhsType->data.typeTuple->types[i]->type == NodeType::PARAMETER);
        if (lhsType->data.typeTuple->types[i]->data.parameter->name == nameToMatch)
        {
            nodes.top()->data.dot->resolved = none(nodes.top()->metadata.region);
            if (LLVMGetTypeKind(LLVMTypeOf(c)) == LLVMPointerTypeKind)
            {
                auto indices = (LLVMValueRef*) malloc(2 * sizeof(LLVMValueRef));
                indices[0] = LLVMConstInt(LLVMInt32Type(), (unsigned long long int) 0, 0);
                indices[1] = LLVMConstInt(LLVMInt32Type(), (unsigned long long int) i, 0);

                nodes.top()->metadata.llvm = LLVMBuildGEP(builders.top(), c, indices, 2, "struct_extract");
            }
            else
            {
                auto realValue = LLVMBuildExtractValue(builders.top(), c, (unsigned int) i, "struct_extract");
                nodes.top()->metadata.llvm = LLVMBuildAlloca(builders.top(), LLVMTypeOf(realValue), "alloca_dot");
                LLVMBuildStore(builders.top(), realValue, (LLVMValueRef) nodes.top()->metadata.llvm);
            }
        }
    }
}

void CodegenVisitor::visitNeg(Neg* n)
{
    auto val = cg(n->value);

    if (LLVMGetTypeKind(LLVMTypeOf(val)) == LLVMFloatTypeKind)
    {
        nodes.top()->metadata.llvm = LLVMBuildFNeg(builders.top(), val, "neg");
    }
    else
    {
        nodes.top()->metadata.llvm = LLVMBuildNeg(builders.top(), val, "neg");
    }
}

void CodegenVisitor::visitCast(Cast* c)
{
    auto val = cg(c->value);

    auto castTo = llvmTypeOf(c->type);
    if (LLVMGetTypeKind(castTo) == LLVMFunctionTypeKind)
    {
        castTo = LLVMPointerType(castTo, 0);
    }

    auto casted = LLVMBuildBitCast(builders.top(), val, castTo, "bitcast");
    nodes.top()->metadata.llvm = LLVMBuildAlloca(builders.top(), LLVMTypeOf(casted), "alloca_cast");
    LLVMBuildStore(builders.top(), casted, (LLVMValueRef) nodes.top()->metadata.llvm);
}

void CodegenVisitor::visitHeap(Heap* h)
{
    auto val = cg(h->value);

    // TODO(chad): allow for user-defined allocator instead
    auto pointer_to_val = LLVMBuildMalloc(builders.top(), LLVMTypeOf(val), "heap_decl");

    LLVMBuildStore(builders.top(), val, pointer_to_val);

    nodes.top()->metadata.llvm = pointer_to_val;
}

void CodegenVisitor::visitSizeof(Sizeof* s)
{
    auto type = llvmTypeOf(s->type);
    auto nullType = LLVMConstPointerNull(LLVMPointerType(type, 0));
    LLVMValueRef indices[1] = { LLVMConstInt(LLVMInt32Type(), 1, 0) };
    auto sizeGEP = LLVMBuildGEP(builders.top(), nullType, indices, 1, "fake_nullptr_sizeof");
    auto size = LLVMBuildPtrToInt(builders.top(), sizeGEP, LLVMInt32Type(), "casted_ptr_to_int");
    nodes.top()->metadata.llvm = size;
}

void CodegenVisitor::visitAssert(Assert* a)
{
    auto val = cg(a->value);

    // build conditional trap
    auto trapBlock = LLVMAppendBasicBlock(currentFunctions.top(), "lbl_then");
    auto contBlock = LLVMAppendBasicBlock(currentFunctions.top(), "lbl_cont");

    LLVMBuildCondBr(builders.top(), val, contBlock, trapBlock);

    LLVMPositionBuilderAtEnd(builders.top(), trapBlock);
    llvm::Function* trapFunc = llvm::Intrinsic::getDeclaration((llvm::Module*) mod, llvm::Intrinsic::trap);
    LLVMBuildCall(builders.top(), (LLVMValueRef) trapFunc, {}, 0, "");
    LLVMBuildBr(builders.top(), contBlock);

    LLVMPositionBuilderAtEnd(builders.top(), contBlock);
}

void CodegenVisitor::visitStringLiteral(StringLiteral* s)
{
    if (nodes.top()->metadata.scope->isModuleLevel && builders.empty())
    {
        return;
    }

    LLVMValueRef const_str = LLVMConstString(s->s.c_str(), (unsigned int) s->s.size(), 1);
    LLVMValueRef storage = LLVMBuildAlloca(builders.top(), LLVMTypeOf(const_str), "storage_str");
    LLVMBuildStore(builders.top(), const_str, storage);

    auto bitcast_storage = LLVMBuildBitCast(builders.top(),
                            storage,
                            LLVMPointerType(LLVMInt8Type(), 0),
                            "bc_str");

    auto string_slice = LLVMBuildAlloca(builders.top(), llvmTypeOf(getStringType()), "string_slice");
    nodes.top()->metadata.llvm = string_slice;
    auto length_field = LLVMBuildStructGEP(builders.top(), string_slice, 0, "length_field");
    LLVMBuildStore(builders.top(), LLVMConstInt(LLVMInt32Type(), s->s.length(), 0), length_field);
    auto ptr_field = LLVMBuildStructGEP(builders.top(), string_slice, 1, "ptr_field");
    LLVMBuildStore(builders.top(), bitcast_storage, ptr_field);
}

void CodegenVisitor::visitCharacter(Character *c)
{
    nodes.top()->metadata.llvm = LLVMConstInt(LLVMInt8Type(), (int) c->c, 0);
}

void CodegenVisitor::visitPipe(Pipe* p)
{
    nodes.top()->metadata.llvm = cg(p->resolved);
}

void CodegenVisitor::visitRequire(Require* r) { }

void CodegenVisitor::visitLabel(Label *l)
{
    LLVMBasicBlockRef label;
    if (!l->created)
    {
        l->created = true;
        label = LLVMAppendBasicBlock(currentFunctions.top(), l->name.c_str());
    }
    else
    {
        label = (LLVMBasicBlockRef) nodes.top()->metadata.llvm;
    }

    LLVMBuildBr(builders.top(), label);
    LLVMPositionBuilderAtEnd(builders.top(), label);
    nodes.top()->metadata.llvm = label;
}

void CodegenVisitor::visitJump(Jump *j)
{
    if (!j->resolved->data.label->created)
    {
        j->resolved->data.label->created = true;
        auto label = LLVMAppendBasicBlock(currentFunctions.top(), j->name.c_str());
        j->resolved->metadata.llvm = label;
    }

    LLVMBuildBr(builders.top(), (LLVMBasicBlockRef) j->resolved->metadata.llvm);
}

bool CodegenVisitor::isConstant(Node* n)
{
    auto ntype = n->type;

    switch (ntype)
    {
        case NodeType::TYPEPOINTER:
            return isConstant(n->data.typePointer->type);
        case NodeType::ADDRESSOF:
            return isConstant(n->data.addressOf->value);
        case NodeType::DEREFERENCE:
            return isConstant(n->data.dereference->value);
        case NodeType::SYMBOL:
            return isConstant(n->data.symbol->resolved);
        case NodeType::NEG:
            return true;
        case NodeType::NOT:
            return isConstant(n->data.not_->value);
        case NodeType::DECLARATION:
            if (n->data.declaration->global) { return false; } // always need to load global variables
            if (n->metadata.scope->isModuleLevel)
            {
                return n->data.declaration->declarationType == DeclarationType::IMMUTABLE;
            }
            return n->data.declaration->external;
        case NodeType::APPLY:
        {
            auto realFunc = sanitize(n->data.apply->operator_);
            while (realFunc->type == NodeType::SYMBOL)
            {
                realFunc = realFunc->data.symbol->resolved;
            }

            auto funcType = sanitizeType(typeOf(n->data.apply->operator_));

            return funcType->type == NodeType::TYPECONSTANT
                   && funcType->data.typeConstant->type != ConstantType::STRING;
        }
        case NodeType::DOT:
        {
            if (n->data.dot->resolved != nullptr && n->data.dot->resolved->type != NodeType::NONE)
            {
                return isConstant(n->data.dot->resolved);
            }

            // if this dot has a module as an lhs, then it is constant.
            auto sanitized = sanitize(n->data.dot->lhs);
            if (sanitized->type == NodeType::MODULE)
            {
                return true;
            }
            if (sanitized->type == NodeType::TYPECONSTANT
                && sanitized->data.typeConstant->type == ConstantType::STRING)
            {
                return n->data.dot->rhs->data.symbol->value == "ptr";
            }
            return false;
        }
        case NodeType::DO:
        {
            return isConstant(n->data.do_->identifier);
        }
        case NodeType::BINOP:
        {
            return !n->data.binop->is_eq;
        }
        case NodeType::CAST:
        case NodeType::VALUETUPLE:
        case NodeType::STRINGLITERAL:
            return false;
        case NodeType::REC:
        case NodeType::HEAP:
        case NodeType::ARGUMENT:
        case NodeType::PARAMETER:
        case NodeType::FUNCTION:
        case NodeType::SIZEOF:
        case NodeType::CONSTANT:
        case NodeType::CHARACTER:
        case NodeType::TYPEDEFINITION:
        case NodeType::PIPE:
        case NodeType::QUESTION:
        case NodeType::LABEL:
        case NodeType::JUMP:
            return true;
        default:
            auto debug = ntype;
            assert(false);
    }
}

LLVMValueRef CodegenVisitor::convertToNumericType(Node* n, ConstantType type)
{
    // TODO(chad)
    if (type == ConstantType::I8
        || type == ConstantType::CHAR
        || type == ConstantType::U8)
    {
        return convertToI8(n);
    }
    if (type == ConstantType::I16
        || type == ConstantType::U16)
    {
        return ConvertToI16(n);
    }
    if (type == ConstantType::I32
        || type == ConstantType::U32)
    {
        return convertToI32(n);
    }
    else if (type == ConstantType::I64
        || type == ConstantType::U64)
    {
        return convertToI64(n);
    }
    else if (type == ConstantType::F32)
    {
        return convertToF32(n);
    }
    else if (type == ConstantType::F64)
    {
        return convertToF64(n);
    }

    assert(false);
    return nullptr;
}

LLVMValueRef CodegenVisitor::convertToI8(Node* n)
{
    auto typ = sanitizeType(typeOf(n));

    switch (typ->type)
    {
        case NodeType::TYPECONSTANT:
        {
            auto tc = typ->data.typeConstant;

            if (tc->type == ConstantType::I8
                || tc->type == ConstantType::CHAR
                || tc->type == ConstantType::U8)
            {
                return cg(n);
            }
            else if (tc->type == ConstantType::I32
                     || tc->type == ConstantType::U32
                     || tc->type == ConstantType::I64
                     || tc->type == ConstantType::U64)
            {
                return LLVMBuildTrunc(builders.top(), cg(n), LLVMInt8Type(), "trunc_i64_i32");
            }
            else if (tc->type == ConstantType::F32
                     || tc->type == ConstantType::F64)
            {
                return LLVMBuildFPToSI(builders.top(), cg(n), LLVMInt8Type(), "fp_to_si");
            }

            assert(false);
        }
        default:
            assert(false);
            break;
    }

    assert(false);
    return nullptr;
}

LLVMValueRef CodegenVisitor::ConvertToI16(Node* n)
{
    auto typ = sanitizeType(typeOf(n));

    switch (typ->type)
    {
        case NodeType::TYPECONSTANT:
        {
            auto tc = typ->data.typeConstant;

            if (tc->type == ConstantType::I8
                || tc->type == ConstantType::CHAR
                || tc->type == ConstantType::U8)
            {
                return LLVMBuildZExt(builders.top(), cg(n), LLVMInt32Type(), "zext_i8_i32");
            }
            else if (tc->type == ConstantType::I16
                     || tc->type == ConstantType::U16)
            {
                return cg(n);
            }
            else if (tc->type == ConstantType::I32
                     || tc->type == ConstantType::U32
                     || tc->type == ConstantType::I64
                     || tc->type == ConstantType::U64)
            {
                return LLVMBuildTrunc(builders.top(), cg(n), LLVMInt8Type(), "trunc_to_i32");
            }
            else if (tc->type == ConstantType::F32
                     || tc->type == ConstantType::F64)
            {
                return LLVMBuildFPToSI(builders.top(), cg(n), LLVMInt8Type(), "fp_to_si");
            }

            assert(false);
        }
        default:
            assert(false);
            break;
    }

    assert(false);
    return nullptr;
}

LLVMValueRef CodegenVisitor::convertToI32(Node* n)
{
    auto typ = sanitizeType(typeOf(n));

    switch (typ->type)
    {
        case NodeType::TYPECONSTANT:
        {
            auto tc = typ->data.typeConstant;

            if (tc->type == ConstantType::I8
                || tc->type == ConstantType::CHAR
                || tc->type == ConstantType::U8
                || tc->type == ConstantType::I16
                || tc->type == ConstantType::U16)
            {
                return LLVMBuildZExt(builders.top(), cg(n), LLVMInt32Type(), "zext_i8_i32");
            }
            else if (tc->type == ConstantType::I32
                || tc->type == ConstantType::U32)
            {
                return cg(n);
            }
            else if (tc->type == ConstantType::I64
                || tc->type == ConstantType::U64)
            {
                return LLVMBuildTrunc(builders.top(), cg(n), LLVMInt32Type(), "trunc_i64_i32");
            }
            else if (tc->type == ConstantType::F32
                     || tc->type == ConstantType::F64)
            {
                return LLVMBuildFPToSI(builders.top(), cg(n), LLVMInt32Type(), "fp_to_si");
            }

            assert(false);
        }
        default:
            assert(false);
            break;
    }

    assert(false);
    return nullptr;
}

LLVMValueRef CodegenVisitor::convertToI64(Node* n)
{
    auto typ = typeOf(n);

    switch (typ->type)
    {
        case NodeType::TYPECONSTANT:
        {
            auto tc = typ->data.typeConstant;

            if (tc->type == ConstantType::I64
                || tc->type == ConstantType::U64)
            {
                return cg(n);
            }
            else if (tc->type == ConstantType::I8
                     || tc->type == ConstantType::U8
                     || tc->type == ConstantType::CHAR
                     || tc->type == ConstantType::I16
                     || tc->type == ConstantType::U16
                     || tc->type == ConstantType::I32
                     || tc->type == ConstantType::U32)
            {
                return LLVMBuildZExt(builders.top(), cg(n), LLVMInt64Type(), "zext_i32_i64");
            }
            else if (tc->type == ConstantType::F32
                || tc->type == ConstantType::F64)
            {
                return LLVMBuildFPToSI(builders.top(), cg(n), LLVMInt64Type(), "fp_to_si");
            }

            assert(false);
        }
        default:
            assert(false);
            break;
    }

    assert(false);
    return nullptr;
}

LLVMValueRef CodegenVisitor::convertToF32(Node* n)
{
    auto typ = typeOf(n);

    switch (typ->type)
    {
        case NodeType::TYPECONSTANT:
        {
            auto tc = typ->data.typeConstant;

            if (tc->type == ConstantType::I8
                || tc->type == ConstantType::U8
                || tc->type == ConstantType::CHAR
                || tc->type == ConstantType::I16
                || tc->type == ConstantType::U16
                || tc->type == ConstantType::I32
                || tc->type == ConstantType::U32
                || tc->type == ConstantType::I64
                || tc->type == ConstantType::U64)
            {
                return LLVMBuildSIToFP(builders.top(), cg(n), LLVMFloatType(), "si_to_fp");
            }
            else if (tc->type == ConstantType::F32)
            {
                return cg(n);
            }
            else if (tc->type == ConstantType::F64)
            {
                return LLVMBuildFPExt(builders.top(), cg(n), LLVMDoubleType(), "fpext_f32_f64");
            }

            assert(false);
        }
        default:
            assert(false);
            break;
    }

    assert(false);
    return nullptr;
}

LLVMValueRef CodegenVisitor::convertToF64(Node* n)
{
    auto typ = typeOf(n);

    switch (typ->type)
    {
        case NodeType::TYPECONSTANT:
        {
            auto tc = typ->data.typeConstant;

            if (tc->type == ConstantType::I8
                || tc->type == ConstantType::U8
                || tc->type == ConstantType::CHAR
                || tc->type == ConstantType::I16
                || tc->type == ConstantType::U16
                || tc->type == ConstantType::I32
                || tc->type == ConstantType::U32
                || tc->type == ConstantType::I64
                || tc->type == ConstantType::U64)
            {
                return LLVMBuildSIToFP(builders.top(), cg(n), LLVMDoubleType(), "si2fp");
            }
            else if (tc->type == ConstantType::F32)
            {
                return LLVMBuildFPExt(builders.top(), cg(n), LLVMDoubleType(), "fpext_f32_to_f64");
            }
            else if (tc->type == ConstantType::F64)
            {
                return cg(n);
            }

            assert(false);
        }
        default:
            assert(false);
            break;
    }

    assert(false);
    return nullptr;
}

LLVMValueRef get_array_length(LLVMValueRef c, LLVMBuilderRef builder)
{
    if (LLVMGetTypeKind(LLVMTypeOf(c)) == LLVMPointerTypeKind)
    {
        LLVMValueRef indices[] = {
            LLVMConstInt(LLVMInt32Type(), 0, 0),
            LLVMConstInt(LLVMInt32Type(), 0, 0)
        };

        return LLVMBuildGEP(builder, c, indices, 2, "slice_size_gep");
    }
    else
    {
        auto realValue = LLVMBuildExtractValue(builder, c, (unsigned int) 0, "struct_extract");
        auto ret = LLVMBuildAlloca(builder, LLVMTypeOf(realValue), "alloca_dot");
        LLVMBuildStore(builder, realValue, ret);
        return ret;
    }
}

void CodegenVisitor::visitDeferreds(Scope *s, bool ret)
{
    for (auto d : s->deferreds)
    {
        visitMany(d->statements);
    }

    if (ret && (s->parent != nullptr) && (s->importsNonStatic))
    {
        visitDeferreds(s->parent, ret);
    }
}
