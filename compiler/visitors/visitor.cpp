#include "visitor.h"

#define PRINT_LOCATION true

bool typesMatch(Node *t1, Node *t2);

// Visitor
void Visitor::visit_base(Node *n)
{
    visited[n] = nullptr;

    nodes.push(n);

    switch (n->type)
    {
        case NodeType::NONE:
            visitNone(*n->data.none);
            break;
        case NodeType::SYMBOL:
            visitSymbol(n->data.symbol);
            break;
        case NodeType::CONSTANT:
            visitConstant(n->data.constant);
            break;
        case NodeType::MODULE:
            visitModule(n->data.module);
            break;
        case NodeType::DECLARATION:
            visitDeclaration(n->data.declaration);
            break;
        case NodeType::PARAMETER:
            visitParameter(n->data.parameter);
            break;
        case NodeType::ASSIGNMENT:
            visitAssignment(n->data.assignment);
            break;
        case NodeType::TYPETUPLE:
            visitTypeTuple(n->data.typeTuple);
            break;
        case NodeType::TYPEFUNCTION:
            visitTypeFunction(*n->data.typeFunction);
            break;
        case NodeType::TYPEPOINTER:
            visitTypePointer(n->data.typePointer);
            break;
        case NodeType::TYPECONSTANT:
            visitTypeConstant(n->data.typeConstant);
            break;
        case NodeType::APPLY:
            visitApply(n->data.apply);
            break;
        case NodeType::TYPEDEFINITION:
            visitTypeDefinition(n->data.typeDefinition);
            break;
        case NodeType::REC:
            visitRec(n->data.rec);
            break;
        case NodeType::FUNCTION:
            visitFunction(n->data.function);
            break;
        case NodeType::RET:
            visitRet(n->data.ret);
            break;
        case NodeType::TYPEOF:
            visitTypeof(n->data.typeof_);
            break;
        case NodeType::DEREFERENCE:
            visitDereference(n->data.dereference);
            break;
        case NodeType::ADDRESSOF:
            visitAddressOf(n->data.addressOf);
            break;
        case NodeType::IF:
            visitIf(n->data.if_);
            break;
        case NodeType::WHILE:
            visitWhile(n->data.while_);
            break;
        case NodeType::BINOP:
            visitBinOp(n->data.binop);
            break;
        case NodeType::VALUETUPLE:
            visitValueTuple(n->data.valueTuple);
            break;
        case NodeType::DOT:
            visitDot(n->data.dot);
            break;
        case NodeType::NEG:
            visitNeg(n->data.neg);
            break;
        case NodeType::CAST:
            visitCast(n->data.cast);
            break;
        case NodeType::HEAP:
            visitHeap(n->data.heap);
            break;
        case NodeType::NOT:
            visitNot(n->data.not_);
            break;
        case NodeType::LABEL:
            visitLabel(n->data.label);
            break;
        case NodeType::JUMP:
            visitJump(n->data.jump);
            break;
        case NodeType::QUESTION:
            visitQuestion(n->data.question);
            break;
        case NodeType::IMPORT:
            visitImport(n->data.import);
            break;
        case NodeType::REQUIRE:
            visitRequire(n->data.require);
            break;
        case NodeType::UNIT:
            visitUnit(n->data.unit);
            break;
        case NodeType::PARSEERROR:
            visitParseError(n->data.parseError);
            break;
        case NodeType::STRINGLITERAL:
            visitStringLiteral(n->data.stringLiteral);
            break;
        case NodeType::CHARACTER:
            visitCharacter(n->data.character);
            break;
        case NodeType::ARGUMENT:
            visitArgument(n->data.argument);
            break;
        case NodeType::ASSERT:
            visitAssert(n->data.assert_);
            break;
        case NodeType::SIZEOF:
            visitSizeof(n->data.sizeof_);
            break;
        case NodeType::DEFER:
            visitDefer(n->data.defer);
            break;
        case NodeType::SPREAD:
            visitSpread(n->data.spread);
            break;
        case NodeType::PIPE:
            visitPipe(n->data.pipe);
            break;
        case NodeType::DO:
            visitDo(n->data.do_);
            break;
        default:
            auto debug = n->type;
            assert(false);
    }

    visited[n] = nodes.top()->metadata.llvm;

    nodes.pop();
}

void Visitor::forceVisit(Node *n)
{
    visit_base(n);
}

void Visitor::visit(Node* n)
{
    if (n == nullptr)
    {
        return;
    }

    if (visited.find(n) != visited.end())
    {
        // already visited!
        return;
    }

    visit_base(n);
}

void Visitor::visitMany(vector<Node *> t)
{
    for (auto _t : t)
    {
        visit(_t);
    }
}

Node *Visitor::sanitizeTypeHelper(Node *n, unordered_map<Node *, bool> *visited)
{
    if (visited->find(n) != visited->end())
    {
        errors.push_back(ParseError{"circular type definition!", n->metadata.region});
        return none(n->metadata.region);
    }

    visited->insert(pair<Node *, bool>(n, true));

    if (n->type == NodeType::SYMBOL)
    {
        return sanitizeTypeHelper(n->data.symbol->resolved, visited);
    }
    if (n->type == NodeType::TYPEDEFINITION)
    {
        return sanitizeTypeHelper(n->data.typeDefinition->type, visited);
    }
    if (n->type == NodeType::PARAMETER)
    {
        return sanitizeTypeHelper(n->data.parameter->type, visited);
    }
    if (n->type == NodeType::TYPEOF)
    {
        return sanitizeTypeHelper(n->data.typeof_->resolved, visited);
    }
    if (n->type == NodeType::DOT)
    {
        return sanitizeTypeHelper(n->data.dot->resolved, visited);
    }
    if (n->type == NodeType::REC)
    {
        return sanitizeTypeHelper(n->data.rec->resolved, visited);
    }
    if (n->type == NodeType::TYPEPOINTER)
    {
        return typePointer(sanitizeTypeHelper(n->data.typePointer->type, visited), n->metadata.region);
    }

    return n;
}

Node *Visitor::getStringType()
{
// type string = {length: i32, ptr: *i8}
    return typeDefinition("string",
                          typeTuple({
                                        parameter("length",
                                                  typeConstant(ConstantType::I32, Region{}),
                                                  constInt(ConstantType::I32, 0, Region{}),
                                                  0,
                                                  Region{}),
                                        parameter("ptr",
                                                  typePointer(
                                                      typeConstant(ConstantType::I8, Region{}),
                                                      Region{}),
                                                  constNil(Region{}),
                                                  0,
                                                  Region{})
                                    }, Region{}),
                          Region{});
}

Node* Visitor::sanitizeType(Node* n)
{
    unordered_map<Node *, bool> visited;
    return sanitizeTypeHelper(n, &visited);
}

bool Visitor::typesMatch(Node* t1, Node* t2)
{
    if (t1->type == NodeType::SYMBOL
        && t2->type == NodeType::SYMBOL
        && t1->data.symbol->value != t2->data.symbol->value)
    {
        return false;
    }

    if (t1 == t2)
    {
        return true;
    }

    t1 = sanitizeType(typeOf(t1));
    t2 = sanitizeType(typeOf(t2));

    if (t1 == t2)
    {
        return true;
    }

    if (t1->type == NodeType::NONE && t2->type == NodeType::NONE)
    {
        return true;
    }

    // char and i8 match (for now)
    // TODO(chad): unicode
    if (t1->type == NodeType::TYPECONSTANT && t1->data.typeConstant->type == ConstantType::I8
        && t2->type == NodeType::TYPECONSTANT && t2->data.typeConstant->type == ConstantType::CHAR)
    {
        return true;
    }
    if (t2->type == NodeType::TYPECONSTANT && t2->data.typeConstant->type == ConstantType::I8
        && t1->type == NodeType::TYPECONSTANT && t1->data.typeConstant->type == ConstantType::CHAR)
    {
        return true;
    }

    if (t1->type == NodeType::TYPECONSTANT
        && t1->data.typeConstant->type == ConstantType::NIL
        && t2->type == NodeType::TYPEPOINTER)
    {
        return true;
    }

    if (t2->type == NodeType::TYPECONSTANT
        && t2->data.typeConstant->type == ConstantType::NIL
        && t1->type == NodeType::TYPEPOINTER)
    {
        return true;
    }

    if (t1->type != t2->type)
    {
        return false;
    }

    auto typ = t1->type;
    switch (typ)
    {
        case NodeType::SYMBOL:
            return t1->data.symbol->value == t2->data.symbol->value;
        case NodeType::TYPETUPLE:
        {
            if (t1->data.typeTuple->types.size() != t2->data.typeTuple->types.size())
            {
                return false;
            }

            auto isMatch = true;
            for (int i = 0; i < t1->data.typeTuple->types.size(); i++)
            {
                if (!typesMatch(t1->data.typeTuple->types[i], t2->data.typeTuple->types[i]))
                {
                    isMatch = false;
                }
            }
            return isMatch;
        }
        case NodeType::TYPEPOINTER:
        {
            return typesMatch(t1->data.typePointer->type, t2->data.typePointer->type);
        }
        case NodeType::PARAMETER:
            return typesMatch(t1->data.parameter->type, t2->data.parameter->type);
        case NodeType::TYPEFUNCTION:
        {
            if (t1->data.typeFunction->input.types.size() != t2->data.typeFunction->input.types.size())
            {
                return false;
            }

            for (auto i = 0; i < t1->data.typeFunction->input.types.size(); i++)
            {
                if (!typesMatch(t1->data.typeFunction->input.types[i], t2->data.typeFunction->input.types[i]))
                {
                    return false;
                }
            }

            return typesMatch(t1->data.typeFunction->output, t2->data.typeFunction->output);
        }
        case NodeType::TYPECONSTANT:
            return t1->data.typeConstant->type == t2->data.typeConstant->type;
        default:
            assert(false);
    }

    assert(false);
}


// typeOf
Node* Visitor::typeOf(Node* n)
{
    switch (n->type)
    {
        case NodeType::CONSTANT:
        {
            switch (n->data.constant->type)
            {
                case ConstantType::I8:
                    return typeConstant(ConstantType::I8, n->metadata.region);
                case ConstantType::I16:
                    return typeConstant(ConstantType::I16, n->metadata.region);
                case ConstantType::I32:
                    return typeConstant(ConstantType::I32, n->metadata.region);
                case ConstantType::I64:
                    return typeConstant(ConstantType::I64, n->metadata.region);
                case ConstantType::U8:
                    return typeConstant(ConstantType::U8, n->metadata.region);
                case ConstantType::U16:
                    return typeConstant(ConstantType::U16, n->metadata.region);
                case ConstantType::U32:
                    return typeConstant(ConstantType::U32, n->metadata.region);
                case ConstantType::U64:
                    return typeConstant(ConstantType::U64, n->metadata.region);
                case ConstantType::F32:
                    return typeConstant(ConstantType::F32, n->metadata.region);
                case ConstantType::F64:
                    return typeConstant(ConstantType::F64, n->metadata.region);
                case ConstantType::BOOL:
                    return typeConstant(ConstantType::BOOL, n->metadata.region);
                case ConstantType::STRING:
                    return typeConstant(ConstantType::STRING, n->metadata.region);
                case ConstantType::CHAR:
                    return typeConstant(ConstantType::CHAR, n->metadata.region);
                case ConstantType::NIL:
                    return typeConstant(ConstantType::NIL, n->metadata.region);
                case ConstantType::TUPLE:
                {
                    auto types = vector<Node*>();
                    auto i = 0;
                    for (auto v : n->data.constant->data.tuple.values)
                    {
                        assert(v->type == NodeType::PARAMETER);
                        types.push_back(v);
                        i += 1;
                    }
                    return typeTuple(types, n->metadata.region);
                }
                default:
                    assert(false);
            }
        }
        case NodeType::FUNCTION:
        {
            if (n->data.function->type->type != NodeType::NONE)
            {
                return n->data.function->type;
            }

            auto inputTypes = vector<Node*>();
            for (auto p : n->data.function->parameters)
            {
                inputTypes.push_back(p);
            }

            // todo(chad): recursive!! (need to check inside arbitrarily deep if statements for example)
            Node* returnType = none();
            for (auto s : n->data.function->block)
            {
                if (s->type == NodeType::RET)
                {
                    returnType = typeOf(s);
                }
            }

            auto ret = typeFunction(inputTypes, returnType, n->metadata.region);
            n->data.function->type = ret;
            return ret;
        }
        case NodeType::RET:
            return typeOf(n->data.ret->value);
        case NodeType::SYMBOL:
        {
            if (n->data.symbol->resolved->type == NodeType::NONE)
            {
                return parseError(fmt::format("could not resolve symbol {}", n->data.symbol->value),
                                       n->metadata.region);
            }
            return typeOf(n->data.symbol->resolved);
        }
        case NodeType::DECLARATION:
        {
            if (n->data.declaration->type->type != NodeType::NONE)
            {
                return typeOf(n->data.declaration->type);
            }
            return typeOf(n->data.declaration->initialValue);
        }
        case NodeType::PARAMETER:
        {
            if (n->data.parameter->type->type != NodeType::NONE)
            {
                return typeOf(n->data.parameter->type);
            }
            return typeOf(n->data.parameter->initialValue);
        }
        case NodeType::TYPEOF:
            return n->data.typeof_->resolved;
        case NodeType::DEREFERENCE:
        {
            auto typ = sanitizeType(typeOf(n->data.dereference->value));
            if (typ->type != NodeType::TYPEPOINTER)
            {
                return parseError("cannot dereference a non-pointer", n->metadata.region);
            }
            return typ->data.typePointer->type;
        }
        case NodeType::ADDRESSOF:
            return typePointer(typeOf(n->data.addressOf->value), n->metadata.region);
        case NodeType::APPLY:
        {
            auto funcType = typeOf(n->data.apply->operator_);
            auto sanitized = sanitizeType(funcType);

            if (sanitized->type == NodeType::TYPECONSTANT)
            {
                return sanitized;
            }
            else if (sanitized->type == NodeType::TYPETUPLE)
            {
                return funcType; // want to return funcType instead of sanitized here, to get the actual typeDefinition instead of just a type tuple
            }
            else if (sanitized->type == NodeType::TYPEFUNCTION)
            {
                return typeOf(sanitized->data.typeFunction->output);
            }
            else if (sanitized->type == NodeType::TYPEPOINTER)
            {
                return typeOf(sanitized->data.typePointer->type);
            }
            else if (sanitized->type == NodeType::SYMBOL
                     && isNumericType(sanitized->data.symbol->value))
            {
                return typeOf(sanitized);
            }

            string msg = "unrecognized type for apply: ";
            msg.append(TYPETOSTRING(sanitized->type));
            errors.push_back(ParseError{msg, sanitized->metadata.region});
        }
            break;
        case NodeType::VALUETUPLE:
        {
            auto types = vector<Node*>();
            auto i = 0;
            for (auto v : n->data.valueTuple->values)
            {
                assert(v->type == NodeType::PARAMETER);
//                types.push_back(typeOf(v));
                types.push_back(parameter(v->data.parameter->name,
                                          typeOf(v->data.parameter->type),
                                          none(),
                                          v->data.parameter->order,
                                          v->metadata.region));
                i += 1;
            }
            return typeTuple(types, n->metadata.region);
        }
        case NodeType::DOT:
        {
            auto lhsType = typeOf(n->data.dot->lhs);
            while (lhsType->type == NodeType::TYPEPOINTER)
            {
                lhsType = lhsType->data.typePointer->type;
            }
            while (lhsType->type == NodeType::SYMBOL
                   && lhsType->data.symbol->resolved->type != NodeType::NONE)
            {
                lhsType = lhsType->data.symbol->resolved;
            }
            if (lhsType->type == NodeType::TYPEOF)
            {
                lhsType = typeOf(lhsType->data.typeof_->resolved);
            }

            if (lhsType->type == NodeType::MODULE)
            {
                assert(n->data.dot->rhs->type == NodeType::SYMBOL);
                for (auto t : lhsType->data.module->statements)
                {
                    if (t->type == NodeType::DECLARATION
                        && t->data.declaration->name == n->data.dot->rhs->data.symbol->value)
                    {
                        return typeOf(t->data.declaration->type);
                    }
                    if (t->type == NodeType::MODULE
                        && t->data.module->name == n->data.dot->rhs->data.symbol->value)
                    {
                        return t;
                    }
                    if (t->type == NodeType::TYPEDEFINITION
                        && t->data.typeDefinition->name == n->data.dot->rhs->data.symbol->value)
                    {
                        return t;
                    }
                }

                // didn't find anything, symbol resolution error
                return parseError(
                    fmt::format("could not resolve symbol {}", n->data.dot->rhs->data.symbol->value),
                    n->data.dot->rhs->metadata.region);
            }

            while (lhsType->type == NodeType::TYPEPOINTER)
            {
                lhsType = lhsType->data.typePointer->type;
            }

            auto st = sanitizeType(lhsType);
            if (st->type == NodeType::TYPETUPLE)
            {
                if (n->data.dot->rhs->type == NodeType::CONSTANT
                    && n->data.dot->rhs->data.constant->type == ConstantType::I32)
                {
                    auto fieldIndex = n->data.dot->rhs->data.constant->data.intvalue;
                    if (fieldIndex >= st->data.typeTuple->types.size())
                    {
                        errors.push_back(ParseError{
                            fmt::format("struct type only has {} fields", st->data.typeTuple->types.size()),
                            n->data.dot->rhs->metadata.region
                        });
                        return none(n->data.dot->rhs->metadata.region);
                    }
                    return typeOf(st->data.typeTuple->types[fieldIndex]);
                }

                for (auto t : st->data.typeTuple->types)
                {
                    assert(t->type == NodeType::PARAMETER);
                    if (t->data.parameter->name == n->data.dot->rhs->data.symbol->value)
                    {
                        return typeOf(t->data.parameter->type);
                    }
                }

                auto err = ParseError{
                    fmt::format("could not find field {} in struct definition",
                                n->data.dot->rhs->data.symbol->value),
                    nodes.top()->metadata.region
                };
                err.notes.push_back(fmt::format("struct type declared here: {}",
                                                st->metadata.region));
                errors.push_back(err);
                return none(nodes.top()->metadata.region);
            }
            if (st->type == NodeType::TYPECONSTANT
                && st->data.typeConstant->type == ConstantType::STRING)
            {
                if (n->data.dot->rhs->data.symbol->value == "ptr")
                {
                    return typePointer(typeConstant(ConstantType::I8, n->data.dot->rhs->metadata.region), n->data.dot->rhs->metadata.region);
                }
                else if (n->data.dot->rhs->data.symbol->value == "length")
                {
                    return typeConstant(ConstantType::I32, n->data.dot->rhs->metadata.region);
                }

                string msg = fmt::format("expected 'ptr' or 'length' for dot of string, not '{}'",
                                         n->data.dot->rhs->data.symbol->value);
                errors.push_back(ParseError{msg, nodes.top()->metadata.region});
                return none(nodes.top()->metadata.region);
            }

            errors.push_back(ParseError{
                fmt::format(
                    "cannot perform dot on {}",
                    TYPETOSTRING(sanitizeType(typeOf(n->data.dot->lhs))->type)
                ),
                n->metadata.region
            });

            return none(n->metadata.region);
        }
        case NodeType::NEG:
            return typeOf(n->data.neg->value);
        case NodeType::CAST:
            return typeOf(n->data.cast->type);
        case NodeType::SIZEOF:
            return typeConstant(ConstantType::U32, n->metadata.region);
        case NodeType::NOT:
            return typeOf(n->data.not_->value);
        case NodeType::LABEL:
        case NodeType::JUMP:
            return none();
        case NodeType::QUESTION:
            return typeConstant(ConstantType::BOOL, n->metadata.region);
        case NodeType::BINOP:
            if (n->data.binop->type == OpType::PIPE)
            {
                auto funcType = typeOf(n->data.binop->rhs);

                if (funcType->type == NodeType::TYPEFUNCTION)
                {
                    return typeOf(funcType->data.typeFunction->output);
                }

                return funcType;
            }
            return typeOf(n->data.binop->lhs);
        case NodeType::ARGUMENT:
            return typeOf(n->data.argument->value);
        case NodeType::TYPEDEFINITION:
        case NodeType::TYPEPOINTER:
        case NodeType::TYPETUPLE:
        case NodeType::TYPEFUNCTION:
        case NodeType::MODULE:
        case NodeType::TYPECONSTANT:
            return n;
        case NodeType::NONE:
            return none();
        case NodeType::REC:
            return typeOf(n->data.rec->resolved);
        case NodeType::STRINGLITERAL:
            return typeOf(getStringType());
        case NodeType::CHARACTER:
            return typeConstant(ConstantType::CHAR, n->metadata.region);
        case NodeType::PIPE:
        {
            Node* funcType;
            if (n->data.pipe->resolved->type != NodeType::NONE)
            {
                funcType = typeOf(n->data.pipe->resolved);
            }
            else
            {
                funcType = sanitizeType(typeOf(n->data.pipe->func));
            }
            if (funcType->type == NodeType::TYPEFUNCTION)
            {
                return typeOf(funcType->data.typeFunction->output);
            }
            return funcType;
        }
        case NodeType::HEAP:
        {
            auto heapType = typeOf(n->data.heap->value);
            return typePointer(heapType, n->metadata.region);
        }
        case NodeType::DO:
        {
            return typeOf(n->data.do_->identifier);
        }
        default:
            auto dbg = n->type;
            assert(false);
    }

    return none(n->metadata.region);
}

Node* resolveDot(Dot* d)
{
    if (d->resolved->type != NodeType::NONE)
    {
        return d->resolved;
    }

    auto realLhs = d->lhs;
    if (realLhs->type == NodeType::SYMBOL)
    {
        realLhs = d->lhs->metadata.scope->resolve(d->lhs->data.symbol->value, d->lhs->metadata.region);
    }
    if (realLhs->type == NodeType::DOT)
    {
        auto maybe = resolveDot(d->lhs->data.dot);
        if (maybe->type != NodeType::NONE) { realLhs = maybe; }
    }

    if (d->rhs->type == NodeType::CONSTANT)
    {
        return none();
    }

    assert(d->rhs->type == NodeType::SYMBOL);
    if (realLhs->type == NodeType::MODULE)
    {
        auto resolved = realLhs->metadata.scope->resolve(d->rhs->data.symbol->value, d->rhs->metadata.region);
        d->resolved = resolved;
        return resolved;
    }

    return none();
}

bool isNumericType(string s)
{
    return s == "i8"
           || s == "u8"
           || s == "i16"
           || s == "u16"
           || s == "i32"
           || s == "u32"
           || s == "i64"
           || s == "u64"
           || s == "f32"
           || s == "f64";
}

bool isBuiltinType(string s)
{
    return isNumericType(s) || s == "bool" || s == "char";
}

Node* Visitor::sanitize(Node* n)
{
    Node* ret = n;

    if (ret->type == NodeType::SYMBOL)
    {
        if (isNumericType(ret->data.symbol->value))
        {
            return ret;
        }
        ret = sanitize(ret->data.symbol->resolved);
    }
    if (ret->type == NodeType::DOT)
    {
        if (ret->data.dot->resolved->type != NodeType::NONE)
        {
            ret = sanitize(ret->data.dot->resolved);
        }
        else
        {
            resolveDot(ret->data.dot);
            if (ret->data.dot->resolved->type != NodeType::NONE)
            {
                ret = sanitize(ret->data.dot->resolved);
            }
        }
    }
    if (ret->type == NodeType::CAST)
    {
        ret = sanitize(ret->data.cast->type);
    }
//    if (ret->type == NodeType::TYPEDEFINITION)
//    {
//        ret = sanitize(ret->data.typeDefinition->type);
//    }
    if (ret->type == NodeType::PARAMETER)
    {
        ret = sanitize(ret->data.parameter->type);
    }
    if (ret->type == NodeType::REC)
    {
        ret = sanitize(ret->data.rec->resolved);
    }
    if (ret->type == NodeType::TYPEOF)
    {
        if (ret->data.typeof_->resolved->type == NodeType::NONE)
        {
            ret->data.typeof_->resolved = typeOf(ret->data.typeof_->value);
        }
        ret = sanitize(ret->data.typeof_->resolved);
    }
    if (ret->type == NodeType::ARGUMENT)
    {
        ret = sanitize(ret->data.argument->value);
    }

    return ret;
}

void TypePropagationVisitor::locateModulesWithType(Node *type, Scope *s, vector<Node *> *modules)
{
    for (auto v : s->values)
    {
        if (v.second->type == NodeType::MODULE)
        {
            // loop through annotations. If we have an @scope(...) annotation, compare with type
            for (auto a : v.second->metadata.annotations)
            {
                if (a->type == NodeType::APPLY
                    && a->data.apply->operands.size() > 0
                    && typesMatch(a->data.apply->operands[0]->data.argument->value, type))
                {
                    modules->push_back(v.second);
                }
            }
        }
    }

    if (s->parent != nullptr)
    {
        locateModulesWithType(type, s->parent, modules);
    }
}

vector<Ordering> get_ordering(vector<Node*> expectedTypes,
                              vector<string> givenArgs,
                              vector<ParseError> *errorsList,
                              Region callSite)
{
    //- All expectedTypes must be parameters
    for (auto et : expectedTypes) { assert (et->type == NodeType::PARAMETER); }

    //- If expectedTypes has nameless parameters, then cannot reorder
    if (!expectedTypes.empty() && expectedTypes[0]->data.parameter->name == "")
    {
        vector<Ordering> identity_ordering;
        for (auto i = 0; i < givenArgs.size(); i++)
        {
            identity_ordering.push_back(Ordering{i, i});
        }
        return identity_ordering;
    }


    //- for given, assert that the first named argument is after the last unnamed argument
    auto encounteredNamedArg = false;
    for (auto gp : givenArgs)
    {
        auto hasNamedArg = gp != "";

        if (!hasNamedArg && encounteredNamedArg)
        {
            errorsList->push_back(ParseError{
                "The first named argument must come after the last unnamed argument",
                callSite
            });
        }
        if (hasNamedArg)
        {
            encounteredNamedArg = true;
        }
    }

    //- for target, assert that the first default argument is after the last non-default argument
    auto encounteredDefaultParam = false;
    for (auto et : expectedTypes)
    {
        auto hasDefaultParam = et->data.parameter->initialValue->type != NodeType::NONE;

        if (!hasDefaultParam && encounteredDefaultParam)
        {
            errorsList->push_back(ParseError{
                "The first argument with a default value must come after the argument without a default value",
                callSite
            });
        }
        if (hasDefaultParam)
        {
            encounteredDefaultParam = true;
        }
    }

    vector<Ordering> actual_ordering;

    //- for given, loop through and assign by order until we get to the named arguments
    for (int i = 0; i < givenArgs.size(); i++)
    {
        auto hasNamedArg = givenArgs[i] != "";
        if (hasNamedArg)
        {
            auto found_idx = -1;
            for (auto j = 0; j < expectedTypes.size(); j++)
            {
                if (expectedTypes[j]->data.parameter->name == givenArgs[i])
                {
                    found_idx = j;
                    break;
                }
            }
            actual_ordering.push_back(Ordering{i, found_idx});
        }
        else
        {
            actual_ordering.push_back(Ordering{i, i});
        }
    }

    //- when done, assert that everything in target has either been associated or has a default
    for (auto i = 0; i < expectedTypes.size(); i++)
    {
        if (expectedTypes[i]->data.parameter->initialValue->type == NodeType::NONE)
        {
            auto found = false;
            for (auto j = 0; j < actual_ordering.size(); j++)
            {
                if (actual_ordering[j].target_index == i)
                {
                    found = true;
                }
            }
            if (!found)
            {
                // todo(chad): report on both calling and declaration sites
                errorsList->push_back(ParseError{
                    fmt::format("expected parameter {} ({}) to be filled",
                                i,
                                expectedTypes[i]->data.parameter->name),
                    callSite
                });
            }
        }
    }

    return actual_ordering;
}
