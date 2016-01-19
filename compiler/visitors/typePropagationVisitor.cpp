#include "visitor.h"

void TypePropagationVisitor::visitDeclaration(Declaration* d)
{
    visit(d->type);
    if (d->type->type != NodeType::NONE)
    {
        auto sanitizedInitialValue = sanitize(d->initialValue);
        if (sanitizedInitialValue->type == NodeType::FUNCTION)
        {
            sanitizedInitialValue->data.function->type = d->type;
        }
    }

    visit(d->initialValue);

    // fill in type if none is there
    if (d->type->type == NodeType::NONE)
    {
        d->type = typeOf(d->initialValue);
    }
    else if (d->initialValue->type != NodeType::NONE)
    {
        auto sanitizedInitialValue = sanitize(d->initialValue);
        if (sanitizedInitialValue->type == NodeType::FUNCTION)
        {
            sanitizedInitialValue->data.function->type = d->type;
        }

        // there was a type given.  See if it matches with the initialValue
        auto givenType = sanitizeType(typeOf(d->type));
        auto initialValueType = sanitizeType(typeOf(d->initialValue));

        if (!typesMatch(givenType, initialValueType))
        {
            auto err = ParseError{fmt::format("type mismatch. declared type is {0}, but rhs type is {1}",
                                              typeDescription(givenType),
                                              typeDescription(initialValueType)),
                                  nodes.top()->metadata.region};
            err.notes.push_back(fmt::format("declared type declared here: {0}", givenType->metadata.region));
            errors.push_back(err);
        }
    }

    if (d->type->type == NodeType::NONE)
    {
        errors.push_back(ParseError{"could not determine type of declaration", nodes.top()->metadata.region});
    }
}

void TypePropagationVisitor::visitDot(Dot* d)
{
    resolveDot(d);
    if (d->resolved != nullptr)
    {
        visit(d->resolved);
    }

    visit(d->lhs);
}

void TypePropagationVisitor::visitParameter(Parameter* p)
{
    visit(p->initialValue);
    visit(p->type);

    // fill in type if none is there
    if (p->type->type == NodeType::NONE)
    {
        p->type = typeOf(p->initialValue);
    }
}

void TypePropagationVisitor::visitFunction(Function* f)
{
    currentParentScopes.push(nodes.top()->metadata.scope);

    for (auto p : f->parameters)
    {
        visit(p->data.declaration->type);
        visit(p->data.declaration->initialValue);
    }

    visitMany(f->block);

    f->type = typeOf(nodes.top());

    currentParentScopes.pop();
}

void TypePropagationVisitor::visitBinOp(BinOp* b)
{
    visit(b->lhs);
    visit(b->rhs);

    // if types do not match, error!!
    if (b->type != OpType::PIPE && !typesMatch(sanitizeType(typeOf(b->lhs)), sanitizeType(typeOf(b->rhs))))
    {
        auto err = ParseError{fmt::format("type mismatch. LHS type is {0}, but RHS type is {1}",
                                          typeDescription(sanitizeType(typeOf(b->lhs))),
                                          typeDescription(sanitizeType(typeOf(b->rhs)))),
                              nodes.top()->metadata.region};
        err.notes.push_back(fmt::format("LHS declared here: {0}", typeOf(b->lhs)->metadata.region));
        errors.push_back(err);
    }
}

void TypePropagationVisitor::visitTypeof(Typeof* t)
{
    visit(t->value);
    if (t->resolved->type == NodeType::NONE)
    {
        auto r = typeOf(t->value);

        if (t->is_rt)
        {
            auto typ = typeOf(r);
            auto realType = sanitizeType(typ);
            if (realType->type != NodeType::TYPEFUNCTION)
            {
                errors.push_back(ParseError{
                    "cannot get type of non-function",
                    nodes.top()->metadata.region
                });
            }
            r = typeOf(realType->data.typeFunction->output);
        }

        t->resolved = r;
    }
}

void TypePropagationVisitor::visitPipe(Pipe *p)
{
    auto realFunc = p->func;
    if (p->func->type == NodeType::APPLY) { realFunc = p->func->data.apply->operator_; }
    if (realFunc->type == NodeType::SYMBOL && realFunc->data.symbol->resolved->type == NodeType::NONE)
    {
        auto typ = typeOf(p->args);
        p->resolved = typ->metadata.scope->resolve(realFunc->data.symbol->value, realFunc->metadata.region);
        realFunc->data.symbol->resolved = p->resolved;

        if (realFunc->data.symbol->resolved->type == NodeType::NONE)
        {
            // look in scope and parent scopes for any scope module for this type
            vector<Node *> modules = {};
            locateModulesWithType(typ, nodes.top()->metadata.scope, &modules);

            for (auto m : modules)
            {
                for (auto s : m->data.module->statements)
                {
                    if (s->type == NodeType::DECLARATION
                        && s->data.declaration->name == realFunc->data.symbol->value)
                    {
                        p->resolved = s;
                        if (p->func->type == NodeType::APPLY)
                        {
                            p->func->data.apply->operator_ = s;
                        }
                        else
                        {
                            p->func = s;
                        }
                        realFunc = s;
                    }
                }
            }
        }
    }

    auto pfunc = sanitize(p->func);
    if (pfunc->type == NodeType::NONE)
    {
        errors.push_back(ParseError{"Could not resolve rhs of pipe", p->func->metadata.region});
    }

    auto pargs = argument("", p->args, p->args->metadata.region);
    if (pfunc->type == NodeType::APPLY)
    {
        // if we are shoving args into an apply, then just do that.
        vector<Node*> newParams;
        newParams.push_back(pargs);
        for (auto pp : pfunc->data.apply->operands)
        {
            newParams.push_back(pp);
        }
        pfunc->data.apply->operands = newParams;

        p->resolved = pfunc;
    }
    else
    {
        // otherwise, create an apply
        p->resolved = apply(p->func, { pargs }, nodes.top()->metadata.region);
    }

    p->resolved->setParentScope(nodes.top());

    visit(p->resolved);
}

void TypePropagationVisitor::visitApply(Apply* a)
{
    visitMany(a->operands);

    // avoiding infinite loops is a plus
    if (a->operator_->type != NodeType::REC)
    {
        visit(a->operator_);
    }

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
        if (a->operands.size() != 1)
        {
            errors.push_back(ParseError{
                "expected 1 argument for type conversion",
                a->operator_->metadata.region
            });
        }
    }
    else if (realFuncType->type == NodeType::TYPECONSTANT)
    {
        errors.push_back(ParseError{
            "unsupported function call",
            nodes.top()->metadata.region
        });
    }

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
        return;
    }

    if (realFunc->type == NodeType::FUNCTION)
    {
        assert(realFuncType->type == NodeType::TYPEFUNCTION);

        // if default value, initialize to that first
        for (auto i = 0; i < realFunc->data.function->parameters.size(); i++)
        {
            assert(realFunc->data.function->parameters[i]->type == NodeType::PARAMETER);
        }
    }
    else if (realFuncType->type == NodeType::TYPETUPLE)
    {
        realParameterCount = (unsigned int) realFuncType->data.typeTuple->types.size();
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
                    projectedGivenArgs.push_back(projectedType->data.typeTuple->types[pt]->data.parameter->name);
                }
            }
            else
            {
                projectedGivenArgs.push_back(ap->data.argument->name);
            }
        }

        // any default parameters?
        auto defaultParameterCount = 0;
        for (auto i = 0; i < realParameterCount; i++)
        {
            auto param = realFuncType->data.typeTuple->types[i];
            assert(param->type == NodeType::PARAMETER);
            if (param->data.parameter->initialValue->type != NodeType::NONE)
            {
                defaultParameterCount += 1;
            }
        }

        auto expectedArgCount = realFuncType->data.typeTuple->types.size();
        auto operandsCount = a->operands.size();
        if (defaultParameterCount + operandsCount < expectedArgCount)
        {
            errors.push_back(ParseError{
                fmt::format("expected {} args", expectedArgCount),
                a->operator_->metadata.region
            });
        }

        assert(realFuncType->type == NodeType::TYPETUPLE);
        auto expectedTypes = realFuncType->data.typeTuple->types;
        auto givenArgs = a->operands;
        auto ordering = get_ordering(expectedTypes, projectedGivenArgs, &errors, a->operator_->metadata.region);

        for (auto o : ordering)
        {
            // check for type mismatch
            if (expectedTypes.size() <= o.target_index)
            {
                errors.push_back(ParseError{fmt::format("too many arguments : expected {}, received {}",
                                                   expectedTypes.size(),
                                                   o.target_index + 1),
                                       nodes.top()->metadata.region});
                return;
            }
            if (a->operands.size() > o.given_index
                && a->operands[o.given_index]->data.argument->value->type != NodeType::SPREAD)
            {
                auto targetType = expectedTypes[o.target_index];
                auto givenType = typeOf(a->operands[o.given_index]);

                if (!typesMatch(targetType, givenType))
                {
                    errors.push_back(ParseError{
                        fmt::format("type mismatch for argument {}: expected {}, given {}",
                                    o.given_index,
                                    typeDescription(targetType),
                                    typeDescription(givenType)),
                        a->operands[o.given_index]->metadata.region
                    });
                }
            }
        }

        return;
    }
    else if (realFunc->type == NodeType::TYPEDEFINITION)
    {
        if (!typesMatch(sanitizeType(typeOf(realFunc)), realFuncType))
        {
            errors.push_back(ParseError{
                "types do not match",
                nodes.top()->metadata.region
            });
        }

        return;
    }
    else if (realFuncType->type == NodeType::TYPECONSTANT)
    {
        if (!typesMatch(sanitizeType(typeOf(realFunc)), realFuncType))
        {
            errors.push_back(ParseError{
                "types do not match",
                nodes.top()->metadata.region
            });
        }

        return;
    }
    else if (realFuncType->type == NodeType::TYPEPOINTER)
    {
        if (parameterCount != 1)
        {
            errors.push_back(ParseError{
                "expected 1 parameter for type conversion",
                nodes.top()->metadata.region
            });
        }

        return;
    }

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
                projectedGivenArgs.push_back(projectedType->data.typeTuple->types[pt]->data.parameter->name);
            }
        }
        else
        {
            projectedGivenArgs.push_back(ap->data.argument->name);
        }
    }

    if (realFuncType->type != NodeType::TYPEFUNCTION)
    {
      errors.push_back(ParseError{
        "expected function",
        realFunc->metadata.region
      });

      return;
    }

    auto expectedTypes = realFuncType->data.typeFunction->input.types;
    auto ordering = get_ordering(expectedTypes, projectedGivenArgs, &errors, a->operator_->metadata.region);

    for (auto o : ordering)
    {
        // check for type mismatch
        if (expectedTypes.size() <= o.target_index)
        {
            errors.push_back(ParseError{fmt::format("too many arguments : expected {}, received {}",
                                               expectedTypes.size(),
                                               o.target_index + 1),
                                   nodes.top()->metadata.region});
            return;
        }
        if (a->operands.size() > o.given_index
            && a->operands[o.given_index]->data.argument->value->type != NodeType::SPREAD)
        {
            auto targetType = expectedTypes[o.target_index];
            auto givenType = typeOf(a->operands[o.given_index]);

            if (!typesMatch(targetType, givenType))
            {
                errors.push_back(ParseError{
                    fmt::format("type mismatch for argument {}: expected {}, given {}",
                                o.given_index,
                                typeDescription(targetType),
                                typeDescription(givenType)),
                    a->operands[o.given_index]->metadata.region
                });
            }
        }
    }
}

void TypePropagationVisitor::visitAssignment(Assignment* a)
{
    visit(a->lhs);
    visit(a->rhs);

    auto decl = sanitize(a->lhs);
    if (decl->type == NodeType::DECLARATION
        && decl->data.declaration->declarationType == DeclarationType::IMMUTABLE)
    {
        auto error = ParseError{"cannot assign to an immutable declaration", nodes.top()->metadata.region};
        error.notes.push_back(fmt::format("original declaration is here: {0}", decl->metadata.region));
        errors.push_back(error);
    }

    auto lhsType = sanitizeType(typeOf(a->lhs));
    auto rhsType = sanitizeType(typeOf(a->rhs));

    if (lhsType->type == NodeType::NONE)
    {
        auto err = ParseError{"could not resolve LHS of assignment", a->lhs->metadata.region};
        errors.push_back(err);
    }
    if (rhsType->type == NodeType::NONE)
    {
        auto err = ParseError{"could not resolve RHS of assignment", a->rhs->metadata.region};
        errors.push_back(err);
    }
    if (!typesMatch(lhsType, rhsType))
    {
        auto err = ParseError{fmt::format("type mismatch. LHS type is {0}, but RHS type is {1}",
                                          typeDescription(lhsType),
                                          typeDescription(rhsType)),
                                          nodes.top()->metadata.region};
        err.notes.push_back(fmt::format("LHS declared here: {0}", lhsType->metadata.region));
        errors.push_back(err);
    }
}

void TypePropagationVisitor::visitIf(If* i)
{
    currentParentScopes.push(nodes.top()->metadata.scope);

    visit(i->if_branch->condition);
    visitMany(i->if_branch->statements);

    for (auto br : i->elif_branches)
    {
        visit(br->condition);
        visitMany(br->statements);
    }

    visit(i->else_branch->condition);
    visitMany(i->else_branch->statements);

    currentParentScopes.pop();
}

void TypePropagationVisitor::visitWhile(While* w)
{
    currentParentScopes.push(nodes.top()->metadata.scope);

    visit(w->condition);
    visitMany(w->statements);

    currentParentScopes.pop();
}

void TypePropagationVisitor::visitModule(Module* m)
{
    currentParentScopes.push(nodes.top()->metadata.scope);

    visitMany(m->statements);

    currentParentScopes.pop();
}

void TypePropagationVisitor::visitRequire(Require* r) { }

void TypePropagationVisitor::visitCast(Cast *c)
{
    visit(c->type);
    visit(c->value);
}

void TypePropagationVisitor::visitRec(Rec *r)
{
    visit(r->resolved);
}

void TypePropagationVisitor::visitDereference(Dereference *d)
{
    visit(d->value);
    auto typ = sanitizeType(typeOf(d->value));
    if (typ->type != NodeType::TYPEPOINTER)
    {
        errors.push_back(ParseError{
            "cannot dereference a non-pointer",
            nodes.top()->metadata.region
        });
    }
}

void TypePropagationVisitor::visitValueTuple(ValueTuple *v)
{
    visitMany(v->values);
    for (auto val : v->values)
    {
        if (val->data.parameter->type == nullptr || val->data.parameter->type->type == NodeType::NONE)
        {
            val->data.parameter->type = typeOf(val);
        }
    }
}

void TypePropagationVisitor::visitConstant(Constant *c)
{
    if (c->type == ConstantType::TUPLE)
    {
        visitValueTuple(&c->data.tuple);
    }
}

void TypePropagationVisitor::visitQuestion(Question *q)
{
    visit(q->value);

    if (sanitizeType(typeOf(q->value))->type != NodeType::TYPEPOINTER)
    {
        errors.push_back(ParseError{
            "expected pointer type for '?' postfix operator",
            nodes.top()->metadata.region
        });
    }
}

