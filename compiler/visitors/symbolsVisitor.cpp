#include "visitor.h"

void insertStatementIntoScope(Node* s, Node* topNode)
{
    s->setParentScope(topNode);

    if (s->type == NodeType::MODULE)
    {
        topNode->insertIntoScope(s->data.module->name, s);
    }
    else if (s->type == NodeType::DECLARATION)
    {
        topNode->insertIntoScope(s->data.declaration->name, s);
    }
    else if (s->type == NodeType::TYPEDEFINITION)
    {
        topNode->insertIntoScope(s->data.typeDefinition->name, s);
    }
}

void insertStatementsIntoScope(vector<Node*> statements, Node* topNode)
{
    for (auto s : statements)
    {
        insertStatementIntoScope(s, topNode);
    }
}

void SymbolsVisitor::visitSymbol(Symbol* s)
{
    if (s->resolved != nullptr && s->resolved->type != NodeType::NONE)
    {
        return;
    }

    // built-in type?
    if (isBuiltinType(s->value))
    {
        if (s->value == "i8")
        {
            s->resolved = typeConstant(ConstantType::I8, nodes.top()->metadata.region);
        }
        else if (s->value == "i16")
        {
            s->resolved = typeConstant(ConstantType::I16, nodes.top()->metadata.region);
        }
        else if (s->value == "i32")
        {
            s->resolved = typeConstant(ConstantType::I32, nodes.top()->metadata.region);
        }
        else if (s->value == "i64")
        {
            s->resolved = typeConstant(ConstantType::I64, nodes.top()->metadata.region);
        }
        else if (s->value == "u8")
        {
            s->resolved = typeConstant(ConstantType::U8, nodes.top()->metadata.region);
        }
        else if (s->value == "u16")
        {
            s->resolved = typeConstant(ConstantType::U16, nodes.top()->metadata.region);
        }
        else if (s->value == "u32")
        {
            s->resolved = typeConstant(ConstantType::U32, nodes.top()->metadata.region);
        }
        else if (s->value == "u64")
        {
            s->resolved = typeConstant(ConstantType::U64, nodes.top()->metadata.region);
        }
        else if (s->value == "f32")
        {
            s->resolved = typeConstant(ConstantType::F32, nodes.top()->metadata.region);
        }
        else if (s->value == "f64")
        {
            s->resolved = typeConstant(ConstantType::F64, nodes.top()->metadata.region);
        }
        else if (s->value == "bool")
        {
            s->resolved = typeConstant(ConstantType::BOOL, nodes.top()->metadata.region);
        }
        else if (s->value == "char")
        {
            s->resolved = typeConstant(ConstantType::CHAR, nodes.top()->metadata.region);
        }

        return;
    }

    s->resolved = nodes.top()->metadata.scope->resolve(s->value, nodes.top()->metadata.region);
    if (s->resolved->type == NodeType::NONE && s->needs_resolution)
    {
        errors.push_back(ParseError{
            fmt::format("could not resolve symbol {}", s->value),
            nodes.top()->metadata.region
        });
    }
}

void SymbolsVisitor::visitUnit(Unit* u)
{
    for (auto m : u->modules)
    {
        m->setParentScope(nodes.top());
    }

    insertStatementsIntoScope(u->modules, nodes.top());

    visitMany(u->modules);
}

void SymbolsVisitor::visitModule(Module* m)
{
    // Modules get their own scope
    currentParentScopes.push(nodes.top()->metadata.scope);

    // If annotated with @scope(FOO.bar), resolve FOO.bar
    for (Node *a : nodes.top()->metadata.annotations)
    {
        if (a->type == NodeType::APPLY
            && a->data.apply->operator_->type == NodeType::SYMBOL
            && a->data.apply->operator_->data.symbol->value == "scope")
        {
            if (a->data.apply->operands.size() == 0)
            {
                errors.push_back(ParseError{"call to @scope needs a parameter", nodes.top()->metadata.region});
            }
            else
            {
                auto param = a->data.apply->operands[0]->data.argument->value;
                param->setParentScope(nodes.top());
                visit(param);
            }
        }
    }

    addStringType(m);

    // Assign parent scopes for children, put all of the module's symbols into its own scope
    insertStatementsIntoScope(m->statements, nodes.top());

    // recursively resolve all symbols in the module
    visitMany(m->statements);

    currentParentScopes.pop();
}

void SymbolsVisitor::visitDeclaration(Declaration* d)
{
    if (d->type == nullptr)
    {
        errors.push_back(ParseError{"could not determine type of declaration", nodes.top()->metadata.region});
    }
    else
    {
        d->type->setParentScope(nodes.top());
    }

    d->initialValue->setParentScope(nodes.top());

    visit(d->initialValue);
    if (d->type != nullptr)
    {
        visit(d->type);
    }
}

void SymbolsVisitor::visitParameter(Parameter* p)
{
    p->type->setParentScope(nodes.top());
    p->initialValue->setParentScope(nodes.top());

    visit(p->type);
    visit(p->initialValue);
}

void SymbolsVisitor::visitArgument(Argument *a)
{
    a->value->setParentScope(nodes.top());
    visit(a->value);
}

void SymbolsVisitor::visitAssignment(Assignment* a)
{
    a->lhs->setParentScope(nodes.top());
    a->rhs->setParentScope(nodes.top());

    visit(a->lhs);
    visit(a->rhs);
}

void SymbolsVisitor::visitTypeTuple(TypeTuple* t)
{
    for (auto type : t->types)
    {
        type->setParentScope(nodes.top());
    }

    visitMany(t->types);
}

void SymbolsVisitor::visitTypeFunction(TypeFunction t)
{
    visitTypeTuple(&t.input);
    t.output->setParentScope(nodes.top());

    visitTypeTuple(&t.input);
    visit(t.output);
}

void SymbolsVisitor::visitTypePointer(TypePointer* t)
{
    t->type->setParentScope(nodes.top());
    visit(t->type);
}

void SymbolsVisitor::visitApply(Apply* a)
{
    a->operator_->setParentScope(nodes.top());
    for (auto p : a->operands)
    {
        p->setParentScope(nodes.top());
    }
    visit(a->operator_);
    visitMany(a->operands);
}

void SymbolsVisitor::visitTypeDefinition(TypeDefinition* t)
{
    if (t->type == nullptr)
    {
        errors.push_back(ParseError{"could not determine type", nodes.top()->metadata.region});
        return;
    }

    t->type->setParentScope(nodes.top());
    nodes.top()->insertIntoScope(t->name, t->type);
    visit(t->type);
}

void SymbolsVisitor::visitFunction(Function* f)
{
    rec.push(typeof_(nodes.top(), false, nodes.top()->metadata.region));

    currentParentScopes.push(nodes.top()->metadata.scope);

    for (auto s : f->parameters)
    {
        if (s->type != NodeType::PARAMETER)
        {
            errors.push_back(ParseError{"expected parameter", s->metadata.region});
        }

        s->setParentScope(nodes.top());
        s->data.parameter->type->setParentScope(nodes.top());
        s->data.parameter->initialValue->setParentScope(nodes.top());

        nodes.top()->insertIntoScope(s->data.parameter->name, s);

        visit(s);
        visit(s->data.parameter->type);
        visit(s->data.parameter->initialValue);
    }

    auto hasReturn = false;
    for (auto b : f->block)
    {
        if (b->type == NodeType::LABEL)
        {
            insertStatementIntoScope(b, nodes.top());
            b->metadata.scope->isModuleLevel = false;
            visit(b);
        }
    }
    for (auto b : f->block)
    {
        if (b->type == NodeType::LABEL)
        {
            continue;
        }

        insertStatementIntoScope(b, nodes.top());
        b->metadata.scope->isModuleLevel = false;

        visit(b);

        if (b->type == NodeType::RET)
        {
            hasReturn = true;
        }
    }

    if (!hasReturn)
    {
        auto region = nodes.top()->metadata.region;
        auto retVoid = ret(none(region), region);
        retVoid->setParentScope(nodes.top());
        f->block.push_back(retVoid);
    }

    currentParentScopes.pop();

    rec.pop();
}

void SymbolsVisitor::visitRet(Ret* r)
{
    r->value->setParentScope(nodes.top());
    visit(r->value);
}

void SymbolsVisitor::visitRec(Rec *r)
{
    auto resolved = rec.top();
    r->resolved = resolved;
}

void SymbolsVisitor::visitTypeof(Typeof* t)
{
    t->value->setParentScope(nodes.top());
    visit(t->value);
}

void SymbolsVisitor::visitDereference(Dereference* d)
{
    d->value->setParentScope(nodes.top());
    visit(d->value);
}

void SymbolsVisitor::visitAddressOf(AddressOf* a)
{
    a->value->setParentScope(nodes.top());
    visit(a->value);

    if (nodes.top()->metadata.scope->isModuleLevel)
    {
        errors.push_back(ParseError{
            "cannot use addressof operator at module level",
            nodes.top()->metadata.region
        });
    }
}

void SymbolsVisitor::visitIf(If *i)
{
    currentParentScopes.push(nodes.top()->metadata.scope);

    i->if_branch->condition->setParentScope(nodes.top());
    for (auto s : i->if_branch->statements)
    {
        s->setParentScope(nodes.top());
    }

    for (auto br : i->elif_branches)
    {
        br->condition->setParentScope(nodes.top());
        for (auto s : br->statements)
        {
            s->setParentScope(nodes.top());
        }
    }

    if (!i->else_branch->statements.empty())
    {
        i->else_branch->condition->setParentScope(nodes.top());
        for (auto s : i->else_branch->statements)
        {
            s->setParentScope(nodes.top());
        }
    }

    // insert into scope
    insertStatementsIntoScope(i->if_branch->statements, nodes.top());
    for (auto elif_branch : i->elif_branches)
    {
        insertStatementsIntoScope(elif_branch->statements, nodes.top());
    }
    insertStatementsIntoScope(i->else_branch->statements, nodes.top());

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

void SymbolsVisitor::visitDo(Do *d)
{
    currentParentScopes.push(nodes.top()->metadata.scope);

    for (auto s : d->statements)
    {
        s->setParentScope(nodes.top());
    }
    d->identifier->setParentScope(nodes.top());

    insertStatementsIntoScope(d->statements, nodes.top());

    visitMany(d->statements);
    visit(d->identifier);

    currentParentScopes.pop();
}

void SymbolsVisitor::visitWhile(While* w)
{
    currentParentScopes.push(nodes.top()->metadata.scope);

    w->condition->setParentScope(nodes.top());
    for (auto s : w->statements)
    {
        s->setParentScope(nodes.top());
    }

    // insert into scope
    insertStatementsIntoScope(w->statements, nodes.top());

    visit(w->condition);
    visitMany(w->statements);

    currentParentScopes.pop();
}

void SymbolsVisitor::visitDefer(Defer* d)
{
    for (auto s : d->statements)
    {
        s->setParentScope(nodes.top());
    }

    // insert into scope
    insertStatementsIntoScope(d->statements, nodes.top());

    visitMany(d->statements);
}

void SymbolsVisitor::visitValueTuple(ValueTuple* v)
{
    for (auto value : v->values)
    {
        value->setParentScope(nodes.top());
        visit(value);
    }
}

void SymbolsVisitor::visitBinOp(BinOp *b)
{
    b->lhs->setParentScope(nodes.top());
    b->rhs->setParentScope(nodes.top());

    visit(b->lhs);
    visit(b->rhs);
}

void SymbolsVisitor::visitDot(Dot* d)
{
    d->lhs->setParentScope(nodes.top());
    visit(d->lhs);
    if (d->lhs->type == NodeType::SYMBOL)
    {
        visit(d->lhs->data.symbol->resolved);
    }
}

void SymbolsVisitor::visitNeg(Neg* n)
{
    n->value->setParentScope(nodes.top());
    visit(n->value);
}

void SymbolsVisitor::visitCast(Cast* c)
{
    c->type->setParentScope(nodes.top());
    c->value->setParentScope(nodes.top());

    visit(c->type);
    visit(c->value);
}

void SymbolsVisitor::visitHeap(Heap* h)
{
    h->value->setParentScope(nodes.top());
    visit(h->value);
}

void SymbolsVisitor::visitSizeof(Sizeof* s)
{
    s->type->setParentScope(nodes.top());

    visit(s->type);
}

void SymbolsVisitor::visitAssert(Assert* a)
{
    a->value->setParentScope(nodes.top());

    visit(a->value);
}

void SymbolsVisitor::visitNot(Not* n)
{
    n->value->setParentScope(nodes.top());
    visit(n->value);
}

void SymbolsVisitor::visitQuestion(Question *q)
{
    q->value->setParentScope(nodes.top());
    visit(q->value);
}

void SymbolsVisitor::visitImport(Import* i)
{
    i->value->setParentScope(nodes.top());
    visit(i->value);

    // bring all symbols from i into scope
    auto value = i->value;
    if (i->value->type == NodeType::SYMBOL)
    {
        value = i->value->data.symbol->resolved;
    }
    if (i->value->type == NodeType::DOT)
    {
        value = resolveDot(i->value->data.dot);
    }
    if (value == nullptr)
    {
        errors.push_back(ParseError{"unrecognized target for import", nodes.top()->metadata.region});
        return;
    }
    if (value->type != NodeType::MODULE)
    {
        errors.push_back(ParseError{fmt::format("cannot import a {0}", TYPETOSTRING(value->type)),
                                    value->metadata.region});
        return;
    }
    for (auto s : value->data.module->statements)
    {
        if (s->type == NodeType::DECLARATION)
        {
            nodes.top()->metadata.scope->parent->values[s->data.declaration->name] = s;
        }
        else if (s->type == NodeType::TYPEDEFINITION)
        {
            nodes.top()->metadata.scope->parent->values[s->data.typeDefinition->name] = s;
        }
        else if (s->type == NodeType::MODULE)
        {
            nodes.top()->metadata.scope->parent->values[s->data.module->name] = s;
        }
    }
}

void SymbolsVisitor::visitSpread(Spread* s)
{
    s->value->setParentScope(nodes.top());
    visit(s->value);
}

void SymbolsVisitor::visitJump(Jump *j)
{
    j->resolved = nodes.top()->metadata.scope->resolve(j->name, nodes.top()->metadata.region);
    if (j->resolved->type == NodeType::NONE)
    {
        errors.push_back(ParseError{
            fmt::format("could not resolve label {}", j->name),
            nodes.top()->metadata.region
        });
    }
}

void SymbolsVisitor::visitPipe(Pipe* p)
{
    p->args->setParentScope(nodes.top());
    p->func->setParentScope(nodes.top());

    visit(p->args);
    if (p->func->type == NodeType::APPLY)
    {
        if (p->func->data.apply->operator_->type == NodeType::SYMBOL)
        {
            p->func->data.apply->operator_->data.symbol->needs_resolution = false;
        }
    }
    else if (p->func->type == NodeType::SYMBOL)
    {
        p->func->data.symbol->needs_resolution = false;
    }
    visit(p->func);
}

void SymbolsVisitor::visitRequire(Require* r)
{
    // import all modules from resolved
    assert(r->resolved->type == NodeType::UNIT);
    for (auto m : r->resolved->data.unit->modules)
    {
        if (m->type == NodeType::MODULE)
        {
            nodes.top()->metadata.scope->parent->values[m->data.module->name] = m;
        }
    }

    visit(r->resolved);
}

void SymbolsVisitor::visitLabel(Label *l)
{
    // if key already exists then there's an error
    auto found = nodes.top()->metadata.scope->values.find(l->name);
    if (found != nodes.top()->metadata.scope->values.end())
    {
        auto error = ParseError{fmt::format("redeclaration of symbol {}", l->name), nodes.top()->metadata.region};
        error.notes.push_back(fmt::format("previous declaration was here: {}", found->second->metadata.region));
    }

    nodes.top()->metadata.scope->parent->values[l->name] = nodes.top();
}

