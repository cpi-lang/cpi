#include "visitor.h"

void PrintVisitor::visitNone(None n)
{
    cout << "_";
}

void PrintVisitor::visitUnit(Unit* u)
{
    for (auto m : u->modules)
    {
        forceVisit(m);
        cout << endl;
    }
}

void PrintVisitor::visitSymbol(Symbol* s)
{
    if (s->resolved != nullptr)
    {
        cout << s->value;
    }
    else
    {
        cout << s->value;
    }
}

void PrintVisitor::visitConstant(Constant* c)
{
    switch (c->type)
    {
        case ConstantType::I32:
            cout << c->data.intvalue;
            break;
        case ConstantType::F32:
            cout << c->data.floatvalue;
            break;
        case ConstantType::BOOL:
            cout << (c->data._bool ? "true" : "false");
            break;
        case ConstantType::STRING:
            cout << c->data._string;
            break;
        case ConstantType::CHAR:
            cout << c->data._char;
            break;
        case ConstantType::NIL:
            cout << "nil";
            break;
        default:
            assert(false);
    }
}

void PrintVisitor::visitModule(Module* m)
{
    cout << "module " << m->name << endl;

    indent();
    cout << "{" << endl;

    indentationLevel += 1;
    for (auto s : m->statements)
    {
        indent();
        forceVisit(s);
        terminate();
        cout << endl;
    }
    indentationLevel -= 1;

    indent();
    cout << "}";
}

void PrintVisitor::visitDeclaration(Declaration* d)
{
    cout << d->name;

    switch (d->declarationType)
    {
        case DeclarationType::IMMUTABLE:
            if (d->type->type != NodeType::NONE)
            {
                cout << " : ";
                forceVisit(d->type);

                if (d->initialValue->type != NodeType::NONE)
                {
                    cout << " = ";
                    forceVisit(d->initialValue);
                }
            }
            else
            {
                cout << " := ";
                forceVisit(d->initialValue);
            }
            break;
        case DeclarationType::MUTABLE:
            if (d->type->type != NodeType::NONE)
            {
                cout << " :: ";
                forceVisit(d->type);
                if (d->initialValue->type != NodeType::NONE)
                {
                    cout << " = ";
                    forceVisit(d->initialValue);
                }
            }
            else
            {
                cout << " ::= ";
                forceVisit(d->initialValue);
            }
            break;
    }
}

void PrintVisitor::visitParameter(Parameter* p)
{
    if (p->name != "")
    {
        cout << p->name;
        if (p->type->type != NodeType::NONE)
        {
            cout << " : ";
        }
    }

    if (p->type->type != NodeType::NONE)
    {
        forceVisit(p->type);

        if (p->initialValue->type != NodeType::NONE)
        {
            cout << " = ";
            forceVisit(p->initialValue);
        }
    }

    else if (p->type->type == NodeType::NONE && p->initialValue->type != NodeType::NONE)
    {
        cout << " := ";
        forceVisit(p->initialValue);
    }
}

void PrintVisitor::visitArgument(Argument *a)
{
    if (a->name != "")
    {
        cout << a->name << " : ";
    }
    forceVisit(a->value);
}

void PrintVisitor::visitAssignment(Assignment* a)
{
    forceVisit(a->lhs);
    cout << " = ";
    forceVisit(a->rhs);
}

void PrintVisitor::visitTypeTuple(TypeTuple* t)
{
    unsigned long i = 0;
    auto count = t->types.size();

    cout << "{";
    for(auto type : t->types)
    {
        forceVisit(type);

        i += 1;
        if (i != count)
        {
            cout << ", ";
        }
    }
    cout << "}";
}

void PrintVisitor::visitTypeFunction(TypeFunction t)
{
    visitTypeTuple(&t.input);
    cout << " => ";
    forceVisit(t.output);
}

void PrintVisitor::visitTypeConstant(TypeConstant *t)
{
    switch (t->type)
    {
        case ConstantType::NIL:
        {
            cout << "nil";
        }
            break;
        case ConstantType::CHAR:
        {
            cout << "char";
        }
            break;
        case ConstantType::STRING:
        {
            cout << "string";
        }
            break;
        case ConstantType::BOOL:
        {
            cout << "bool";
        }
            break;
        case ConstantType::I8:
        {
            cout << "i8";
        }
            break;
        case ConstantType::I16:
        {
            cout << "i16";
        }
            break;
        case ConstantType::I32:
        {
            cout << "i32";
        }
            break;
        case ConstantType::I64:
        {
            cout << "i64";
        }
            break;
        case ConstantType::U8:
        {
            cout << "u8";
        }
            break;
        case ConstantType::U16:
        {
            cout << "u16";
        }
            break;
        case ConstantType::U32:
        {
            cout << "u32";
        }
            break;
        case ConstantType::U64:
        {
            cout << "u64";
        }
            break;
        case ConstantType::F32:
        {
            cout << "f32";
        }
            break;
        case ConstantType::F64:
        {
            cout << "f64";
        }
            break;
        case ConstantType::None: {
            cout << "_";
        }
        case ConstantType::TUPLE: {
            cout << "tuple";
        }
            break;
    }
}

void PrintVisitor::visitTypePointer(TypePointer* t)
{
    cout << "*";

    // wrap function type for clarity
//    if (t->type->type == NodeType::TYPEFUNCTION)
//    {
        cout << "(";
        forceVisit(t->type);
        cout << ")";
        return;
//    }

//    forceVisit(t->type);
}

void PrintVisitor::visitApply(Apply* a)
{
    forceVisit(a->operator_);

    cout << "(";
    unsigned long i = 0;
    auto count = a->operands.size();
    for (auto p : a->operands)
    {
        forceVisit(p);

        i += 1;
        if (i != count)
        {
            cout << ", ";
        }
    }
    cout << ")";
}

void PrintVisitor::visitTypeDefinition(TypeDefinition* t)
{
    cout << "type " << t->name << " = ";
    forceVisit(t->type);
}

void PrintVisitor::visitRec(Rec* r)
{
    if (r->resolved != nullptr)
    {
        cout << "rec<<";
        cout << TYPETOSTRING(r->resolved->type);
        cout << ">>";
    }
    else
    {
        cout << "rec";
    }
}

void PrintVisitor::visitFunction(Function* f)
{
    cout << "{";
    auto paramCount = 0;
    for (auto s : f->parameters)
    {
        if (s->type != NodeType::PARAMETER)
        {
            errors.push_back(ParseError{"expected parameter", s->metadata.region});
        }
        cout << s->data.parameter->name;
        if (s->data.declaration->type->type != NodeType::NONE)
        {
            cout << " : ";
            forceVisit(s->data.declaration->type);
            if (paramCount < f->parameters.size() - 1)
            {
                cout << ", ";
            }
        }
        paramCount += 1;
    }
    cout << "}";

    cout << " => ";

    if (f->block.empty())
    {
        cout << "{}";
        return;
    }
    else if (f->block.size() == 1)
    {
        if (f->block[0]->type != NodeType::RET)
        {
            errors.push_back(ParseError{fmt::format("expected return statement, not {0}", TYPETOSTRING(f->block[0]->type)),
                                        f->block[0]->metadata.region});
        }
        forceVisit(f->block[0]->data.ret->value);
        terminate();
        return;
    }

    cout << endl;
    indent();
    cout << "{" << endl;

    unsigned int i = 0;
    indentationLevel += 1;
    for (auto b : f->block)
    {
        indent();
        forceVisit(b);
        i += 1;

        if (i != f->block.size())
        {
            cout << endl;
        }
    }
    indentationLevel -= 1;

    cout << endl;
    indent();
    cout << "}";
}

void PrintVisitor::visitRet(Ret *r)
{
    cout << "ret ";
    forceVisit(r->value);
}

void PrintVisitor::visitTypeof(Typeof* t)
{
    if (t->is_rt)
    {
        cout << "rt ";
    }
    else
    {
        cout << "typeof ";
    }
    forceVisit(t->value);
}

void PrintVisitor::visitDereference(Dereference *d)
{
    cout << "*";
    forceVisit(d->value);
}

void PrintVisitor::visitAddressOf(AddressOf* a)
{
    cout << "&";
    forceVisit(a->value);
}

void PrintVisitor::visitIf(If* i)
{
    cout << "if ";
    forceVisit(i->if_branch->condition);
    cout << endl; indent();
    cout << "{" << endl;
    indentationLevel += 1;
    for (auto s : i->if_branch->statements)
    {
        indent(); forceVisit(s);
    }
    indentationLevel -= 1;
    cout << endl; indent(); cout << "}";

    for (auto br : i->elif_branches)
    {
        cout << endl; indent();
        cout << "elif ";
        forceVisit(br->condition);
        cout << endl; indent();
        cout << "{" << endl;
        indentationLevel += 1;
        for (auto s : br->statements)
        {
            indent(); forceVisit(s);
        }
        indentationLevel -= 1;
        cout << endl; indent(); cout << "}";
    }

    if (i->else_branch->statements.empty())
    {
        return;
    }

    cout << endl; indent();
    cout << "else" << endl; indent();
    cout << "{" << endl;
    indentationLevel += 1;
    for (auto s : i->else_branch->statements)
    {
        indent(); forceVisit(s);
    }
    indentationLevel -= 1;
    cout << endl; indent(); cout << "}";
}

void PrintVisitor::visitDo(Do* d)
{
    cout << "do";
    if (d->identifier != nullptr && d->identifier->type != NodeType::NONE)
    {
        cout << "(";
        forceVisit(d->identifier);
        cout << ")";
    }
    else
    {
        cout << " ";
    }
    cout << "{" << endl;
    indentationLevel += 1;
    for (auto s : d->statements)
    {
        indent(); forceVisit(s);
    }
    indentationLevel -= 1;
    cout << endl; indent(); cout << "}";
}

void PrintVisitor::visitWhile(While* w)
{
    cout << "while ";
    forceVisit(w->condition);
    cout << endl; indent();
    cout << "{" << endl;
    indentationLevel += 1;
    for (auto s : w->statements)
    {
        indent(); forceVisit(s); cout << endl;
    }
    indentationLevel -= 1;
    indent();
    cout << "}";
}

void PrintVisitor::visitDefer(Defer* d)
{
    cout << "defer " << endl;
    indent();
    cout << "{" << endl;
    indentationLevel += 1;
    for (auto s : d->statements)
    {
        indent(); forceVisit(s); cout << endl;
    }
    indentationLevel -= 1;
    indent(); cout << "}";
}

void PrintVisitor::visitValueTuple(ValueTuple* v)
{
    cout << "#{";
    auto count = 0;
    for (auto i = 0; i < v->values.size(); i++)
    {
        forceVisit(v->values[i]);
        if (i <= count)
        {
            cout << ", ";
        }
    }
    cout << "}";
}

void PrintVisitor::visitBinOp(BinOp* b)
{
    cout << "(";
    forceVisit(b->lhs);
    cout << " ";
    switch (b->type)
    {
        case OpType::EQEQ:
            cout << "==";
            break;
        case OpType::NEQ:
            cout << "!=";
            break;
        case OpType::PLUS:
            cout << "+";
            break;
        case OpType::MINUS:
            cout << "-";
            break;
        case OpType::PIPE:
            cout << "|";
            break;
        case OpType::STAR:
            cout << "*";
            break;
        case OpType::LT:
            cout << "<";
            break;
        case OpType::LTE:
            cout << "<=";
            break;
        case OpType::GT:
            cout << ">";
            break;
        case OpType::GTE:
            cout << ">=";
            break;
        case OpType::FSLASH:
            cout << "/";
            break;
        case OpType::OR:
            cout << "or";
            break;
        case OpType::MOD:
            cout << "mod";
            break;
        default:
            assert(false);
    }
    cout << " ";
    forceVisit(b->rhs);
    cout << ")";
}

void PrintVisitor::visitDot(Dot* d)
{
    if (d->resolved != nullptr && d->resolved->type != NodeType::NONE)
    {
        forceVisit(d->resolved);
    }
    else
    {
        forceVisit(d->lhs);
        cout << ".";
        forceVisit(d->rhs);
    }
}

void PrintVisitor::visitNeg(Neg* n)
{
    cout << "-";
    forceVisit(n->value);
}

void PrintVisitor::visitCast(Cast* c)
{
    cout << "cast(";
    forceVisit(c->type);
    cout << ") ";
    forceVisit(c->value);
}

void PrintVisitor::visitHeap(Heap* h)
{
    cout << "heap ";
    forceVisit(h->value);
}

void PrintVisitor::visitSizeof(Sizeof* s)
{
    cout << "sizeof(";
    forceVisit(s->type);
    cout << ")";
}

void PrintVisitor::visitAssert(Assert* a)
{
    cout << "assert ";
    forceVisit(a->value);
}

void PrintVisitor::visitNot(Not* n)
{
    cout << "!";
    forceVisit(n->value);
}

void PrintVisitor::visitQuestion(Question *q)
{
    forceVisit(q->value);
    cout << "?";
}

void PrintVisitor::visitImport(Import* i)
{
    cout << "import ";
    forceVisit(i->value);
}

void PrintVisitor::visitRequire(Require *r)
{
    cout << "require " << r->fileName;
}

void PrintVisitor::visitStringLiteral(StringLiteral* s)
{
    cout << "\"" << s->s << "\"";
}

void PrintVisitor::visitPipe(Pipe* p)
{
    forceVisit(p->args);
    cout << " | ";
    forceVisit(p->func);
}
