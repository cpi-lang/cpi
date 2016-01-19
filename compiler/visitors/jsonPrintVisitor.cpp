#include "visitor.h"

using nlohmann::json;

json toJson(Location l)
{
    return {
        {"line", l.line},
        {"column", l.col}
    };
}

json toJson(Region r)
{
    return {
        {"fileName", r.fileName},
        {"start", toJson(r.start)},
        {"end", toJson(r.end)}
    };
}

json JsonPrintVisitor::metadata(Node *n)
{
    return {
        {"region", toJson(n->metadata.region)},
        {"annotations", n->metadata.annotations}
    };
}

json JsonPrintVisitor::visitRetOutput(Node *n)
{
    forceVisit(n);
//    output["metadata"] = metadata(n);
    return output;
}

json JsonPrintVisitor::visitManyRetOutput(vector<Node *> n)
{
    auto r = json::array();
    for (auto _n : n)
    {
        r.push_back(visitRetOutput(_n));
    }
    return r;
}

void JsonPrintVisitor::visitNone(None n)
{
    output = {{"kind", "none"}};
}

void JsonPrintVisitor::visitUnit(Unit* u)
{
    output = {
        {"kind", "unit"},
        {"children", visitManyRetOutput(u->modules)}
    };
}

void JsonPrintVisitor::visitModule(Module* m)
{
    output = {
        {"kind", "module"},
        {"name", m->name},
        {"children", visitManyRetOutput(m->statements)}
    };
}

void JsonPrintVisitor::visitSymbol(Symbol *s)
{
    output = {
        {"kind", "symbol"},
        {"value", s->value}
    };
}

void JsonPrintVisitor::visitConstant(Constant* c)
{
    output = {
        {"kind", "constant"}
    };

    switch (c->type)
    {
        case ConstantType::None:
        {
            output["type"] = "none";
            output["value"] = {{"kind", "none"}};
        }
            break;
        case ConstantType::I8:
        {
            output["type"] = "i8";
            output["value"] = c->data.intvalue;
        }
            break;
        case ConstantType::I16:
        {
            output["type"] = "i16";
            output["value"] = c->data.intvalue;
        }
            break;
        case ConstantType::I32:
        {
            output["type"] = "i32";
            output["value"] = c->data.intvalue;
        }
            break;
        case ConstantType::I64:
        {
            output["type"] = "i64";
            output["value"] = c->data.intvalue;
        }
            break;
        case ConstantType::U8:
        {
            output["type"] = "u8";
            output["value"] = c->data.intvalue;
        }
            break;
        case ConstantType::U16:
        {
            output["type"] = "u16";
            output["value"] = c->data.intvalue;
        }
            break;
        case ConstantType::U32:
        {
            output["type"] = "u32";
            output["value"] = c->data.intvalue;
        }
            break;
        case ConstantType::U64:
        {
            output["type"] = "u64";
            output["value"] = c->data.intvalue;
        }
            break;
        case ConstantType::F32:
        {
            output["type"] = "f32";
            output["value"] = c->data.floatvalue;
        }
            break;
        case ConstantType::F64:
        {
            output["type"] = "f64";
            output["value"] = c->data.floatvalue;
        }
            break;
        case ConstantType::BOOL:
        {
            output["type"] = "bool";
            output["value"] = c->data._bool;
        }
            break;
        case ConstantType::CHAR:
        {
            output["type"] = "char";
            output["value"] = c->data._char;
        }
            break;
        case ConstantType::STRING:
        {
            output["type"] = "string";
            output["value"] = c->data._string;
        }
            break;
        case ConstantType::NIL:
        {
            output["type"] = "nil";
            output["value"] = nullptr;
        }
            break;
        case ConstantType::TUPLE:
        {
            auto values = visitManyRetOutput(c->data.tuple.values);

            output = {
                {"kind", "constant"},
                {"type", "tuple"},
                {"values", values}
            };
        }
            break;
    }
}

void JsonPrintVisitor::visitDeclaration(Declaration* d)
{
    auto dtype = d->type;
    if (d->type->type == NodeType::TYPEDEFINITION)
    {
        dtype = symbol(d->type->data.typeDefinition->name, d->type->metadata.region);
    }

    output = {
        {"kind", "declaration"},
        {"name", d->name},
        {"type", visitRetOutput(dtype)},
        {"initialValue", visitRetOutput(d->initialValue)},
        {"declarationType", d->declarationType == DeclarationType::IMMUTABLE ? "immutable" : "mutable"},
        {"external", d->external},
        {"global", d->global}
    };
}

void JsonPrintVisitor::visitParameter(Parameter* p)
{
    output = {
        {"kind", "parameter"},
        {"name", p->name},
        {"type", visitRetOutput(p->type)},
        {"order", p->order},
        {"initialValue", visitRetOutput(p->initialValue)}
    };
}

void JsonPrintVisitor::visitArgument(Argument *a)
{
    output = {
        {"kind", "argument"},
        {"name", a->name},
        {"value", visitRetOutput(a->value)}
    };
}

void JsonPrintVisitor::visitAssignment(Assignment* a)
{
    output = {
        {"kind", "assignment"},
        {"lhs", visitRetOutput(a->lhs)},
        {"rhs", visitRetOutput(a->rhs)}
    };
}

void JsonPrintVisitor::visitTypeTuple(TypeTuple* t)
{
    output = {
        {"kind", "typeTuple"},
        {"types", visitManyRetOutput(t->types)},
        {"isAnonymous", t->isAnonymous}
    };
}

void JsonPrintVisitor::visitTypeFunction(TypeFunction t)
{
    output = {
        {"kind", "typeFunction"},
        {"input", visitManyRetOutput(t.input.types)},
        {"output", visitRetOutput(t.output)}
    };
}

void JsonPrintVisitor::visitTypeConstant(TypeConstant *t)
{
    output = {{"kind", "typeBasic"}};

    switch (t->type)
    {
        case ConstantType::NIL:
        {
            output["type"] = "nil";
        }
            break;
        case ConstantType::CHAR:
        {
            output["type"] = "char";
        }
            break;
        case ConstantType::STRING:
        {
            output["type"] = "string";
        }
            break;
        case ConstantType::BOOL:
        {
            output["type"] = "bool";
        }
            break;
        case ConstantType::I8:
        {
            output["type"] = "i8";
        }
            break;
        case ConstantType::I16:
        {
            output["type"] = "i16";
        }
            break;
        case ConstantType::I32:
        {
            output["type"] = "i32";
        }
            break;
        case ConstantType::I64:
        {
            output["type"] = "i64";
        }
            break;
        case ConstantType::U8:
        {
            output["type"] = "u8";
        }
            break;
        case ConstantType::U16:
        {
            output["type"] = "u16";
        }
            break;
        case ConstantType::U32:
        {
            output["type"] = "u32";
        }
            break;
        case ConstantType::U64:
        {
            output["type"] = "u64";
        }
            break;
        case ConstantType::F32:
        {
            output["type"] = "f32";
        }
            break;
        case ConstantType::F64:
        {
            output["type"] = "f64";
        }
            break;
        case ConstantType::None: {
            output["type"] = "none";
        }
            break;
        case ConstantType::TUPLE: {
            output["type"] = "tuple";
        }
            break;
    }
}

void JsonPrintVisitor::visitTypePointer(TypePointer* t)
{
    output = {
        {"kind", "typePointer"},
        {"type", visitRetOutput(t->type)}
    };
}

void JsonPrintVisitor::visitApply(Apply* a)
{
    output = {
        {"kind", "apply"},
        {"operator", visitRetOutput(a->operator_)},
        {"operands", visitManyRetOutput(a->operands)}
    };
}

void JsonPrintVisitor::visitTypeDefinition(TypeDefinition* t)
{
    output = {
        {"kind", "typeDefinition"},
        {"name", t->name},
        {"type", visitRetOutput(t->type)}
    };
}

void JsonPrintVisitor::visitRec(Rec* r)
{
    output = {
        {"kind", "rec"}
    };
}

void JsonPrintVisitor::visitFunction(Function* f)
{
    output = {
        {"kind", "function"},
        {"type", visitRetOutput(f->type)},
        {"parameters", visitManyRetOutput(f->parameters)},
        {"statements", visitManyRetOutput(f->block)}
    };
}

void JsonPrintVisitor::visitRet(Ret *r)
{
    output = {
        {"kind", "ret"},
        {"value", visitRetOutput(r->value)}
    };
}

void JsonPrintVisitor::visitTypeof(Typeof* t)
{
    if (t->is_rt)
    {
        output = {
            {"kind", "rettype"},
            {"value", visitRetOutput(t->value)}
        };
    }
    else
    {
        output = {
            {"kind", "typeof"},
            {"value", visitRetOutput(t->value)}
        };
    }
}

void JsonPrintVisitor::visitDereference(Dereference *d)
{
    output = {
        {"kind", "dereference"},
        {"value", visitRetOutput(d->value)}
    };
}

void JsonPrintVisitor::visitAddressOf(AddressOf* a)
{
    output = {
        {"kind", "addressOf"},
        {"value", visitRetOutput(a->value)}
    };
}

void JsonPrintVisitor::visitIf(If* i)
{
    vector<json> elifBranches = {};
    for (auto eb : i->elif_branches)
    {
        elifBranches.push_back({
                                   {"condition", visitRetOutput(eb->condition)},
                                   {"statements", visitManyRetOutput(eb->statements)}
                               });
    }

    output = {
        {"kind", "if"},
        {"ifBranch", {
                     {"condition", visitRetOutput(i->if_branch->condition)},
                     {"statements", visitManyRetOutput(i->if_branch->statements)}
                 }},
        {"elifBranches", elifBranches},
        {"elseBranch", {
                     {"condition", visitRetOutput(i->else_branch->condition)},
                     {"statements", visitManyRetOutput(i->else_branch->statements)}
                 }}
    };
}

void JsonPrintVisitor::visitDo(Do* d)
{
    output = {
        {"kind", "do"},
        {"outputIdentifier", visitRetOutput(d->identifier)},
        {"statements", visitManyRetOutput(d->statements)}
    };
}

void JsonPrintVisitor::visitWhile(While* w)
{
    output = {
        {"kind", "while"},
        {"condition", visitRetOutput(w->condition)},
        {"statements", visitManyRetOutput(w->statements)}
    };
}

void JsonPrintVisitor::visitDefer(Defer* d)
{
    output = {
        {"kind", "defer"},
        {"statements", visitManyRetOutput(d->statements)}
    };
}

void JsonPrintVisitor::visitValueTuple(ValueTuple* v)
{
    output = {
        {"kind", "tuple"},
        {"values", visitManyRetOutput(v->values)}
    };
}

void JsonPrintVisitor::visitBinOp(BinOp* b)
{
    output = {
        {"kind", "binOp"},
        {"lhs", visitRetOutput(b->lhs)},
        {"rhs", visitRetOutput(b->rhs)}
    };

    switch (b->type)
    {
        case OpType::EQEQ:
            output["op"] = "==";
            break;
        case OpType::NEQ:
            output["op"] = "!=";
            break;
        case OpType::PLUS:
            output["op"] = "+";
            break;
        case OpType::MINUS:
            output["op"] = "-";
            break;
        case OpType::PIPE:
            output["op"] = "|";
            break;
        case OpType::STAR:
            output["op"] = "*";
            break;
        case OpType::LT:
            output["op"] = "<";
            break;
        case OpType::LTE:
            output["op"] = "<=";
            break;
        case OpType::GT:
            output["op"] = ">";
            break;
        case OpType::GTE:
            output["op"] = ">=";
            break;
        case OpType::FSLASH:
            output["op"] = "/";
            break;
        case OpType::OR:
            output["op"] = "or";
            break;
        case OpType::MOD:
            output["op"] = "mod";
            break;
        default:
            assert(false);
    }
}

void JsonPrintVisitor::visitDot(Dot* d)
{
    output = {
        {"kind", "dot"},
        {"lhs", visitRetOutput(d->lhs)},
        {"rhs", visitRetOutput(d->rhs)}
    };
}

void JsonPrintVisitor::visitNeg(Neg* n)
{
    output = {
        {"kind", "neg"},
        {"value", visitRetOutput(n->value)}
    };
}

void JsonPrintVisitor::visitCast(Cast* c)
{
    output = {
        {"kind", "cast"},
        {"type", visitRetOutput(c->type)},
        {"value", visitRetOutput(c->value)}
    };
}

void JsonPrintVisitor::visitHeap(Heap* h)
{
    output = {
        {"kind", "heap"},
        {"value", visitRetOutput(h->value)}
    };
}

void JsonPrintVisitor::visitSizeof(Sizeof* s)
{
    output = {
        {"kind", "sizeof"},
        {"type", visitRetOutput(s->type)}
    };
}

void JsonPrintVisitor::visitAssert(Assert* a)
{
    output = {
        {"kind", "assert"},
        {"value", visitRetOutput(a->value)}
    };
}

void JsonPrintVisitor::visitNot(Not* n)
{
    output = {
        {"kind", "not"},
        {"value", visitRetOutput(n->value)}
    };
}

void JsonPrintVisitor::visitQuestion(Question *q)
{
    output = {
        {"kind", "question"},
        {"value", visitRetOutput(q->value)}
    };
}

void JsonPrintVisitor::visitImport(Import* i)
{
    output = {
        {"kind", "import"},
        {"value", visitRetOutput(i->value)}
    };
}

void JsonPrintVisitor::visitRequire(Require *r)
{
    output = {
        {"kind", "require"},
        {"fileName", r->fileName}
    };
}

void JsonPrintVisitor::visitStringLiteral(StringLiteral* s)
{
    output = {
        {"kind", "stringLiteral"},
        {"value", s->s}
    };
}

void JsonPrintVisitor::visitPipe(Pipe* p)
{
    output = {
        {"kind", "pipe"},
        {"operator", visitRetOutput(p->func)},
        {"operands", visitRetOutput(p->args)}
    };
}

void JsonPrintVisitor::visitCharacter(Character *c)
{
    string s = "c";
    s[0] = c->c;

    output = {
        {"kind", "char"},
        {"value", s}
    };
}

void JsonPrintVisitor::visitSpread(Spread *s)
{
    output = {
        {"kind", "spread"},
        {"value", visitRetOutput(s->value)}
    };
}
