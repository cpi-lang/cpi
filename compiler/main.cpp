#include <iostream>
#include <fstream>

#include "codegen.h"
#include "parser.h"

using namespace std;

bool ends_with(std::string const &fullString, std::string const &ending)
{
    if (fullString.length() >= ending.length())
    {
        return (0 == fullString.compare (fullString.length() - ending.length(), ending.length(), ending));
    }
    else
    {
        return false;
    }
}

void print_usage()
{
    cout << "usage: cpi input.(cpi | json) [output.ll | output.json]" << endl;
    exit(-1);
}

void visitErrors(Visitor *v)
{
    if (v->errors.empty())
    {
        return;
    }

    for (auto e : v->errors)
    {
        reportError(e);
    }

    exit(-1);
}

int main(int argc, char **argv)
{
    if (argc != 2 && argc != 3)
    {
        print_usage();
    }

    auto fileName = argv[1];

    auto json = ends_with(fileName, "json");
    if (!json && !ends_with(fileName, "cpi"))
    {
        print_usage();
    }

    string strippedName = fileName;
    if (json)
    {
        strippedName = strippedName.substr(0, strippedName.length() - string(".json").length());
    }
    else
    {
        strippedName = strippedName.substr(0, strippedName.length() - string(".cpi").length());
    }

    string output_file_name;

    if (argc == 3)
    {
        output_file_name = string(argv[2]);
    }
    else
    {
        output_file_name = strippedName;
    }

    if (argc == 3
        && !ends_with(string(argv[2]), ".ll")
        && !ends_with(string(argv[2]), ".json"))
    {
        print_usage();
    }

    // READ
    auto source = readFile(fileName);

    Node *m;
    if (json)
    {
        m = parse_unit_from_json(source);
    }
    else
    {
        m = parse_unit(lex(source, fileName));
    }

    // SETUP
    auto rv = new RequireVisitor();
    rv->visit(m);
    visitErrors(rv);

    auto sv = new SymbolsVisitor();
    sv->visit(m);
    visitErrors(sv);

    auto tpv = new TypePropagationVisitor();
    tpv->visit(m);
    visitErrors(tpv);

    // PRINT
//    (new PrintVisitor())->visit(m);

    if (ends_with(output_file_name, ".json"))
    {
        auto jpv = new JsonPrintVisitor();
        jpv->visit(m);

        std::ofstream out(output_file_name);
        out << jpv->output.dump(2);
        out.close();

        return 0;
    }

    // CODEGEN
    auto cv = new CodegenVisitor();
    cv->visit(m);
    visitErrors(cv);
    LLVMSetTarget(cv->mod, "x86_64-apple-macosx10.10.0");

    // RUN (LLVM)
//    LLVMDumpModule(cv->mod);

    LLVMPrintModuleToFile(cv->mod, fmt::format("{}.ll", strippedName).c_str(), nullptr);

    return 0;
}
