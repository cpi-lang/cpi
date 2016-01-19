#include <fstream>
#include "visitor.h"
#include "../parser.h"

void RequireVisitor::visitRequire(Require *r)
{
    // include this file in the list of files to parse
    auto fileName = r->fileName;
    if (requireds.find(fileName) == requireds.end())
    {
        requireds[fileName] = none();
        auto newRead = readFile(fileName);
        auto newLexed = lex(newRead, fileName);
        auto newUnit = parse_unit(newLexed);

        requireds[fileName] = newUnit;
        r->resolved = newUnit;

        visit(newUnit);
    }
    else
    {
        r->resolved = requireds[fileName];
    }
}
