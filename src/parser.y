%require "3.8"
%language "c++"
%define api.token.constructor
%define api.value.type variant
%define api.value.automove
%define parse.lac full

%define api.namespace { pyrite }

%define api.parser.class  { Parser }

%code requires {
    #include "ast.h"

    namespace pyrite {
        class Lexer;
    }
}

%debug

%locations

%define parse.error verbose

%{

#include <cstdint>
#include <iostream>
#include <memory>
#include <vector>
#include "error.h"
#include "ast.h"
#include "parser.hh"
#include "lexer.h"

pyrite::Parser::symbol_type yylex(pyrite::Lexer& lexer) {
    return lexer.nextToken();
}

using std::make_unique;

%}

%lex-param { pyrite::Lexer& lexer }
%parse-param { pyrite::Lexer& lexer }
%parse-param { std::string fileName }
%parse-param {std::unique_ptr<pyrite::AstNode> *ast}

%initial-action {
    // Set the file name on the initial location (goes into compilation-unit).
    @$.initialize(&fileName);
}

%token <std::string> IDENTIFIER STRING_LITERAL
%token <int64_t> INTEGER_LITERAL
%token <double> FLOAT_LITERAL

%start compilation-unit

%type <std::unique_ptr<AstNode>> definition
%type <std::vector<std::unique_ptr<AstNode>>> definitions

%%

compilation-unit: definitions {
    *ast = make_unique<CompilationUnitNode>($1);
}

definitions: /* empty */ {
    /* Bison default-initializes it automatically */
}
| definitions definition {
    auto list = $1;
    list.push_back(std::move($2));
    $$ = std::move(list);
}

definition: IDENTIFIER {}

%%

void pyrite::Parser::error(const pyrite::Parser::location_type& location, const std::string& message) {
    throw PyriteException(message, location.begin.line, location.begin.column);
}
