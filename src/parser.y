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

pyrite::AstMetadata createMetadata(const pyrite::location &location) {
    pyrite::AstMetadata metadata;
    metadata.line = location.begin.line;
    metadata.column = location.begin.column;
    return metadata;
}

%}

%lex-param { pyrite::Lexer& lexer }
%parse-param { pyrite::Lexer& lexer }
%parse-param { std::string fileName }
%parse-param {std::unique_ptr<pyrite::AstNode> *ast}

%initial-action {
    // Set the file name on the initial location (goes into compilation-unit).
    @$.initialize(&fileName);
}

%token <std::string> IDENTIFIER "identifier" STRING_LITERAL "string literal"
%token <int64_t> INTEGER_LITERAL "integer literal"
%token <double> FLOAT_LITERAL "float literal"

%token TRUE "true" FALSE "false"

%token RETURN "return"

%token I8 "i8" I16 "i16" I32 "i32" I64 "i64" U8 "u8" U16 "u16" U32 "u32" U64 "u64" F32 "f32" F64 "f64" BOOL "bool" CHAR "char" VOID "void"
%token AUTO "auto" ANY "any"
%token ENUM "enum"

%token MUT "mut"

%token PLUS "+" MINUS "-" STAR "*" SLASH "/" PERCENT "%" AMPERSAND "&" PIPE "|" CARET "^" TILDE "~" BANG "!" EQUALS "=" LESS "<" GREATER ">" QUESTION "?" COLON ":" DOT "." COMMA "," SEMICOLON ";"

%token EQUALS_EQUALS "==" BANG_EQUALS "!=" LESS_EQUALS "<=" GREATER_EQUALS ">=" PIPE_PIPE "||" AMPERSAND_AMPERSAND "&&" PLUS_PLUS "++" MINUS_MINUS "--" LESS_LESS "<<" GREATER_GREATER ">>"

%token ARROW "->"

%token PLUS_EQUALS "+=" MINUS_EQUALS "-=" STAR_EQUALS "*=" SLASH_EQUALS "/=" PERCENT_EQUALS "%=" AMPERSAND_EQUALS "&=" PIPE_EQUALS "|=" CARET_EQUALS "^=" LESS_LESS_EQUALS "<<=" GREATER_GREATER_EQUALS ">>="

%token LEFT_PAREN "(" RIGHT_PAREN ")" LEFT_BRACE "{" RIGHT_BRACE "}" LEFT_BRACKET "[" RIGHT_BRACKET "]"

%start compilation-unit

%type <std::vector<std::string>> piped-identifier-list

%type <std::unique_ptr<AstNode>> definition expression statement block-statement
%type <std::vector<std::unique_ptr<AstNode>>> definitions statement-list

%type <std::unique_ptr<Type>> type
%type <std::vector<std::unique_ptr<Type>>> piped-type-list
%type <NameAndType> name-and-type
%type <std::vector<NameAndType>> name-and-type-list

%right "="
%left "||" "&&"
%left "==" "!="
%left "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/" "%"
%left "|" "^" "&"

%%

compilation-unit: definitions {
    *ast = make_unique<CompilationUnitNode>($1, createMetadata(@1));
}

definitions: /* empty */ {
    /* Bison default-initializes it automatically */
}
| definitions definition {
    auto list = $1;
    list.push_back($2);
    $$ = std::move(list);
}

definition:
type IDENTIFIER "=" expression ";" {
    $$ = std::make_unique<VariableDefinitionNode>($1, $2, $4, false, createMetadata(@1));
}
| "mut" type IDENTIFIER "=" expression ";" {
    $$ = std::make_unique<VariableDefinitionNode>($2, $3, $5, true, createMetadata(@1));
}
| type IDENTIFIER "(" name-and-type-list ")" block-statement {
    $$ = std::make_unique<FunctionDefinitionNode>($2, $4, $1, $6, createMetadata(@1));
}

statement:
definition
| block-statement
| expression ";"
| "return" expression ";" {
    $$ = std::make_unique<ReturnStatementNode>($2, createMetadata(@1));
}

block-statement: "{" statement-list "}" {
    $$ = std::make_unique<BlockStatementNode>($2, createMetadata(@1));
}

statement-list: /* empty */ {
    /* Bison default-initializes it automatically */
}
| statement-list statement {
    auto list = $1;
    list.push_back($2);
    $$ = std::move(list);
}

piped-type-list: type {
    $$.push_back($1);
}
| piped-type-list "|" type {
    auto list = $1;
    list.push_back($3);
    $$ = std::move(list);
}

name-and-type-list: /* empty */ {
    /* Bison default-initializes it automatically */
}
| name-and-type {
    $$.push_back($1);
}
| name-and-type-list "," name-and-type {
    auto list = $1;
    list.push_back($3);
    $$ = std::move(list);
}

name-and-type: type IDENTIFIER {
    $$ = NameAndType{$2, $1};
}

type:
"i8" {
    $$ = std::make_unique<IntegerType>(8, true);
}
| "i16" {
    $$ = std::make_unique<IntegerType>(16, true);
}
| "i32" {
    $$ = std::make_unique<IntegerType>(32, true);
}
| "i64" {
    $$ = std::make_unique<IntegerType>(64, true);
}
| "u8" {
    $$ = std::make_unique<IntegerType>(8, false);
}
| "u16" {
    $$ = std::make_unique<IntegerType>(16, false);
}
| "u32" {
    $$ = std::make_unique<IntegerType>(32, false);
}
| "u64" {
    $$ = std::make_unique<IntegerType>(64, false);
}
| "f32" {
    $$ = std::make_unique<FloatType>(32);
}
| "f64" {
    $$ = std::make_unique<FloatType>(64);
}
| "bool" {
    $$ = std::make_unique<BooleanType>();
}
| "char" {
    $$ = std::make_unique<CharType>();
}
| "void" {
    $$ = std::make_unique<VoidType>();
}
| "auto" {
    $$ = std::make_unique<AutoType>();
}
| "any" {
    $$ = std::make_unique<AnyType>();
}
| "[" type "]" {
    $$ = std::make_unique<ArrayType>($2);
}
| "any" "(" piped-type-list ")" {
    $$ = std::make_unique<UnionType>($3);
}
| "enum" "(" piped-identifier-list ")" {
    $$ = std::make_unique<EnumType>($3);
}
| IDENTIFIER {
    $$ = std::make_unique<IdentifiedType>($1);
}

expression:
INTEGER_LITERAL {
    $$ = std::make_unique<IntegerLiteralNode>($1, createMetadata(@1));
}
| FLOAT_LITERAL {
    $$ = std::make_unique<FloatLiteralNode>($1, createMetadata(@1));
}
| STRING_LITERAL {
    $$ = std::make_unique<StringLiteralNode>($1, createMetadata(@1));
}
| "true" {
    $$ = std::make_unique<BooleanLiteralNode>(true, createMetadata(@1));
}
| "false" {
    $$ = std::make_unique<BooleanLiteralNode>(false, createMetadata(@1));
}
| IDENTIFIER {
    $$ = std::make_unique<VariableReferenceNode>($1, createMetadata(@1));
}
| "(" expression ")" {
    auto node = $2;
    node->getMetadata().parennedExpression = true;
    $$ = std::move(node);
}
| expression "+" expression {
    $$ = std::make_unique<BinaryExpressionNode>(BinaryOperator::ADD, $1, $3, createMetadata(@1));
}
| expression "-" expression {
    $$ = std::make_unique<BinaryExpressionNode>(BinaryOperator::SUBTRACT, $1, $3, createMetadata(@1));
}
| expression "*" expression {
    $$ = std::make_unique<BinaryExpressionNode>(BinaryOperator::MULTIPLY, $1, $3, createMetadata(@1));
}
| expression "/" expression {
    $$ = std::make_unique<BinaryExpressionNode>(BinaryOperator::DIVIDE, $1, $3, createMetadata(@1));
}
| expression "%" expression {
    $$ = std::make_unique<BinaryExpressionNode>(BinaryOperator::MODULO, $1, $3, createMetadata(@1));
}
| expression "==" expression {
    $$ = std::make_unique<BinaryExpressionNode>(BinaryOperator::EQUAL, $1, $3, createMetadata(@1));
}
| expression "!=" expression {
    $$ = std::make_unique<BinaryExpressionNode>(BinaryOperator::NOT_EQUAL, $1, $3, createMetadata(@1));
}
| expression "<" expression {
    $$ = std::make_unique<BinaryExpressionNode>(BinaryOperator::LESS_THAN, $1, $3, createMetadata(@1));
}
| expression ">" expression {
    $$ = std::make_unique<BinaryExpressionNode>(BinaryOperator::GREATER_THAN, $1, $3, createMetadata(@1));
}
| expression "<=" expression {
    $$ = std::make_unique<BinaryExpressionNode>(BinaryOperator::LESS_THAN_OR_EQUAL, $1, $3, createMetadata(@1));
}
| expression ">=" expression {
    $$ = std::make_unique<BinaryExpressionNode>(BinaryOperator::GREATER_THAN_OR_EQUAL, $1, $3, createMetadata(@1));
}

piped-identifier-list: IDENTIFIER {
    $$.push_back($1);
}
| piped-identifier-list "|" IDENTIFIER {
    auto list = $1;
    list.push_back($3);
    $$ = std::move(list);
}

%%

void pyrite::Parser::error(const pyrite::Parser::location_type& location, const std::string& message) {
    throw PyriteException(message, location.begin.line, location.begin.column);
}
