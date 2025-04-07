%{
    open Ast.Ast_types
    open Parsed_ast
%}

%token <int> INT
%token <string> ID
%token <string> STRING_LITERAL
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LANGLE
%token RANGLE
%token RARROW
%token RETURN
%token COMMA
%token SEMICOLON
%token EQUAL
%token PLUS
%token MINUS
%token MULT
%token DIV
%token REM
%token AND
%token OR
%token BANG
%token LET
%token TYPE_INT
%token TYPE_BOOL
%token TYPE_VOID
%token TYPE_STRING
%token TRUE
%token FALSE
%token FUNCTION
%token IF
%token ELSE
%token MAIN
%token MODULE
%token IMPORT
%token EXTERN
%token EOF

%right EQUAL
%left PLUS MINUS LANGLE RANGLE
%left MULT DIV REM
%left AND OR
%nonassoc BANG

%start <Parsed_ast.program> program

%%

program:
    | imports=list(import_stmt); functions=list(function_defn); main=main_block; EOF { Program(imports, functions, main) }
    | imports=list(import_stmt); functions=list(function_defn); EOF { Program(imports, functions, Block []) }
    ;

import_stmt:
    | IMPORT; module_name=ID; SEMICOLON { Import(module_name) }
    ;

function_defn:
    | FUNCTION; name=ID; params=params; RARROW; return_type=diablo_type; body=block { TFunction(name, params, return_type, body) }
    ;

module_defn:
    | MODULE; name=ID; LBRACE; functions=list(function_defn); RBRACE { Module(name, functions) }
    ;

block:
    | LBRACE; exprs=separated_list(SEMICOLON, expr); RBRACE { Block(exprs) }
    ;

expr:
    | LPAREN; e=expr; RPAREN { e }
    | i=INT { Integer i }
    | id=ID { Identifier id }
    | TRUE { Boolean true }
    | FALSE { Boolean false }
    | s=STRING_LITERAL { StringLiteral s }
    | op=un_op e=expr { UnOp(op, e) }
    | e1=expr op=bin_op e2=expr { BinOp(op, e1, e2) }
    | LET; id=ID; EQUAL; e=expr { Let(id, e) }
    | IF; cond_expr=expr; then_expr=block; ELSE; else_expr=block { If(cond_expr, then_expr, else_expr) }
    | fn=ID; fn_args=args { Call(fn, fn_args) }
    | EXTERN; fn=ID; fn_args=args { ExternCall(fn, fn_args) }
    | RETURN; e=expr { e }
    ;

param:
    | param_type=diablo_type; name=ID; { TParam(param_type, name) }
    ;

params:
    | LPAREN; params=separated_list(COMMA, param); RPAREN { params }
    ;

args:
    | LPAREN; args=separated_list(COMMA, expr); RPAREN { args }

diablo_type:
    | TYPE_INT { TInt }
    | TYPE_BOOL { TBool }
    | TYPE_VOID { TVoid }
    | TYPE_STRING { TString }
    ;

main_block:
    | MAIN; LPAREN; RPAREN; body=block { body }
    ;

%inline un_op:
    | BANG { UnOpNot }
    | MINUS { UnOpNegate }
    ;

%inline bin_op:
    | PLUS { BinOpPlus }
    | MINUS { BinOpMinus }
    | MULT { BinOpMult }
    | DIV { BinOpDiv }
    | REM { BinOpRem }
    | LANGLE { BinOpLessThan }
    | RANGLE { BinOpGreaterThan }
    | LANGLE EQUAL { BinOpLessThanEqual }
    | RANGLE EQUAL { BinOpGreaterThanEqual }
    | AND { BinOpAnd }
    | OR { BinOpOr }
    | EQUAL EQUAL { BinOpEqual }
    | BANG EQUAL { BinOpNotEqual }
    ;
