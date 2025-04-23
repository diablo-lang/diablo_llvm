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
%token LBRACKET
%token RBRACKET
%token RARROW
%token RETURN
%token COMMA
%token COLON
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
%token TYPE_INT
%token TYPE_BOOL
%token TYPE_UNIT
%token TYPE_STRING
%token TRUE
%token FALSE
%token UNIT
%token FUNCTION
%token IF
%token ELSE
%token MODULE
%token IMPORT
%token EOF

%right EQUAL
%left PLUS MINUS LANGLE RANGLE
%left MULT DIV REM
%left AND OR
%nonassoc BANG

%start <Parsed_ast.module_file> module_file
%start <Parsed_ast.program> program

%%

program:
    | imports=list(import_decl); declarations=list(top_level_decl) EOF { Program(imports, declarations) }
    ;

module_file:
    | imports=list(import_decl); module_defn=module_defn; EOF { Module(imports, module_defn) }
    ;

top_level_decl:
    | b=global_let_binding { b }
    | f=function_defn { f }
    ;

import_decl:
    | IMPORT; module_name=ID; SEMICOLON { Import(module_name) }
    ;

global_let_binding:
    | id=ID; EQUAL; e=expr; SEMICOLON { Let(id, e) }
    ;

function_defn:
    | FUNCTION; name=ID; LPAREN; params=separated_list(COMMA, param_defn); RPAREN; RARROW; return_type=ty_const; LBRACE; body=expr; RBRACE { Function(name, params, body, return_type) }
    ;

param_defn:
    | name=ID; COLON; ty=ty_const; { name, ty }
    ;

module_defn:
    | MODULE; name=ID; LBRACE; decls=list(top_level_decl); RBRACE { ModuleDefinition(name, decls) }
    ;

expr:
    | LPAREN; e=expr; RPAREN { e }
    | id=ID { Identifier id }
    | i=INT { Integer i }
    | TRUE { Boolean true }
    | FALSE { Boolean false }
    | s=STRING_LITERAL { StringLiteral s }
    | UNIT { Unit }
    | LBRACKET; l=separated_list(COMMA, expr); RBRACKET { List(l) }
    | op=un_op e=expr { UnOp(op, e) }
    | e1=expr op=bin_op e2=expr { BinOp(op, e1, e2) }
    | id=ID; EQUAL; e=expr; SEMICOLON; body=expr; { LetIn(id, e, body) }
    | IF; LPAREN; cond_expr=expr; RPAREN; LBRACE; then_expr=expr; RBRACE; ELSE; LBRACE; else_expr=expr RBRACE; { If(cond_expr, then_expr, else_expr) }
    | FUNCTION; LPAREN; params=separated_list(COMMA, param_defn); RPAREN; RARROW; return_type=ty_const; LBRACE; body=expr; RBRACE { Lambda(params, body, return_type) }
    | fn=expr; fn_args=args { Call(fn, fn_args) }
    | RETURN; e=expr { e }
    ;

args:
    | LPAREN; args=separated_list(COMMA, expr); RPAREN { args }

ty_const:
    | TYPE_INT { TConst "int" }
    | TYPE_BOOL { TConst "bool" }
    | TYPE_UNIT { TConst "unit" }
    | TYPE_STRING { TConst "str" }
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
