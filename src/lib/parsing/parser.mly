%{
    open Ast
%}

%token <int> INT
%token <string> ID
%token LPAREN RPAREN LBRACE RBRACE
%token T_INT T_BOOL T_STRING
%token PLUS MINUS MULT DIV
%token EQUALS EQ NE LT LE GT GE AND OR
%token PACKAGE VAR COMMA EOF

%type <string> identifier
%type <expr> numeric expr 
%type <statement list> func_decl_args
%type <expr list> call_args
%type <block> program stmts block
%type <statement> stmt var_decl func_decl
%type <bin_op> comparison

%start program

%%

program:
| package=package global_vars=list(global_var) stmts=stmts EOF { Program(package, global_vars, stmts) };

global_var: 
| VAR id=ID type_id=type_id { GlobalVar(id, type_id) }
| VAR id=ID type_id=type_id EQUALS expr=expr { GlobalInitVar(id, type_id, expr) }

type_id: 
| T_INT { TInt }
| T_BOOL { TBool }
| T_STRING { TString}

package:
| PACKAGE id=ID { Package(id) }

stmts : 
| stmt { [$1] }
| stmts stmt { $1@[$2] }

stmt : 
| var_decl {$1} | func_decl {$1}
| expr { Expr($1) };

block : 
| LBRACE stmts RBRACE { $2 }
| LBRACE RBRACE { [] };

var_decl : 
| identifier identifier { VariableDeclaration($1, $2) }
| identifier identifier EQUALS expr { VariableDeclarationExpr($1, $2, $4) };

func_decl : 
| identifier identifier LPAREN func_decl_args RPAREN block { FunctionDeclaration($1, $2, $4, $6)};

func_decl_args : 
| {[]}
| var_decl {[$1]}
| func_decl_args COMMA var_decl {$1@[$3]};

identifier : 
| ID { $1 };

numeric : 
| INT { Value(Int($1)) }

expr : 
| identifier EQUALS expr { Assignment($1, $3)}
| identifier LPAREN call_args RPAREN { MethodCall($1, $3)}
| identifier { Identifier($1)}
| numeric { $1 }
| expr comparison expr { BinaryOperator($1, $2, $3)}
| LPAREN expr RPAREN {$2};

call_args : 
| {[]}
| expr {[$1]}
| call_args COMMA expr {$1@[$3]};

comparison : 
| EQ {Eq}
| NE {Ne}
| LT {Lt}
| LE {Le}
| GT {Gt}
| GE {Ge}
| PLUS {Plus}
| MINUS {Minus}
| MULT {Mult}
| DIV {Div}
| AND {And}
| OR {Or};

%%