%{ 
    open Ast.Ast_types  
%}

%token LPAREN RPAREN LBRACE RBRACE
%token <string> ID
%token T_INT T_BOOL T_STRING
%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token FUNC COMMA
%token VAR EQUALS DECL 
%token PACKAGE 
%token EOF
%token FORCE IGNORE 

%type <program> program
%type <id> package
%type <statement> statement 
%type <var> var global_var
%type <id> id 
%type <type_id> type_id
%type <value> value 
%type <func> func
%type <param> param
%type <expr> expr 
%type <block> block 
%type <structure> structure 
%type <command> command 

%type <func list> list(func)
%type <var list> list(global_var)
%type <param list> loption(separated_nonempty_list(COMMA,param))
%type <param list> separated_nonempty_list(COMMA,param)

%start program

%%

program:
| package=package; global_vars=list(global_var) funcs=list(func) EOF { 
    { package = package; global_vars = global_vars; funcs = funcs } 
}

package: 
| PACKAGE id=id { id } 

id: 
| id=ID { id }

type_id: 
| T_INT { T_Int } 
| T_BOOL { T_Bool }
| T_STRING { T_String }

value: 
| int_val=INT { Int(int_val) }
| bool_val=BOOL { Bool(bool_val) }
| string_val=STRING { String(string_val) }

var:
| var=global_var { var }
| id=id DECL expr=expr { VarDecl(id, expr) }
| id=id EQUALS expr=expr { VarAssign(id, expr) }

global_var:
| VAR id=id type_id=type_id { VarNonInit(id, type_id) }
| VAR id=id type_id=type_id EQUALS expr=expr { VarInit(id, type_id, expr) }

func: 
| FUNC id=id LPAREN params=separated_list(COMMA, param) RPAREN block=block {
     { name = id; params = params; body = block; return_type = None } 
  }

block: 
| LBRACE commands=list(command) RBRACE { 
    Block({ contents = commands; effects = None; cost = None; is_parallel = None })
  }
| IGNORE LBRACE commands=list(command) RBRACE { Ignore(commands) }
| FORCE LBRACE commands=list(command) RBRACE { Force(commands) }

command: 
| structure=structure { Structure(structure) }
| statement=statement { Statement(statement) }

param: 
| id=id type_id=type_id { (id, type_id) }

expr: 
| value=value { Value(value) }

statement: 
| var=var { Var(var) }
| expr=expr { Expr(expr) }

structure: 
| func=func { Func(func) }
| _if=if { _if }

if
| IF condition=expr contents=block { If({ condition = condition; contents=contents }) }
| IF condition=expr if_contents=block list(else_if) ELSE else_contents=block {}

else_if: 
| ELSE IF condition=expr contents = block {  } 

%%