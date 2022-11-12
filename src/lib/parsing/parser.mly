%{ 
    open Ast.Parsed_ast  
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

%type <program> program
%type <package> package
%type <var> global_var
%type <statement> statement 
%type <var> var 
%type <id> id 
%type <type_id> type_id
%type <func> func
%type <param> param

%start program

%%

program:
| package=package; global_vars=list(global_var) funcs=list(func) EOF { Program(package, global_vars, funcs) }

package:
| PACKAGE id=id { Package(id) }

global_var:
| VAR id=id type_id=type_id { VarNonInit(id, type_id) }
| VAR id=id type_id=type_id EQUALS value=value { VarInit(id, type_id, value) }

id: 
| id=ID { ID(id) }

type_id: 
| T_INT { T_Int } 
| T_BOOL { T_Bool }
| T_STRING { T_String }

value: 
| int_val=INT { Int(int_val) }
| bool_val=BOOL { Bool(bool_val) }
| string_val=STRING { String(string_val) }

func: 
| FUNC id=id LPAREN params=separated_list(COMMA, param) RPAREN LBRACE contents=list(statement) RBRACE { Function(id, params, contents) }

param: 
| id=id type_id=type_id { Param(id, type_id) }

statement: 
| var=var { Var(var) }

var: 
| VAR id=id type_id=type_id { VarNonInit(id, type_id) }
| VAR id=id type_id=type_id EQUALS value=value { VarInit(id, type_id, value) }
| VAR id=id DECL value=value { VarDecl(id, value) }


%%