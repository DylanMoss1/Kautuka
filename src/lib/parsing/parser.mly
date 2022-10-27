%{ 
    open Ast.Ast_structure  
%}

%token LPAREN RPAREN LBRACE RBRACE
%token <string> ID
%token T_INT T_BOOL T_STRING
%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token FUNC COMMA
%token VAR EQUALS 
%token PACKAGE 
%token EOF

%type <program> program
%type <package> package
%type <global_var> global_var
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
| VAR id=id type_id=type_id { GlobalVar(id, type_id) }
| VAR id=id type_id=type_id EQUALS value=value { GlobalVarInit(id, type_id, value) }

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
| FUNC id=id LPAREN params=separated_list(COMMA, param) RPAREN LBRACE contents=list(global_var) RBRACE { Function(id, params, contents) }

param: 
| id=id type_id=type_id { Param(id, type_id) }


%%