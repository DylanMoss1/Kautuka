%{ 
    open Annotated_ast 
    open Ast.Ast_types

    let block_wrapper command =
        Block_struct
          { contents = [ command ];
            annotations: Func_init_annotations.t = { block_type = Default }
          }

    let parse_return_type = function 
    | Some(return_type) -> return_type  
    | None -> T_Unit 
%}

%token LPAREN RPAREN LBRACE RBRACE
%token <string> ID
%token T_INT T_BOOL T_STRING
%token <int> INT
%token <bool> BOOL
%token <string> STRING
%token FUNC COMMA
%token SEMICOLON RANGE
%token VAR EQUALS DECL 
%token PACKAGE
%token EOF
%token FORCEPAR FORCESEQ IGNORE
%token IF ELSE ELIF
%token WHILE FOR
%token BREAK CONTINUE
%token INCREMENT DECREMENT
%token PLUS MINUS MULT DIV MOD EQ NE LT LE GT GE AND OR NOT 
%token PRINT INPUT OPEN READ WRITE APPEND 
%type <(Func_init_annotations.t, Import_empty_annotations.t) program> program
%type <id> id 
%type <id> package
%type <statement> statement 
%type <var> var global_var var_mod
%type <type_id> type_id
%type <value> value 
%type <Func_init_annotations.t func> func
%type <param> param
%type <expr> expr
%type <Func_init_annotations.t block> block _else 
%type <Func_init_annotations.t structure> structure condition
%type <Func_init_annotations.t command> command
%type <Func_init_annotations.t condition_template> condition_template else_if
%type <control> control
%type <func_call> func_call 
%type <unop> unop
%type <binop> binop

%type <type_id option> option(type_id)
%type <expr list> loption(separated_nonempty_list(COMMA,expr)) separated_nonempty_list(COMMA,expr)
%type <Func_init_annotations.t block option> option(_else)
%type <Func_init_annotations.t condition_template list> list(else_if)
%type <Func_init_annotations.t command list> list(command)
%type <Func_init_annotations.t func list> list(func)
%type <var list> list(global_var)
%type <param list> loption(separated_nonempty_list(COMMA,param)) separated_nonempty_list(COMMA,param)

%left MINUS PLUS
%left MULT DIV MOD
%left EQ NE LT LE GT GE
%left AND OR  
%nonassoc NOT

%start program

%%

program:
| package=package; global_vars=list(global_var) funcs=list(func) EOF { 
    { package = package; imports = (); global_vars = global_vars; funcs = funcs } 
}

package: 
| PACKAGE id=id { id } 

id: 
| id=ID { ID({ name = id }) }

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
| var_mod=var_mod { var_mod }

global_var:
| VAR id=id type_id=type_id { VarNonInit(id, type_id) }
| VAR id=id type_id=type_id EQUALS expr=expr { VarInit(id, type_id, expr) }

var_mod: 
| INCREMENT id=id { Pre_inc(id) }
| DECREMENT id=id { Pre_dec(id) }
| id=id INCREMENT { Post_inc(id) }
| id=id DECREMENT { Post_dec(id) }

func: 
| FUNC id=id LPAREN params=separated_list(COMMA, param) RPAREN return_type=option(type_id) block=block {
     { name = id; params = params; body = block; return_type = (parse_return_type return_type) } 
  }

block: 
| LBRACE commands=list(command) RBRACE { 
    { contents = commands; annotations = { block_type = Default } }
  }
| IGNORE LBRACE commands=list(command) RBRACE { { contents = commands; annotations = { block_type = Ignore } } }
| FORCEPAR LBRACE commands=list(command) RBRACE { { contents = commands; annotations = { block_type = Force_par } } }
| FORCESEQ LBRACE commands=list(command) RBRACE { { contents = commands; annotations = { block_type = Force_seq } } }

command: 
| structure=structure { Structure(structure) }
| statement=statement { Statement(statement) }

param: 
| id=id type_id=type_id { (id, type_id) }

expr: 
| unop=unop expr=expr { Unop(unop, expr) }
| expr1=expr binop=binop expr2=expr { Binop(expr1, binop, expr2) }
| LPAREN expr=expr RPAREN { Paren(expr) } 
| value=value { Value(value) }
| var=id { VarRead(var) }

%inline unop: 
| NOT { Not }
| MINUS { U_Minus }

%inline binop: 
| PLUS { Plus }
| MINUS { B_Minus }
| MULT { Mult }
| DIV { Div }
| MOD { Mod }
| LT { Lt }
| LE { Le }
| GT { Gt }
| GE { Ge }
| EQ { Eq } 
| NE { Ne }
| AND { And }
| OR { Or }

statement:
| var=var { Var(var) }
| func_call=func_call { Func_call(func_call) }
| control=control { Control(control) }

control: 
| BREAK { Break }
| CONTINUE { Continue }

func_call: 
| name=id LPAREN args=separated_list(COMMA, expr) RPAREN { User_func( { name = name; args = args } ) }
| PRINT LPAREN arg=expr RPAREN { Print(arg) }
| INPUT LPAREN RPAREN { Input }
| OPEN LPAREN arg=expr RPAREN { Open(arg) } 
| READ LPAREN arg=expr RPAREN { Read(arg) }
| WRITE LPAREN arg1=id COMMA arg2=expr RPAREN { Write( { file = arg1; contents = arg2 } ) } 
| APPEND LPAREN arg1=id COMMA arg2=expr RPAREN { Append( { file = arg1; contents = arg2 } ) } 

structure:
| condition=condition { condition }
| block=block { Block_struct(block) }
| WHILE cond=expr block=block { While( { condition = cond; contents = block } ) }
| FOR init=var SEMICOLON cond=expr SEMICOLON iter=var contents=block {
    block_wrapper (Structure(For_loop( { init = init; cond = cond; iter = iter; contents = contents } )))
} 
| FOR item=id DECL RANGE iterator=id contents=block {
    block_wrapper (Structure(For_each( { item = item; iterator = iterator; contents = contents } )))
}
| FOR cond=expr contents=block { While( { condition = cond; contents = contents } ) }
| FOR contents=block { While( { condition = Value(Bool(true)); contents = contents } ) }

condition_template: 
| condition=expr contents=block { { condition = condition; contents = contents } } 

condition:
| IF _if=condition_template else_if=list(else_if) else_contents=option(_else) {
    If(
    { 
      _if = _if;
      else_if = else_if;
      else_contents = else_contents
    })
}

else_if: 
| ELIF else_if=condition_template { else_if } 

_else: 
| ELSE content=block { content }

%%