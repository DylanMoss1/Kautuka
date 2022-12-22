%{ 
    open Ast.Ast_types
    open Parser_types 

    let block_wrapper command =
        Block_struct
          { contents = [ command ];
            annotations = Parsed_ast.create_block_annot Default
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
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot, Parsed_ast.import_annot) program> program
%type <string> package
%type <Parsed_ast.var_annot statement> statement 
%type <Parsed_ast.var_annot var> var
%type <Parsed_ast.var_annot var_statement> var_statement global_var var_mod
%type <type_id> type_id
%type <value> value 
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot) func> func
%type <Parsed_ast.var_annot param> param
%type <Parsed_ast.var_annot expr> expr
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot) block> block _else 
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot) structure> structure condition
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot) command> command
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot) condition_template> condition_template else_if
%type <control> control
%type <Parsed_ast.var_annot func_call> func_call 
%type <unop> unop
%type <binop> binop

%type <type_id option> option(type_id)
%type <Parsed_ast.var_annot expr list> loption(separated_nonempty_list(COMMA,expr)) separated_nonempty_list(COMMA,expr)
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot) block option> option(_else)
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot) condition_template list> list(else_if)
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot) command list> list(command)
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot) func list> list(func)
%type <Parsed_ast.var_annot var_statement list> list(global_var)
%type <Parsed_ast.var_annot param list> loption(separated_nonempty_list(COMMA,param)) separated_nonempty_list(COMMA,param)

%left MINUS PLUS
%left MULT DIV MOD
%left EQ NE LT LE GT GE
%left AND OR  
%nonassoc NOT

%start program

%%

program:
| package=package; global_vars=list(global_var) funcs=list(func) EOF { 
    { package; imports = Parsed_ast.create_import_annot (); global_vars; funcs } 
}

package: 
| PACKAGE id=ID { id } 

type_id: 
| T_INT { T_Int } 
| T_BOOL { T_Bool }
| T_STRING { T_String }

value: 
| int_val=INT { Int(int_val) }
| bool_val=BOOL { Bool(bool_val) }
| string_val=STRING { String(string_val) }

var: 
| name=ID { { name } }

var_statement:
| var_statement=global_var { var_statement }
| var=var DECL expr=expr { VarDecl(var, expr) }
| var=var EQUALS expr=expr { VarAssign(var, expr) }
| var_mod=var_mod { var_mod }

global_var:
| VAR var=var type_id=type_id { VarNonInit(var, type_id) }
| VAR var=var type_id=type_id EQUALS expr=expr { VarInit(var, type_id, expr) }

var_mod: 
| INCREMENT var=var { Pre_inc(var) }
| DECREMENT var=var { Pre_dec(var) }
| var=var INCREMENT { Post_inc(var) }
| var=var DECREMENT { Post_dec(var) }

func: 
| FUNC var=var LPAREN params=separated_list(COMMA, param) RPAREN return_type=option(type_id) block=block {
     { name = var; params = params; body = block; return_type = (parse_return_type return_type) } 
  }

block: 
| LBRACE commands=list(command) RBRACE { 
    { contents = commands; annotations = Parsed_ast.create_block_annot Default }
  }
| IGNORE LBRACE commands=list(command) RBRACE { { contents = commands; annotations = Parsed_ast.create_block_annot Ignore } }
| FORCEPAR LBRACE commands=list(command) RBRACE { { contents = commands; annotations = Parsed_ast.create_block_annot Force_par } }
| FORCESEQ LBRACE commands=list(command) RBRACE { { contents = commands; annotations = Parsed_ast.create_block_annot Force_seq } }

command: 
| structure=structure { Structure(structure) }
| statement=statement { Statement(statement) }

param: 
| var=var type_id=type_id { (var, type_id) }

expr: 
| unop=unop expr=expr { Unop(unop, expr) }
| expr1=expr binop=binop expr2=expr { Binop(expr1, binop, expr2) }
| LPAREN expr=expr RPAREN { Paren(expr) } 
| value=value { Value(value) }
| var=var { VarRead(var) }

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
| var_statement=var_statement { Var_statement(var_statement) }
| func_call=func_call { Func_call(func_call) }
| control=control { Control(control) }

control: 
| BREAK { Break }
| CONTINUE { Continue }

func_call: 
| name=var LPAREN args=separated_list(COMMA, expr) RPAREN { User_func( { name; args } ) }
| PRINT LPAREN arg=expr RPAREN { Print(arg) }
| INPUT LPAREN RPAREN { Input }
| OPEN LPAREN arg=expr RPAREN { Open(arg) } 
| READ LPAREN arg=var RPAREN { Read(arg) }
| WRITE LPAREN arg1=var COMMA arg2=expr RPAREN { Write( { file = arg1; contents = arg2 } ) } 
| APPEND LPAREN arg1=var COMMA arg2=expr RPAREN { Append( { file = arg1; contents = arg2 } ) } 

structure:
| condition=condition { condition }
| block=block { Block_struct(block) }
| WHILE cond=expr block=block { While( { condition = cond; contents = block } ) }
| FOR init=var_statement SEMICOLON cond=expr SEMICOLON iter=var_statement contents=block {
    block_wrapper (Structure(For_loop( { init = init; cond = cond; iter = iter; contents = contents } )))
} 
| FOR item=var DECL RANGE iterator=var contents=block {
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