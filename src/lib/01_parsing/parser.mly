%{ 
  open Ast.Ast_types
  open Parser_types 
  open Util 
 
  let generate_ref = File_ref.get_new_generated_ref 
  let user_ref = File_ref.get_new_user_ref 

  type ('block, 'var, 'expr) program_contents = 
  { global_vars : ('var, 'expr) var_statement list
  ; funcs : ('block, 'var, 'expr) func list
  }

  let block_wrapper command =
      Block_struct
        { contents = [ command ];
          annotations = Parsed_ast.create_block_annot Default
        }

  let parse_return_type = function 
  | Some(return_type) -> return_type  
  | None -> T_Unit 

  let create_annotated_plus_binop left right =
    { expr = Binop (left, Plus, right)
    ; annotations = Parsed_ast.create_expr_annot ()
    }


  let plus_equals_desugar var annotated_expr =
    Var_assign
      ( var
      , create_annotated_plus_binop
          { expr = Var_read var; annotations = Parsed_ast.create_expr_annot () }
          annotated_expr )
%}

%token NEWLINE
%token LPAREN RPAREN LBRACE RBRACE
%token <string> ID
%token T_INT T_BOOL T_STRING T_FILE
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
%token FOR
%token BREAK CONTINUE RETURN 
%token INCREMENT DECREMENT
%token PLUS PLUS_EQUALS MINUS MULT EQ NE LT LE GT GE AND OR NOT
%token PRINT INPUT OPEN READ WRITE APPEND UNDERSCORE
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot, Parsed_ast.import_annot, Parsed_ast.expr_annot) program> program
%type <string> package
%type <(Parsed_ast.var_annot, Parsed_ast.expr_annot) statement> statement 
%type <Parsed_ast.var_annot var> var
%type <(Parsed_ast.var_annot, Parsed_ast.expr_annot) var_statement> var_statement global_var
%type <type_id> type_id
%type <value> value 
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot, Parsed_ast.expr_annot) func> func
%type <Parsed_ast.var_annot param> param
%type <(Parsed_ast.var_annot, Parsed_ast.expr_annot) expr> expr
%type <(Parsed_ast.var_annot, Parsed_ast.expr_annot) annotated_expr> annotated_expr
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot, Parsed_ast.expr_annot) block> block _else 
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot, Parsed_ast.expr_annot) structure> structure condition
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot, Parsed_ast.expr_annot) command> command
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot, Parsed_ast.expr_annot) condition_template> condition_template else_if
%type <control> control
%type <(Parsed_ast.var_annot, Parsed_ast.expr_annot) func_call> func_call 
%type <unop> unop
%type <binop> binop
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot, Parsed_ast.expr_annot) program_contents> program_contents

%type <(Parsed_ast.var_annot, Parsed_ast.expr_annot) annotated_expr list> separated_nonempty_list(COMMA, annotated_expr) loption(separated_nonempty_list(COMMA,annotated_expr))
%type <type_id option> option(type_id)
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot, Parsed_ast.expr_annot) block option> option(_else)
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot, Parsed_ast.expr_annot) condition_template list> list(else_if)
%type <(Parsed_ast.block_annot, Parsed_ast.var_annot, Parsed_ast.expr_annot) command list> list(command) nonempty_list(command)
%type <(Parsed_ast.var_annot param) list> loption(separated_nonempty_list(COMMA,param)) separated_nonempty_list(COMMA,param)
%type <unit option> option(NEWLINE)


%left MINUS PLUS
%left MULT
%left EQ NE LT LE GT GE
%left AND OR  
%nonassoc NOT

%start program

%%

program: 
| package=package NEWLINE program_contents=program_contents EOF { 
  { package; imports = Parsed_ast.create_import_annot (); global_vars = program_contents.global_vars; funcs = program_contents.funcs }
}

program_contents: 
| func=func NEWLINE program_contents=program_contents {
  { program_contents with funcs = func :: program_contents.funcs } 
}
| global_var=global_var NEWLINE program_contents=program_contents { 
  { program_contents with global_vars = global_var :: program_contents.global_vars } 
}
| func=func { { global_vars = []; funcs = [func] } }

package:
| PACKAGE id=ID { id } 

type_id: 
| T_INT { T_Int } 
| T_BOOL { T_Bool }
| T_STRING { T_String }
| T_FILE {T_File}

value: 
| int_val=INT { Int(int_val) }
| bool_val=BOOL { Bool(bool_val) }
| string_val=STRING { String(string_val) }

var: 
| name=ID { Parsed_ast.create_var_annot { name } }
| UNDERSCORE { Parsed_ast.create_var_annot { name = "_" } }

var_statement:
| var_statement=global_var { var_statement }
| var=var DECL annotated_expr=annotated_expr { Var_decl(var, annotated_expr) }
| var=var EQUALS annotated_expr=annotated_expr { Var_assign(var, annotated_expr) }
| var=var PLUS_EQUALS annotated_expr=annotated_expr { plus_equals_desugar var annotated_expr }
| var=var INCREMENT { Post_inc(var) }
| var=var DECREMENT { Post_dec(var) }

global_var:
| VAR var=var type_id=type_id { Var_non_init(var, type_id) }
| VAR var=var type_id=type_id EQUALS annotated_expr=annotated_expr { Var_init(var, type_id, annotated_expr) }
| VAR var=var EQUALS annotated_expr=annotated_expr { Var_decl(var, annotated_expr) }

func: 
| FUNC var=var LPAREN params=separated_list(COMMA, param) RPAREN return_type=option(type_id) block=block {
     { name = var; params = params; body = block; return_type = (parse_return_type return_type) } 
  }

block: 
| LBRACE option(NEWLINE) commands=nonempty_list(command) option(NEWLINE) RBRACE { 
    { contents = commands; annotations = Parsed_ast.create_block_annot Default }
  }
| LBRACE option(NEWLINE) RBRACE { { contents = []; annotations = Parsed_ast.create_block_annot Default } }
| IGNORE LBRACE commands=list(command) RBRACE { { contents = commands; annotations = Parsed_ast.create_block_annot Ignore } }
| FORCEPAR LBRACE commands=list(command) RBRACE { { contents = commands; annotations = Parsed_ast.create_block_annot Force_par } }
| FORCESEQ LBRACE commands=list(command) RBRACE { { contents = commands; annotations = Parsed_ast.create_block_annot Force_seq } }

command: 
| structure=structure NEWLINE { Structure(structure) }
| statement=statement NEWLINE { Statement(statement) }

param: 
| var=var type_id=type_id { (var, type_id) }

expr: 
| unop=unop annotated_expr=annotated_expr { Unop(unop, annotated_expr) }
| annotated_expr1=annotated_expr binop=binop annotated_expr2=annotated_expr { Binop(annotated_expr1, binop, annotated_expr2) }
| LPAREN annotated_expr=annotated_expr RPAREN { Paren(annotated_expr) } 
| value=value { Value(value) }
| var=var { Var_read(var) }
| func_call=func_call { Func_call(func_call) }

annotated_expr: 
| expr=expr { { expr; annotations=Parsed_ast.create_expr_annot () } }

%inline unop: 
| NOT { Not }
| MINUS { U_Minus }

%inline binop: 
| PLUS { Plus }
| MINUS { B_Minus }
| MULT { Mult }
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
| RETURN annotated_expr=annotated_expr { Return(annotated_expr) }
| control=control { Control(control) }
| annotated_expr=annotated_expr { Expr(annotated_expr) }

control:
| BREAK { Break }
| CONTINUE { Continue }

func_call: 
| name=var LPAREN args=separated_list(COMMA, annotated_expr) RPAREN { User_func( { name; args } ) }
| PRINT LPAREN arg=annotated_expr RPAREN { Print(arg) }
| INPUT LPAREN upper=INT RPAREN { Input(Upper(upper)) }
| INPUT LPAREN lower=INT COMMA upper=INT RPAREN { Input(Both(lower, upper)) }
| OPEN LPAREN arg=annotated_expr RPAREN { Open(arg, generate_ref ()) }
| OPEN LPAREN arg=annotated_expr COMMA ref=INT RPAREN { Open(arg, user_ref ref) }
| READ LPAREN arg=annotated_expr COMMA upper=INT RPAREN { Read(arg, Upper(upper)) }
| READ LPAREN arg=annotated_expr COMMA lower=INT COMMA upper=INT RPAREN { Read(arg, Both(lower, upper)) }
| WRITE LPAREN arg1=annotated_expr COMMA arg2=annotated_expr RPAREN { Write( { file = arg1; contents = arg2 } ) } 
| APPEND LPAREN arg1=annotated_expr COMMA arg2=annotated_expr RPAREN { Append( { file = arg1; contents = arg2 } ) } 

structure:
| condition=condition { condition }
| block=block { Block_struct(block) }
| FOR init=var_statement SEMICOLON cond=annotated_expr SEMICOLON iter=var_statement contents=block {
    block_wrapper (Structure(For_loop( { init = init; cond = cond; iter = iter; contents = contents } )))
} 
| FOR item=var DECL RANGE iterator=annotated_expr contents=block {
    block_wrapper (Structure(For_each( { item = item; iterator = iterator; contents = contents } )))
}

condition_template: 
| condition=annotated_expr contents=block { { condition = condition; contents = contents } } 

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