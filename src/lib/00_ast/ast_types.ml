open! Core
open Util.Extended_set

type 'var var = 'var

type type_id =
  | T_Int
  | T_Bool
  | T_String
  | T_Unit
[@@deriving of_sexp, sexp_of, compare]

type value =
  | Int of int
  | Bool of bool
  | String of string

type unop =
  | Not
  | U_Minus

type binop =
  | Plus
  | B_Minus
  | Mult
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | And
  | Or

type ('var, 'expr) user_func =
  { name : 'var var
  ; args : ('var, 'expr) annotated_expr list
  }

and ('var, 'expr) write_template =
  { file : 'var var
  ; contents : ('var, 'expr) annotated_expr
  }

and ('var, 'expr) func_call =
  | User_func of ('var, 'expr) user_func
  | Print of ('var, 'expr) annotated_expr
  | Input
  | Open of ('var, 'expr) annotated_expr
  | Read of 'var var
  | Write of ('var, 'expr) write_template
  | Append of ('var, 'expr) write_template

and ('var, 'expr) expr =
  | Unop of unop * ('var, 'expr) annotated_expr
  | Binop of ('var, 'expr) annotated_expr * binop * ('var, 'expr) annotated_expr
  | Paren of ('var, 'expr) annotated_expr
  | Value of value
  | VarRead of 'var var
  | Func_call of ('var, 'expr) func_call

and ('var, 'expr) annotated_expr =
  { expr : ('var, 'expr) expr
  ; annotations : 'expr
  }

type ('var, 'expr) var_statement =
  | VarNonInit of 'var var * type_id
  | VarInit of 'var var * type_id * ('var, 'expr) annotated_expr
  | VarDecl of 'var var * ('var, 'expr) annotated_expr
  | VarAssign of 'var var * ('var, 'expr) annotated_expr
  | Pre_inc of 'var var
  | Pre_dec of 'var var
  | Post_inc of 'var var
  | Post_dec of 'var var

type control =
  | Break
  | Continue

type ('var, 'expr) statement =
  | Var_statement of ('var, 'expr) var_statement
  | Return of ('var, 'expr) annotated_expr
  | Control of control
  | Expr of ('var, 'expr) annotated_expr

type ('block, 'var, 'expr) for_loop =
  { init : ('var, 'expr) var_statement
  ; cond : ('var, 'expr) annotated_expr
  ; iter : ('var, 'expr) var_statement
  ; contents : ('block, 'var, 'expr) block
  }

and ('block, 'var, 'expr) for_each =
  { item : 'var var
  ; iterator : ('var, 'expr) annotated_expr
  ; contents : ('block, 'var, 'expr) block
  }

and ('block, 'var, 'expr) condition_template =
  { condition : ('var, 'expr) annotated_expr
  ; contents : ('block, 'var, 'expr) block
  }

and ('block, 'var, 'expr) if_record =
  { _if : ('block, 'var, 'expr) condition_template
  ; else_if : ('block, 'var, 'expr) condition_template list
  ; else_contents : ('block, 'var, 'expr) block option
  }

and ('block, 'var, 'expr) structure =
  | Block_struct of ('block, 'var, 'expr) block
  | If of ('block, 'var, 'expr) if_record
  | For_loop of ('block, 'var, 'expr) for_loop
  | For_each of ('block, 'var, 'expr) for_each

and ('block, 'var, 'expr) command =
  | Structure of ('block, 'var, 'expr) structure
  | Statement of ('var, 'expr) statement

and ('block, 'var, 'expr) block =
  { contents : ('block, 'var, 'expr) command list
  ; annotations : 'block
  }

and 'var param = 'var var * type_id

and ('block, 'var, 'expr) func =
  { name : 'var var
  ; params : 'var param list
  ; body : ('block, 'var, 'expr) block
  ; return_type : type_id
  }

type ('block, 'var, 'import, 'expr) program =
  { package : string
  ; imports : 'import
  ; global_vars : ('var, 'expr) var_statement list
  ; funcs : ('block, 'var, 'expr) func list
  }
