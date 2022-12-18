open! Core
open Util.Extended_set

type 'var var = 'var

type type_id =
  | T_Int
  | T_Bool
  | T_String
  | T_Unit

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
  | Div
  | Mod
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | And
  | Or

type 'var expr =
  | Unop of unop * 'var expr
  | Binop of 'var expr * binop * 'var expr
  | Paren of 'var expr
  | Value of value
  | VarRead of 'var var

type 'var var_statement =
  | VarNonInit of 'var var * type_id
  | VarInit of 'var var * type_id * 'var expr
  | VarDecl of 'var var * 'var expr
  | VarAssign of 'var var * 'var expr
  | Pre_inc of 'var var
  | Pre_dec of 'var var
  | Post_inc of 'var var
  | Post_dec of 'var var

type 'var user_func =
  { name : 'var var 
  ; args : 'var expr list
  }

type 'var write_template =
  { file : 'var var
  ; contents : 'var expr
  }

type 'var func_call =
  | User_func of 'var user_func
  | Print of 'var expr
  | Input
  | Open of 'var expr
  | Read of 'var expr
  | Write of 'var write_template
  | Append of 'var write_template

type control =
  | Break
  | Continue

type 'var statement =
  | Var_statement of 'var var_statement
  | Func_call of 'var func_call
  | Control of control

type ('block, 'var) for_loop =
  { init : 'var var_statement
  ; cond : 'var expr
  ; iter : 'var var_statement
  ; contents : ('block, 'var) block
  }

and ('block, 'var) for_each =
  { item : 'var var
  ; iterator : 'var var
  ; contents : ('block, 'var) block
  }

and ('block, 'var) condition_template =
  { condition : 'var expr
  ; contents : ('block, 'var) block
  }

and ('block, 'var) if_record =
  { _if : ('block, 'var) condition_template
  ; else_if : ('block, 'var) condition_template list
  ; else_contents : ('block, 'var) block option
  }

and ('block, 'var) structure =
  | Block_struct of ('block, 'var) block
  | If of ('block, 'var) if_record
  | While of ('block, 'var) condition_template
  | For_loop of ('block, 'var) for_loop
  | For_each of ('block, 'var) for_each

and ('block, 'var) command =
  | Structure of ('block, 'var) structure
  | Statement of 'var statement

and ('block, 'var) block =
  { contents : ('block, 'var) command list
  ; annotations : 'block
  }

and 'var param = 'var var * type_id

and ('block, 'var) func =
  { name : 'var var
  ; params : 'var param list
  ; body : ('block, 'var) block
  ; return_type : type_id
  }

type ('block, 'var, 'import) program =
  { package : string
  ; imports : 'import
  ; global_vars : 'var var_statement list
  ; funcs : ('block, 'var) func list
  }
