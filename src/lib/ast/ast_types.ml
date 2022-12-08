open! Core

type block_type =
  | Default
  | Ignore
  | Force_par
  | Force_seq

type id = ID of string [@@deriving equal, of_sexp, sexp_of, compare]

type type_id =
  | T_Int
  | T_Bool
  | T_String
  | T_Unit
[@@deriving equal, of_sexp, sexp_of, compare]

type value =
  | Int of int
  | Bool of bool
  | String of string
[@@deriving equal, of_sexp, sexp_of, compare]

type unop =
  | Not
  | U_Minus
[@@deriving equal, of_sexp, sexp_of, compare]

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
[@@deriving equal, of_sexp, sexp_of, compare]

type expr =
  | Unop of unop * expr
  | Binop of expr * binop * expr
  | Paren of expr
  | Value of value
  | Var of id
[@@deriving equal, of_sexp, sexp_of, compare]

type var =
  | VarNonInit of id * type_id
  | VarInit of id * type_id * expr
  | VarDecl of id * expr
  | VarAssign of id * expr
  | Pre_inc of id
  | Pre_dec of id
  | Post_inc of id
  | Post_dec of id
[@@deriving equal, of_sexp, sexp_of, compare]

type user_func =
  { name : id
  ; args : expr list
  }

type write_template =
  { file : id
  ; contents : expr
  }

type func_call =
  | User_func of user_func
  | Print of expr
  | Input
  | Open of expr
  | Read of expr
  | Write of write_template
  | Append of write_template

type control =
  | Break
  | Continue

type statement =
  | Var of var
  | Func_call of func_call
  | Control of control

type 'a for_loop =
  { init : var
  ; cond : expr
  ; iter : var
  ; contents : 'a block
  }

and 'a for_each =
  { item : id
  ; iterator : id
  ; contents : 'a block
  }

and 'a for_cond =
  { cond : expr
  ; contents : 'a block
  }

and 'a condition_template =
  { condition : expr
  ; contents : 'a block
  }

and 'a if_record =
  { _if : 'a condition_template
  ; else_if : 'a condition_template list
  ; else_contents : 'a block option
  }

and 'a structure =
  | Func of 'a func
  | Block_struct of 'a block
  | If of 'a if_record
  | While of 'a condition_template
  | For_loop of 'a for_loop
  | For_each of 'a for_each

and 'a command =
  | Structure of 'a structure
  | Statement of statement

and 'a block_record =
  { contents : 'a command list
  ; annotations : 'a
  }

and 'a block =
  | Block of 'a block_record
  | Go_block of 'a command list

and param = id * type_id

and 'a func =
  { name : id
  ; params : param list
  ; body : 'a block
  ; return_type : type_id
  }

type 'a program =
  { package : id
  ; imports : string list option
  ; global_vars : var list
  ; funcs : 'a func list
  }
