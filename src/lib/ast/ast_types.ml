open Core

type id = ID of string [@@deriving equal, of_sexp, sexp_of, compare]

type type_id =
  | T_Int
  | T_Bool
  | T_String
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

type statement =
  | Expr of expr
  | Var of var
  | Func_call of func_call

type param = id * type_id
type effects = int
type cost = float

type for_loop =
  { init : expr
  ; cond : expr
  ; iter : expr
  ; contents : block
  }

and for_each =
  { item : id
  ; iterator : id
  ; contents : block
  }

and condition_template =
  { condition : expr
  ; contents : block
  }

and if_record =
  { _if : condition_template
  ; else_if : condition_template list
  ; else_contents : block option
  }

and structure =
  | Func of func
  | Block of block
  | If of if_record
  | While of condition_template
  | For_loop of for_loop
  | For_each of for_each

and command =
  | Structure of structure
  | Statement of statement

and block_record =
  { contents : command list
  ; effects : effects option
  ; cost : cost option
  ; is_parallel : bool option
  }

and block =
  | Block of block_record
  | Ignore of command list
  | Go_block of command list

and func =
  { name : id
  ; params : param list
  ; body : block
  ; return_type : type_id option
  }

type program =
  { package : id
  ; imports : string list
  ; global_vars : var list
  ; funcs : func list
  }

(* type non_variants =
   | User_func of user_func
   | Write_template of write_template
   | Param of param
   | For_loop of for_loop
   | For_each of for_each
   | Condition_template of condition_template
   | If_record of if_record
   | Block_record of block_record
   | Func of func
   | Program of program *)

(* type ast =
   | Id of id
   | Type_id of type_id
   | Value of value
   | Unop of unop
   | Binop of binop
   | Expr of expr
   | Var of var
   | User_func of user_func
   | Write_template of write_template
   | Func_call of func_call
   | Statement of statement
   | Param of param
   | For_loop of for_loop
   | For_each of for_each
   | Condition_template of condition_template
   | If_record of if_record
   | Structure of structure
   | Command of command
   | Block_record of block_record
   | Block of block
   | Func of func
   | Program of program *)
