open! Core

type id = string

type type_id =
  | T_Int
  | T_Bool
  | T_String

type value =
  | Int of int
  | Bool of bool
  | String of string

type unop =
  | Not
  | Minus

type binop =
  | Plus
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

type expr =
  | Unop of unop * expr
  | Binop of expr * binop * expr
  | Paren of expr
  | Value of value
  | Var of id

type var =
  | VarNonInit of id * type_id
  | VarInit of id * type_id * expr
  | VarDecl of id * expr
  | VarAssign of id * expr

type user_func = {
  name : id;
  args : expr list;
}

type write_template = {
  file : id;
  contents : expr;
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

type for_loop = {
  init : expr;
  cond : expr;
  iter : expr;
  contents : block;
}

and for_each = {
  item : id;
  iterator : id;
  contents : block;
}

and condition_template = {
  condition : expr;
  contents : block;
}

and if_record = {
  _if : condition_template;
  else_if : condition_template list;
  else_contents : block option;
}

and structure =
  | Func of func
  | Block of block
  | If of if_record
  | While of expr * block
  | For_loop of for_loop
  | For_each of for_each

and command =
  | Structure of structure
  | Statement of statement

and block_record = {
  contents : command list;
  effects : effects option;
  cost : cost option;
  is_parallel : bool option;
}

and block =
  | Block of block_record
  | Ignore of command list
  | Force of command list

and func = {
  name : id;
  params : param list;
  body : block;
  return_type : type_id option;
}

type program = {
  package : id;
  global_vars : var list;
  funcs : func list;
}
