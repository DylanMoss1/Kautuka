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

type expr = Value of value

type var =
  | VarNonInit of id * type_id
  | VarInit of id * type_id * expr
  | VarDecl of id * expr
  | VarAssign of id * expr

type statement =
  | Expr of expr
  | Var of var

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

and if_record = {
  condition : expr;
  contents : block;
}

and structure =
  | Func of func
  | Block of block
  | If of expr * block * block
  | While of expr * block
  | For of for_loop
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
