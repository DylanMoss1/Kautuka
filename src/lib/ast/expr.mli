type expr =
  | Int of int
  | Double of float
  | Assignment of string * expr
  | MethodCall of string * expr list
  | BinaryOperator of expr * string * expr
  | Identifier of string

type statement =
  | Expr of expr
  | VariableDeclarationExpr of
      string * string * expr (* type, id, assignment expr *)
  | VariableDeclaration of string * string
  | FunctionDeclaration of string * string * statement list * statement list

type block = statement list
