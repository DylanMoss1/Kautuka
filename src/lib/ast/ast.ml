type bin_op = 
| Eq
| Ne
| Lt
| Le
| Gt
| Ge
| Plus
| Minus
| Mult
| Div 
| And 
| Or

type type_id = 
| TInt 
| TBool 
| TString 

type value = 
| Int of int 
| Bool of bool
| String of string

type expr =
  | Value of value
  | Assignment of string * expr
  | MethodCall of string * expr list
  | BinaryOperator of expr * bin_op * expr
  | Identifier of string

type statement =
  | Expr of expr
  | VariableDeclarationExpr of
      string * string * expr (* type, id, assignment expr *)
  | VariableDeclaration of string * string
  | FunctionDeclaration of string * string * statement list * statement list

type block = statement list

type global_var = 
| GlobalVar of string * type_id
| GlobalInitVar of string * type_id * value 

type global_vars = global_var list

type package = 
| Package of string

type program = 
| Program of package * global_vars * (statement list)
