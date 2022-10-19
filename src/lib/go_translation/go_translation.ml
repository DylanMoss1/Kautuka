open Ast

let par_wrap s = "(" ^ s ^ ")"

let ast_expr_to_go expr = 
  match expr with
  | Int(value) -> value
  | Double(value) -> value
  | Assignment(var, e) ->  of string * expr
  | MethodCall(name, args) -> name ^ par_wrap (String.concat ", " arg)
  | BinaryOperator(e1, op, e2) -> String.concat " " [ast_expr_to_go e1; op; ast_expr_to_go e2]
  | Identifier(id) -> id

let ast_statement_to_go statement =
  match statement with
  | Expr.Expr _ -> "Expr "
  | Expr.VariableDeclarationExpr _ -> "VariableDeclarationExpr "
  | Expr.VariableDeclaration _ -> "VariableDeclaration "
  | Expr.FunctionDeclaration _ -> "FunctionDeclaration "

let ast_block_to_go ast_block =
  List.fold_left
    (fun acc statement -> acc ^ ast_statement_to_go statement ^ "\n\n")
    "" ast_block
