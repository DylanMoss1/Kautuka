open Ast.Expr

let par_wrap s = "(" ^ s ^ ")"

let rec ast_expr_to_go expr = 
  match expr with
  | Int(value) -> string_of_int value
  | Double(value) -> string_of_float value
  | Assignment(var, e) -> var ^ " = " ^ (ast_expr_to_go e)
  | MethodCall(name, args) -> name ^ par_wrap (String.concat ", " (List.map ast_expr_to_go args))
  | BinaryOperator(e1, op, e2) -> String.concat " " [ast_expr_to_go e1; op; ast_expr_to_go e2]
  | Identifier(id) -> id

let rec ast_statement_to_go statement =
  match statement with
  | Expr(e) -> ast_expr_to_go(e)
  | VariableDeclarationExpr(t, var, e) -> String.concat " " [t; var; ast_expr_to_go e]
  | VariableDeclaration(t, var) -> String.concat " " [t; var]
  | FunctionDeclaration(t, name, args, body) -> (
    t ^ " " ^ name 
    ^ par_wrap (List.fold_left (fun acc statement -> acc ^ ast_statement_to_go statement ^ ", ") "" args)
    ^ "{\n" ^ (List.fold_left (fun acc statement -> acc ^ ast_statement_to_go statement ^ "\n") "" body) ^ "}\n"
  )

let ast_block_to_go ast_block =
  List.fold_left
    (fun acc statement -> acc ^ ast_statement_to_go statement ^ "\n")
    "" ast_block
