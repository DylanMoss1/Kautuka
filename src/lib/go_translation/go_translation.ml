(* open Ast

   let par_wrap s = "(" ^ s ^ ")"

   let ast_bin_op_to_string = function
     | Eq -> "=="
     | Ne -> "!="
     | Lt -> "<"
     | Le -> "<="
     | Gt -> ">"
     | Ge -> ">="
     | Plus -> "+"
     | Minus -> "-"
     | Mult -> "*"
     | Div -> "/"
     | And -> "&&"
     | Or -> "||"

   let ast_value_to_go = function
     | Int i -> string_of_int i
     | Bool b -> string_of_bool b
     | String s -> s

   let rec ast_expr_to_go = function
     | Value value -> ast_value_to_go value
     | Assignment (var, e) -> var ^ " = " ^ ast_expr_to_go e
     | MethodCall (name, args) ->
         name ^ par_wrap (String.concat ", " (List.map ast_expr_to_go args))
     | BinaryOperator (e1, op, e2) ->
         String.concat " "
           [ ast_expr_to_go e1; ast_bin_op_to_string op; ast_expr_to_go e2 ]
     | Identifier id -> id

   let rec ast_statement_to_go = function
     | Expr e -> ast_expr_to_go e
     | VariableDeclarationExpr (t, var, e) ->
         String.concat " " [ t; var; ast_expr_to_go e ]
     | VariableDeclaration (t, var) -> String.concat " " [ t; var ]
     | FunctionDeclaration (t, name, args, body) ->
         t ^ " " ^ name
         ^ par_wrap
             (List.fold_left
                (fun acc statement -> acc ^ ast_statement_to_go statement ^ ", ")
                "" args)
         ^ "{\n"
         ^ List.fold_left
             (fun acc statement -> acc ^ ast_statement_to_go statement ^ "\n")
             "" body
         ^ "}\n"

   let ast_program_to_go = function
     | Program(_, _, statement_list) -> List.fold_left (fun acc statement -> acc ^ ast_statement_to_go statement ^ "\n") "" statement_list *)
