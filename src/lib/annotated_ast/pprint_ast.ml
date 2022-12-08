open Core
open Ast 

let map_concat ~sep ~f x = String.concat ~sep (List.map ~f x)

let string_of_id = function
  | ID s -> s


let string_of_type_id = function
  | T_Int -> "int"
  | T_Bool -> "bool"
  | T_String -> "string"
  | T_Unit -> ""


let string_of_unop = function
  | Not -> "!"
  | U_Minus -> "-"


let string_of_binop = function
  | Plus -> "+"
  | B_Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Eq -> "=="
  | Ne -> "!="
  | And -> "&&"
  | Or -> "||"


let string_of_value = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | String s -> Fmt.str "\"%s\"" s


let rec string_of_expr = function
  | Unop (unop, expr) ->
    Fmt.str "%s %s" (string_of_unop unop) (string_of_expr expr)
  | Binop (expr1, binop, expr2) ->
    Fmt.str
      "%s %s %s"
      (string_of_expr expr1)
      (string_of_binop binop)
      (string_of_expr expr2)
  | Paren expr -> Fmt.str "(%s)" (string_of_expr expr)
  | Value value -> Fmt.str "%s" (string_of_value value)
  | Var id -> Fmt.str "%s" (string_of_id id)


let string_of_var = function
  | VarNonInit (id, type_id) ->
    Fmt.str "var %s %s" (string_of_id id) (string_of_type_id type_id)
  | VarInit (id, type_id, expr) ->
    Fmt.str
      "var %s %s = %s"
      (string_of_id id)
      (string_of_type_id type_id)
      (string_of_expr expr)
  | VarDecl (id, expr) ->
    Fmt.str "%s := %s" (string_of_id id) (string_of_expr expr)
  | VarAssign (id, expr) ->
    Fmt.str "%s = %s" (string_of_id id) (string_of_expr expr)


(* REMOVE TYPE ANNOTATION HERE *)
let string_of_user_func (user_func : user_func) =
  Fmt.str
    "%s(%s)"
    (string_of_id user_func.name)
    (map_concat ~sep:", " ~f:string_of_expr user_func.args)


let string_of_write_template write_template =
  Fmt.str
    "%s, %s"
    (string_of_id write_template.file)
    (string_of_expr write_template.contents)


let string_of_func_call = function
  | User_func user_func -> string_of_user_func user_func
  | Print expr -> Fmt.str "println(%s)" (string_of_expr expr)
  | Input -> "input()"
  | Open expr -> Fmt.str "open(%s)" (string_of_expr expr)
  | Read expr -> Fmt.str "read(%s)" (string_of_expr expr)
  | Write write_template ->
    Fmt.str "write(%s)" (string_of_write_template write_template)
  | Append write_template ->
    Fmt.str "append(%s)" (string_of_write_template write_template)


let string_of_statement = function
  | Expr expr -> string_of_expr expr
  | Var var -> string_of_var var
  | Func_call func_call -> string_of_func_call func_call


let rec string_of_for_loop for_loop =
  Fmt.str
    "for %s;%s;%s {\n%s\n}"
    (string_of_expr for_loop.init)
    (string_of_expr for_loop.cond)
    (string_of_expr for_loop.iter)
    (string_of_block for_loop.contents)


and string_of_for_each for_each =
  Fmt.str
    "for %s := %s {\n%s\n}"
    (string_of_id for_each.item)
    (string_of_id for_each.iterator)
    (string_of_block for_each.contents)


and string_of_condition_template condition_template =
  Fmt.str
    "%s {\n%s\n}"
    (string_of_expr condition_template.condition)
    (string_of_block condition_template.contents)


and string_of_if_record if_record =
  Fmt.str
    "if %s %s"
    (map_concat
       ~sep:" else if "
       ~f:string_of_condition_template
       (List.append [ if_record._if ] if_record.else_if))
    (match if_record.else_contents with
    | Some else_contents -> string_of_block else_contents
    | None -> "")


and string_of_structure = function
  | Func func -> string_of_func func
  | Block_struct block -> string_of_block block
  | If if_record -> string_of_if_record if_record
  | While while_loop -> string_of_condition_template while_loop
  | For_loop for_loop -> string_of_for_loop for_loop
  | For_each for_each -> string_of_for_each for_each


and string_of_command = function
  | Structure structure -> string_of_structure structure
  | Statement statement -> string_of_statement statement


and string_of_block_type = function
  | Default -> "Default"
  | Ignore -> "Ignore"
  | Force_par -> "Force_par"
  | Force_seq -> "Force_seq"


and string_of_block_record block_record =
  map_concat ~sep:"\n" ~f:string_of_command block_record.contents


and string_of_block = function
  | Block block_record -> Fmt.str "{%s}" (string_of_block_record block_record)
  | Go_block commands ->
    Fmt.str
      "go func (){%s}()"
      (List.map ~f:string_of_command commands |> String.concat ~sep:"\n")


and string_of_param (id, type_id) =
  Fmt.str "%s %s" (string_of_id id) (string_of_type_id type_id)


and string_of_func func =
  Fmt.str
    "func %s (%s) %s {\n%s\n}"
    (string_of_id func.name)
    (map_concat ~sep:", " ~f:string_of_param func.params)
    (string_of_type_id func.return_type)
    (string_of_block func.body)


let string_of_program program =
  Fmt.str "package %s\n\n" (string_of_id program.package)
  ^
  match program.imports with
  | None | Some [] -> ""
  | Some imports ->
    (map_concat
       ~sep:"\n"
       ~f:(fun import -> Fmt.str "import \"%s\"" import)
       imports
    ^ "\n\n")
    ^ map_concat ~sep:"\n" ~f:string_of_var program.global_vars
    ^ "\n\n"
    ^ map_concat ~sep:"\n" ~f:string_of_func program.funcs
