open Core
open Ast_types

let map_concat ~sep ~f x = String.concat ~sep (List.map ~f x)
let string_of_var ~string_of_var_annot (var : 'var) = string_of_var_annot var

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


let rec string_of_user_func
    ~string_of_var_annot
    (user_func : ('var, 'expr) user_func)
  =
  Fmt.str
    "%s(%s)"
    (string_of_var ~string_of_var_annot user_func.name)
    (map_concat
       ~sep:", "
       ~f:(string_of_annotated_expr ~string_of_var_annot)
       user_func.args)


and string_of_write_template ~string_of_var_annot write_template =
  Fmt.str
    "%s, %s"
    (string_of_var ~string_of_var_annot write_template.file)
    (string_of_annotated_expr ~string_of_var_annot write_template.contents)


and string_of_func_call ~string_of_var_annot = function
  | User_func user_func -> string_of_user_func ~string_of_var_annot user_func
  | Print expr ->
    Fmt.str
      "fmt.Println(%s)"
      (string_of_annotated_expr ~string_of_var_annot expr)
  | Input -> "input()"
  | Open expr ->
    Fmt.str "open(%s)" (string_of_annotated_expr ~string_of_var_annot expr)
  | Read var -> Fmt.str "read(%s)" (string_of_var ~string_of_var_annot var)
  | Write write_template ->
    Fmt.str
      "write(%s)"
      (string_of_write_template ~string_of_var_annot write_template)
  | Append write_template ->
    Fmt.str
      "append(%s)"
      (string_of_write_template ~string_of_var_annot write_template)


and string_of_expr ~string_of_var_annot = function
  | Unop (unop, expr) ->
    Fmt.str
      "%s %s"
      (string_of_unop unop)
      (string_of_annotated_expr ~string_of_var_annot expr)
  | Binop (expr1, binop, expr2) ->
    Fmt.str
      "%s %s %s"
      (string_of_annotated_expr ~string_of_var_annot expr1)
      (string_of_binop binop)
      (string_of_annotated_expr ~string_of_var_annot expr2)
  | Paren expr ->
    Fmt.str "(%s)" (string_of_annotated_expr ~string_of_var_annot expr)
  | Value value -> string_of_value value
  | Var_read var -> string_of_var ~string_of_var_annot var
  | Func_call func_call -> string_of_func_call ~string_of_var_annot func_call


and string_of_annotated_expr ~string_of_var_annot annotated_expr =
  let { expr; annotations = _ } = annotated_expr in
  string_of_expr ~string_of_var_annot expr


let string_of_var_statement ~string_of_var_annot = function
  | Var_non_init (var, type_id) ->
    Fmt.str
      "var %s %s"
      (string_of_var ~string_of_var_annot var)
      (string_of_type_id type_id)
  | Var_init (var, type_id, expr) ->
    Fmt.str
      "var %s %s = %s"
      (string_of_var ~string_of_var_annot var)
      (string_of_type_id type_id)
      (string_of_annotated_expr ~string_of_var_annot expr)
  | Var_decl (var, expr) ->
    Fmt.str
      "%s := %s"
      (string_of_var ~string_of_var_annot var)
      (string_of_annotated_expr ~string_of_var_annot expr)
  | Var_assign (var, expr) ->
    Fmt.str
      "%s = %s"
      (string_of_var ~string_of_var_annot var)
      (string_of_annotated_expr ~string_of_var_annot expr)
  | Post_inc var -> Fmt.str "%s++" (string_of_var ~string_of_var_annot var)
  | Post_dec var -> Fmt.str "%s--" (string_of_var ~string_of_var_annot var)


let string_of_control = function
  | Continue -> "continue"
  | Break -> "break"


let string_of_statement ~string_of_var_annot = function
  | Control control -> string_of_control control
  | Return annotated_expr ->
    Fmt.str
      "return %s"
      (string_of_annotated_expr ~string_of_var_annot annotated_expr)
  | Var_statement var_statement ->
    string_of_var_statement ~string_of_var_annot var_statement
  | Expr annotated_expr ->
    string_of_annotated_expr ~string_of_var_annot annotated_expr


let rec string_of_for_loop ~string_of_block_annot ~string_of_var_annot for_loop =
  Fmt.str
    "for %s;%s;%s {\n%s\n}"
    (string_of_var_statement ~string_of_var_annot for_loop.init)
    (string_of_annotated_expr ~string_of_var_annot for_loop.cond)
    (string_of_var_statement ~string_of_var_annot for_loop.iter)
    (string_of_block
       ~string_of_block_annot
       ~string_of_var_annot
       for_loop.contents)


and string_of_for_each ~string_of_block_annot ~string_of_var_annot for_each =
  Fmt.str
    "for %s := %s {\n%s\n}"
    (string_of_var ~string_of_var_annot for_each.item)
    (string_of_annotated_expr ~string_of_var_annot for_each.iterator)
    (string_of_block
       ~string_of_block_annot
       ~string_of_var_annot
       for_each.contents)


and string_of_condition_template
    ~string_of_block_annot
    ~string_of_var_annot
    condition_template
  =
  Fmt.str
    "%s {\n%s\n}"
    (string_of_annotated_expr ~string_of_var_annot condition_template.condition)
    (string_of_block
       ~string_of_block_annot
       ~string_of_var_annot
       condition_template.contents)


and string_of_if_record ~string_of_block_annot ~string_of_var_annot if_record =
  Fmt.str
    "if %s %s"
    (map_concat
       ~sep:" else if "
       ~f:
         (string_of_condition_template
            ~string_of_block_annot
            ~string_of_var_annot)
       (List.append [ if_record._if ] if_record.else_if))
    (match if_record.else_contents with
    | Some else_contents ->
      string_of_block ~string_of_block_annot ~string_of_var_annot else_contents
    | None -> "")


and string_of_structure ~string_of_block_annot ~string_of_var_annot = function
  | Block_struct block ->
    string_of_block ~string_of_block_annot ~string_of_var_annot block
  | If if_record ->
    string_of_if_record ~string_of_block_annot ~string_of_var_annot if_record
  | For_loop for_loop ->
    string_of_for_loop ~string_of_block_annot ~string_of_var_annot for_loop
  | For_each for_each ->
    string_of_for_each ~string_of_block_annot ~string_of_var_annot for_each


and string_of_command ~string_of_block_annot ~string_of_var_annot = function
  | Structure structure ->
    (string_of_structure ~string_of_block_annot ~string_of_var_annot) structure
  | Statement statement -> string_of_statement ~string_of_var_annot statement


and string_of_block ~string_of_block_annot ~string_of_var_annot block =
  Fmt.str
    "//%s\n%s"
    (string_of_block_annot block.annotations)
    (map_concat
       ~sep:"\n"
       ~f:(string_of_command ~string_of_block_annot ~string_of_var_annot)
       block.contents)


and string_of_param ~string_of_var_annot (var, type_id) =
  Fmt.str
    "%s %s"
    (string_of_var ~string_of_var_annot var)
    (string_of_type_id type_id)


and string_of_func ~string_of_block_annot ~string_of_var_annot func =
  Fmt.str
    "func %s (%s) %s {\n%s\n}"
    (string_of_var ~string_of_var_annot func.name)
    (map_concat ~sep:", " ~f:(string_of_param ~string_of_var_annot) func.params)
    (string_of_type_id func.return_type)
    (string_of_block ~string_of_block_annot ~string_of_var_annot func.body)


let string_of_program
    ~string_of_block_annot
    ~string_of_var_annot
    ~string_of_import_annot
    program
  =
  Fmt.str "package %s\n\n" program.package
  ^ string_of_import_annot program.imports
  ^ map_concat
      ~sep:"\n"
      ~f:(string_of_var_statement ~string_of_var_annot)
      program.global_vars
  ^ "\n\n"
  ^ map_concat
      ~sep:"\n"
      ~f:(string_of_func ~string_of_block_annot ~string_of_var_annot)
      program.funcs
