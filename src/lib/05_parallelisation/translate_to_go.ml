open Core
open Ast.Ast_types
open Reorder_and_parallelise
open Util

let map_concat ~sep ~f x = String.concat ~sep (List.map ~f x)
let string_of_var (var : 'var) = Parallelisation_ast.string_of_var_annot var

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


let rec string_of_user_func (user_func : ('var, 'expr) user_func) =
  Fmt.str
    "%s(%s)"
    (string_of_var user_func.name)
    (map_concat ~sep:", " ~f:string_of_annotated_expr user_func.args)


and string_of_write_template write_template =
  Fmt.str
    "%s, %s"
    (string_of_var write_template.file)
    (string_of_annotated_expr write_template.contents)


and string_of_func_call = function
  | User_func user_func -> string_of_user_func user_func
  | Print expr -> Fmt.str "fmt.Println(%s)" (string_of_annotated_expr expr)
  | Input -> "input()"
  | Open expr -> Fmt.str "open(%s)" (string_of_annotated_expr expr)
  | Read var -> Fmt.str "read(%s)" (string_of_var var)
  | Write write_template ->
    Fmt.str "write(%s)" (string_of_write_template write_template)
  | Append write_template ->
    Fmt.str "append(%s)" (string_of_write_template write_template)


and string_of_expr = function
  | Unop (unop, expr) ->
    Fmt.str "%s %s" (string_of_unop unop) (string_of_annotated_expr expr)
  | Binop (expr1, binop, expr2) ->
    Fmt.str
      "%s %s %s"
      (string_of_annotated_expr expr1)
      (string_of_binop binop)
      (string_of_annotated_expr expr2)
  | Paren expr -> Fmt.str "(%s)" (string_of_annotated_expr expr)
  | Value value -> string_of_value value
  | Var_read var -> string_of_var var
  | Func_call func_call -> string_of_func_call func_call


and string_of_annotated_expr annotated_expr =
  let { expr; annotations = _ } = annotated_expr in
  string_of_expr expr


let string_of_var_statement = function
  | Var_non_init (var, type_id) ->
    Fmt.str "var %s %s" (string_of_var var) (string_of_type_id type_id)
  | Var_init (var, type_id, expr) ->
    Fmt.str
      "var %s %s = %s"
      (string_of_var var)
      (string_of_type_id type_id)
      (string_of_annotated_expr expr)
  | Var_decl (var, expr) ->
    Fmt.str "%s := %s" (string_of_var var) (string_of_annotated_expr expr)
  | Var_assign (var, expr) ->
    Fmt.str "%s = %s" (string_of_var var) (string_of_annotated_expr expr)
  | Post_inc var -> Fmt.str "%s++" (string_of_var var)
  | Post_dec var -> Fmt.str "%s--" (string_of_var var)


let string_of_control = function
  | Continue -> "continue"
  | Break -> "break"


let string_of_statement = function
  | Control control -> string_of_control control
  | Return annotated_expr ->
    Fmt.str "return %s" (string_of_annotated_expr annotated_expr)
  | Var_statement var_statement -> string_of_var_statement var_statement
  | Expr annotated_expr -> string_of_annotated_expr annotated_expr


let rec string_of_for_loop for_loop =
  Fmt.str
    "for %s;%s;%s {\n%s\n}"
    (string_of_var_statement for_loop.init)
    (string_of_annotated_expr for_loop.cond)
    (string_of_var_statement for_loop.iter)
    (string_of_block for_loop.contents)


and string_of_for_each for_each =
  Fmt.str
    "for %s := %s {\n%s\n}"
    (string_of_var for_each.item)
    (string_of_annotated_expr for_each.iterator)
    (string_of_block for_each.contents)


and string_of_condition_template condition_template =
  Fmt.str
    "%s {\n%s\n}"
    (string_of_annotated_expr condition_template.condition)
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
  | Block_struct block -> string_of_block block
  | If if_record -> string_of_if_record if_record
  | For_loop for_loop -> string_of_for_loop for_loop
  | For_each for_each -> string_of_for_each for_each


and string_of_command = function
  | Structure structure -> string_of_structure structure
  | Statement statement -> string_of_statement statement


and string_of_block (block : (Parallelisation_ast.block_annot, 'a, 'b) block) =
  match block.annotations.parallelise_contents with
  | Some num_block ->
    let wg_var = Alpha.create in
    Fmt.str
      "var %s sync.WaitGroup\n%s.Add(%d)\n%s"
      (Alpha.string_of_t wg_var)
      (Alpha.string_of_t wg_var)
      num_block
      (String.concat
         ~sep:"\n"
         (List.map
            ~f:(fun block_item ->
              Fmt.str "go func(){\n%s\n}()" (string_of_command block_item))
            block.contents))
  | None ->
    Fmt.str "%s" (map_concat ~sep:"\n" ~f:string_of_command block.contents)


and string_of_param (var, type_id) =
  Fmt.str "%s %s" (string_of_var var) (string_of_type_id type_id)


and string_of_func func =
  Fmt.str
    "func %s (%s) %s {\n%s\n}"
    (string_of_var func.name)
    (map_concat ~sep:", " ~f:string_of_param func.params)
    (string_of_type_id func.return_type)
    (string_of_block func.body)


let string_of_program program =
  Fmt.str "package %s\n\n" program.package
  ^ Parallelisation_ast.string_of_import_annot program.imports
  ^ map_concat ~sep:"\n" ~f:string_of_var_statement program.global_vars
  ^ "\n\n"
  ^ map_concat ~sep:"\n" ~f:string_of_func program.funcs
