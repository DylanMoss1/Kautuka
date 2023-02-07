open Core
open Ast.Ast_types
open Reorder_and_parallelise
open Util
open Side_effect_system.Alpha_conversion

let alpha_generator = Alpha.create
let map_concat ~sep ~f x = String.concat ~sep (List.map ~f x)
let go_of_var (var : 'var) = Alpha.string_of_t var.alpha

let go_of_type_id = function
  | T_Int -> "int"
  | T_Bool -> "bool"
  | T_String -> "go"
  | T_Unit -> ""


let go_of_unop = function
  | Not -> "!"
  | U_Minus -> "-"


let go_of_binop = function
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


let go_of_value = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | String s -> Fmt.str "\"%s\"" s


let rec go_of_user_func (user_func : ('var, 'expr) user_func) =
  Fmt.str
    "%s(%s)"
    (go_of_var user_func.name)
    (map_concat ~sep:", " ~f:go_of_annotated_expr user_func.args)


and go_of_write_template write_template =
  Fmt.str
    "%s, %s"
    (go_of_var write_template.file)
    (go_of_annotated_expr write_template.contents)


and go_of_func_call = function
  | User_func user_func -> go_of_user_func user_func
  | Print expr -> Fmt.str "fmt.Println(%s)" (go_of_annotated_expr expr)
  | Input -> "input()"
  | Open expr -> Fmt.str "open(%s)" (go_of_annotated_expr expr)
  | Read var -> Fmt.str "read(%s)" (go_of_var var)
  | Write write_template ->
    Fmt.str "write(%s)" (go_of_write_template write_template)
  | Append write_template ->
    Fmt.str "append(%s)" (go_of_write_template write_template)


and go_of_expr = function
  | Unop (unop, expr) ->
    Fmt.str "%s %s" (go_of_unop unop) (go_of_annotated_expr expr)
  | Binop (expr1, binop, expr2) ->
    Fmt.str
      "%s %s %s"
      (go_of_annotated_expr expr1)
      (go_of_binop binop)
      (go_of_annotated_expr expr2)
  | Paren expr -> Fmt.str "(%s)" (go_of_annotated_expr expr)
  | Value value -> go_of_value value
  | Var_read var -> go_of_var var
  | Func_call func_call -> go_of_func_call func_call


and go_of_annotated_expr annotated_expr =
  let { expr; annotations = _ } = annotated_expr in
  go_of_expr expr


let go_of_var_statement = function
  | Var_non_init (var, type_id) ->
    Fmt.str "var %s %s" (go_of_var var) (go_of_type_id type_id)
  | Var_init (var, type_id, expr) ->
    Fmt.str
      "var %s %s = %s"
      (go_of_var var)
      (go_of_type_id type_id)
      (go_of_annotated_expr expr)
  | Var_decl (var, expr) ->
    Fmt.str "%s := %s" (go_of_var var) (go_of_annotated_expr expr)
  | Var_assign (var, expr) ->
    Fmt.str "%s = %s" (go_of_var var) (go_of_annotated_expr expr)
  | Post_inc var -> Fmt.str "%s++" (go_of_var var)
  | Post_dec var -> Fmt.str "%s--" (go_of_var var)


let go_of_control = function
  | Continue -> "continue"
  | Break -> "break"


let go_of_statement = function
  | Control control -> go_of_control control
  | Return annotated_expr ->
    Fmt.str "return %s" (go_of_annotated_expr annotated_expr)
  | Var_statement var_statement -> go_of_var_statement var_statement
  | Expr annotated_expr -> go_of_annotated_expr annotated_expr


let rec go_of_for_loop for_loop =
  Fmt.str
    "for %s;%s;%s {\n%s\n}"
    (go_of_var_statement for_loop.init)
    (go_of_annotated_expr for_loop.cond)
    (go_of_var_statement for_loop.iter)
    (go_of_block for_loop.contents)


and go_of_for_each for_each =
  Fmt.str
    "for %s := %s {\n%s\n}"
    (go_of_var for_each.item)
    (go_of_annotated_expr for_each.iterator)
    (go_of_block for_each.contents)


and go_of_condition_template condition_template =
  Fmt.str
    "%s {\n%s\n}"
    (go_of_annotated_expr condition_template.condition)
    (go_of_block condition_template.contents)


and go_of_if_record if_record =
  Fmt.str
    "if %s %s"
    (map_concat
       ~sep:" else if "
       ~f:go_of_condition_template
       (List.append [ if_record._if ] if_record.else_if))
    (match if_record.else_contents with
    | Some else_contents -> go_of_block else_contents
    | None -> "")


and go_of_structure = function
  | Block_struct block -> Fmt.str "{%s}" (go_of_block block)
  | If if_record -> go_of_if_record if_record
  | For_loop for_loop -> go_of_for_loop for_loop
  | For_each for_each -> go_of_for_each for_each


and go_of_command = function
  | Structure structure -> go_of_structure structure
  | Statement statement -> go_of_statement statement


and go_of_block (block : (Parallelisation_ast.block_annot, 'a, 'b) block) =
  match block.annotations.parallelise_contents with
  | Some num_block ->
    let wg_var = Alpha.get_new_alpha alpha_generator in
    Fmt.str
      "var wg_%s sync.WaitGroup\nwg_%s.Add(%d)\n%s\nwg_%s.Wait()"
      (Alpha.string_of_t wg_var)
      (Alpha.string_of_t wg_var)
      num_block
      (String.concat
         ~sep:"\n"
         (List.map
            ~f:(fun block_item ->
              Fmt.str "go func(){\n%s\nwg_%s.Done()\n}()" (go_of_command block_item) (Alpha.string_of_t wg_var))
            block.contents))
      (Alpha.string_of_t wg_var)
  | None -> Fmt.str "%s" (map_concat ~sep:"\n" ~f:go_of_command block.contents)


and go_of_param (var, type_id) =
  Fmt.str "%s %s" (go_of_var var) (go_of_type_id type_id)


and go_of_func (func : ('c, 'd, 'e) func) =
  Fmt.str
    "func %s (%s) %s {\n%s\n}"
    (go_of_var func.name)
    (map_concat ~sep:", " ~f:go_of_param func.params)
    (go_of_type_id func.return_type)
    (go_of_block func.body)


let go_of_program program =
  Fmt.str "package %s\n\n" program.package
  ^ Parallelisation_ast.string_of_import_annot program.imports
  ^ map_concat ~sep:"\n" ~f:go_of_var_statement program.global_vars
  ^ "\n\n"
  ^ map_concat ~sep:"\n\n" ~f:go_of_func program.funcs


let pipeline_ast program =
  Out_channel.write_all
    (Fmt.str "./files/go_program.go")
    ~data:(go_of_program program)
