open Core
open Ast.Ast_types
open Reorder_and_parallelise
open Util
open Preperation.Alpha_conversion
open Preperation.Import
open Cost_analysis.Cost

let threshold = 300
let has_sync = ref false
let has_math = ref false
let has_input = ref false
let has_open = ref false
let has_read = ref false
let has_write = ref false
let has_append = ref false
let alpha_generator = Alpha.create
let map_concat ~sep ~f x = String.concat ~sep (List.map ~f x)
let go_of_var var = Alpha.string_of_t var.alpha

let go_of_type_id = function
  | T_Int -> "int"
  | T_Bool -> "bool"
  | T_String -> "string"
  | T_Unit -> ""
  | T_File -> "*os.File"


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
    (go_of_annotated_expr write_template.file)
    (go_of_annotated_expr write_template.contents)


and go_of_func_call = function
  | User_func user_func -> go_of_user_func user_func
  | Print expr -> Fmt.str "fmt.Println(%s)" (go_of_annotated_expr expr)
  | Input _ ->
    has_input := true;
    "input()"
  | Open (expr, _) ->
    has_open := true;
    Fmt.str "open(%s)" (go_of_annotated_expr expr)
  | Read (var, _) ->
    has_read := true;
    Fmt.str "read(%s)" (go_of_annotated_expr var)
  | Write write_template ->
    has_write := true;
    Fmt.str "write(%s)" (go_of_write_template write_template)
  | Append write_template ->
    has_append := true;
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


let rec go_of_for_loop ~sequential for_loop =
  Fmt.str
    "for %s;%s;%s {\n%s\n}"
    (go_of_var_statement for_loop.init)
    (go_of_annotated_expr for_loop.cond)
    (go_of_var_statement for_loop.iter)
    (go_of_block ~sequential for_loop.contents)


and go_of_for_each ~sequential for_each =
  Fmt.str
    "for _, %s := range %s {\nvar %s string = string(%s)\n    %s\n}"
    (go_of_var for_each.item)
    (go_of_annotated_expr for_each.iterator)
    (go_of_var for_each.item)
    (go_of_var for_each.item)
    (go_of_block ~sequential for_each.contents)


and go_of_condition_template ~sequential condition_template =
  Fmt.str
    "%s {\n%s\n}"
    (go_of_annotated_expr condition_template.condition)
    (go_of_block ~sequential condition_template.contents)


and go_of_if_record ~sequential if_record =
  Fmt.str
    "if %s %s"
    (map_concat
       ~sep:" else if "
       ~f:(go_of_condition_template ~sequential)
       (List.append [ if_record._if ] if_record.else_if))
    (match if_record.else_contents with
    | Some else_contents -> go_of_block ~sequential else_contents
    | None -> "")


and go_of_structure ~sequential = function
  | Block_struct block -> Fmt.str "{%s}" (go_of_block ~sequential block)
  | If if_record -> go_of_if_record ~sequential if_record
  | For_loop for_loop -> go_of_for_loop ~sequential for_loop
  | For_each for_each -> go_of_for_each ~sequential for_each


and go_of_command ~sequential = function
  | Structure structure -> go_of_structure ~sequential structure
  | Statement statement -> go_of_statement statement


and get_exec_path_cond runtime_cost =
  Fmt.str "%s < %s" (Cost.go_of_t runtime_cost) (Int.to_string threshold)


and par_group ~sequential wg_var num_block block_contents =
  has_sync := true;
  Fmt.str
    "var wg_%s sync.WaitGroup\nwg_%s.Add(%d)\n%s\nwg_%s.Wait()"
    (Alpha.string_of_t wg_var)
    (Alpha.string_of_t wg_var)
    num_block
    (String.concat
       ~sep:"\n"
       (List.map
          ~f:(fun block_item ->
            Fmt.str
              "go func(){\n%s\nwg_%s.Done()\n}()"
              (go_of_command ~sequential block_item)
              (Alpha.string_of_t wg_var))
          block_contents))
    (Alpha.string_of_t wg_var)


and seq_group ~sequential block_contents =
  map_concat ~sep:"\n" ~f:(go_of_command ~sequential) block_contents


and contains_substring search target =
  match String.substr_index search ~pattern:target with
  | None -> false
  | _ -> true


and go_of_block
    ~sequential
    (block : (Parallelisation_ast.block_annot, 'a, 'b) block)
  =
  match block.annotations.parallelise_contents, sequential with
  | Some num_block, false ->
    let wg_var = Alpha.get_new_alpha alpha_generator in
    let block_contents = block.contents in
    let seq_str = seq_group ~sequential block_contents in
    if sequential
    then seq_str
    else (
      let exec_path_cond = get_exec_path_cond block.annotations.runtime_cost in
      if contains_substring exec_path_cond "math.Pow" then has_math := true;
      Fmt.str
        "if %s {%s} else {%s}"
        exec_path_cond
        seq_str
        (par_group ~sequential wg_var num_block block_contents))
  | _ ->
    Fmt.str
      "%s"
      (map_concat ~sep:"\n" ~f:(go_of_command ~sequential) block.contents)


and go_of_param (var, type_id) =
  Fmt.str "%s %s" (go_of_var var) (go_of_type_id type_id)


and go_of_func ~sequential (func : ('c, 'd, 'e) func) =
  Fmt.str
    "func %s (%s) %s {\n%s\n}"
    (go_of_var func.name)
    (map_concat ~sep:", " ~f:go_of_param func.params)
    (go_of_type_id func.return_type)
    (go_of_block ~sequential func.body)


let if_cond_then_str cond str = if cond then str else ""

let input has_input =
  if_cond_then_str
    has_input
    "func input() string { \n\
    \  var __s string\n\
    \  fmt.Scan(&__s)\n\
    \  return __s\n\
     }\n\n"


let check has_check =
  if_cond_then_str
    has_check
    "func check(err error) {\n\tif err != nil {\n\t\t\tpanic(err)\n\t}\n}\n\n"


let _open has_open =
  if_cond_then_str
    has_open
    "func open(filename string) *os.File {\n\
     file, err := os.OpenFile(filename, os.O_APPEND|os.O_WRONLY, 0666)\n\
     check(err)\n\
     return file\n\
     }\n\n"


let read has_read =
  if_cond_then_str
    has_read
    "func read(file *os.File) string {\n\
    \  dat, err := os.ReadFile(file.Name())\n\
    \  check(err)\n\
    \  return string(dat)\n\
     }\n\n"


let write has_write =
  if_cond_then_str
    has_write
    "func write(file *os.File, contents string) {\n\
    \  file, err := os.Create(file.Name())\n\
    \  check(err) \n\
    \  _, err = file.WriteString(contents)\n\
    \  check(err)\n\
     }\n\n"


let append has_append =
  if_cond_then_str
    has_append
    "func append(file *os.File, contents string) {\n\
    \  _, err := file.WriteString(contents)\n\
    \  check(err)\n\
     }\n\n"


let extra_funcs has_input has_open has_read has_write has_append =
  let has_check = has_open || has_read || has_write || has_append in
  input has_input
  ^ check has_check
  ^ _open has_open
  ^ read has_read
  ^ write has_write
  ^ append has_append


let go_of_program ~sequential program =
  Fmt.str "package %s\n\n" program.package
  ^ Parallelisation_ast.string_of_import_annot program.imports
  ^ extra_funcs !has_input !has_open !has_read !has_write !has_append
  ^ map_concat ~sep:"\n" ~f:go_of_var_statement program.global_vars
  ^ "\n\n"
  ^ map_concat ~sep:"\n\n" ~f:(go_of_func ~sequential) program.funcs


let add_import program cond_ref import =
  if !cond_ref
  then { program with imports = Import_annotation.add program.imports import }
  else program


let pipeline_ast ~sequential program =
  let output_file =
    if sequential
    then "./files/compiled/seq_go_program.go"
    else "./files/compiled/go_program.go"
  in
  let _ = go_of_program ~sequential program in
  let program = add_import program has_sync I_Sync in
  let program = add_import program has_math I_Math in
  Out_channel.write_all output_file ~data:(go_of_program ~sequential program)
