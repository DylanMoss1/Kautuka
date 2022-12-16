open! Core
open Util.Extended_set
open Ast.Ast_types
open Ast.Annotated_ast
open Pipeline_ast
open Parsing.Parser_types

type import =
  | I_Fmt
  | I_Os
[@@deriving of_sexp, sexp_of, compare]

module Import = struct
  type t = import [@@deriving of_sexp, sexp_of, compare]

  let string_of_t t =
    Fmt.str
      "import \"%s\""
      (match t with
      | I_Fmt -> "fmt"
      | I_Os -> "os")


  let create x = x
end

module Import_set = Make_extended_set (Import)
module Import_ast = Annotated_ast (Block_type_annotation) (Import_set)

module Import_ast_mapping : Type_ast_mapping = struct
  type result = Import_ast.import_annotation
  type old_block_annot = Parsed_ast.block_annotation
  type old_import_annot = Parsed_ast.import_annotation
  type block_annot = Import_ast.block_annotation
  type import_annot = Import_ast.import_annotation

  let empty_result () = Import_set.empty
  let collect_results = Import_set.union_of_list
  let no_import new_ast = new_ast, Import_set.empty
  let relay_import new_ast result = new_ast, result
  let program new_program result = { new_program with imports = result }, result
  let func = relay_import
  let param = relay_import
  let block = relay_import
  let command = relay_import
  let structure = relay_import
  let statement = relay_import
  let for_loop = relay_import
  let for_each = relay_import
  let condition_template = relay_import
  let if_record = relay_import
  let control = no_import

  let func_call func_call result =
    match func_call with
    | User_func user_func -> User_func user_func, result
    | Print expr -> Print expr, Import_set.add result I_Fmt
    | Input -> Input, Import_set.add result I_Fmt
    | Open expr -> Open expr, Import_set.add result I_Fmt
    | Read expr -> Read expr, Import_set.add result I_Fmt
    | Write write_template -> Write write_template, Import_set.add result I_Fmt
    | Append write_template ->
      Append write_template, Import_set.add result I_Fmt


  let write_template = relay_import
  let user_func = relay_import
  let var = relay_import
  let expr = relay_import
  let binop = no_import
  let unop = no_import
  let value = no_import
  let type_id = no_import
  let id = no_import
end

module Import_pipeline = Ast_pipeline (Import_ast_mapping)

(* 
   
  Import_pipeline.pipeline_program : 

*)


(* open Core
open Ast.Ast_types
open Util.Extended_set

type import =
  | I_Fmt
  | I_Os
[@@deriving of_sexp, sexp_of, compare]

module type Type_import = sig
  include Type_item

  val create : import -> t
end

module Import : Type_import = struct
  type t = import [@@deriving of_sexp, sexp_of, compare]

  let string_of_t t =
    Fmt.str
      "import \"%s\""
      (match t with
      | I_Fmt -> "fmt"
      | I_Os -> "os")


  let create x = x
end

module Import_set = Make_extended_set (Import)

let append_tuple (xs, ys) (x, y) = x :: xs, y :: ys
let unzip l = List.fold_left ~f:append_tuple ~init:([], []) l


(* let map_ast ~map ast =
  let mapped_ast, result = map ast in 
 *)



(* Takes an ast mapping function and result union function 
   and returns (modified_ast, results) *)
let modify_ast_list_and_get_results ~ast_mapping ~result_union ast =
  let modified_ast, result_list = unzip (List.map ~f:ast_mapping ast) in
  let result = result_union result_list in
  modified_ast, result


(* An implementation of modify_ast_and_get_results for imports*)
let get_import_list =
  modify_ast_list_and_get_results ~result_union:Import_set.union_of_list


let no_import item = item, Import_set.empty

let nested_import_on_ast ~ast_mapping ~modified_ast ?(add_import = None) ast =
  let import_ast, import = ast_mapping ast in
  match add_import with
  | None -> modified_ast import_ast, import
  | Some add_import -> modified_ast import_ast, Import_set.add import add_import


let rec import_of_expr expr =
  let nested_import_on_expr expr =
    nested_import_on_ast ~ast_mapping:import_of_expr expr
  in
  match expr with
  | Unop (unop, expr) ->
    nested_import_on_expr ~modified_ast:(fun x -> Unop (unop, x)) expr
  | Binop (expr1, binop, expr2) ->
    let import_ast_expr1, import_expr1 = import_of_expr expr1 in
    let import_ast_expr2, import_expr2 = import_of_expr expr2 in
    ( Binop (import_ast_expr1, binop, import_ast_expr2)
    , Import_set.union import_expr1 import_expr2 )
  | Paren expr -> nested_import_on_expr ~modified_ast:(fun x -> Paren x) expr
  | expr -> no_import expr


let nested_import_on_expr expr =
  nested_import_on_ast ~ast_mapping:import_of_expr expr


let import_of_var = function
  | VarInit (id, type_id, expr) ->
    nested_import_on_expr ~modified_ast:(fun x -> VarInit (id, type_id, x)) expr
  | VarDecl (id, expr) ->
    nested_import_on_expr ~modified_ast:(fun x -> VarDecl (id, x)) expr
  | VarAssign (id, expr) ->
    nested_import_on_expr ~modified_ast:(fun x -> VarAssign (id, x)) expr
  | var -> no_import var


let import_of_write_template (write_template : write_template)
    : write_template * Import_set.t
  =
  nested_import_on_expr
    ~modified_ast:(fun x -> { write_template with contents = x })
    write_template.contents


let import_of_func_call = function
  | Print expr -> nested_import_on_expr ~modified_ast:(fun x -> Print x) expr
  | Input -> Input, Import_set.create (Import.create I_Fmt)
  | Open expr -> nested_import_on_expr ~modified_ast:(fun x -> Open x) expr
  | Read expr -> nested_import_on_expr ~modified_ast:(fun x -> Read x) expr
  | Write write_template ->
    nested_import_on_ast
      ~ast_mapping:import_of_write_template
      ~modified_ast:(fun x -> Write x)
      ~add_import:(Some (Import.create I_Os))
      write_template
  | Append write_template ->
    nested_import_on_ast
      ~ast_mapping:import_of_write_template
      ~modified_ast:(fun x -> Append x)
      ~add_import:(Some (Import.create I_Os))
      write_template
  | func_call -> no_import func_call


let import_of_statement = function
  | Var var ->
    nested_import_on_ast
      ~ast_mapping:import_of_var
      ~modified_ast:(fun x -> Var x)
      var
  | Func_call func_call ->
    nested_import_on_ast
      ~ast_mapping:import_of_func_call
      ~modified_ast:(fun x -> Func_call x)
      func_call
  | statement -> no_import statement


let rec import_of_for_loop for_loop =
  let import_ast_init, import_init = import_of_var for_loop.init in
  let import_ast_cond, import_cond = import_of_expr for_loop.cond in
  let import_ast_iter, import_iter = import_of_var for_loop.iter in
  let import_ast_contents, import_contents =
    import_of_block for_loop.contents
  in
  let import_for_loop =
    Import_set.union_of_list
      [ import_init; import_cond; import_iter; import_contents ]
  in
  ( { init = import_ast_init
    ; cond = import_ast_cond
    ; iter = import_ast_iter
    ; contents = import_ast_contents
    }
  , import_for_loop )


and import_of_for_each (for_each : 'a for_each) =
  nested_import_on_ast
    ~ast_mapping:import_of_block
    ~modified_ast:(fun x -> { for_each with contents = x })
    for_each.contents


and import_of_condition_template condition_template =
  let import_ast_condition, import_condition =
    import_of_expr condition_template.condition
  in
  let import_ast_contents, import_contents =
    import_of_block condition_template.contents
  in
  ( { condition = import_ast_condition; contents = import_ast_contents }
  , Import_set.union import_condition import_contents )


and import_of_if_record if_record =
  let import_ast_if, import_if = import_of_condition_template if_record._if in
  let import_ast_else_if, import_else_if =
    get_import_list ~ast_mapping:import_of_condition_template if_record.else_if
  in
  let import_ast_else_contents, import_else_contents =
    match if_record.else_contents with
    | Some contents ->
      nested_import_on_ast
        ~ast_mapping:import_of_block
        ~modified_ast:(fun x -> Some x)
        contents
    | None -> None, Import_set.empty
  in
  ( { _if = import_ast_if
    ; else_if = import_ast_else_if
    ; else_contents = import_ast_else_contents
    }
  , Import_set.union_of_list [ import_if; import_else_if; import_else_contents ]
  )


and import_of_structure = function
  | Block_struct block ->
    nested_import_on_ast
      ~ast_mapping:import_of_block
      ~modified_ast:(fun x -> Block_struct x)
      block
  | If if_record ->
    nested_import_on_ast
      ~ast_mapping:import_of_if_record
      ~modified_ast:(fun x -> If x)
      if_record
  | While condition_template ->
    nested_import_on_ast
      ~ast_mapping:import_of_condition_template
      ~modified_ast:(fun x -> While x)
      condition_template
  | For_loop for_loop ->
    nested_import_on_ast
      ~ast_mapping:import_of_for_loop
      ~modified_ast:(fun x -> For_loop x)
      for_loop
  | For_each for_each ->
    nested_import_on_ast
      ~ast_mapping:import_of_for_each
      ~modified_ast:(fun x -> For_each x)
      for_each


and import_of_command = function
  | Structure structure ->
    nested_import_on_ast
      ~ast_mapping:import_of_structure
      ~modified_ast:(fun x -> Structure x)
      structure
  | Statement statement ->
    nested_import_on_ast
      ~ast_mapping:import_of_statement
      ~modified_ast:(fun x -> Statement x)
      statement


and import_of_block block =
  let import_ast_contents, import_contents =
    get_import_list ~ast_mapping:import_of_command block.contents
  in
  { block with contents = import_ast_contents }, import_contents


and import_of_func func =
  nested_import_on_ast
    ~ast_mapping:import_of_block
    ~modified_ast:(fun x -> { func with body = x })
    func.body


let import_ast_of_program program =
  let import_ast_global_vars, import_global_vars =
    get_import_list ~ast_mapping:import_of_var program.global_vars
  in
  let import_ast_funcs, import_funcs =
    get_import_list ~ast_mapping:import_of_func program.funcs
  in
  let imports = Import_set.union import_global_vars import_funcs in
  { package = program.package
  ; imports = Some imports
  ; global_vars = import_ast_global_vars
  ; funcs = import_ast_funcs
  } *)
