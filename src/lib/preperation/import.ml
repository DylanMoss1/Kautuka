open! Core
open Util.Extended_set
open Ast.Ast_types
open Ast.Annotated_ast
open Ast_pipeline
open Parsing.Parser_types

type import =
  | I_Fmt
  | I_Os
[@@deriving of_sexp, sexp_of, compare]

module Import = struct
  type t = import [@@deriving of_sexp, sexp_of, compare]

  let string_of_t t =
    Fmt.str
      "\"%s\""
      (match t with
      | I_Fmt -> "fmt"
      | I_Os -> "os")


  let create x = x
end

module Import_some_annotation = Make_extended_set (Import)

module Import_ast =
  Annotated_ast (Block_type_annotation) (Var_name_annotation)
    (Import_some_annotation)

module Unknown_import_ast_mapping = struct
  type result = Import_ast.import_annot
  type old_block_annot = Parsed_ast.block_annot
  type old_var_annot = Parsed_ast.var_annot
  type old_import_annot = Parsed_ast.import_annot
  type new_block_annot = Import_ast.block_annot
  type new_var_annot = Import_ast.var_annot
  type new_import_annot = Import_ast.import_annot

  let collect_results = Import_some_annotation.union_of_list
  let empty_result () = Import_some_annotation.empty

  include No_env
end

module Import_ast_mapping = struct
  include Default_ast_mapping (Unknown_import_ast_mapping)

  let func_call env func_call (result : result) =
    match func_call with
    | User_func user_func -> env, User_func user_func, result
    | Print expr -> env, Print expr, Import_some_annotation.add result I_Fmt
    | Input -> env, Input, Import_some_annotation.add result I_Fmt
    | Open expr -> env, Open expr, Import_some_annotation.add result I_Fmt
    | Read expr -> env, Read expr, Import_some_annotation.add result I_Fmt
    | Write write_template ->
      env, Write write_template, Import_some_annotation.add result I_Fmt
    | Append write_template ->
      env, Append write_template, Import_some_annotation.add result I_Fmt


  let program ~env ~new_package ~old_import:_ ~new_global_vars ~new_funcs ~result =
    (env, { package = new_package
      ; imports = result
      ; global_vars = new_global_vars
      ; funcs = new_funcs
      }
    , result )
end

module Import_ast_pipeline = Ast_pipeline (Import_ast_mapping)
