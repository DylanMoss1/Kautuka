(* open! Core
open Util.Extended_set
open Ast.Ast_types
open Ast.Annotated_ast
open Ast_pipeline
open Parsing.Parser_types

module Variable_ast = Annotated_ast (Block_type_annotation) (Import_set)

module Unknown_import_ast_mapping = struct
  type result = Import_ast.import_annotation
  type old_block_annot = Parsed_ast.block_annotation
  type old_import_annot = Parsed_ast.import_annotation
  type new_block_annot = Import_ast.block_annotation
  type new_import_annot = Import_ast.import_annotation

  let collect_results = Import_set.union_of_list
  let empty_result () = Import_set.empty
end

module Import_ast_mapping = struct
  include Default_ast_mapping (Unknown_import_ast_mapping)

  let func_call func_call (result : result) =
    match func_call with
    | User_func user_func -> User_func user_func, result
    | Print expr -> Print expr, Import_set.add result I_Fmt
    | Input -> Input, Import_set.add result I_Fmt
    | Open expr -> Open expr, Import_set.add result I_Fmt
    | Read expr -> Read expr, Import_set.add result I_Fmt
    | Write write_template -> Write write_template, Import_set.add result I_Fmt
    | Append write_template ->
      Append write_template, Import_set.add result I_Fmt


  let program new_package _ new_global_vars new_funcs result =
    ( { package = new_package
      ; imports = result
      ; global_vars = new_global_vars
      ; funcs = new_funcs
      }
    , result )
end

module Import_ast_pipeline = Ast_pipeline (Import_ast_mapping) *)
