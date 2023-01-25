open! Core
open Util.Extended_set
open Ast.Ast_types
open Ast.Annotated_ast
open Ast.Ast_pipeline
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

module Import_annotation = struct
  include Make_extended_set (Import)

  let string_of_t t =
    Fmt.str
      "import (%s)\n"
      (String.concat
         ~sep:"\n"
         (List.sort
            ~compare:String.compare
            (List.map ~f:Import.string_of_t (elements t))))
end

module Import_ast =
  Annotated_ast (Block_Annotation) (Var_name_annotation) (Import_annotation)
    (Expr_empty_annotation)

module Import_ast_mapping = struct
  include
    Default_ast_mapping (Parsed_ast) (Import_ast) (Empty_context)
      (Import_annotation)

  let add_result = Import_annotation.add

  let func_call env func_call result =
    match func_call with
    | User_func user_func -> env, User_func user_func, result
    | Print expr -> env, Print expr, add_result result I_Fmt
    | Input -> env, Input, add_result result I_Fmt
    | Open expr -> env, Open expr, add_result result I_Fmt
    | Read expr -> env, Read expr, add_result result I_Fmt
    | Write write_template -> env, Write write_template, add_result result I_Fmt
    | Append write_template ->
      env, Append write_template, add_result result I_Fmt


  let program
      ~env
      ~new_package
      ~old_import:_
      ~new_global_vars
      ~new_funcs
      ~result
    =
    ( env
    , { package = new_package
      ; imports = result
      ; global_vars = new_global_vars
      ; funcs = new_funcs
      }
    , result )
end

module Import_ast_pipeline = Ast_pipeline (Import_ast_mapping)
