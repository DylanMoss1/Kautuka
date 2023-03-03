open! Core
open Util
open Util.Item
open Util.Context
open Ast.Annotated_ast
open Ast.Ast_pipeline
open Parsing.Parser_types
open Preperation.Import
open Ast.Ast_types

type alpha_var =
  { name : string
  ; alpha : Alpha.t
  }
[@@deriving compare, sexp_of, of_sexp]

module Alpha_conversion_annotation = struct
  type t = alpha_var [@@deriving compare, sexp_of, of_sexp]

  let string_of_t t =
    Fmt.str "var{name: %s, alpha: %s}" t.name (Alpha.string_of_t t.alpha)
end

let string_of_scoped_vars alpha_list =
  Fmt.str
    "{%s}"
    (String.concat ~sep:", " (List.map ~f:Alpha.string_of_t alpha_list))


module Block_scoped_vars_annotation = struct
  type t =
    { block_type : block_type
    ; scoped_vars : Alpha.t list
    }

  let create block_type scoped_vars = { block_type; scoped_vars }

  let string_of_t t =
    Fmt.str
      "{block_type: %s, scoped_vars: %s}"
      (string_of_block_type t.block_type)
      (string_of_scoped_vars t.scoped_vars)
end

module Alpha_conversion_context =
  Make_context (String_item) (Alpha)
    (struct
      let t = false
    end)

module Alpha_conversion_ast =
  Annotated_ast (Block_scoped_vars_annotation) (Alpha_conversion_annotation)
    (Import_annotation)
    (Expr_empty_annotation)

module Alpha_conversion_ast_mapping = struct
  include
    Default_ast_mapping (Import_ast) (Alpha_conversion_ast)
      (Alpha_conversion_context)
      (Empty_result)

  exception Unbound_var of string

  let alpha_generator = Alpha.create

  let var env (var : old_var_annot) ~var_effect =
    match var_effect with
    | Init ->
      (* Raises an exception if var exists in the inner-most scope *)
      let (_ : 'a option) =
        Alpha_conversion_context.get_value_outside_scope var.name env
      in
      let alpha =
        Alpha.get_new_alpha
          ~is_main:(String.compare var.name "main" = 0)
          alpha_generator
      in
      add_to_env var.name alpha env, { name = var.name; alpha }, empty_result
    | Read | Write ->
      (match Alpha_conversion_context.get_value var.name env with
      | Some alpha -> env, { name = var.name; alpha }, empty_result
      | None -> raise (Unbound_var var.name))


  let block ~env ~old_env ~new_contents ~old_annotations ~result =
    ( env
    , old_env
    , { contents = new_contents
      ; annotations =
          Block_scoped_vars_annotation.create
            old_annotations
            (Alpha_conversion_context.get_all_values env)
      }
    , result )
end

module Alpha_conversion_ast_pipeline =
  Ast_pipeline (Alpha_conversion_ast_mapping)
