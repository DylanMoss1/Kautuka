open! Core
open Ast.Ast_types
open Util

type 'a var_type =
  | Func_var of 'a var
  | Value_var of 'a var

type side_effect_operation =
  | Read
  | Write
[@@deriving of_sexp, sexp_of, compare]

type side_effect_channel =
  | Console
  | File
  | Var of Alpha.t
[@@deriving of_sexp, sexp_of, compare]

module Side_effect = struct
  type t = side_effect_operation * side_effect_channel
  [@@deriving of_sexp, sexp_of, compare]

  let string_of_side_effect_channel = function
    | Console -> "console"
    | File -> "file"
    | Var uuid -> Fmt.str "var[%s]" (Alpha.string_of_t uuid)


  let string_of_t = function
    | Read, effect_type -> "R:" ^ string_of_side_effect_channel effect_type
    | Write, effect_type -> "W:" ^ string_of_side_effect_channel effect_type


  let create x = x

  let is_disjoint t1 t2 =
    let t1_operation, t1_channel = t1 in
    let t2_operation, t2_channel = t2 in
    match t1_operation, t2_operation with
    | Read, Read -> true
    | _, _ ->
      (match t1_channel, t2_channel with
      | Console, Console -> false
      | File, File -> false
      | Var alpha1, Var alpha2 -> Alpha.compare alpha1 alpha2 <> 0
      | _ -> true)


  let extract_var (_, channel) =
    match channel with
    | Var alpha -> Some alpha
    | _ -> None
end
