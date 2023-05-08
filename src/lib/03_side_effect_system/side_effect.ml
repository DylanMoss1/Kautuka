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
  | File of File_ref.t
  | Var of Alpha.t
[@@deriving of_sexp, sexp_of, compare]

module Side_effect = struct
  type t = side_effect_operation * side_effect_channel
  [@@deriving of_sexp, sexp_of, compare]

  let string_of_side_effect_channel = function
    | Console -> "console"
    | File file_ref -> Fmt.str "file(%s)" (File_ref.string_of_t file_ref)
    | Var uuid -> Fmt.str "var[%s]" (Alpha.string_of_t uuid)


  let string_of_t = function
    | Read, effect_type -> "R:" ^ string_of_side_effect_channel effect_type
    | Write, effect_type -> "W:" ^ string_of_side_effect_channel effect_type


  let create x = x

  let is_non_interfering t1 t2 =
    match t1, t2 with
    | (Read, _), (Read, _) -> true
    | (_, File file_ref1), (_, File file_ref2) ->
      File_ref.disjoint file_ref1 file_ref2
    | (_, channel1), (_, channel2) ->
      compare_side_effect_channel channel1 channel2 <> 0


  (* 
  let is_non_interfering t1 t2 =
    print_endline (string_of_t t1);
    print_endline (string_of_t t2);
    let y = is_non_interfering_x t1 t2 in
    print_endline (string_of_bool y);
    y *)

  let extract_var (_, channel) =
    match channel with
    | Var alpha -> Some alpha
    | _ -> None
end
