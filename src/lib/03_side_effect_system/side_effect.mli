open! Core
open Util

type 'a var_type =
  | Func_var of 'a
  | Value_var of 'a

type side_effect_operation =
  | Read
  | Write
[@@deriving of_sexp, sexp_of, compare]

type side_effect_channel =
  | Console
  | File of File_ref.t
  | Var of Util.Alpha.t
[@@deriving of_sexp, sexp_of, compare]

module Side_effect : sig
  type t [@@deriving of_sexp, sexp_of, compare]

  val string_of_t : t -> string
  val create : side_effect_operation * side_effect_channel -> t
  val is_non_interfering : t -> t -> bool
  val extract_var : t -> Alpha.t option
end
