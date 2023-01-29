open! Core

type alpha_generator
type t [@@deriving of_sexp, sexp_of, compare]

val create : alpha_generator
val get_new_alpha : ?is_main:bool -> alpha_generator -> t
val string_of_t : t -> string
