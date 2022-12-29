open! Core

type t [@@deriving of_sexp, sexp_of, compare]

val create : unit -> t
val string_of_t : t -> string
