open! Core

module type Type_item = sig
  type t [@@deriving of_sexp, sexp_of, compare]

  val string_of_t : t -> string
end
