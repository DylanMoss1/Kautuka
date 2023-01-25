open! Core

module type Item = sig
  type t [@@deriving of_sexp, sexp_of, compare]

  val string_of_t : t -> string
end

module String_item = struct
  type t = string [@@deriving of_sexp, sexp_of, compare]

  let string_of_t t = t
  let create x = x
end

module Unit_item = struct
  type t = unit [@@deriving of_sexp, sexp_of, compare]

  let string_of_t _ = ""
end
