open! Core

type t = int [@@deriving of_sexp, sexp_of, compare]

let counter = ref 0

let create =
  let x = !counter in
  counter := x + 1;
  x


let string_of_t t = Fmt.str "id{%d}" t
