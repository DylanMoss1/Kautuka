open! Core

type t = string [@@deriving of_sexp, sexp_of, compare]

let counter = ref 0

let get_id_num =
  let x = !counter in
  counter := x + 1;
  x


let create = Fmt.str "id{%d}" get_id_num
let string_of_t t = t
