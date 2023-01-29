open! Core

type alpha_generator = int ref
type t = int [@@deriving of_sexp, sexp_of, compare]

let t = ref 0
let create = t

let get_new_alpha ?(is_main = false) t =
  if is_main
  then 0
  else (
    t := !t + 1;
    !t)


let is_t_main t = t = 0
let string_of_t t = if is_t_main t then "main" else Fmt.str "alpha_%d" t
