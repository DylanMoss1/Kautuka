(* open! Core

type 'a t =
  | Unit : unit t
  | Cons : 'a * 'b t -> ('a -> 'b) t

let unit : unit t = Unit

let append x t = Cons(x, t)

let hd : ('a -> 'b) t -> 'a = function
  | Cons (x, _) -> x

let tl : ('a -> 'b) t -> 'b t = function
  | Cons (_, xs) -> xs

(* let x = Cons *)

(* type annotation =
  { a : int
  ; b : string
  }

let annotation_of_tup hetro_tup : annotation =
  let y, ys = next hetro_tup in
  { a = y; b = fst ys }


let annotation_of_tup hetro_tup : annotation =
  let y, ys = next hetro_tup in
  { a = fst ys; b = y } *) *)
