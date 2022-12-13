open! Core

module type Type_item = sig
  type t [@@deriving of_sexp, sexp_of, compare]

  val string_of_t : t -> string
end

module type Type_extended_set = sig
  include Set.S

  val create : Elt.t -> t
  val union_of_list : t list -> t
  val string_of_t : t -> string
end

module Make_extended_set (I : Type_item) : Type_extended_set 

(* module type Extended_set = sig
  include module type of Set 

  val union_of_list : ('elt, 'cmp) t list -> ('elt, 'cmp) t
  val string_of_t : ('elt, 'cmp) t -> string
end

module Make_extended_set (I : Item) : Extended_set with type t = I.t *)