open! Core

module type Type_result = sig
  type t

  val empty : t
  val join : t -> t -> t
  val join_list : t list -> t
end
