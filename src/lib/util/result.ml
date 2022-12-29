open! Core

module type Type_result = sig
  type t

  val empty : t
  val join : t -> t -> t
end
