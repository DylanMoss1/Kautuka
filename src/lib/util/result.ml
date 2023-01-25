open! Core

module type Result = sig
  type t

  val empty : t
  val join : t -> t -> t
  val join_list : t list -> t
  val union_list : t list -> t
end

module Empty_result = struct
  type t = unit

  let empty = ()
  let join _ _ = ()
  let join_list _ = ()
  let union_list _ = ()
end
