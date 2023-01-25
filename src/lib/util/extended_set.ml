open! Core
open Item

module type Extended_set = sig
  include Set.S

  val create : Elt.t -> t
  val union_of_list : t list -> t
  val string_of_t : t -> string
  val join : t -> t -> t
  val join_list : t list -> t
end

module Make_extended_set (I : Item) = struct
  include Set.Make (I)

  let create elt = add empty elt

  let string_of_t t =
    Fmt.str
      "{%s}"
      (String.concat
         ~sep:", "
         (List.sort
            ~compare:String.compare
            (List.map ~f:I.string_of_t (elements t))))


  let union_of_list = List.fold_left ~init:empty ~f:union
  let join = union
  let join_list = union_of_list
end
