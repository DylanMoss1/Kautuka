open! Core
open Item

module type Type_extended_set = sig
  include Set.S

  val create : Elt.t -> t
  val union_of_list : t list -> t
  val string_of_t : t -> string
end

module Make_extended_set (I : Type_item) = struct
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


  let join = union
  let join_list = List.fold_left ~init:empty ~f:union
end
