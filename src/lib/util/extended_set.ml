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

module Make_extended_set (I : Type_item) = struct
  include Set.Make (I)

  let create elt = add empty elt
  let union_of_list l = List.fold_left ~f:union ~init:empty l

  let string_of_t t =
    Fmt.str
      "import (%s)\n"
      (String.concat
         ~sep:"\n"
         (List.sort
            ~compare:String.compare
            (List.map ~f:I.string_of_t (elements t))))
end
