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
      "{%s}"
      (String.concat
         ~sep:", "
         (List.sort
            ~compare:String.compare
            (List.map ~f:I.string_of_t (elements t))))
end

(* 
module Extended_set (I : Item) = struct
  include Set.Make (I)

  let union_of_list l = List.fold_left ~f:union ~init:empty l

  let string_of_t t =
    Fmt.str
      "{%s}"
      (String.concat
         ~sep:", "
         (List.sort
            ~compare:String.compare
            (List.map ~f:I.string_of_t (elements t))))
end *)

(* module Effect = struct
  module T = struct
    type t = effect [@@deriving of_sexp, sexp_of, compare]
  end

  include T
  include Comparable.Make (T)
end

module Effect_set = struct
  include Set.Make (Effect)

  let union_of_list l = List.fold_left ~f:union ~init:empty l

  let to_string set =
    Fmt.str
      "{%s}"
      (String.concat
         ~sep:", "
         (List.sort
            ~compare:String.compare
            (List.map ~f:string_of_effect (elements set))))

  let create effect = add empty effect 
end *)