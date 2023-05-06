open! Core

type ref_generator = int ref
type id_generator = int ref

type ref_type =
  | User of int
  | Generated of int
[@@deriving of_sexp, sexp_of, compare]

type t = int * ref_type [@@deriving of_sexp, sexp_of, compare]

let ref_generator = ref 0
let id_generator = ref 0

let get_new_id id_generator =
  id_generator := !id_generator + 1;
  !id_generator


let get_new_ref ref_generator =
  ref_generator := !ref_generator + 1;
  !ref_generator


let get_new_generated_ref () =
  get_new_id id_generator, Generated (get_new_ref ref_generator)


let get_new_user_ref i = get_new_id id_generator, User i

let string_of_t = function
  | id, User ref -> Fmt.str "user[id:%d, user_ref:%d]" id ref
  | id, Generated ref -> Fmt.str "gen[id:%d, gen_ref:%d]" id ref


let get_id (id, _) = id

let disjoint (id1, ref1) (id2, ref2) =
  compare_ref_type ref1 ref2 = 0 && id1 <> id2
