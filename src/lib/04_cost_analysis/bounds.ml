open! Core
open Util
open Util.Extended_set

type int_bound = int * int [@@deriving of_sexp, sexp_of, compare]
(* (lower_bound, upper_bound) *)

let sum_int_bounds (l1, u1) (l2, u2) = l1 + l2, u1 + u2
let negate_int_bound (l, u) = -1 * l, -1 * u

module Coef_var = struct
  type t = Alpha.t * int (* αⁿ *) [@@deriving of_sexp, sexp_of, compare]

  let string_of_t (alpha, power) =
    Fmt.str "%s^%s" (Alpha.string_of_t alpha) (Int.to_string power)
end

module Coef_var_set = struct
  include Make_extended_set (Coef_var)

  let string_of_t t =
    String.concat
      (List.sort
         ~compare:String.compare
         (List.map
            ~f:(fun t -> Fmt.str "(%s)" (Coef_var.string_of_t t))
            (elements t)))
end

module Parametric_bound = struct
  type t = int_bound * Coef_var_set.t
  (* [<lower_int, upper_int> x αⁿβᵐ... ] *)
  [@@deriving of_sexp, sexp_of, compare]

  let string_of_t ((lower_bound, upper_bound), coef_var_set) =
    Fmt.str
      "<%s,%s>%s"
      (Int.to_string lower_bound)
      (Int.to_string upper_bound)
      (Coef_var_set.string_of_t coef_var_set)
end

module Parametric_bound_set = struct
  include Make_extended_set (Parametric_bound)

  let set_of_list l =
    List.fold_left ~init:empty ~f:(fun set elem -> union set (create elem)) l


  let string_of_t t =
    String.concat
      ~sep:" + "
      (List.sort
         ~compare:String.compare
         (List.map ~f:Parametric_bound.string_of_t (elements t)))


  let rec add_parametric_bound_to_list
      ?(acc = [])
      pbound_list
      (int_bound, coef_vars)
    =
    match pbound_list with
    | (existing_int_bound, existing_coef_vars) :: pbound_list ->
      if Coef_var_set.compare coef_vars existing_coef_vars = 1
      then
        acc
        @ ((sum_int_bounds int_bound existing_int_bound, coef_vars)
          :: pbound_list)
      else
        add_parametric_bound_to_list
          ~acc:((existing_int_bound, existing_coef_vars) :: acc)
          pbound_list
          (int_bound, coef_vars)
    | [] -> (int_bound, coef_vars) :: acc


  let simplify t =
    set_of_list
      (List.fold_left
         ~init:[]
         ~f:(fun acc pbound -> add_parametric_bound_to_list acc pbound)
         (elements t))


  let sum t1 t2 = simplify (union t1 t2)

  let negate t =
    set_of_list
      (List.map
         ~f:(fun (int_bound, coef_var_set) ->
           negate_int_bound int_bound, coef_var_set)
         (elements t))


  let subtract t1 t2 = sum t1 (negate t2)
end

type cost_list = Parametric_bound_set.t list

let sum_cost_list =
  List.fold_left ~init:Parametric_bound_set.empty ~f:Parametric_bound_set.sum
