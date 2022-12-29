open! Core
open Util

let join_term ~join_term_if_matching term term_list =
  let rec inner_join_term checked_list remaining_list =
    match remaining_list with
    | remaining_term :: remaining_list ->
      (match join_term_if_matching term remaining_term with
      | Some joined_term -> checked_list @ (joined_term :: remaining_list)
      | None -> inner_join_term (remaining_term :: checked_list) remaining_list)
    | [] -> term :: checked_list
  in
  inner_join_term [] term_list


let all_pairs xs ys =
  List.fold_left
    ~init:[]
    ~f:(fun acc_x x ->
      acc_x @ List.fold_left ~init:[] ~f:(fun acc_y y -> (x, y) :: acc_y) ys)
    xs


let all_pairs_map ~f (xs : 'a) (ys : 'a) : 'a = List.map ~f (all_pairs xs ys)

let string_of_list
    ?(sep = "")
    ?(left_format = "")
    ?(right_format = "")
    ~string_of_item
    l
  =
  String.concat
    ~sep
    (List.sort
       ~compare:String.compare
       (List.map ~f:(fun x -> left_format ^ string_of_item x ^ right_format) l))


module Integer_bound = struct
  type t = int * int (* <l, u> *) [@@deriving of_sexp, sexp_of, compare]

  let zero = 0, 0
  let one = 1, 1
  let create t = t

  let sum t1 t2 =
    let l1, u1 = t1 in
    let l2, u2 = t2 in
    l1 + l2, u1 + u2


  let negate t =
    let l, u = t in
    -1 * l, -1 * u


  let multiply t1 t2 =
    let l1, u1 = t1 in
    let l2, u2 = t2 in
    l1 * l2, u1 * u2


  let string_of_t t =
    let l, u = t in
    Fmt.str "<%d,%d>" l u
end

module Variable_bound = struct
  type t = (Alpha.t * int) list
  (* αⁿβᵐ... *) [@@deriving of_sexp, sexp_of, compare]

  let string_of_item (alpha, power) =
    Fmt.str "%s^%s" (Alpha.string_of_t alpha) (Int.to_string power)


  let string_of_t =
    string_of_list ~left_format:"(" ~right_format:")" ~string_of_item


  let empty = []

  let multiply_var_terms_if_matching var_term1 var_term2 =
    let var1, power1 = var_term1 in
    let var2, power2 = var_term2 in
    if Alpha.compare var1 var2 = 0 then Some (var1, power1 + power2) else None


  let multiply_var_term var_term t =
    join_term ~join_term_if_matching:multiply_var_terms_if_matching var_term t


  let reduce =
    List.fold_left ~init:[] ~f:(fun acc cost_term ->
        multiply_var_term cost_term acc)


  let multiply t1 t2 = reduce (t1 @ t2)
end

module Cost = struct
  type t = (Integer_bound.t * Variable_bound.t) list
  [@@deriving of_sexp, sexp_of, compare]
  (* (<l1,u1>αⁿβᵐ...) + (<l2,u2>αˢβᵗ...) + ... *)

  let string_of_item (int_bound, var_bound) =
    Fmt.str
      "%s%s"
      (Integer_bound.string_of_t int_bound)
      (Variable_bound.string_of_t var_bound)


  let string_of_t t = string_of_list ~sep:" + " ~string_of_item t

  let add_cost_terms_if_matching cost_term1 cost_term2 =
    let int_bound1, var_bound1 = cost_term1 in
    let int_bound2, var_bound2 = cost_term2 in
    if Variable_bound.compare var_bound1 var_bound2 = 0
    then Some (Integer_bound.sum int_bound1 int_bound2, var_bound1)
    else None


  let add_cost_term cost_term t =
    join_term ~join_term_if_matching:add_cost_terms_if_matching cost_term t


  let empty = []
  let create_int_cost int_bound = [ int_bound, Variable_bound.empty ]
  (* let zero = create_int_cost Integer_bound.zero *)
  (* let one = create_int_cost Integer_bound.one *)

  let reduce =
    List.fold_left ~init:[] ~f:(fun acc cost_term ->
        add_cost_term cost_term acc)


  let sum t1 t2 = reduce (t1 @ t2)

  let negate =
    List.map ~f:(fun cost_term ->
        let int_bound, var_bound = cost_term in
        Integer_bound.negate int_bound, var_bound)


  let subtract t1 t2 = sum t1 (negate t2)
  let add_cost_term cost_term t = reduce (cost_term :: t)
  (* let join = sum *)

  let multiply_cost_terms
      (cost_term1 : Integer_bound.t * Variable_bound.t)
      (cost_term2 : Integer_bound.t * Variable_bound.t)
      : Integer_bound.t * Variable_bound.t
    =
    let int_bound1, var_bound1 = cost_term1 in
    let int_bound2, var_bound2 = cost_term2 in
    ( Integer_bound.multiply int_bound1 int_bound2
    , Variable_bound.multiply var_bound1 var_bound2 )


  let multiply (t1 : t) (t2 : t) : t =
    reduce (all_pairs_map ~f:(fun (x, y) -> multiply_cost_terms x y) t1 t2)
end
