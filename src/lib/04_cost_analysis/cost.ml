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

  let go_of_t (l, u) = Float.to_string ((Int.to_float l +. Int.to_float u) /. 2.)
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


  let union t1 t2 =
    let l1, u1 = t1 in
    let l2, u2 = t2 in
    Int.min l1 l2, Int.max u1 u2


  let zero_lower t =
    let _, u = t in
    0, u


  let zero_upper t =
    let l, _ = t in
    l, 0


  let round_to_int f = int_of_float (Float.round f)
  let apply_linear m c x = round_to_int (c +. (m *. float_of_int x))
  let fit_linear m c ((l, u) : t) : t = apply_linear m c l, apply_linear m c u
end

module Variable_bound = struct
  type t = (Alpha.t * int) list
  (* αⁿβᵐ... *) [@@deriving of_sexp, sexp_of, compare]

  exception Variable_not_in_map

  let create_single_var_bound alpha = [ alpha, 1 ]

  let string_of_item (alpha, power) =
    Fmt.str "%s^%s" (Alpha.string_of_t alpha) (Int.to_string power)


  let go_of_item tagged_variables (alpha, power) =
    let alpha_str =
      if List.exists tagged_variables ~f:(fun x -> Alpha.compare x alpha = 0)
      then Fmt.str "len(%s)" (Alpha.string_of_t alpha)
      else Alpha.string_of_t alpha
    in
    Fmt.str "math.Pow(float64(%s), %s)" alpha_str (Int.to_string power)


  let string_of_t =
    string_of_list ~left_format:"(" ~right_format:")" ~string_of_item


  let go_of_t t tagged_variables =
    string_of_list
      ~sep:" * "
      ~left_format:"("
      ~right_format:")"
      ~string_of_item:(go_of_item tagged_variables)
      t


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

  let apply_map map t =
    let alpha, power = t in
    let rec iterate_map = function
      | (beta, cost) :: map ->
        if Alpha.compare alpha beta = 0 then cost, power else iterate_map map
      | [] -> raise Variable_not_in_map
    in
    iterate_map map
end

module Cost = struct
  type t = (Integer_bound.t * Variable_bound.t) list
  [@@deriving of_sexp, sexp_of, compare]
  (* (<l1,u1>αⁿβᵐ...) + (<l2,u2>αˢβᵗ...) + ... *)

  exception Negative_power

  let string_of_item (int_bound, var_bound) =
    Fmt.str
      "%s%s"
      (Integer_bound.string_of_t int_bound)
      (Variable_bound.string_of_t var_bound)


  let go_of_item tagged_variables (int_bound, var_bound) =
    let int_go = Integer_bound.go_of_t int_bound in
    let var_go = Variable_bound.go_of_t var_bound tagged_variables in
    if String.compare var_go "" = 0
    then int_go
    else Fmt.str "%s * %s" int_go var_go


  let string_of_t t = string_of_list ~sep:" + " ~string_of_item t

  let go_of_t t tagged_variables =
    string_of_list ~sep:" + " ~string_of_item:(go_of_item tagged_variables) t


  let add_cost_terms_if_matching cost_term1 cost_term2 =
    let int_bound1, var_bound1 = cost_term1 in
    let int_bound2, var_bound2 = cost_term2 in
    if Variable_bound.compare var_bound1 var_bound2 = 0
    then Some (Integer_bound.sum int_bound1 int_bound2, var_bound1)
    else None


  let add_cost_term cost_term t =
    join_term ~join_term_if_matching:add_cost_terms_if_matching cost_term t


  let empty = []
  let create_int_bound_cost int_bound = [ int_bound, Variable_bound.empty ]
  let create_int_cost i = [ Integer_bound.create (i, i), Variable_bound.empty ]

  let create_int_bound (l, u) =
    [ Integer_bound.create (l, u), Variable_bound.empty ]


  let zero = create_int_bound_cost Integer_bound.zero
  let one = create_int_bound_cost Integer_bound.one

  let default_input_bound =
    create_int_bound_cost (Integer_bound.create (0, 100))


  let create_single_var_bound alpha =
    [ Integer_bound.create (1, 1), Variable_bound.create_single_var_bound alpha
    ]


  let sum_reduce =
    List.fold_left ~init:[] ~f:(fun acc cost_term ->
        add_cost_term cost_term acc)


  let sum t1 t2 = sum_reduce (t1 @ t2)

  let negate =
    List.map ~f:(fun cost_term ->
        let int_bound, var_bound = cost_term in
        Integer_bound.negate int_bound, var_bound)


  let subtract t1 t2 = sum t1 (negate t2)
  let add_cost_term cost_term t = sum_reduce (cost_term :: t)

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
    sum_reduce (all_pairs_map ~f:(fun (x, y) -> multiply_cost_terms x y) t1 t2)


  let fit_linear m c =
    List.map ~f:(fun (int_bound, var_bound) ->
        Integer_bound.fit_linear m c int_bound, var_bound)


  (* let scalar_multiply t ~scalar = 
    multiply t (create_int_cost scalar) *)

  let rec exponent t power =
    if power = 0
    then one
    else if power >= 1
    then multiply t (exponent t (power - 1))
    else raise Negative_power


  let substitute_cost_term map (cost_term : Integer_bound.t * Variable_bound.t) =
    (* print_endline "1";
    print_endline
      (String.concat
         ~sep:","
         (List.map
            ~f:(fun (alpha, t) ->
              Fmt.str "%s:%s" (Alpha.string_of_t alpha) (string_of_t t))
            map));
    let i, v = cost_term in
    print_endline
      (Fmt.str
         "%s%s"
         (Integer_bound.string_of_t i)
         (Variable_bound.string_of_t v)); *)
    let int_bound, var_bound = cost_term in
    List.fold_left
      ~init:(create_int_bound_cost int_bound)
      ~f:(fun acc var_term ->
        multiply
          acc
          (let cost, power = Variable_bound.apply_map map var_term in
           exponent cost power))
      var_bound


  let substitute map =
    List.fold_left ~init:zero ~f:(fun acc cost_term ->
        sum acc (substitute_cost_term map cost_term))


  let union_cost_terms_if_matching cost_term1 cost_term2 =
    let int_bound1, var_bound1 = cost_term1 in
    let int_bound2, var_bound2 = cost_term2 in
    if Variable_bound.compare var_bound1 var_bound2 = 0
    then Some (Integer_bound.union int_bound1 int_bound2, var_bound1)
    else None


  let union_cost_term cost_term t =
    join_term ~join_term_if_matching:union_cost_terms_if_matching cost_term t


  let union_reduce =
    List.fold_left ~init:[] ~f:(fun acc cost_term ->
        union_cost_term cost_term acc)


  let union t1 t2 = union_reduce (t1 @ t2)
  let union_of_list = List.fold_left ~init:[] ~f:union

  let create_lower_upper_bounded_cost ~(lower : t) ~(upper : t) : t =
    let zeroed_lower =
      List.map
        ~f:(fun (int_bound, var_bound) ->
          Integer_bound.zero_upper int_bound, var_bound)
        lower
    in
    let zeroed_upper =
      List.map
        ~f:(fun (int_bound, var_bound) ->
          Integer_bound.zero_lower int_bound, var_bound)
        upper
    in
    sum zeroed_lower zeroed_upper
end
