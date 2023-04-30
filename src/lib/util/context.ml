open! Core
open Item

module type Context = sig
  type key
  type value
  type t

  val empty : t
  val add_item : key -> value -> t -> t
  val add_item_no_replace : key -> value -> t -> t
  val add_new_scope : is_func:bool -> t -> t
  val remove_scope : is_func:bool -> t -> t
  val get_value : key -> t -> value option
  val get_value_outside_scope : key -> t -> value option

  (* val apply_unary_fun : f:(value -> value) -> t -> t *)
  (* val apply_bin_fun : f:(value -> value -> value) -> t -> t -> t *)
  val get_all_values : t -> value list
  val string_of_t : t -> string

  val get_post_loop_context
    :  start_env:t
    -> end_env:t
    -> sum:(value -> value -> value)
    -> subtract:(value -> value -> value)
    -> multiply:('a -> value -> value)
    -> iterations:'a
    -> t
end

module type Bool_value = sig
  val t : bool
end

module Make_context (Key : Item) (Value : Item) (Is_func_context : Bool_value) =
struct
  type key = Key.t
  type value = Value.t
  type t = (key * value) list list

  exception Empty_scope
  exception Key_in_inner_scope
  exception Different_shaped_envs

  let string_of_key_value_pair (key, value) =
    Fmt.str "(%s,%s)" (Key.string_of_t key) (Value.string_of_t value)


  let string_of_scope scope =
    Fmt.str
      "[%s]"
      (String.concat ~sep:", " (List.map ~f:string_of_key_value_pair scope))


  let string_of_t t =
    Fmt.str "[%s]" (String.concat ~sep:", " (List.map ~f:string_of_scope t))


  let empty = [ [] ]

  let rec add_item_to_scope key value scope acc =
    match scope with
    | (k, v) :: remaining_scope ->
      if Key.compare key k = 0
      then Some (acc @ ((key, value) :: remaining_scope))
      else add_item_to_scope key value remaining_scope ((k, v) :: acc)
    | [] -> None


  let add_item key value env =
    let rec add_item_to_env key value remaining_env acc =
      match remaining_env with
      | scope :: remaining_env ->
        (match add_item_to_scope key value scope [] with
        | Some scope -> acc @ (scope :: remaining_env)
        | None -> add_item_to_env key value remaining_env (scope :: acc))
      | [] ->
        (match env with
        | inner_scope :: remaining_scope ->
          ((key, value) :: inner_scope) :: remaining_scope
        | [] -> raise Empty_scope)
    in
    add_item_to_env key value env []


  let add_item_no_replace key value env =
    match env with
    | inner_scope :: remaining_scope ->
      ((key, value) :: inner_scope) :: remaining_scope
    | [] -> raise Empty_scope


  (* let rec add_item_to_scope key = function
    | (k, v) :: ts ->
      if Key.compare k key = 0 then Some v else get_value_in_scope key ts
    | [] -> None


  let add_item key value = function
    | ts :: tss ->
      (match get_value_in_scope key ts with
      | None -> get_value key tss
      | Some value -> Some value)
    | [] -> None *)

  (* match t with
    | ts :: tss -> ((key, value) :: ts) :: tss
    | [] -> raise Empty_scope *)

  let add_new_scope ~is_func t =
    if Is_func_context.t && not is_func then t else [] :: t


  let remove_scope ~is_func t =
    if Is_func_context.t && not is_func
    then t
    else (
      match t with
      | _ :: xs -> xs
      | [] -> raise Empty_scope)


  let rec get_value_in_scope key = function
    | (k, v) :: ts ->
      if Key.compare k key = 0 then Some v else get_value_in_scope key ts
    | [] -> None


  let rec get_value key = function
    | ts :: tss ->
      (match get_value_in_scope key ts with
      | None -> get_value key tss
      | Some value -> Some value)
    | [] -> None


  let get_value_outside_scope key = function
    | ts :: tss ->
      (match get_value_in_scope key ts with
      | Some _ -> raise Key_in_inner_scope
      | None -> get_value key tss)
    | [] -> raise Empty_scope


  let apply_unary_fun_scope ~f = List.map ~f:(fun (key, value) -> key, f value)

  let apply_unary_fun ~f =
    List.map ~f:(fun scope -> apply_unary_fun_scope ~f scope)


  let rec apply_bin_fun_scope ~f scope1 scope2 =
    match scope1, scope2 with
    | (key1, value1) :: scope1, (key2, value2) :: scope2 ->
      if Key.compare key1 key2 = 0
      then (key1, f value1 value2) :: apply_bin_fun_scope ~f scope1 scope2
      else raise Different_shaped_envs
    | [], [] -> []
    | _ -> raise Different_shaped_envs


  let rec apply_bin_fun ~f t1 t2 =
    match t1, t2 with
    | scope1 :: t1, scope2 :: t2 ->
      apply_bin_fun_scope ~f scope1 scope2 :: apply_bin_fun ~f t1 t2
    | [], [] -> []
    | _ -> raise Different_shaped_envs


  let get_all_values =
    List.fold_left ~init:[] ~f:(fun acc scope ->
        acc @ List.map ~f:(fun (_, value) -> value) scope)


  let get_post_loop_context
      ~start_env
      ~end_env
      ~sum
      ~subtract
      ~multiply
      ~iterations
    =
    let delta_env = apply_bin_fun end_env start_env ~f:subtract in
    let delta_env = apply_unary_fun delta_env ~f:(multiply iterations) in
    apply_bin_fun start_env delta_env ~f:sum

  (* let delta_env =
      Type_cost_context.apply_bin_fun
        ~f:(Type_cost.apply_cost_bin_fun ~f:Cost.subtract)
        (Type_cost_context.remove_scope ~is_func:true end_env)
        (Type_cost_context.remove_scope ~is_func:true start_env)
    in
    let delta_env =
      Type_cost_context.apply_unary_fun
        ~f:(Type_cost.apply_cost_unary_fun ~f:(Cost.multiply iterations))
        delta_env
    in
    let x =
      Type_cost_context.apply_bin_fun
        ~f:(Type_cost.apply_cost_bin_fun ~f:Cost.sum)
        start_env
        (Type_cost_context.add_new_scope ~is_func:true delta_env)
    in
    (* print_endline (Type_cost_context.string_of_t x); *)
    x *)

  (* let delta_scope =
      match end_env with
      | end_scope :: _ ->
        List.fold_left
          ~init:[]
          ~f:(fun acc (key, end_value) ->
            match get_value key start_env with
            | Some start_value ->
              ( key
              , sum
                  (multiply (subtract end_value start_value) iterations)
                  start_value )
              :: acc
            | None -> acc)
          end_scope
      | _ -> raise Empty_scope
    in
    match start_env with
    | start_scope :: remaining_scope ->
      (delta_scope @ start_scope) :: remaining_scope
    | _ -> raise Empty_scope *)
end
