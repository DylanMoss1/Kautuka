val join_term
  :  join_term_if_matching:('a -> 'a -> 'a option)
  -> 'a
  -> 'a list
  -> 'a list

val all_pairs : 'a list -> 'b list -> ('a * 'b) list
val all_pairs_map : f:('a * 'a -> 'a) -> 'a list -> 'a list -> 'a list

val string_of_list
  :  ?sep:string
  -> ?left_format:string
  -> ?right_format:string
  -> string_of_item:('a -> string)
  -> 'a list
  -> string

module Integer_bound : sig
  type t = int * int [@@deriving of_sexp, sexp_of, compare]

  val zero : int * int
  val one : int * int
  val create : 'a -> 'a
  val sum : int * int -> int * int -> int * int
  val negate : int * int -> int * int
  val multiply : int * int -> int * int -> int * int
  val string_of_t : int * int -> string
  val union : int * int -> int * int -> int * int
  val zero_lower : 'a * 'b -> int * 'b
  val zero_upper : 'a * 'b -> 'a * int
end

module Variable_bound : sig
  type t = (Util.Alpha.t * int) list

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int

  exception Variable_not_in_map

  val string_of_item : Util.Alpha.t * int -> string
  val string_of_t : ?sep:string -> (Util.Alpha.t * int) list -> string
  val empty : 'a list

  val multiply_var_terms_if_matching
    :  Util.Alpha.t * int
    -> Util.Alpha.t * int
    -> (Util.Alpha.t * int) option

  val multiply_var_term
    :  Util.Alpha.t * int
    -> (Util.Alpha.t * int) list
    -> (Util.Alpha.t * int) list

  val reduce : (Util.Alpha.t * int) list -> (Util.Alpha.t * int) list

  val multiply
    :  (Util.Alpha.t * int) list
    -> (Util.Alpha.t * int) list
    -> (Util.Alpha.t * int) list

  val apply_map : (Util.Alpha.t * 'a) list -> Util.Alpha.t * 'b -> 'a * 'b
end

module Cost : sig
  type t = (Integer_bound.t * Variable_bound.t) list

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val compare : t -> t -> int

  exception Negative_power

  val string_of_item : (int * int) * (Util.Alpha.t * int) list -> string
  val string_of_t : ((int * int) * (Util.Alpha.t * int) list) list -> string

  val add_cost_terms_if_matching
    :  (int * int) * Variable_bound.t
    -> (int * int) * Variable_bound.t
    -> ((int * int) * Variable_bound.t) option

  val empty : 'a list
  val create_int_bound_cost : 'a -> ('a * 'b list) list
  val create_int_cost : 'a -> (('a * 'a) * 'b list) list
  val zero : ((int * int) * 'a list) list
  val one : ((int * int) * 'a list) list
  val default_input_bound : ((int * int) * 'a list) list

  val sum_reduce
    :  ((int * int) * Variable_bound.t) list
    -> ((int * int) * Variable_bound.t) list

  val sum
    :  ((int * int) * Variable_bound.t) list
    -> ((int * int) * Variable_bound.t) list
    -> ((int * int) * Variable_bound.t) list

  val negate : ((int * int) * 'a) list -> ((int * int) * 'a) list

  val subtract
    :  ((int * int) * Variable_bound.t) list
    -> ((int * int) * Variable_bound.t) list
    -> ((int * int) * Variable_bound.t) list

  val add_cost_term
    :  (int * int) * Variable_bound.t
    -> ((int * int) * Variable_bound.t) list
    -> ((int * int) * Variable_bound.t) list

  val multiply_cost_terms
    :  Integer_bound.t * Variable_bound.t
    -> Integer_bound.t * Variable_bound.t
    -> Integer_bound.t * Variable_bound.t

  val multiply : t -> t -> t
  val exponent : t -> int -> t

  val substitute_cost_term
    :  (Util.Alpha.t * t) list
    -> Integer_bound.t * Variable_bound.t
    -> t

  val substitute
    :  (Util.Alpha.t * t) list
    -> (Integer_bound.t * Variable_bound.t) list
    -> (Integer_bound.t * Variable_bound.t) list

  val union_cost_terms_if_matching
    :  (int * int) * Variable_bound.t
    -> (int * int) * Variable_bound.t
    -> ((int * int) * Variable_bound.t) option

  val union_cost_term
    :  (int * int) * Variable_bound.t
    -> ((int * int) * Variable_bound.t) list
    -> ((int * int) * Variable_bound.t) list

  val union_reduce
    :  ((int * int) * Variable_bound.t) list
    -> ((int * int) * Variable_bound.t) list

  val union
    :  ((int * int) * Variable_bound.t) list
    -> ((int * int) * Variable_bound.t) list
    -> ((int * int) * Variable_bound.t) list

  val union_of_list
    :  ((int * int) * Variable_bound.t) list list
    -> ((int * int) * Variable_bound.t) list

  val create_lower_upper_bounded_cost : lower:t -> upper:t -> t
end
