open! Core

exception EmptyScope

type ('a, 'b) t

val empty : ('a, 'b) t
val add_new_item : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
val add_new_scope : ('a, 'b) t -> ('a, 'b) t
val remove_scope : ('a, 'b) t -> ('a, 'b) t

val get_value_in_scope
  :  equal: ('a -> 'a -> bool)
  -> 'a
  -> ('a, 'b) t
  -> 'b option

val string_of_t : string_of_key: ('a -> string) -> string_of_val: ('b -> string) -> ('a, 'b) t -> string