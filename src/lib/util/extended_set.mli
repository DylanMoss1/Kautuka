open! Core
open Item

module type Extended_set = sig
  module Elt : sig
    type t

    val t_of_sexp : Sexplib0.Sexp.t -> t
    val sexp_of_t : t -> Sexplib0.Sexp.t

    type comparator_witness

    val comparator : (t, comparator_witness) Core.Comparator.comparator
  end

  type t = (Elt.t, Elt.comparator_witness) Base.Set.t

  val compare : t Base.Exported_for_specific_uses.Ppx_compare_lib.compare

  type named = (Elt.t, Elt.comparator_witness) Base.Set.Named.t

  val length : t -> int
  val is_empty : t -> bool
  val iter : t -> f:(Elt.t -> unit) -> unit
  val fold : t -> init:'accum -> f:('accum -> Elt.t -> 'accum) -> 'accum

  val fold_result
    :  t
    -> init:'accum
    -> f:('accum -> Elt.t -> ('accum, 'e) Base.Result.t)
    -> ('accum, 'e) Base.Result.t

  val exists : t -> f:(Elt.t -> bool) -> bool
  val for_all : t -> f:(Elt.t -> bool) -> bool
  val count : t -> f:(Elt.t -> bool) -> int

  val sum
    :  (module Base__Container_intf.Summable with type t = 'sum)
    -> t
    -> f:(Elt.t -> 'sum)
    -> 'sum

  val find : t -> f:(Elt.t -> bool) -> Elt.t option
  val find_map : t -> f:(Elt.t -> 'a option) -> 'a option
  val to_list : t -> Elt.t list
  val to_array : t -> Elt.t array
  val invariants : t -> bool
  val mem : t -> Elt.t -> bool
  val add : t -> Elt.t -> t
  val remove : t -> Elt.t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val symmetric_diff : t -> t -> (Elt.t, Elt.t) Base.Either.t Base.Sequence.t
  val compare_direct : t -> t -> int
  val equal : t -> t -> bool
  val is_subset : t -> of_:t -> bool
  val are_disjoint : t -> t -> bool

  module Named : sig
    val is_subset : named -> of_:named -> unit Base.Or_error.t
    val equal : named -> named -> unit Base.Or_error.t
  end

  val fold_until
    :  t
    -> init:'b
    -> f:('b -> Elt.t -> ('b, 'final) Base.Container.Continue_or_stop.t)
    -> finish:('b -> 'final)
    -> 'final

  val fold_right : t -> init:'b -> f:(Elt.t -> 'b -> 'b) -> 'b

  val iter2
    :  t
    -> t
    -> f:([ `Both of Elt.t * Elt.t | `Left of Elt.t | `Right of Elt.t ] -> unit)
    -> unit

  val filter : t -> f:(Elt.t -> bool) -> t
  val partition_tf : t -> f:(Elt.t -> bool) -> t * t
  val elements : t -> Elt.t list
  val min_elt : t -> Elt.t option
  val min_elt_exn : t -> Elt.t
  val max_elt : t -> Elt.t option
  val max_elt_exn : t -> Elt.t
  val choose : t -> Elt.t option
  val choose_exn : t -> Elt.t
  val split : t -> Elt.t -> t * Elt.t option * t
  val group_by : t -> equiv:(Elt.t -> Elt.t -> bool) -> t list
  val find_exn : t -> f:(Elt.t -> bool) -> Elt.t
  val nth : t -> int -> Elt.t option
  val remove_index : t -> int -> t
  val to_tree : t -> (Elt.t, Elt.comparator_witness) Core.Set_intf.Tree.t

  val to_sequence
    :  ?order:[ `Decreasing | `Increasing ]
    -> ?greater_or_equal_to:Elt.t
    -> ?less_or_equal_to:Elt.t
    -> t
    -> Elt.t Base.Sequence.t

  val binary_search
    :  t
    -> compare:(Elt.t -> 'key -> int)
    -> Base.Binary_searchable.Which_target_by_key.t
    -> 'key
    -> Elt.t option

  val binary_search_segmented
    :  t
    -> segment_of:(Elt.t -> [ `Left | `Right ])
    -> Base.Binary_searchable.Which_target_by_segment.t
    -> Elt.t option

  val merge_to_sequence
    :  ?order:[ `Decreasing | `Increasing ]
    -> ?greater_or_equal_to:Elt.t
    -> ?less_or_equal_to:Elt.t
    -> t
    -> t
    -> (Elt.t, Elt.t) Base__Set_intf.Merge_to_sequence_element.t Base.Sequence.t

  val to_map
    :  t
    -> f:(Elt.t -> 'data)
    -> (Elt.t, 'data, Elt.comparator_witness) Base.Map.t

  val quickcheck_observer
    :  Elt.t Core.Quickcheck.Observer.t
    -> t Core.Quickcheck.Observer.t

  val quickcheck_shrinker
    :  Elt.t Core.Quickcheck.Shrinker.t
    -> t Core.Quickcheck.Shrinker.t

  val empty : t
  val singleton : Elt.t -> t
  val union_list : t list -> t
  val of_list : Elt.t list -> t
  val of_sequence : Elt.t Base.Sequence.t -> t
  val of_array : Elt.t array -> t
  val of_sorted_array : Elt.t array -> t Base.Or_error.t
  val of_sorted_array_unchecked : Elt.t array -> t
  val of_increasing_iterator_unchecked : len:int -> f:(int -> Elt.t) -> t
  val stable_dedup_list : Elt.t list -> Elt.t list
  val map : ('a, 'b) Base.Set.t -> f:('a -> Elt.t) -> t
  val filter_map : ('a, 'b) Base.Set.t -> f:('a -> Elt.t option) -> t
  val of_tree : (Elt.t, Elt.comparator_witness) Core.Set_intf.Tree.t -> t
  val of_hash_set : Elt.t Core.Hash_set.t -> t
  val of_hashtbl_keys : (Elt.t, 'a) Core.Hashtbl.t -> t
  val of_map_keys : (Elt.t, 'a, Elt.comparator_witness) Base.Map.t -> t

  val quickcheck_generator
    :  Elt.t Core.Quickcheck.Generator.t
    -> t Core.Quickcheck.Generator.t

  module Provide_of_sexp : functor
    (Elt : sig
       val t_of_sexp : Sexplib0.Sexp.t -> Elt.t
     end)
    -> sig
    val t_of_sexp : Sexplib0.Sexp.t -> t
  end

  module Provide_bin_io : functor
    (Elt : sig
       val bin_size_t : Elt.t Bin_prot.Size.sizer
       val bin_write_t : Elt.t Bin_prot.Write.writer
       val bin_read_t : Elt.t Bin_prot.Read.reader
       val __bin_read_t__ : (int -> Elt.t) Bin_prot.Read.reader
       val bin_shape_t : Bin_prot.Shape.t
       val bin_writer_t : Elt.t Bin_prot.Type_class.writer
       val bin_reader_t : Elt.t Bin_prot.Type_class.reader
       val bin_t : Elt.t Bin_prot.Type_class.t
     end)
    -> sig
    val bin_size_t : t Bin_prot.Size.sizer
    val bin_write_t : t Bin_prot.Write.writer
    val bin_read_t : t Bin_prot.Read.reader
    val __bin_read_t__ : (int -> t) Bin_prot.Read.reader
    val bin_shape_t : Bin_prot.Shape.t
    val bin_writer_t : t Bin_prot.Type_class.writer
    val bin_reader_t : t Bin_prot.Type_class.reader
    val bin_t : t Bin_prot.Type_class.t
  end

  module Provide_hash : functor
    (Elt : sig
       val hash_fold_t : Base.Hash.state -> Elt.t -> Base.Hash.state
     end)
    -> sig
    val hash_fold_t : t Base.Exported_for_specific_uses.Ppx_hash_lib.hash_fold

    val hash
      :  t
      -> Base.Exported_for_specific_uses.Ppx_hash_lib.Std.Hash.hash_value
  end

  val t_of_sexp : Sexplib0.Sexp.t -> t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val create : Elt.t -> t
  val union_of_list : t list -> t
  val string_of_t : t -> string
  val join : t -> t -> t
  val join_list : t list -> t
end

module Make_extended_set (I : Item) : Extended_set with type Elt.t = I.t
