(**
   A cache implementation with an eviction policy based on entry size,
   recomputation cost, and recent access frequency.

   Each entry has a priority for retention given by:

       (frequency / size) * (cost / size)

   where [frequency] reflects recent access activity, [size] is the
   amount of cache capacity consumed by the entry, and [cost] reflects
   the cost of recomputing the entry.

   Higher-priority entries are retained preferentially.

   The cache is automatically shrunk by a fixed percentage when its
   capacity is reached. This operation is called a "collection".
   A collection runs in O(n log n), where n is the number of entries
   in the cache.
*)

module type Cache = sig
  type key
  [@@deriving show]

  (** A cache is an in-memory key-value store where some bindings can be
      evicted at any time to make room for newer entries. *)
  type 'v t

  (** Create a cache of size capacity 1.0.

      @param decay is the decay factor
      (1 - α where α is the so-called smoothing factor) used to compute
      recent access frequencies of cache entries as exponential moving
      averages (EMA).
      At each clock tick triggered by a cache access,
      the previous access frequency of an entry is
      multiplied by [decay]. For the entry being accessed,
      [1 - decay] is added to form the entry's updated access frequency.
      The default value of [decay] is 0.9. It must be positive and less
      than 1.0. A value closer to 1 increases the "window" used to
      compute the recent access frequency of a cache entry, causing
      the cache to react more slowly to changes in access patterns.
      Conversely, a smaller value shrinks this window, giving more
      weight to more recent accesses.

      @param min_fill is the target cache occupancy after a collection pass.
      The default is 0.8. It must be positive and less than 1.

      @raise Invalid_argument on invalid parameters
  *)
  val create :
    ?decay:float ->
    ?min_fill:float ->
    float -> 'v t

  (** Get an element from the cache. It may not exist even if it was added
      previously.

      Each [get] call marks the entry as "used" and increments the cache's
      clock used to compute the recent usage frequency of all entries
      during a collection.

      A [get] operation costs O(1) under the usual assumptions.
  *)
  val get : 'v t -> key -> 'v option

  (** Put or replace an element into the cache with the specified
      size and recomputation cost.

      If a binding already exists, it is replaced with a fresh entry that
      resets size, boost, and access stats.

      If the cache is full as a result, a collection is triggered,
      costing O(n log n) where n is the number of entries.
      A collection shrinks a full cache down to a fixed percentage
      ([min_fill]). As a result, the amortized cost of a [put]
      is O(log n).

      When the cache is not full, the cost of adding an element is constant.

      @param cost is the estimated recomputation cost of the value
      expressed in arbitrary units. It defaults to [size].
      A common alternative is to set it to a constant regardless of [size].

      @param size is the entry's estimated size relative to the cache's
      capacity. It may not exceed 1.0. For example, a value of 0.01 indicates
      that the entry occupies 1% of the cache's capacity. The declared size
      is only a number that should reflect the cost of keeping the object
      in memory, not necessarily just the number of bytes used.
      Keep in mind that long-lived, highly-fragmented objects may incur
      significant garbage collector scanning costs.

      @raise Invalid_argument on an invalid boost or size
  *)
  val put :
    ?cost:float ->
    ?size:float ->
    'v t -> key -> 'v -> unit

  (** Same as [put] but return the list of freshly evicted entries if any. *)
  val put_full :
    ?cost:float ->
    ?size:float ->
    'v t -> key -> 'v -> (key * 'v) list

  (** Check whether an entry is in the cache without counting it as
      a use of the associated value. *)
  val mem : 'v t -> key -> bool

  (** Remove an entry from the cache if it exists. *)
  val remove : 'v t -> key -> unit

  (** Clear the whole cache. *)
  val clear : 'v t -> unit

  (** Export the entries to a list. *)
  val to_list : 'v t -> (key * 'v) list

  type stats
  [@@deriving show]

  type short_stats
  [@@deriving show]

  (** Export internal statistics about global cache activity and
      invidual entries.
      Subject to change. *)
  val stats : 'v t -> stats

  (** Smaller dump than {!stats}.
      Subject to change. *)
  val short_stats : 'v t -> short_stats
end

(** Parameters needed for the functor application.
    The requirements for the [hash] and [equal] functions are the same
    as for the standard [Hashtbl.Make] module. *)
module type Param = sig
  (** The type of cache key *)
  type t

  (** Here, [Hashtbl.hash] will work on simple keys. *)
  val hash : t -> int

  (** Here, [(=)] will work on simple keys. *)
  val equal : t -> t -> bool

  (** This is for printing purposes, not for core cache functionality. *)
  val show : t -> string
end

(** Functor needed to create a usable cache module.
    It is used similarly to the [Hashtbl.Make] functor of the standard
    library. *)
module Make (P : Param) : Cache with type key = P.t
