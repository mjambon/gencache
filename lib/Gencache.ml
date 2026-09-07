(*
   A cache for objects that vary in size or recomputation cost
*)

open Printf

let default_size = 1.

module Clock : sig
  type t = private {
    capacity: float; (** amount of data accessed during one cycle *)
    mutable time: float; (** time in number of cycles *)
  }

  val create : float -> t

  (** Increment the clock by a fraction of a cycle given as the
      size of a cache entry. *)
  val tick : t -> float -> unit
end
=
struct
  type t = {
    capacity: float;
    mutable time: float;
  }

  let create capacity = {
    capacity;
    time = 0.
  }

  let tick clock entry_size =
    clock.time <- clock.time +. entry_size /. clock.capacity
end

module type Param = sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val show : t -> string
end

module type Cache = sig
  type key
  [@@deriving show]

  type 'v t

  val create :
    ?decay:float ->
    ?major_share:float ->
    ?min_fill:float ->
    ?min_reuses_for_promotion:int ->
    float -> 'v t

  val get : 'v t -> key -> 'v option

  val put :
    ?cost:float ->
    ?size:float ->
    'v t -> key -> 'v -> unit

  val put_full :
    ?cost:float ->
    ?size:float ->
    'v t -> key -> 'v -> (key * 'v) list

  val mem : 'v t -> key -> bool
  val remove : 'v t -> key -> unit
  val clear : 'v t -> unit
  val to_list : 'v t -> (key * 'v) list

  type stats
  [@@deriving show]

  type short_stats
  [@@deriving show]

  val stats : 'v t -> stats
  val short_stats : 'v t -> short_stats
end

module Make_naive (Param: Param) =
struct
  type key = Param.t

  let show_key = Param.show
  let pp_key fmt key = Format.pp_print_string fmt (show_key key)

  module Hashtbl = Hashtbl.Make (Param)

  type 'v entry = {
    value: 'v;
    size: float;
    cost: float;
    mutable access_count: int; (* includes the 'put' *)
    mutable last_access: float;
    mutable exponential_moving_frequency: float;
  }

  (* 'decay' is a decay factor over 1 cycle, used to compute frequency as
    an exponential moving average.
    See https://en.wikipedia.org/wiki/Exponential_smoothing *)
  type 'v t = {
    (* cache parameters *)
    capacity: float;
    decay: float;
    min_fill: float;
    (* mutable state *)
    mutable initializing: bool; (* turns false at the first collection *)
    mutable fill: float;
    clock: Clock.t;
    entries: 'v entry Hashtbl.t;
  }

  let get_frequency cache (e : _ entry) =
    let dt = cache.clock.time -. e.last_access in
    (cache.decay ** dt) *. e.exponential_moving_frequency

  let get_priority cache (e : _ entry) =
    (get_frequency cache e /. e.size) *. (e.cost /. e.size)

  let access cache (e : _ entry) =
    let now = cache.clock.time in
    let dt = now -. e.last_access in
    if dt > 0. then (
      e.access_count <- e.access_count + 1;
      let emf = e.exponential_moving_frequency in
      let decay = cache.decay in
      e.last_access <- now;
      e.exponential_moving_frequency <- (1. -. decay) +. (decay ** dt) *. emf;
    )

  let create_shared
      ?(decay = 0.9)
      ?major_share
      ?(min_fill = 0.7)
      ~clock
      capacity : _ t =
    (match major_share with
    | None -> ()
    | Some _ ->
        ksprintf invalid_arg
        "Gencache create: meaningless major_share parameter; do not use"
        capacity
    );
    if not (capacity > 0. && Float.is_finite capacity) then
      ksprintf invalid_arg
        "Gencache create: invalid capacity: %g"
        capacity;
    if not (decay > 0. && decay < 1.) then
      ksprintf invalid_arg
        "Gencache create: invalid decay: %g"
        decay;
    if not (min_fill > 0. && min_fill < 1.) then
      ksprintf invalid_arg
        "Gencache create: invalid min_fill: %g"
        min_fill;
    {
      capacity;
      decay;
      min_fill;
      fill = 0.;
      initializing = true;
      clock;
      entries = Hashtbl.create 100;
    }

  let create ?decay ?major_share ?min_fill ?min_reuses_for_promotion capacity =
    (match min_reuses_for_promotion with
     | None -> ()
     | Some _ ->
         ksprintf invalid_arg
           "Gencache create: the option 'min_reuses_for_promotion' \
            is ignored for a naive cache"
    );
    create_shared ?decay ?major_share ?min_fill
      ~clock:(Clock.create capacity) capacity

  let initialization_free_space cache =
    if cache.initializing then
      Some ((1. -. cache.fill) *. cache.capacity)
    else
      None

  let clear cache =
    cache.fill <- 0.;
    Hashtbl.clear cache.entries

  (* The entry must exist in the table to not screw up fill ratio tracking *)
  let remove_entry cache k e =
    cache.fill <- max 0. (cache.fill -. (e.size /. cache.capacity));
    Hashtbl.remove cache.entries k

  let remove cache k =
    match Hashtbl.find_opt cache.entries k with
    | None -> ()
    | Some e ->
        remove_entry cache k e

  let rec remove_bottom_entries acc cache xs =
    match xs with
    | [] -> List.rev acc
    | (_prio, k, e) :: xs ->
        if cache.fill > cache.min_fill then (
          remove_entry cache k e;
          remove_bottom_entries ((k, e.value) :: acc) cache xs
        )
        else
          List.rev acc

  let fast_sort cmp xs =
    let ar = Array.of_list xs in
    Array.fast_sort cmp ar;
    Array.to_list ar

  (* Obtain the priority for each cache entry, sort by increasing score,
     and remove bottom-scoring entries until the cache occupancy
     reaches the min_fill threshold. *)
  let run_collection cache =
    cache.initializing <- false;
    Hashtbl.fold (fun k e acc ->
        let priority = get_priority cache e in
        (priority, k, e) :: acc
      ) cache.entries []
    |> fast_sort (fun (p1, _, _) (p2, _, _) -> Float.compare p1 p2)
    |> remove_bottom_entries [] cache

  let is_promotable ~min_reuses_for_promotion e =
    e.access_count >= 1 + min_reuses_for_promotion

  let remove_promotable_entries ~min_reuses_for_promotion cache =
    let removed_entries =
      Hashtbl.fold (fun k e acc ->
        if is_promotable ~min_reuses_for_promotion e then
          (k, e) :: acc
        else
          acc
      ) cache.entries []
    in
    List.iter (fun (k, _) -> remove cache k) removed_entries;
    removed_entries

  let check ?promote cache =
    if cache.fill >= 1. then (
      let major_evictions =
        match promote with
        | None -> []
        | Some promote -> promote ()
      in
      let evictions = run_collection cache in
      List.rev_append major_evictions evictions
    )
    else
      []

  let access_entry cache e =
    let clock = cache.clock in
    Clock.tick clock e.size;
    access cache e

  let get cache k =
    match Hashtbl.find_opt cache.entries k with
    | None -> None
    | Some e ->
        access_entry cache e;
        Some e.value

  (* We don't want to count this an access *)
  let mem cache k =
    Hashtbl.mem cache.entries k

  let put_entry ?promote ~count_access cache k e =
    cache.fill <- cache.fill +. (e.size /. cache.capacity);
    Hashtbl.add cache.entries k e;
    if count_access then
      access_entry cache e;
    check ?promote cache

  let put_internal
      ?cost
      ?promote
      ?(size = default_size)
      cache k v =
    let cost = Option.value cost ~default:size in
    if not (size > 0.) then
      ksprintf invalid_arg "Gencache put: invalid size value: %g" size;
    if size > cache.capacity then
      ksprintf invalid_arg
        "Cache.put: entry size exceeds cache capacity: %g > %g"
        size cache.capacity;
    if not (cost > 0. && Float.is_finite cost) then
      ksprintf invalid_arg "Gencache put: invalid cost value: %g" cost;
    remove cache k;
    (* Guess an average initial value for the frequency:
       assume the entry is hit proportionally to the size it occupies in the
       cache. The access frequency of a hypothetical entry that fills up
       the cache is 1. *)
    let initial_frequency = size /. cache.capacity in
    let e = {
      value = v;
      size;
      cost;
      access_count = 0;
      last_access = cache.clock.time;
      exponential_moving_frequency = initial_frequency;
    } in
    put_entry ?promote ~count_access:true cache k e

   let put_full ?cost ?size cache k v =
     put_internal ?cost ?size cache k v

   let put ?cost ?size cache k v =
    put_full ?cost ?size cache k v |> ignore

  (*
     Used to receive a promoted entry from the minor cache
     into the major cache. The entry is imported as-is.

     It's important that the frequency be preserved.
     Unlike a regular 'put', this doesn't count as an access.
  *)
  let import_promoted_entry cache k e =
    put_entry ~count_access:false cache k e

  let to_list cache =
    Hashtbl.fold (fun k e acc -> (k, e.value) :: acc) cache.entries []

  type single_entry_stats = {
    size: float;
    cost: float;
    frequency: float;
    priority: float;
  }
  [@@deriving show { with_path = false }]

  type short_stats = {
    capacity: float; (* maximum space that could be occupied by the entries *)
    occupancy: float; (* space occupied by the entries *)
    decay: float;
    min_fill: float;
    fill: float;
    time: float;
    num_entries: int;
  }
  [@@deriving show { with_path = false }]

  type entry_stats = (key * single_entry_stats) list
  [@@deriving show]

  type stats = {
    short_stats: short_stats;
    entry_stats: entry_stats;
  }
  [@@deriving show { with_path = false }]

  let short_stats (cache : _ t) : short_stats =
    {
      capacity = cache.capacity;
      occupancy = cache.fill *. cache.capacity;
      decay = cache.decay;
      min_fill = cache.min_fill;
      fill = cache.fill;
      time = cache.clock.time;
      num_entries = Hashtbl.length cache.entries;
    }

  let single_entry_stats cache (e : _ entry) : single_entry_stats =
    {
      size = e.size;
      cost = e.cost;
      frequency = get_frequency cache e;
      priority = get_priority cache e;
    }

  let entry_stats (cache : _ t) =
    Hashtbl.fold (fun k e acc ->
      (k, single_entry_stats cache e) :: acc)
      cache.entries []
    |> fast_sort (fun (_, a) (_, b) -> Float.compare b.priority a.priority)

  let stats cache =
    {
      short_stats = short_stats cache;
      entry_stats = entry_stats cache;
    }
end

(* Two-generation cache

   minor cache: holds new entries for which the access frequency is not
   known reliably yet
   major cache: holds entries for which the access frequency is known
*)
module Make (Param: Param): (Cache with type key = Param.t) = struct
  (* Module implementing a minor or major cache *)
  module Subcache = Make_naive (Param)

  type key = Subcache.key
  [@@deriving show]

  type 'v t = {
    total_capacity: float;
    major_share: float;
    min_reuses_for_promotion: int;
    minor: 'v Subcache.t;
    major: 'v Subcache.t;
  }

  let create
      ?decay ?(major_share = 0.6) ?min_fill
      ?(min_reuses_for_promotion = 2) total_capacity =
    if not (major_share > 0. && major_share < 1.) then
      ksprintf invalid_arg
        "Gencache create: invalid major_share: %g" major_share;
     if not (min_reuses_for_promotion >= 0) then
      ksprintf invalid_arg
        "Gencache create: invalid min_reuses_for_promotion: %d"
        min_reuses_for_promotion;
    let major_capacity = major_share *. total_capacity in
    let minor_capacity = (1. -. major_share) *. total_capacity in
    let clock = Clock.create total_capacity in
    {
      total_capacity;
      major_share;
      min_reuses_for_promotion;
      minor = Subcache.create_shared ?decay ?min_fill ~clock minor_capacity;
      major = Subcache.create_shared ?decay ?min_fill ~clock major_capacity;
    }

  let get cache k =
    match Subcache.get cache.major k with
    | None -> Subcache.get cache.minor k
    | some -> some

  (*
     This function is called just before a minor collection.
     It transfers suitable entries from the minor cache to the major cache.
  *)
  let promote cache () =
    let entries =
      Subcache.remove_promotable_entries
        ~min_reuses_for_promotion:cache.min_reuses_for_promotion
        cache.minor
    in
    let evicted =
      List.fold_left (fun acc (k, entry) ->
        List.rev_append
          (Subcache.import_promoted_entry cache.major k entry)
          acc
      ) [] entries
    in
    evicted

  (*
     Add an entry to the minor cache. It it already exists in the major
     cache, it must be removed.

     During the initialization phase however, i.e. when the major cache hasn't
     filled up yet, entries are added directly to the major cache.
  *)
  let put_full ?cost ?(size = default_size) cache k v =
    Subcache.remove cache.major k;
    match Subcache.initialization_free_space cache.major with
    | Some available_space when size < 0.99 *. available_space ->
        let evicted = Subcache.put_full ?cost ~size cache.major k v in
        assert (evicted = []);
        []
    | _ ->
        (* Return evicted entries:
           - from the major cache due to promotions;
           - from the minor cache *)
        Subcache.put_internal
          ?cost ~size ~promote:(promote cache) cache.minor k v

  let put ?cost ?size cache k v =
    put_full ?cost ?size cache k v |> ignore

  let mem cache k =
    Subcache.mem cache.major k || Subcache.mem cache.minor k

  let remove cache k =
    Subcache.remove cache.major k;
    Subcache.remove cache.minor k

  let clear cache =
    Subcache.clear cache.major;
    Subcache.clear cache.minor

  let to_list cache =
    List.rev_append
      (Subcache.to_list cache.minor) (Subcache.to_list cache.major)

  type short_stats = {
    total_capacity: float;
    occupancy: float;
    minor_share: float;
    major_share: float;
    min_reuses_for_promotion: int;
    minor: Subcache.short_stats;
    major: Subcache.short_stats;
  }
  [@@deriving show { with_path = false }]

  type stats = {
    total_capacity: float;
    occupancy: float;
    minor_share: float;
    major_share: float;
    min_reuses_for_promotion: int;
    minor: Subcache.stats;
    major: Subcache.stats;
  }
  [@@deriving show { with_path = false }]

  let short_stats (cache : _ t) : short_stats =
    let minor = Subcache.short_stats cache.minor in
    let major = Subcache.short_stats cache.major in
    {
      total_capacity = cache.total_capacity;
      occupancy = minor.occupancy +. major.occupancy;
      minor_share = 1. -. cache.major_share;
      major_share = cache.major_share;
      min_reuses_for_promotion = cache.min_reuses_for_promotion;
      minor;
      major;
    }

  let stats (cache : _ t) : stats =
    let minor = Subcache.stats cache.minor in
    let major = Subcache.stats cache.major in
    {
      total_capacity = cache.total_capacity;
      occupancy = minor.short_stats.occupancy +. major.short_stats.occupancy;
      minor_share = 1. -. cache.major_share;
      major_share = cache.major_share;
      min_reuses_for_promotion = cache.min_reuses_for_promotion;
      minor;
      major;
    }
end
