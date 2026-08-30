(*
   A cache for objects that vary in size or recomputation cost
*)

open Printf

module type Param = sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val show : t -> string
end

module type S = sig
  type key
  [@@deriving show]

  type 'v t

  val create :
    ?decay:float ->
    ?min_fill:float ->
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

module Make (H: Param): (S with type key = H.t) =
struct
  type key = H.t

  let show_key = H.show
  let pp_key fmt key = Format.pp_print_string fmt (show_key key)

  module Hashtbl = Hashtbl.Make (H)

  type 'v entry = {
    value: 'v;
    size: float;
    cost: float;
    mutable last_access: int;
    mutable exponential_moving_frequency: float;
  }

  (* 'decay' is a decay factor per clock tick, used to compute frequency as
    an exponential moving average.
    See https://en.wikipedia.org/wiki/Exponential_smoothing *)
  type 'v t = {
    (* cache parameters *)
    capacity: float;
    decay: float;
    min_fill: float;
    (* mutable state *)
    mutable fill: float;
    clock: int ref;
    entries: 'v entry Hashtbl.t;
  }

  let get_frequency cache (e : _ entry) =
    let dt = float (!(cache.clock) - e.last_access) in
    (cache.decay ** dt) *. e.exponential_moving_frequency

  let get_priority cache (e : _ entry) =
    (get_frequency cache e /. e.size) *. (e.cost /. e.size)

  let access cache (e : _ entry) =
    let now = !(cache.clock) in
    let dt = float (now - e.last_access) in
    if dt > 0. then (
      let emf = e.exponential_moving_frequency in
      let decay = cache.decay in
      e.last_access <- now;
      e.exponential_moving_frequency <- (1. -. decay) +. (decay ** dt) *. emf;
    )

  let create_shared
      ?(decay = 0.9)
      ?(min_fill = 0.7)
      ~clock
      capacity : _ t =
    if not (capacity > 0. && Float.is_finite capacity) then
      ksprintf failwith
        "Cache.create: invalid capacity: %g"
        capacity;
    if not (decay > 0. && decay < 1.) then
      ksprintf failwith
        "Cache.create: invalid decay: %g"
        decay;
    if not (min_fill > 0. && min_fill < 1.) then
      ksprintf failwith
        "Cache.create: invalid min_fill: %g"
        min_fill;
    {
      capacity;
      decay;
      min_fill;
      fill = 0.;
      clock;
      entries = Hashtbl.create 100;
    }

  let create ?decay ?min_fill capacity =
    create_shared ?decay ?min_fill ~clock:(ref 0) capacity

  let clear cache =
    cache.fill <- 0.;
    cache.clock := 0;
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
    Hashtbl.fold (fun k e acc ->
        let priority = get_priority cache e in
        (priority, k, e) :: acc
      ) cache.entries []
    |> fast_sort (fun (p1, _, _) (p2, _, _) -> Float.compare p1 p2)
    |> remove_bottom_entries [] cache

  let check cache =
    if cache.fill >= 1. then
      run_collection cache
    else
      []

  let access_entry cache e =
    let clock = cache.clock in
    if !clock = max_int then
      ksprintf failwith "Cache clock overflow: %d" !clock;
    incr clock;
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

  let put_full ?cost ?(size = 1.) cache k v =
    let cost = Option.value cost ~default:size in
    if not (size > 0.) then
      ksprintf invalid_arg "Cache.put: invalid size value: %g" size;
    if size > cache.capacity then
      ksprintf invalid_arg
        "Cache.put: entry size exceeds cache capacity: %g > %g"
        size cache.capacity;
    if not (cost > 0. && Float.is_finite cost) then
      ksprintf invalid_arg "Cache.put: invalid cost value: %g" cost;
    remove cache k;
    (* Guess an average initial value for the frequency:
       assume the entry is hit proportionally to the size it occupies in the
       cache. *)
    let initial_frequency = size /. cache.capacity in
    let e = {
      value = v;
      size;
      cost;
      last_access = !(cache.clock);
      exponential_moving_frequency = initial_frequency;
    } in
    cache.fill <- cache.fill +. (size /. cache.capacity);
    Hashtbl.add cache.entries k e;
    access_entry cache e;
    check cache

  let put ?cost ?size cache k v =
    put_full ?cost ?size cache k v |> ignore

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
    decay: float;
    min_fill: float;
    fill: float;
    clock: int;
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
      decay = cache.decay;
      min_fill = cache.min_fill;
      fill = cache.fill;
      clock = !(cache.clock);
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
