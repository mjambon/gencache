(*
   Test the gencache library
*)

open Printf

module Naive_cache =
  Gencache.Make_naive (struct
    include Int
    let show = Int.to_string
  end)

module Cache =
  Gencache.Make (struct
    include Int
    let show = Int.to_string
  end)

module type Cache = module type of Cache

type benchmark = {
  capacity: float;
  traffic: (int * float * float) list; (* (key, size, cost) *)
  live: int list; (* all of these keys must exist in the cache when done *)
  dead: int list; (* none of these keys must exist in the cache when done *)
  cost: float; (* expected cost of all value computations (cache misses) *)
  savings: float; (* expected savings on recomputations (cache hits) *)
}

let repeat = {
  capacity = 5.;
  traffic = [
    (1, 1., 1.);
    (1, 1., 1.);
    (1, 1., 1.);
    (1, 1., 1.);
    (1, 1., 1.);
    (1, 1., 1.);
    (1, 1., 1.);
  ];
  live = [1];
  dead = [];
  cost = 1.;
  savings = 6.;
}

let never_repeat = {
  capacity = 6.;
  traffic = [
    (1, 1., 1.);
    (2, 1., 1.);
    (3, 1., 1.);
    (4, 1., 1.);
    (5, 1., 1.);
    (6, 1., 1.);
    (7, 1., 1.);
  ];
  live = [6; 7];
  dead = [];
  cost = 7.;
  savings = 0.;
}

let never_repeat_naive = {
  never_repeat with
  live = [5; 6; 7];
  dead = [1; 2; 3];
}

let reuse = {
  capacity = 6.;
  traffic = [
    (* fill the major cache (initialization phase) *)
    (1, 1., 1.);
    (2, 1., 1.);
    (3, 1., 1.);
    (* add '4' to the minor cache enough times to allow a promotion *)
    (4, 1., 1.);
    (4, 1., 1.);
    (4, 1., 1.);
    (* fill the minor cache to trigger a promotion for '4' *)
    (5, 1., 1.);
    (6, 1., 1.);
    (7, 1., 1.);
    (8, 1., 1.);
    (9, 1., 1.);
    (10, 1., 1.);
    (11, 1., 1.);
  ];
  live = [4];
  dead = [5];
  cost = 11.;
  savings = 2.;
}

let reuse_naive = {
  reuse with
  live = [4];
  dead = [3; 5];
}

let medium = {
  capacity = 20.;
  traffic = [
    (1, 1., 1.);
    (2, 1., 1.);
    (3, 1., 1.);
    (4, 1., 1.);
    (5, 1., 1.);
    (6, 1., 1.);
    (7, 1., 1.);
    (8, 1., 1.);
    (9, 1., 1.);
    (10, 1., 1.);
    (11, 1., 1.);
    (12, 1., 1.);
    (13, 1., 1.);
    (14, 1., 1.);
    (15, 1., 1.);
    (16, 1., 1.);
    (17, 1., 1.);
    (18, 1., 1.);
    (19, 1., 1.);
    (20, 1., 1.);
    (* at this point, we're past the initial filling of the major cache *)
    (* promote 3 entries to the major cache *)
    (21, 1., 1.);
    (21, 1., 1.);
    (21, 1., 1.);
    (***********)
    (22, 1., 1.);
    (22, 1., 1.);
    (22, 1., 1.);
    (***********)
    (23, 1., 1.);
    (23, 1., 1.);
    (23, 1., 1.);
    (***********)
    (24, 1., 1.);
    (25, 1., 1.);
    (26, 1., 1.);
    (27, 1., 1.);
    (28, 1., 1.);
    (29, 1., 1.);
    (30, 1., 1.);
  ] @
    (* fill the minor cache many times without affecting the major cache *)
    List.init 970 (fun i -> (31 + i, 1., 1.))
    @ [
      (* find our old entries in the major cache *)
      (21, 1., 1.);
      (22, 1., 1.);
      (23, 1., 1.);
    ];
  live = [21; 22; 23];
  dead = [24; 900];
  cost = 1000.;
  savings = 9.;
}

(* With a non-generational cache, the old cached entries are evicted,
   incurring extra recomputation costs. *)
let medium_naive = {
  medium with
  cost = 1003.;
  savings = 6.;
}

(*
   A benchmark is a test that creates a cache and performs get/put lookups
   in a predefined sequence. The evaluation criterion is the total
   recomputation cost which we try to minimize under various traffic patterns.

   Each run logs data needed for studying the behavior of the cache
   and for troubleshooting.

   Input format: (key, size, cost)
*)
let run_benchmark cache_impl (ben : benchmark) () =
  let module Cache = (val cache_impl : Cache) in
  let cache = Cache.create ben.capacity in
  let hit_savings = ref 0. in
  let miss_cost = ref 0. in
  let get_put (k, size, cost) =
    match Cache.get cache k with
    | None ->
        printf "miss %i\n" k;
        miss_cost := !miss_cost +. cost;
        let evictions = Cache.put_full cache k ~size ~cost () in
        List.iter (fun (k, ()) ->
          printf "evict %i\n" k;
        ) evictions
    | Some () ->
        printf "hit %i\n" k;
        hit_savings := !hit_savings +. cost
  in
  List.iter get_put ben.traffic;
  printf "Cache stats:\n";
  print_endline (Cache.show_stats (Cache.stats cache));
  printf "Recomputation cost: %g\n" !miss_cost;
  printf "Recomputation savings: %g\n" !hit_savings;
  List.iter (fun k ->
    if not (Cache.mem cache k) then
      Testo.fail (sprintf "key missing from the cache: %i\n" k)
  ) ben.live;
  List.iter (fun k ->
    if Cache.mem cache k then
      Testo.fail (sprintf "key should not be in cache: %i\n" k)
  ) ben.dead;
  Testo.(check float) ~msg:"recomputation cost" ben.cost !miss_cost;
  Testo.(check float) ~msg:"recomputation savings" ben.savings !hit_savings

let test_naive name ben =
  Testo.create ~category:["naive"]
    name
    (run_benchmark (module Naive_cache) ben)

let test_gen name ben =
  Testo.create ~category:["generational"]
    name
    (run_benchmark (module Cache) ben)

let tests = [
  test_naive "repeat" repeat;
  test_gen "repeat" repeat;
  test_naive "never repeat" never_repeat_naive;
  test_gen "never repeat" never_repeat;
  test_naive "reuse" reuse_naive;
  test_gen "reuse" reuse;
  test_naive "medium" medium_naive;
  test_gen "medium" medium;
]

let () =
  Testo.interpret_argv ~project_name:"gencache" (fun _env ->
    tests
  )
