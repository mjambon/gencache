(*
   Test the gencache library
*)

open Printf

module Cache =
  Gencache.Make (struct
    include Int
    let show = Int.to_string
  end)

type benchmark = {
  traffic: (int * float * float) list; (* (key, size, cost) *)
  live: int list; (* all of these keys must exist in the cache when done *)
  dead: int list; (* none of these keys must exist in the cache when done *)
  cost: float; (* the expected cost of all values computations (cache misses) *)
}

let simple = {
  traffic = [
    (1, 1., 1.);
    (2, 1., 1.);
    (3, 1., 1.);
    (4, 1., 1.);
    (4, 1., 1.);
    (5, 1., 1.);
    (6, 1., 1.);
    (7, 1., 1.);
  ];
  live = [4];
  dead = [];
  cost = 7.;
}

(*
   A benchmark is a test that creates a cache and performs get/put lookups
   in a predefined sequence. The evaluation criterion is the total
   recomputation cost which we try to minimize under various traffic patterns.

   Each run logs data needed for studying the behavior of the cache
   and for troubleshooting.

   Input format: (key, size, cost)
*)
let run_benchmark (ben : benchmark) () =
  let cache = Cache.create 5. in
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
        printf "hit %i\n" k
  in
  List.iter get_put ben.traffic;
  printf "Cache stats:\n";
  print_endline (Cache.show_stats (Cache.stats cache));
  printf "Miss cost: %g\n" !miss_cost;
  List.iter (fun k ->
    if not (Cache.mem cache k) then
      Testo.fail (sprintf "key missing from the cache: %i\n" k)
  ) ben.live;
  List.iter (fun k ->
    if Cache.mem cache k then
      Testo.fail (sprintf "key should not be in cache: %i\n" k)
  ) ben.dead;
  Testo.(check float) ~msg:"total miss cost" ben.cost !miss_cost

let tests = [
  Testo.create "simple" (run_benchmark simple);
]

let () =
  Testo.interpret_argv ~project_name:"gencache" (fun _env ->
    tests
  )
