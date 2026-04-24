open! Core

(* Per-client mirror of what has been emitted to them. [advance]
   compares the shadow to the current aggregator state, records any
   field-level changes, and updates the shadow in place so the next
   call only sees forward diffs. *)
type t =
  { tables : (Exchange.t, (string, Ticker.t) Hashtbl.t) Hashtbl.t }

let create () = { tables = Hashtbl.create (module Exchange) }

let table_for t exchange =
  Hashtbl.find_or_add t.tables exchange ~default:(fun () ->
    Hashtbl.create (module String))
;;

let seed t ~snapshot =
  List.iter snapshot ~f:(fun (exchange, tickers) ->
    let tbl = table_for t exchange in
    List.iter tickers ~f:(fun (symbol, ticker) ->
      Hashtbl.set tbl ~key:symbol ~data:ticker))
;;

let selected_exchanges t ~aggregator ~exchanges_filter =
  match exchanges_filter with
  | None ->
    (* Everything the aggregator currently carries + anything we've
       already shadowed (so a removal is visible even after the
       aggregator drops the exchange). *)
    let from_agg = Aggregator.exchanges aggregator in
    let from_shadow = Hashtbl.keys t.tables in
    List.dedup_and_sort
      (from_agg @ from_shadow)
      ~compare:Exchange.compare
  | Some xs -> xs
;;

let advance t ~aggregator ~exchanges_filter ~include_index =
  let exchanges = selected_exchanges t ~aggregator ~exchanges_filter in
  let diffs = Queue.create () in
  let removals = Queue.create () in
  List.iter exchanges ~f:(fun exchange ->
    let current =
      Aggregator.snapshot_exchange aggregator exchange
      |> List.map ~f:(fun (sym, t) -> sym, Ticker.apply_subscribe_opts ~include_index t)
      |> String.Map.of_alist_exn
    in
    let tbl = table_for t exchange in
    let exchange_diffs = Queue.create () in
    Map.iteri current ~f:(fun ~key:symbol ~data:curr ->
      let prev = Hashtbl.find tbl symbol in
      let d = Ticker_delta.diff ~prev ~curr in
      if not (Ticker_delta.is_empty d)
      then (
        Queue.enqueue exchange_diffs (symbol, d);
        Hashtbl.set tbl ~key:symbol ~data:curr));
    let to_remove =
      Hashtbl.fold tbl ~init:[] ~f:(fun ~key:symbol ~data:_ acc ->
        if Map.mem current symbol then acc else symbol :: acc)
    in
    List.iter to_remove ~f:(fun symbol -> Hashtbl.remove tbl symbol);
    if not (Queue.is_empty exchange_diffs)
    then (
      let sorted =
        Queue.to_list exchange_diffs
        |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
      in
      Queue.enqueue diffs (exchange, sorted));
    if not (List.is_empty to_remove)
    then (
      let sorted = List.sort to_remove ~compare:String.compare in
      Queue.enqueue removals (exchange, sorted)));
  let diffs =
    Queue.to_list diffs
    |> List.sort ~compare:(fun (a, _) (b, _) -> Exchange.compare a b)
  in
  let removals =
    Queue.to_list removals
    |> List.sort ~compare:(fun (a, _) (b, _) -> Exchange.compare a b)
  in
  diffs, removals
;;
