open! Core
open Crypto_publicdata

let btc_ticker ?(last = 100.) () = { Ticker.empty with last }

let show_state agg exchange =
  printf
    "version=%d count=%d\n"
    (Aggregator.version agg exchange)
    (Aggregator.ticker_count agg exchange)
;;

let%expect_test "empty aggregator has version 0 and no tickers" =
  let agg = Aggregator.create () in
  show_state agg Exchange.Bybit;
  printf "exchanges=%d\n" (List.length (Aggregator.exchanges agg));
  [%expect
    {|
    version=0 count=0
    exchanges=0
    |}]
;;

let%expect_test "upsert adds ticker and bumps version" =
  let agg = Aggregator.create () in
  let changed =
    Aggregator.upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~ticker:(btc_ticker ())
  in
  printf "changed=%b\n" changed;
  show_state agg Exchange.Bybit;
  [%expect
    {|
    changed=true
    version=1 count=1
    |}]
;;

let%expect_test "upsert with identical ticker does not bump version" =
  let agg = Aggregator.create () in
  let t = btc_ticker () in
  let _ : bool = Aggregator.upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~ticker:t in
  let changed = Aggregator.upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~ticker:t in
  printf "second upsert changed=%b\n" changed;
  show_state agg Exchange.Bybit;
  [%expect
    {|
    second upsert changed=false
    version=1 count=1
    |}]
;;

let%expect_test "upsert with different ticker bumps version" =
  let agg = Aggregator.create () in
  let _ : bool =
    Aggregator.upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~ticker:(btc_ticker ~last:100. ())
  in
  let _ : bool =
    Aggregator.upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~ticker:(btc_ticker ~last:101. ())
  in
  show_state agg Exchange.Bybit;
  [%expect {| version=2 count=1 |}]
;;

let%expect_test "remove existing ticker bumps version and drops entry" =
  let agg = Aggregator.create () in
  let _ : bool =
    Aggregator.upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~ticker:(btc_ticker ())
  in
  let removed = Aggregator.remove agg ~exchange:Bybit ~symbol:"BTCUSDT" in
  printf "removed=%b\n" removed;
  show_state agg Exchange.Bybit;
  [%expect
    {|
    removed=true
    version=2 count=0
    |}]
;;

let%expect_test "remove nonexistent is a no-op" =
  let agg = Aggregator.create () in
  let _ : bool =
    Aggregator.upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~ticker:(btc_ticker ())
  in
  let removed = Aggregator.remove agg ~exchange:Bybit ~symbol:"ETHUSDT" in
  printf "removed=%b\n" removed;
  show_state agg Exchange.Bybit;
  [%expect
    {|
    removed=false
    version=1 count=1
    |}]
;;

let%expect_test "remove on empty exchange is a no-op" =
  let agg = Aggregator.create () in
  let removed = Aggregator.remove agg ~exchange:Bybit ~symbol:"BTCUSDT" in
  printf "removed=%b\n" removed;
  show_state agg Exchange.Bybit;
  [%expect
    {|
    removed=false
    version=0 count=0
    |}]
;;

let%expect_test "snapshot returns every stored ticker for the exchange" =
  let agg = Aggregator.create () in
  let _ : bool =
    Aggregator.upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~ticker:(btc_ticker ~last:100. ())
  in
  let _ : bool =
    Aggregator.upsert agg ~exchange:Bybit ~symbol:"ETHUSDT" ~ticker:(btc_ticker ~last:3000. ())
  in
  let snapshot =
    Aggregator.snapshot_exchange agg Exchange.Bybit
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
  List.iter snapshot ~f:(fun (sym, t) -> printf "%s %f\n" sym t.last);
  [%expect
    {|
    BTCUSDT 100.000000
    ETHUSDT 3000.000000
    |}]
;;

let%expect_test "exchanges are isolated" =
  let agg = Aggregator.create () in
  let _ : bool =
    Aggregator.upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~ticker:(btc_ticker ())
  in
  let _ : bool =
    Aggregator.upsert
      agg
      ~exchange:Bybit_spot
      ~symbol:"ETHUSDC"
      ~ticker:(btc_ticker ())
  in
  Aggregator.exchanges agg
  |> List.sort ~compare:Exchange.compare
  |> List.iter ~f:(fun e ->
    printf
      "%s: count=%d version=%d\n"
      (Exchange.to_string e)
      (Aggregator.ticker_count agg e)
      (Aggregator.version agg e));
  [%expect
    {|
    bybit: count=1 version=1
    bybitspot: count=1 version=1
    |}]
;;

let%expect_test "get returns Some for existing and None for missing" =
  let agg = Aggregator.create () in
  let _ : bool =
    Aggregator.upsert
      agg
      ~exchange:Bybit
      ~symbol:"BTCUSDT"
      ~ticker:(btc_ticker ~last:42. ())
  in
  (match Aggregator.get agg ~exchange:Bybit ~symbol:"BTCUSDT" with
   | Some t -> printf "found %f\n" t.last
   | None -> print_endline "missing");
  (match Aggregator.get agg ~exchange:Bybit ~symbol:"NOPEUSDT" with
   | Some _ -> print_endline "unexpected"
   | None -> print_endline "missing");
  (match Aggregator.get agg ~exchange:Bybit_spot ~symbol:"BTCUSDT" with
   | Some _ -> print_endline "leaked"
   | None -> print_endline "missing");
  [%expect
    {|
    found 42.000000
    missing
    missing
    |}]
;;
