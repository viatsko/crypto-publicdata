open! Core
open Crypto_publicdata

let show_parsed body =
  List.iter (Hyperliquid_ws.parse_all_mids body) ~f:(fun (sym, mid) ->
    printf "%s -> %f\n" sym mid)
;;

let%expect_test "allMids frame extracts every coin's mid as a float" =
  let body =
    {|{"channel":"allMids","data":{"mids":{"BTC":"50000.5","ETH":"3000","HYPE":"25.75"}}}|}
  in
  show_parsed body;
  (* Hashtable iteration order isn't stable, so normalise by sorting. *)
  let pairs =
    Hyperliquid_ws.parse_all_mids body
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
  List.iter pairs ~f:(fun (sym, mid) -> printf "sorted: %s %f\n" sym mid);
  [%expect
    {|
    BTC -> 50000.500000
    ETH -> 3000.000000
    HYPE -> 25.750000
    sorted: BTC 50000.500000
    sorted: ETH 3000.000000
    sorted: HYPE 25.750000
    |}]
;;

let%expect_test "non-allMids frames return nothing" =
  show_parsed {|{"channel":"trades","data":{}}|};
  show_parsed {|{"channel":"subscribeResponse","data":{}}|};
  [%expect {| |}]
;;

let%expect_test "malformed json returns nothing" =
  show_parsed "not json";
  [%expect {| |}]
;;

let%expect_test "apply_mid folds mid into bid/ask/last/index without touching mark" =
  let base =
    { Ticker.empty with
      mark = 100.
    ; funding_rate = 0.0001
    ; open_interest = 12345.
    ; volume = 5000.
    }
  in
  let merged = Hyperliquid_ws.apply_mid base 101.5 in
  print_s [%sexp (merged : Ticker.t)];
  [%expect
    {|
    ((bid 101.5) (ask 101.5) (last 101.5) (mark 100) (index 101.5) (percentage 0)
     (open_interest 12345) (funding_rate 0.0001) (funding_time 0) (volume 5000)
     (quote_volume 0))
    |}]
;;

let%expect_test "subscribe frame is the canonical allMids body" =
  print_endline Hyperliquid_ws.subscribe_all_mids_frame;
  [%expect {| {"method":"subscribe","subscription":{"type":"allMids"}} |}]
;;
