open! Core
open Crypto_publicdata

let show ~exchange body =
  match Hyperliquid_rest.snapshot_of_body ~exchange body with
  | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e)
  | Ok tickers ->
    List.iter tickers ~f:(fun (sym, t) ->
      print_s [%sexp (sym : string), (t : Ticker.t)])
;;

let perp_body =
  {|[
    { "universe": [
        { "name": "BTC", "szDecimals": 5, "maxLeverage": 50 },
        { "name": "ETH", "szDecimals": 4, "maxLeverage": 50 } ] },
    [
      { "markPx": "50000", "midPx": "50000.5",
        "funding": "0.0001", "openInterest": "12345",
        "dayNtlVlm": "5000000", "dayBaseVlm": "100", "prevDayPx": "49505" },
      { "markPx": "3000", "midPx": "3001",
        "funding": "0.0002", "openInterest": "5000",
        "dayNtlVlm": "300000", "dayBaseVlm": "100", "prevDayPx": "2970.3" }
    ]
  ]|}
;;

let%expect_test "perp ticker collapses mid into bid/ask/last/index" =
  show ~exchange:Exchange.Hyperliquid perp_body;
  [%expect
    {|
    (BTC
     ((bid 50000.5) (ask 50000.5) (last 50000.5) (mark 50000) (index 50000.5)
      (percentage 1.000908999091001) (open_interest 12345) (funding_rate 0.0001)
      (funding_time 0) (volume 100) (quote_volume 5000000)))
    (ETH
     ((bid 3001) (ask 3001) (last 3001) (mark 3000) (index 3001)
      (percentage 1.0335656331010274) (open_interest 5000) (funding_rate 0.0002)
      (funding_time 0) (volume 100) (quote_volume 300000)))
    |}]
;;

let%expect_test "percentage is zero when prev_day missing" =
  let body =
    {|[ {"universe":[{"name":"NEWCOIN"}]}, [{"markPx":"100","midPx":"100"}] ]|}
  in
  show ~exchange:Exchange.Hyperliquid body;
  [%expect
    {|
    (NEWCOIN
     ((bid 100) (ask 100) (last 100) (mark 100) (index 100) (percentage 0)
      (open_interest 0) (funding_rate 0) (funding_time 0) (volume 0)
      (quote_volume 0)))
    |}]
;;

let%expect_test "spot drops funding_rate to zero" =
  let body =
    {|[ {"universe":[{"name":"PURR/USDC"}]},
       [{"markPx":"0.5","midPx":"0.5","funding":"0.99","dayNtlVlm":"1000"}] ]|}
  in
  show ~exchange:Exchange.Hyperliquid_spot body;
  [%expect
    {|
    (PURR/USDC
     ((bid 0.5) (ask 0.5) (last 0.5) (mark 0.5) (index 0.5) (percentage 0)
      (open_interest 0) (funding_rate 0) (funding_time 0) (volume 0)
      (quote_volume 1000)))
    |}]
;;

let%expect_test "malformed top-level surfaces as Or_error" =
  (match
     Hyperliquid_rest.snapshot_of_body ~exchange:Exchange.Hyperliquid "{}"
   with
   | Error e -> print_endline (Error.to_string_hum e)
   | Ok _ -> print_endline "unexpected ok");
  [%expect {| expected [meta, ctxs] top-level array |}]
;;

let%expect_test "trading_symbols_of_body lists every universe entry" =
  (match Hyperliquid_rest.trading_symbols_of_body perp_body with
   | Ok syms -> List.iter syms ~f:print_endline
   | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e));
  [%expect
    {|
    BTC
    ETH
    |}]
;;

let%expect_test "request_body dispatches per category" =
  print_endline (Hyperliquid_rest.request_body Exchange.Hyperliquid);
  print_endline (Hyperliquid_rest.request_body Exchange.Hyperliquid_spot);
  [%expect
    {|
    {"type":"metaAndAssetCtxs"}
    {"type":"spotMetaAndAssetCtxs"}
    |}]
;;
