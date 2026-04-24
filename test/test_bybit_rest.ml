open! Core
open Crypto_publicdata

let show_tickers ~exchange body =
  match Bybit_rest.tickers_of_body ~exchange body with
  | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e)
  | Ok tickers ->
    List.iter tickers ~f:(fun (sym, t) ->
      print_s [%sexp (sym : string), (t : Ticker.t)])
;;

let linear_body =
  {|
{ "retCode": 0, "retMsg": "OK",
  "result": {
    "category": "linear",
    "list": [
      { "symbol": "BTCUSDT",
        "bid1Price": "50000.0", "ask1Price": "50001.0",
        "lastPrice": "50000.5", "markPrice": "50000.3",
        "indexPrice": "50000.2",
        "price24hPcnt": "0.0123",
        "openInterest": "12345.0",
        "fundingRate": "0.0001",
        "nextFundingTime": "1775766154000",
        "volume24h": "100.0", "turnover24h": "5000025.0" }
    ] } }
|}
;;

let%expect_test "linear ticker parses fully" =
  show_tickers ~exchange:Exchange.Bybit linear_body;
  [%expect
    {|
    (BTCUSDT
     ((bid 50000) (ask 50001) (last 50000.5) (mark 50000.3) (index 50000.2)
      (percentage 1.23) (open_interest 12345) (funding_rate 0.0001)
      (funding_time 1775766154000) (volume 100) (quote_volume 5000025)))
    |}]
;;

let inverse_body =
  {|
{ "result": {
    "list": [
      { "symbol": "BTCUSD",
        "bid1Price": "50000", "ask1Price": "50001",
        "lastPrice": "50000", "markPrice": "50000",
        "indexPrice": "50000",
        "price24hPcnt": "-0.0234",
        "openInterest": "500000000",
        "fundingRate": "0.0001",
        "nextFundingTime": "1775766154000",
        "volume24h": "10", "turnover24h": "500005" }
    ] } }
|}
;;

let%expect_test "inverse open interest is normalized from USD to base" =
  (* 500_000_000 USD / 50_000 = 10_000 contracts' worth of base units. *)
  show_tickers ~exchange:Exchange.Bybit_inverse inverse_body;
  [%expect
    {|
    (BTCUSD
     ((bid 50000) (ask 50001) (last 50000) (mark 50000) (index 50000)
      (percentage -2.34) (open_interest 10000) (funding_rate 0.0001)
      (funding_time 1775766154000) (volume 10) (quote_volume 500005)))
    |}]
;;

let inverse_no_mark_body =
  {|
{ "result": { "list": [
  { "symbol": "ETHUSD", "lastPrice": "2000", "markPrice": "0",
    "openInterest": "100000" } ] } }
|}
;;

let%expect_test "inverse OI falls back to last when mark is zero" =
  show_tickers ~exchange:Exchange.Bybit_inverse inverse_no_mark_body;
  [%expect
    {|
    (ETHUSD
     ((bid 0) (ask 0) (last 2000) (mark 0) (index 0) (percentage 0)
      (open_interest 50) (funding_rate 0) (funding_time 0) (volume 0)
      (quote_volume 0)))
    |}]
;;

let spot_body =
  {|
{ "result": { "list": [
  { "symbol": "ETHUSDC",
    "bid1Price": "3000", "ask1Price": "3001",
    "lastPrice": "3000.5", "markPrice": "",
    "indexPrice": "",
    "price24hPcnt": "0.01",
    "openInterest": "0",
    "fundingRate": "0.9999",
    "nextFundingTime": "0",
    "volume24h": "100", "turnover24h": "0" } ] } }
|}
;;

let%expect_test "spot ignores funding and uses volume*last when turnover is zero" =
  show_tickers ~exchange:Exchange.Bybit_spot spot_body;
  [%expect
    {|
    (ETHUSDC
     ((bid 3000) (ask 3001) (last 3000.5) (mark 0) (index 0) (percentage 1)
      (open_interest 0) (funding_rate 0) (funding_time 0) (volume 100)
      (quote_volume 300050)))
    |}]
;;

let%expect_test "bybit-reported error surfaces as Or_error" =
  let body =
    {|{ "retCode": 10001, "retMsg": "params error", "result": {"list": []} }|}
  in
  (match Bybit_rest.tickers_of_body ~exchange:Exchange.Bybit body with
   | Ok _ -> print_endline "unexpected ok"
   | Error e -> print_endline (Error.to_string_hum e));
  [%expect {| bybit error: retCode=10001 retMsg=params error |}]
;;

let%expect_test "invalid json surfaces as Or_error" =
  (match Bybit_rest.tickers_of_body ~exchange:Exchange.Bybit "{not-json" with
   | Ok _ -> print_endline "unexpected ok"
   | Error _ -> print_endline "error");
  [%expect {| error |}]
;;

let%expect_test "malformed row is skipped rather than failing the list" =
  let body =
    {|{"result":{"list":[
       {"symbol":"BTCUSDT","lastPrice":"100"},
       "not-an-object",
       {"symbol":"","lastPrice":"200"}
     ]}}|}
  in
  show_tickers ~exchange:Exchange.Bybit body;
  [%expect
    {|
    (BTCUSDT
     ((bid 0) (ask 0) (last 100) (mark 0) (index 0) (percentage 0)
      (open_interest 0) (funding_rate 0) (funding_time 0) (volume 0)
      (quote_volume 0)))
    |}]
;;

let instruments_body =
  {|
{ "result": { "list": [
  { "symbol": "BTCUSDT", "status": "Trading" },
  { "symbol": "OLDUSDT", "status": "Closed" },
  { "symbol": "ETHUSDT", "status": "Trading" },
  { "symbol": "", "status": "Trading" } ] } }
|}
;;

let%expect_test "instruments filter keeps only Trading and non-empty symbols" =
  (match Bybit_rest.trading_symbols_of_body instruments_body with
   | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e)
   | Ok syms -> List.iter syms ~f:print_endline);
  [%expect
    {|
    BTCUSDT
    ETHUSDT
    |}]
;;

let%expect_test "url construction routes to the right category" =
  print_endline (Uri.to_string (Bybit_rest.tickers_url Exchange.Bybit));
  print_endline (Uri.to_string (Bybit_rest.tickers_url Exchange.Bybit_spot));
  print_endline (Uri.to_string (Bybit_rest.tickers_url Exchange.Bybit_inverse));
  print_endline (Uri.to_string (Bybit_rest.instruments_info_url Exchange.Bybit));
  [%expect
    {|
    https://api.bybit.com/v5/market/tickers?category=linear
    https://api.bybit.com/v5/market/tickers?category=spot
    https://api.bybit.com/v5/market/tickers?category=inverse
    https://api.bybit.com/v5/market/instruments-info?category=linear
    |}]
;;
