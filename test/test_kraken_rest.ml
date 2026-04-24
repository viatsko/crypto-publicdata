open! Core
open Crypto_publicdata

let show body =
  match Kraken_rest.tickers_of_body body with
  | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e)
  | Ok tickers ->
    List.iter tickers ~f:(fun (sym, t) ->
      print_s [%sexp (sym : string), (t : Ticker.t)])
;;

let body =
  {|{ "tickers": [
      { "symbol": "PF_XBTUSD", "tradeable": true, "suspended": false,
        "bid": 50000, "ask": 50001, "last": 50000.5,
        "markPrice": 50000.3, "indexPrice": 50000.2,
        "change24h": -1.15,
        "vol24h": 1000, "volumeQuote": 50000500, "openInterest": 12345,
        "fundingRate": 5.0 },
      { "symbol": "BTCUSD_250927", "tradeable": true,
        "bid": 0, "ask": 0, "last": 0 },
      { "symbol": "PF_PAUSED", "suspended": true,
        "bid": 1, "ask": 2, "last": 1 } ] }|}
;;

let%expect_test "keep only tradeable PF_ symbols, divide fundingRate by last" =
  show body;
  (* fundingRate 5 / last 50000.5 = 9.9999e-05 (fractional) *)
  [%expect
    {|
    (PF_XBTUSD
     ((bid 50000) (ask 50001) (last 50000.5) (mark 50000.3) (index 50000.2)
      (percentage -1.15) (open_interest 12345)
      (funding_rate 9.99990000099999E-05) (funding_time 0) (volume 1000)
      (quote_volume 50000500)))
    |}]
;;

let%expect_test "trading_symbols_of_body keeps only tradeable PF_ instruments" =
  let body =
    {|{ "instruments": [
        { "symbol": "PF_XBTUSD", "tradeable": true },
        { "symbol": "BTCUSD_250927", "tradeable": true },
        { "symbol": "PF_OFF", "tradeable": false } ] }|}
  in
  (match Kraken_rest.trading_symbols_of_body body with
   | Ok syms -> List.iter syms ~f:print_endline
   | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e));
  [%expect {| PF_XBTUSD |}]
;;

let%expect_test "funding_rate is zero when last is zero (no div-by-zero)" =
  let body =
    {|{ "tickers": [ { "symbol": "PF_NEW", "tradeable": true,
      "last": 0, "fundingRate": 10 } ] }|}
  in
  show body;
  [%expect
    {|
    (PF_NEW
     ((bid 0) (ask 0) (last 0) (mark 0) (index 0) (percentage 0)
      (open_interest 0) (funding_rate 0) (funding_time 0) (volume 0)
      (quote_volume 0)))
    |}]
;;
