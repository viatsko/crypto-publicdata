open! Core
open Crypto_publicdata

let show body =
  match Bitget_rest.tickers_of_body body with
  | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e)
  | Ok tickers ->
    List.iter tickers ~f:(fun (sym, t) ->
      print_s [%sexp (sym : string), (t : Ticker.t)])
;;

let%expect_test "single-endpoint ticker carries every field" =
  let body =
    {|{ "code": "00000", "msg": "success", "data": [
        { "symbol": "BTCUSDT",
          "lastPr": "50000.5", "bidPr": "50000.0", "askPr": "50001.0",
          "markPrice": "50000.3", "indexPrice": "50000.2",
          "change24h": "0.0123",
          "holdingAmount": "12345",
          "fundingRate": "0.0001",
          "fundingTime": "1775766154000",
          "baseVolume": "100", "quoteVolume": "5000025" } ] }|}
  in
  show body;
  [%expect
    {|
    (BTCUSDT
     ((bid 50000) (ask 50001) (last 50000.5) (mark 50000.3) (index 50000.2)
      (percentage 1.23) (open_interest 12345) (funding_rate 0.0001)
      (funding_time 1775766154000) (volume 100) (quote_volume 5000025)))
    |}]
;;

let%expect_test "bitget error code surfaces as Or_error" =
  let body =
    {|{ "code": "40404", "msg": "not found", "data": [] }|}
  in
  (match Bitget_rest.tickers_of_body body with
   | Ok _ -> print_endline "unexpected ok"
   | Error e -> print_endline (Error.to_string_hum e));
  [%expect {| bitget error: code=40404 msg=not found |}]
;;

let%expect_test "trading_symbols_of_body keeps only normal-status contracts" =
  let body =
    {|{ "code": "00000", "data": [
        { "symbol": "BTCUSDT", "symbolStatus": "normal" },
        { "symbol": "OFFUSDT", "symbolStatus": "offline" },
        { "symbol": "ETHUSDT", "symbolStatus": "normal" } ] }|}
  in
  (match Bitget_rest.trading_symbols_of_body body with
   | Ok syms -> List.iter syms ~f:print_endline
   | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e));
  [%expect
    {|
    BTCUSDT
    ETHUSDT
    |}]
;;

let%expect_test "url routing" =
  print_endline (Uri.to_string Bitget_rest.tickers_url);
  print_endline (Uri.to_string Bitget_rest.contracts_url);
  [%expect
    {|
    https://api.bitget.com/api/v2/mix/market/tickers?productType=USDT-FUTURES
    https://api.bitget.com/api/v2/mix/market/contracts?productType=USDT-FUTURES
    |}]
;;
