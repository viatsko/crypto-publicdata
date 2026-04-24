open! Core
open Crypto_publicdata

let%expect_test "ticker_json emits all fields with camelCase keys" =
  let t =
    { Ticker.bid = 50000.
    ; ask = 50001.
    ; last = 50000.5
    ; mark = 50000.3
    ; index = 50000.2
    ; percentage = 1.23
    ; open_interest = 12345.
    ; funding_rate = 0.0001
    ; funding_time = 1775766154000
    ; volume = 100.
    ; quote_volume = 5000025.
    }
  in
  print_endline (Yojson.Safe.to_string (Ticker_json.to_json t));
  [%expect
    {| {"b":50000.0,"a":50001.0,"l":50000.5,"m":50000.3,"i":50000.2,"p":1.23,"o":12345.0,"f":0.0001,"t":1775766154000,"v":100.0,"q":5000025.0} |}]
;;

let%expect_test "tickers_response is sorted by symbol" =
  let tickers =
    [ "ZEBRAUSDT", { Ticker.empty with last = 1. }
    ; "BTCUSDT", { Ticker.empty with last = 50000. }
    ; "MIDUSDT", { Ticker.empty with last = 100. }
    ]
  in
  let json = Ticker_json.tickers_response tickers in
  let pretty = Yojson.Safe.pretty_to_string json in
  print_endline pretty;
  [%expect
    {|
    {
      "tickers": {
        "BTCUSDT": { "l": 50000.0 },
        "MIDUSDT": { "l": 100.0 },
        "ZEBRAUSDT": { "l": 1.0 }
      }
    }
    |}]
;;

let%expect_test "empty tickers produce an empty map" =
  let json = Ticker_json.tickers_response [] in
  print_endline (Yojson.Safe.to_string json);
  [%expect {| {"tickers":{}} |}]
;;

let%expect_test "zero fields are stripped from the wire" =
  print_endline (Yojson.Safe.to_string (Ticker_json.to_json Ticker.empty));
  [%expect {| {} |}]
;;

let%expect_test "only non-zero fields appear in the wire representation" =
  let t = { Ticker.empty with bid = 100.; last = 99.5; funding_time = 1234 } in
  print_endline (Yojson.Safe.to_string (Ticker_json.to_json t));
  [%expect {| {"b":100.0,"l":99.5,"t":1234} |}]
;;
