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
    {| {"bid":50000.0,"ask":50001.0,"last":50000.5,"mark":50000.3,"index":50000.2,"percentage":1.23,"openInterest":12345.0,"fundingRate":0.0001,"fundingTime":1775766154000,"volume":100.0,"quoteVolume":5000025.0} |}]
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
        "BTCUSDT": {
          "bid": 0.0,
          "ask": 0.0,
          "last": 50000.0,
          "mark": 0.0,
          "index": 0.0,
          "percentage": 0.0,
          "openInterest": 0.0,
          "fundingRate": 0.0,
          "fundingTime": 0,
          "volume": 0.0,
          "quoteVolume": 0.0
        },
        "MIDUSDT": {
          "bid": 0.0,
          "ask": 0.0,
          "last": 100.0,
          "mark": 0.0,
          "index": 0.0,
          "percentage": 0.0,
          "openInterest": 0.0,
          "fundingRate": 0.0,
          "fundingTime": 0,
          "volume": 0.0,
          "quoteVolume": 0.0
        },
        "ZEBRAUSDT": {
          "bid": 0.0,
          "ask": 0.0,
          "last": 1.0,
          "mark": 0.0,
          "index": 0.0,
          "percentage": 0.0,
          "openInterest": 0.0,
          "fundingRate": 0.0,
          "fundingTime": 0,
          "volume": 0.0,
          "quoteVolume": 0.0
        }
      }
    }
    |}]
;;

let%expect_test "empty tickers produce an empty map" =
  let json = Ticker_json.tickers_response [] in
  print_endline (Yojson.Safe.to_string json);
  [%expect {| {"tickers":{}} |}]
;;
