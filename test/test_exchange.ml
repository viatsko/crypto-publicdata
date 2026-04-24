open! Core
open Crypto_publicdata

let show_roundtrip s =
  let via =
    match Exchange.of_string s with
    | Some e -> Exchange.to_string e
    | None -> "None"
  in
  print_endline via
;;

let%expect_test "bybit linear" =
  show_roundtrip "bybit";
  [%expect {| bybit |}]
;;

let%expect_test "bybit spot" =
  show_roundtrip "bybitspot";
  [%expect {| bybitspot |}]
;;

let%expect_test "bybit inverse" =
  show_roundtrip "bybitinverse";
  [%expect {| bybitinverse |}]
;;

let%expect_test "binance usdt-m / spot / inverse" =
  show_roundtrip "binance";
  show_roundtrip "binancespot";
  show_roundtrip "binanceinverse";
  [%expect
    {|
    binance
    binancespot
    binanceinverse
    |}]
;;

let%expect_test "hyperliquid perp / spot" =
  show_roundtrip "hyperliquid";
  show_roundtrip "hyperliquidspot";
  [%expect
    {|
    hyperliquid
    hyperliquidspot
    |}]
;;

let%expect_test "unknown is None" =
  show_roundtrip "nonexistent";
  [%expect {| None |}]
;;

let%expect_test "of_string_exn raises on unknown" =
  let result = Or_error.try_with (fun () -> Exchange.of_string_exn "nope") in
  print_s [%sexp (Result.is_error result : bool)];
  [%expect {| true |}]
;;
