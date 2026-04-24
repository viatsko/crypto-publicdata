open! Core
open Crypto_publicdata

let show exchange symbol =
  print_endline
    (match Symbol_normalizer.canonical_base exchange symbol with
     | Some base -> base
     | None -> "None")
;;

let%expect_test "linear USDT" =
  show Exchange.Bybit "BTCUSDT";
  [%expect {| BTC |}]
;;

let%expect_test "linear USDC" =
  show Exchange.Bybit "ETHUSDC";
  [%expect {| ETH |}]
;;

let%expect_test "inverse USD" =
  show Exchange.Bybit_inverse "BTCUSD";
  [%expect {| BTC |}]
;;

let%expect_test "spot USDC" =
  show Exchange.Bybit_spot "ETHUSDC";
  [%expect {| ETH |}]
;;

let%expect_test "USDC-margined perp with 1000 prefix" =
  show Exchange.Bybit "1000BONKPERP";
  [%expect {| 1000BONK |}]
;;

let%expect_test "numeric prefix is preserved" =
  show Exchange.Bybit "1000PEPEUSDT";
  [%expect {| 1000PEPE |}]
;;

let%expect_test "XBT aliased to BTC" =
  show Exchange.Bybit_inverse "XBTUSD";
  [%expect {| BTC |}]
;;

let%expect_test "USDT does not eat the final T of a genuine USD pair" =
  show Exchange.Bybit_inverse "BTCUSD";
  [%expect {| BTC |}]
;;

let%expect_test "empty symbol is None" =
  show Exchange.Bybit "";
  [%expect {| None |}]
;;

let%expect_test "single-character symbol is None" =
  show Exchange.Bybit "X";
  [%expect {| None |}]
;;

let%expect_test "symbol equal to a quote passes through" =
  (* A symbol of exactly "USDT" never appears in real upstream data; the
     stripper refuses to empty the string, and the 2+ char pass-through
     returns it unchanged rather than swallowing a potentially-valid
     exotic. *)
  show Exchange.Bybit "USDT";
  [%expect {| USDT |}]
;;

let%expect_test "symbol with no recognised quote passes through" =
  show Exchange.Bybit "WEIRD";
  [%expect {| WEIRD |}]
;;

let%expect_test "binance usdt-m" =
  show Exchange.Binance "BTCUSDT";
  show Exchange.Binance "1000PEPEUSDT";
  show Exchange.Binance "ETHUSDC";
  [%expect
    {|
    BTC
    1000PEPE
    ETH
    |}]
;;

let%expect_test "binance spot" =
  show Exchange.Binance_spot "ETHUSDT";
  [%expect {| ETH |}]
;;

let%expect_test "binance inverse strips the _PERP suffix before the quote" =
  show Exchange.Binance_inverse "BTCUSD_PERP";
  show Exchange.Binance_inverse "ETHUSD_PERP";
  [%expect
    {|
    BTC
    ETH
    |}]
;;
