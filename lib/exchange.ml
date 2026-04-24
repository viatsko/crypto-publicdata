open! Core

type t =
  | Bybit
  | Bybit_spot
  | Bybit_inverse
  | Binance
  | Binance_spot
  | Binance_inverse
  | Hyperliquid
  | Hyperliquid_spot
  | Bitget
  | Coinbase
  | Kraken
[@@deriving sexp, compare, equal, hash]

let to_string = function
  | Bybit -> "bybit"
  | Bybit_spot -> "bybitspot"
  | Bybit_inverse -> "bybitinverse"
  | Binance -> "binance"
  | Binance_spot -> "binancespot"
  | Binance_inverse -> "binanceinverse"
  | Hyperliquid -> "hyperliquid"
  | Hyperliquid_spot -> "hyperliquidspot"
  | Bitget -> "bitget"
  | Coinbase -> "coinbase"
  | Kraken -> "kraken"
;;

let of_string = function
  | "bybit" -> Some Bybit
  | "bybitspot" -> Some Bybit_spot
  | "bybitinverse" -> Some Bybit_inverse
  | "binance" -> Some Binance
  | "binancespot" -> Some Binance_spot
  | "binanceinverse" -> Some Binance_inverse
  | "hyperliquid" -> Some Hyperliquid
  | "hyperliquidspot" -> Some Hyperliquid_spot
  | "bitget" -> Some Bitget
  | "coinbase" -> Some Coinbase
  | "kraken" -> Some Kraken
  | _ -> None
;;

let of_string_exn s =
  match of_string s with
  | Some e -> e
  | None -> failwithf "Exchange.of_string_exn: unknown exchange %S" s ()
;;
