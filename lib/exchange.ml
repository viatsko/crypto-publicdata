open! Core

type t =
  | Bybit
  | Bybit_spot
  | Bybit_inverse
[@@deriving sexp, compare, equal, hash]

let to_string = function
  | Bybit -> "bybit"
  | Bybit_spot -> "bybitspot"
  | Bybit_inverse -> "bybitinverse"
;;

let of_string = function
  | "bybit" -> Some Bybit
  | "bybitspot" -> Some Bybit_spot
  | "bybitinverse" -> Some Bybit_inverse
  | _ -> None
;;

let of_string_exn s =
  match of_string s with
  | Some e -> e
  | None -> failwithf "Exchange.of_string_exn: unknown exchange %S" s ()
;;
