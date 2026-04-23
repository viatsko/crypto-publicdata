open! Core

(* Trailing-quote strips, longest-first so [USDT] doesn't chew the [T]
   off a genuine USD-ending symbol. [PERP] is last because it's only a
   pseudo-quote — Bybit's USDC-margined perps use [<base>PERP] (e.g.
   [1000BONKPERP]) and we only want to strip it when no real quote
   matched. *)
let quotes = [ "USDT"; "USDC"; "USDD"; "USDE"; "USD"; "EUR"; "PERP" ]

let normalize_alias = function
  | "XBT" -> "BTC"
  | b -> b
;;

let strip_trailing_quote symbol =
  let rec loop = function
    | [] -> if String.length symbol >= 2 then Some symbol else None
    | quote :: rest ->
      (match String.chop_suffix symbol ~suffix:quote with
       | Some base when not (String.is_empty base) -> Some base
       | _ -> loop rest)
  in
  loop quotes
;;

let canonical_base (exchange : Exchange.t) symbol =
  if String.is_empty symbol
  then None
  else (
    match exchange with
    | Bybit | Bybit_spot | Bybit_inverse ->
      strip_trailing_quote symbol |> Option.map ~f:normalize_alias)
;;
