open! Core

let rest_base = "https://futures.kraken.com/derivatives/api"
let instruments_url = Uri.of_string (rest_base ^ "/v3/instruments")
let tickers_url = Uri.of_string (rest_base ^ "/v3/tickers")

(* Kraken's REST serialises numbers as JSON numbers, not strings. Handle
   both [`Float] and [`Int] so integer-shaped values (e.g. 0) parse. *)
let float_field json field =
  match Yojson.Safe.Util.member field json with
  | `Float f -> f
  | `Int i -> Float.of_int i
  | `String s -> Option.value (Float.of_string_opt s) ~default:0.
  | _ -> 0.
;;

let bool_field json field =
  match Yojson.Safe.Util.member field json with
  | `Bool b -> b
  | _ -> false
;;

let string_field json field =
  match Yojson.Safe.Util.member field json with
  | `String s -> s
  | _ -> ""
;;

let parse_envelope body =
  try Ok (Yojson.Safe.from_string body) with
  | Yojson.Json_error msg -> Or_error.error_string ("invalid json: " ^ msg)
;;

let list_under key body =
  let open Or_error.Let_syntax in
  let%bind json = parse_envelope body in
  match Yojson.Safe.Util.member key json with
  | `List items -> Ok items
  | _ -> Or_error.errorf "missing %s list" key
;;

(* Perpetual marker — Kraken also publishes dated futures on the same
   endpoint and those don't belong in the aggregator. [tradeable] is
   the live-status flag; without it the market is paused. *)
let is_tradeable_perp json =
  String.is_prefix (string_field json "symbol") ~prefix:"PF_"
  && bool_field json "tradeable"
;;

let trading_symbols_of_body body =
  let open Or_error.Let_syntax in
  let%map items = list_under "instruments" body in
  List.filter_map items ~f:(fun json ->
    if is_tradeable_perp json
    then (
      let s = string_field json "symbol" in
      if String.is_empty s then None else Some s)
    else None)
;;

let ticker_of_json json =
  match json with
  | `Assoc _ ->
    let symbol = string_field json "symbol" in
    if String.is_empty symbol
       || not (String.is_prefix symbol ~prefix:"PF_")
       || bool_field json "suspended"
    then None
    else (
      let last = float_field json "last" in
      let mark = float_field json "markPrice" in
      (* Kraken REST [fundingRate] is quote-currency per contract; divide
         by last to turn it into the fractional rate every other venue
         exposes directly. WS later overwrites with
         [relative_funding_rate], which is already fractional. *)
      let funding_abs = float_field json "fundingRate" in
      let funding_rate =
        if Float.(last > 0.) then funding_abs /. last else 0.
      in
      let t =
        { Ticker.bid = float_field json "bid"
        ; ask = float_field json "ask"
        ; last
        ; mark
        ; index = float_field json "indexPrice"
        ; percentage = float_field json "change24h"
        ; open_interest = float_field json "openInterest"
        ; funding_rate
        ; funding_time = 0
        ; volume = float_field json "vol24h"
        ; quote_volume = float_field json "volumeQuote"
        }
      in
      Some (symbol, t))
  | _ -> None
;;

let tickers_of_body body =
  let open Or_error.Let_syntax in
  let%map items = list_under "tickers" body in
  List.filter_map items ~f:ticker_of_json
;;

module Io = struct
  open Async

  let fetch uri =
    match%map
      Monitor.try_with (fun () ->
        let%bind response, body_stream = Cohttp_async.Client.get uri in
        let%bind body = Cohttp_async.Body.to_string body_stream in
        let status =
          Cohttp.Response.status response |> Cohttp.Code.code_of_status
        in
        return (status, body))
    with
    | Ok (status, body) when status >= 200 && status < 300 -> Ok body
    | Ok (status, _) ->
      Or_error.errorf "HTTP %d from %s" status (Uri.to_string uri)
    | Error exn -> Or_error.of_exn exn
  ;;

  let fetch_trading_symbols () =
    let%bind body = fetch instruments_url in
    return (Or_error.bind body ~f:trading_symbols_of_body)
  ;;

  let fetch_tickers () =
    let%bind body = fetch tickers_url in
    return (Or_error.bind body ~f:tickers_of_body)
  ;;
end

let fetch_tickers = Io.fetch_tickers
let fetch_trading_symbols = Io.fetch_trading_symbols
