open! Core

(* Coinbase INTX publishes perps over a single REST endpoint; there's
   no public WS, so the adapter polls this on an interval. Percentage
   change-over-24h isn't in the snapshot — the reference implementation
   seeds a daily-open cache from candles and computes it client-side.
   For now we leave [percentage] at zero and revisit when we add the
   daily-opens helper. *)

let instruments_url =
  Uri.of_string "https://api.international.coinbase.com/api/v1/instruments"
;;

let parse_float s = Option.value (Float.of_string_opt s) ~default:0.

let string_field_or json field default =
  match Yojson.Safe.Util.member field json with
  | `String s -> s
  | _ -> default
;;

let parse_float_field json field =
  match Yojson.Safe.Util.member field json with
  | `String s -> parse_float s
  | _ -> 0.
;;

let instruments_of_body body =
  let open Or_error.Let_syntax in
  let%bind json =
    try Ok (Yojson.Safe.from_string body) with
    | Yojson.Json_error msg -> Or_error.error_string ("invalid json: " ^ msg)
  in
  match json with
  | `List items -> Ok items
  | _ -> Or_error.error_string "expected top-level array of instruments"
;;

let keep_instrument json =
  match json with
  | `Assoc _ ->
    String.equal (string_field_or json "trading_state" "") "TRADING"
    && String.equal (string_field_or json "type" "") "PERP"
  | _ -> false
;;

let ticker_of_instrument json =
  if not (keep_instrument json)
  then None
  else (
    let symbol = string_field_or json "symbol" "" in
    if String.is_empty symbol
    then None
    else (
      let quote = Yojson.Safe.Util.member "quote" json in
      let t =
        { Ticker.bid = parse_float_field quote "best_bid_price"
        ; ask = parse_float_field quote "best_ask_price"
        ; last = parse_float_field quote "trade_price"
        ; mark = parse_float_field quote "mark_price"
        ; index = parse_float_field quote "index_price"
        ; percentage = 0.
        ; open_interest = parse_float_field json "open_interest"
        ; funding_rate = parse_float_field quote "predicted_funding"
        ; funding_time = 0
        ; volume = parse_float_field json "qty_24hr"
        ; quote_volume = parse_float_field json "notional_24hr"
        }
      in
      Some (symbol, t)))
;;

let tickers_of_body body =
  let open Or_error.Let_syntax in
  let%map items = instruments_of_body body in
  List.filter_map items ~f:ticker_of_instrument
;;

let trading_symbols_of_body body =
  let open Or_error.Let_syntax in
  let%map items = instruments_of_body body in
  List.filter_map items ~f:(fun json ->
    if keep_instrument json
    then (
      let s = string_field_or json "symbol" "" in
      if String.is_empty s then None else Some s)
    else None)
;;

module Io = struct
  open Async

  let fetch () =
    match%map
      Monitor.try_with (fun () ->
        let%bind response, body_stream =
          Cohttp_async.Client.get instruments_url
        in
        let%bind body = Cohttp_async.Body.to_string body_stream in
        let status =
          Cohttp.Response.status response |> Cohttp.Code.code_of_status
        in
        return (status, body))
    with
    | Ok (status, body) when status >= 200 && status < 300 -> Ok body
    | Ok (status, _) ->
      Or_error.errorf
        "HTTP %d from %s"
        status
        (Uri.to_string instruments_url)
    | Error exn -> Or_error.of_exn exn
  ;;

  let fetch_tickers () =
    let%bind body = fetch () in
    return (Or_error.bind body ~f:tickers_of_body)
  ;;

  let fetch_trading_symbols () =
    let%bind body = fetch () in
    return (Or_error.bind body ~f:trading_symbols_of_body)
  ;;
end

let fetch_tickers = Io.fetch_tickers
let fetch_trading_symbols = Io.fetch_trading_symbols
