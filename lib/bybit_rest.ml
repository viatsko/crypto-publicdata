open! Core

let rest_base = "https://api.bybit.com"

let category_string : Exchange.t -> string = function
  | Bybit -> "linear"
  | Bybit_spot -> "spot"
  | Bybit_inverse -> "inverse"
  | Binance | Binance_spot | Binance_inverse ->
    failwith "Bybit_rest.category_string: not a bybit variant"
;;

let url_with_category path exchange =
  Uri.with_query'
    (Uri.of_string (rest_base ^ path))
    [ "category", category_string exchange ]
;;

let instruments_info_url = url_with_category "/v5/market/instruments-info"
let tickers_url = url_with_category "/v5/market/tickers"

(* Upstream serialises every numeric field as a string, including the
   empty string for "no value". We coerce anything unparseable back to
   zero so downstream consumers can treat zero as "absent". *)
let parse_float s = Option.value (Float.of_string_opt s) ~default:0.
let parse_int s = Option.value (Int.of_string_opt s) ~default:(Int.of_float (parse_float s))

let string_field json field =
  match Yojson.Safe.Util.member field json with
  | `String s -> s
  | _ -> ""
;;

(* Inverse contracts at Bybit are quoted in USD notional (one contract
   == 1 USD). Divide by mark price (or last as a fallback) to bring OI
   into base units so the aggregator holds one consistent meaning. *)
let normalize_open_interest ~exchange ~raw ~mark ~last =
  match (exchange : Exchange.t) with
  | Bybit_inverse ->
    let ref_price = if Float.(mark > 0.) then mark else last in
    if Float.(ref_price > 0.) then raw /. ref_price else 0.
  | Bybit | Bybit_spot | Binance | Binance_spot | Binance_inverse -> raw
;;

let ticker_of_json ~(exchange : Exchange.t) json =
  match json with
  | `Assoc _ ->
    let symbol = string_field json "symbol" in
    if String.is_empty symbol
    then None
    else (
      let last = parse_float (string_field json "lastPrice") in
    let mark = parse_float (string_field json "markPrice") in
    let volume = parse_float (string_field json "volume24h") in
    let turnover = parse_float (string_field json "turnover24h") in
    let quote_volume = if Float.(turnover > 0.) then turnover else volume *. last in
    let raw_oi = parse_float (string_field json "openInterest") in
    let open_interest = normalize_open_interest ~exchange ~raw:raw_oi ~mark ~last in
    let funding_rate =
      match exchange with
      | Bybit_spot -> 0.
      | Bybit | Bybit_inverse | Binance | Binance_spot | Binance_inverse ->
        parse_float (string_field json "fundingRate")
    in
    let funding_time = parse_int (string_field json "nextFundingTime") in
    let percentage = parse_float (string_field json "price24hPcnt") *. 100. in
    Some
      ( symbol
      , { Ticker.bid = parse_float (string_field json "bid1Price")
        ; ask = parse_float (string_field json "ask1Price")
        ; last
        ; mark
        ; index = parse_float (string_field json "indexPrice")
        ; percentage
        ; open_interest
        ; funding_rate
        ; funding_time
        ; volume
        ; quote_volume
        } ))
  | _ -> None
;;

(* Navigate Bybit's standard envelope: {retCode, retMsg, result: {list:
   [...]}}. Missing retCode (common in test fixtures that mock only the
   relevant subtree) is treated as success. *)
let result_list body =
  let open Or_error.Let_syntax in
  let%bind json =
    try Ok (Yojson.Safe.from_string body) with
    | Yojson.Json_error msg -> Or_error.error_string ("invalid json: " ^ msg)
  in
  let%bind () =
    match Yojson.Safe.Util.member "retCode" json with
    | `Int 0 | `Null -> Ok ()
    | `Int code ->
      Or_error.errorf
        "bybit error: retCode=%d retMsg=%s"
        code
        (string_field json "retMsg")
    | _ -> Ok ()
  in
  match
    json |> Yojson.Safe.Util.member "result" |> Yojson.Safe.Util.member "list"
  with
  | `List items -> Ok items
  | _ -> Or_error.error_string "missing result.list"
;;

let tickers_of_body ~exchange body =
  let open Or_error.Let_syntax in
  let%map items = result_list body in
  List.filter_map items ~f:(ticker_of_json ~exchange)
;;

let trading_symbols_of_body body =
  let open Or_error.Let_syntax in
  let%map items = result_list body in
  List.filter_map items ~f:(function
    | `Assoc _ as json ->
      if String.equal (string_field json "status") "Trading"
      then (
        let symbol = string_field json "symbol" in
        if String.is_empty symbol then None else Some symbol)
      else None
    | _ -> None)
;;

module Io = struct
  open Async

  (* Wrap every HTTP call in [Monitor.try_with] — SSL handshake failures,
     DNS errors, and connection resets all surface as exceptions that
     would otherwise kill the whole scheduler. Returning them as
     [Or_error] lets the adapter log and move on. *)
  let fetch url =
    match%map
      Monitor.try_with (fun () ->
        let%bind response, body_stream = Cohttp_async.Client.get url in
        let%bind body = Cohttp_async.Body.to_string body_stream in
        let status =
          Cohttp.Response.status response |> Cohttp.Code.code_of_status
        in
        return (status, body))
    with
    | Ok (status, body) when status >= 200 && status < 300 -> Ok body
    | Ok (status, _) ->
      Or_error.errorf "HTTP %d from %s" status (Uri.to_string url)
    | Error exn -> Or_error.of_exn exn
  ;;

  let fetch_trading_symbols exchange =
    let%bind body = fetch (instruments_info_url exchange) in
    return (Or_error.bind body ~f:trading_symbols_of_body)
  ;;

  let fetch_tickers exchange =
    let%bind body = fetch (tickers_url exchange) in
    return (Or_error.bind body ~f:(tickers_of_body ~exchange))
  ;;
end

let fetch_trading_symbols = Io.fetch_trading_symbols
let fetch_tickers = Io.fetch_tickers
