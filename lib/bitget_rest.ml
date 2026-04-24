open! Core

(* Bitget only publishes USDT-M futures on this port; the [productType]
   query param is fixed. REST snapshot is single-endpoint — one hit to
   [/api/v2/mix/market/tickers] returns every field. *)

let rest_base = "https://api.bitget.com"
let product_type = "USDT-FUTURES"

let contracts_url =
  Uri.with_query'
    (Uri.of_string (rest_base ^ "/api/v2/mix/market/contracts"))
    [ "productType", product_type ]
;;

let tickers_url =
  Uri.with_query'
    (Uri.of_string (rest_base ^ "/api/v2/mix/market/tickers"))
    [ "productType", product_type ]
;;

let parse_float s = Option.value (Float.of_string_opt s) ~default:0.
let parse_int s = Option.value (Int.of_string_opt s) ~default:(Int.of_float (parse_float s))

let string_field json field =
  match Yojson.Safe.Util.member field json with
  | `String s -> s
  | _ -> ""
;;

let data_list body =
  let open Or_error.Let_syntax in
  let%bind json =
    try Ok (Yojson.Safe.from_string body) with
    | Yojson.Json_error msg -> Or_error.error_string ("invalid json: " ^ msg)
  in
  let%bind () =
    match Yojson.Safe.Util.member "code" json with
    | `String "00000" | `Null -> Ok ()
    | `String code ->
      Or_error.errorf
        "bitget error: code=%s msg=%s"
        code
        (string_field json "msg")
    | _ -> Ok ()
  in
  match Yojson.Safe.Util.member "data" json with
  | `List items -> Ok items
  | _ -> Or_error.error_string "missing data"
;;

let ticker_of_json json =
  match json with
  | `Assoc _ ->
    let symbol = string_field json "symbol" in
    if String.is_empty symbol
    then None
    else (
      let t =
        { Ticker.bid = parse_float (string_field json "bidPr")
        ; ask = parse_float (string_field json "askPr")
        ; last = parse_float (string_field json "lastPr")
        ; mark = parse_float (string_field json "markPrice")
        ; index = parse_float (string_field json "indexPrice")
        ; (* [change24h] is already a fraction (0.01 == +1 %); multiply
             by 100 for the wire's percent form. *)
          percentage = parse_float (string_field json "change24h") *. 100.
        ; open_interest = parse_float (string_field json "holdingAmount")
        ; funding_rate = parse_float (string_field json "fundingRate")
        ; funding_time = parse_int (string_field json "fundingTime")
        ; volume = parse_float (string_field json "baseVolume")
        ; quote_volume = parse_float (string_field json "quoteVolume")
        }
      in
      Some (symbol, t))
  | _ -> None
;;

let tickers_of_body body =
  let open Or_error.Let_syntax in
  let%map items = data_list body in
  List.filter_map items ~f:ticker_of_json
;;

let trading_symbols_of_body body =
  let open Or_error.Let_syntax in
  let%map items = data_list body in
  List.filter_map items ~f:(function
    | `Assoc _ as json ->
      (* [symbolStatus = "normal"] is Bitget's live-trading marker. *)
      if String.equal (string_field json "symbolStatus") "normal"
      then (
        let s = string_field json "symbol" in
        if String.is_empty s then None else Some s)
      else None
    | _ -> None)
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
    let%bind body = fetch contracts_url in
    return (Or_error.bind body ~f:trading_symbols_of_body)
  ;;

  let fetch_tickers () =
    let%bind body = fetch tickers_url in
    return (Or_error.bind body ~f:tickers_of_body)
  ;;
end

let fetch_tickers = Io.fetch_tickers
let fetch_trading_symbols = Io.fetch_trading_symbols
