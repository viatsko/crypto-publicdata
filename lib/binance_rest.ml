open! Core

(* Three separate sub-APIs: USDT-M (fapi / PERPETUAL contracts),
   spot (api), and COIN-M inverse (dapi / PERPETUAL contracts).
   Each has its own base host and path prefix; the snapshot builder
   pulls market list + 24h tickers + premium index (for futures) and
   stitches them into a single Ticker per symbol. *)

let rest_base : Exchange.t -> string = function
  | Binance -> "https://fapi.binance.com"
  | Binance_spot -> "https://api.binance.com"
  | Binance_inverse -> "https://dapi.binance.com"
  | Bybit | Bybit_spot | Bybit_inverse | Hyperliquid | Hyperliquid_spot | Bitget ->
    failwith "Binance_rest.rest_base: not a binance variant"
;;

let path_prefix : Exchange.t -> string = function
  | Binance -> "/fapi/v1"
  | Binance_spot -> "/api/v3"
  | Binance_inverse -> "/dapi/v1"
  | _ -> failwith "Binance_rest.path_prefix: not a binance variant"
;;

let url e p = Uri.of_string (rest_base e ^ path_prefix e ^ p)
let exchange_info_url e = url e "/exchangeInfo"
let tickers_24h_url e = url e "/ticker/24hr"

(* Spot has no funding, so there's no premiumIndex endpoint. Return
   None for spot so callers can skip the fetch altogether. *)
let premium_index_url : Exchange.t -> Uri.t option = function
  | Binance_spot -> None
  | Binance -> Some (url Binance "/premiumIndex")
  | Binance_inverse -> Some (url Binance_inverse "/premiumIndex")
  | _ -> failwith "Binance_rest.premium_index_url: not a binance variant"
;;

let parse_float s = Option.value (Float.of_string_opt s) ~default:0.
let parse_int s = Option.value (Int.of_string_opt s) ~default:(Int.of_float (parse_float s))

let string_field json field =
  match Yojson.Safe.Util.member field json with
  | `String s -> s
  | _ -> ""
;;

let top_level_array body =
  let open Or_error.Let_syntax in
  let%bind json =
    try Ok (Yojson.Safe.from_string body) with
    | Yojson.Json_error msg -> Or_error.error_string ("invalid json: " ^ msg)
  in
  match json with
  | `List items -> Ok items
  | `Assoc _ -> Or_error.errorf "expected top-level array, got object"
  | _ -> Or_error.error_string "expected top-level array"
;;

let exchange_info_list body =
  let open Or_error.Let_syntax in
  let%bind json =
    try Ok (Yojson.Safe.from_string body) with
    | Yojson.Json_error msg -> Or_error.error_string ("invalid json: " ^ msg)
  in
  match Yojson.Safe.Util.member "symbols" json with
  | `List items -> Ok items
  | _ -> Or_error.error_string "missing symbols"
;;

(* Symbol filter: [TRADING] status plus, for futures/inverse, PERPETUAL
   contract type and a sensible margin asset. Dated futures
   ([BTCUSDT_250927] etc) come back from the same endpoint and we don't
   want them in the public ticker map. *)
let keep_symbol_for exchange json =
  match json with
  | `Assoc _ ->
    let status = string_field json "status" in
    let trading =
      match (exchange : Exchange.t) with
      | Binance_spot -> String.equal status "TRADING"
      | Binance | Binance_inverse ->
        String.equal status "TRADING"
        && String.equal (string_field json "contractType") "PERPETUAL"
      | Bybit | Bybit_spot | Bybit_inverse | Hyperliquid | Hyperliquid_spot | Bitget ->        false
    in
    let margin_ok =
      match (exchange : Exchange.t) with
      | Binance ->
        (match string_field json "marginAsset" with
         | "USDT" | "USDC" -> true
         | _ -> false)
      | Binance_spot | Binance_inverse -> true
      | Bybit | Bybit_spot | Bybit_inverse | Hyperliquid | Hyperliquid_spot | Bitget ->        true
    in
    trading && margin_ok
  | _ -> false
;;

let trading_symbols_of_body ~exchange body =
  let open Or_error.Let_syntax in
  let%map items = exchange_info_list body in
  List.filter_map items ~f:(fun json ->
    if keep_symbol_for exchange json
    then (
      let s = string_field json "symbol" in
      if String.is_empty s then None else Some s)
    else None)
;;

(* Ticker builders: start from Ticker.empty, fold in one endpoint at a
   time. The merge order is idempotent — fields that only one endpoint
   serves (e.g. mark from premiumIndex) land exactly once. *)

let ticker_from_24h json =
  match json with
  | `Assoc _ ->
    let symbol = string_field json "symbol" in
    if String.is_empty symbol
    then None
    else (
      let last = parse_float (string_field json "lastPrice") in
      let volume = parse_float (string_field json "volume") in
      let quote_volume = parse_float (string_field json "quoteVolume") in
      (* Futures' 24h ticker omits bidPrice/askPrice; spot includes
         them. Either way, picking them up here is harmless — missing
         fields return 0.0 and the bookTicker / premiumIndex merge
         overwrites with live values if they arrive. *)
      let bid = parse_float (string_field json "bidPrice") in
      let ask = parse_float (string_field json "askPrice") in
      let percentage =
        parse_float (string_field json "priceChangePercent")
      in
      let t =
        { Ticker.empty with
          bid
        ; ask
        ; last
        ; percentage
        ; volume
        ; quote_volume
        }
      in
      Some (symbol, t))
  | _ -> None
;;

let merge_premium_index ~base json =
  match json with
  | `Assoc _ ->
    let symbol = string_field json "symbol" in
    if String.is_empty symbol
    then None
    else (
      let mark = parse_float (string_field json "markPrice") in
      let index = parse_float (string_field json "indexPrice") in
      let funding_rate =
        parse_float (string_field json "lastFundingRate")
      in
      let funding_time =
        parse_int (string_field json "nextFundingTime")
      in
      let merged =
        { base with
          Ticker.mark
        ; index
        ; funding_rate
        ; funding_time
        }
      in
      Some (symbol, merged))
  | _ -> None
;;

let tickers_of_24h_body body =
  let open Or_error.Let_syntax in
  let%map items = top_level_array body in
  List.filter_map items ~f:ticker_from_24h
  |> String.Map.of_alist_reduce ~f:(fun _ b -> b)
;;

let premium_map_of_body body =
  let open Or_error.Let_syntax in
  let%map items = top_level_array body in
  items
  |> List.filter_map ~f:(function
    | `Assoc _ as json ->
      let sym = string_field json "symbol" in
      if String.is_empty sym then None else Some (sym, json)
    | _ -> None)
  |> String.Map.of_alist_reduce ~f:(fun _ b -> b)
;;

(* Merge 24h + premium maps. Symbols present in 24h but absent from
   premium (or vice versa) still end up in the result — most likely a
   listing race on the upstream side and the next snapshot will
   reconcile. *)
let merge_snapshot ~tickers ~premium =
  let merged =
    Map.fold tickers ~init:String.Map.empty ~f:(fun ~key:sym ~data:t acc ->
      match Map.find premium sym with
      | Some prem ->
        (match merge_premium_index ~base:t prem with
         | Some (_, m) -> Map.set acc ~key:sym ~data:m
         | None -> Map.set acc ~key:sym ~data:t)
      | None -> Map.set acc ~key:sym ~data:t)
  in
  Map.to_alist merged
;;

let snapshot_of_bodies ~exchange ~tickers_body ~premium_body =
  let open Or_error.Let_syntax in
  let%bind tickers = tickers_of_24h_body tickers_body in
  match (exchange : Exchange.t), premium_body with
  | Binance_spot, _ | _, None -> Ok (Map.to_alist tickers)
  | (Binance | Binance_inverse), Some body ->
    let%map premium = premium_map_of_body body in
    merge_snapshot ~tickers ~premium
  | _ -> Or_error.error_string "not a binance variant"
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

  let fetch_trading_symbols exchange =
    let%bind body = fetch (exchange_info_url exchange) in
    return (Or_error.bind body ~f:(trading_symbols_of_body ~exchange))
  ;;

  let fetch_tickers exchange =
    let%bind tickers_body = fetch (tickers_24h_url exchange) in
    let%bind premium_body =
      match premium_index_url exchange with
      | None -> return (Ok None)
      | Some uri ->
        let%map body = fetch uri in
        Or_error.map body ~f:Option.some
    in
    return
      (let open Or_error.Let_syntax in
       let%bind tickers_body = tickers_body in
       let%bind premium_body = premium_body in
       snapshot_of_bodies ~exchange ~tickers_body ~premium_body)
  ;;
end

let fetch_trading_symbols = Io.fetch_trading_symbols
let fetch_tickers = Io.fetch_tickers
