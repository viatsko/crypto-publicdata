open! Core

(* Hyperliquid exposes a single [/info] POST endpoint that dispatches
   on a [type] field in the body. Perps and spot each have their own
   [metaAndAssetCtxs] call returning a two-element array:
   [[ meta, ctxs ]] where [meta.universe] lists assets in order and
   [ctxs] is a parallel list of their current state. *)

let info_url = Uri.of_string "https://api.hyperliquid.xyz/info"

let request_body : Exchange.t -> string = function
  | Hyperliquid ->
    Yojson.Safe.to_string (`Assoc [ "type", `String "metaAndAssetCtxs" ])
  | Hyperliquid_spot ->
    Yojson.Safe.to_string
      (`Assoc [ "type", `String "spotMetaAndAssetCtxs" ])
  | Bybit | Bybit_spot | Bybit_inverse | Binance | Binance_spot | Binance_inverse
  | Bitget ->
    failwith "Hyperliquid_rest.request_body: not a hyperliquid variant"
;;

let parse_float s = Option.value (Float.of_string_opt s) ~default:0.

let string_of_field json field =
  match Yojson.Safe.Util.member field json with
  | `String s -> s
  | _ -> ""
;;

let float_of_string_field json field =
  match Yojson.Safe.Util.member field json with
  | `String s -> parse_float s
  | _ -> 0.
;;

(* [metaAndAssetCtxs] returns [[ {universe: [{name, ...}, ...]}, [ctx, ...] ]].
   The two arrays are parallel — index [i] of [universe] matches index
   [i] of [ctxs]. *)
let universe_and_ctxs body =
  let open Or_error.Let_syntax in
  let%bind json =
    try Ok (Yojson.Safe.from_string body) with
    | Yojson.Json_error msg -> Or_error.error_string ("invalid json: " ^ msg)
  in
  match json with
  | `List [ meta; `List ctxs ] ->
    let universe = Yojson.Safe.Util.member "universe" meta in
    (match universe with
     | `List names -> Ok (names, ctxs)
     | _ -> Or_error.error_string "missing meta.universe")
  | _ -> Or_error.error_string "expected [meta, ctxs] top-level array"
;;

let symbol_of_universe_entry json =
  match json with
  | `Assoc _ ->
    let name = string_of_field json "name" in
    if String.is_empty name then None else Some name
  | _ -> None
;;

(* Spot entries sometimes have a [@N] form. The [tokens] field carries
   indices into a separate token list; without running that resolution
   we keep the raw name — downstream consumers that care can remap.

   Perps always use the plain base name (BTC, ETH, etc). *)

let ticker_of_ctx ~exchange ctx =
  match ctx with
  | `Assoc _ ->
    let mark = float_of_string_field ctx "markPx" in
    let mid = float_of_string_field ctx "midPx" in
    let funding_rate = float_of_string_field ctx "funding" in
    let open_interest = float_of_string_field ctx "openInterest" in
    let prev_day = float_of_string_field ctx "prevDayPx" in
    let day_ntl_vlm = float_of_string_field ctx "dayNtlVlm" in
    let day_base_vlm = float_of_string_field ctx "dayBaseVlm" in
    let volume = if Float.(day_base_vlm > 0.) then day_base_vlm else 0. in
    let quote_volume = day_ntl_vlm in
    let percentage =
      if Float.(prev_day > 0.) then (mid -. prev_day) /. prev_day *. 100. else 0.
    in
    (* Hyperliquid doesn't stream book depth in the /info endpoint —
       every price column (bid/ask/last/mark/index) collapses to a
       single reference. [mid] is the closest analogue for bid/ask
       and last; [mark] is served distinctly. *)
    let t =
      { Ticker.empty with
        bid = mid
      ; ask = mid
      ; last = mid
      ; mark
      ; index = mid
      ; percentage
      ; open_interest
      ; funding_rate =
          (match (exchange : Exchange.t) with
           | Hyperliquid_spot -> 0.
           | _ -> funding_rate)
      ; volume
      ; quote_volume
      }
    in
    Some t
  | _ -> None
;;

let snapshot_of_body ~exchange body =
  let open Or_error.Let_syntax in
  let%map universe, ctxs = universe_and_ctxs body in
  List.zip_exn universe ctxs
  |> List.filter_map ~f:(fun (u, c) ->
    match symbol_of_universe_entry u, ticker_of_ctx ~exchange c with
    | Some sym, Some t -> Some (sym, t)
    | _ -> None)
;;

let trading_symbols_of_body body =
  let open Or_error.Let_syntax in
  let%map universe, _ = universe_and_ctxs body in
  List.filter_map universe ~f:symbol_of_universe_entry
;;

module Io = struct
  open Async

  let fetch ~body_text =
    match%map
      Monitor.try_with (fun () ->
        let%bind response, body_stream =
          Cohttp_async.Client.post
            ~headers:
              (Cohttp.Header.init_with "content-type" "application/json")
            ~body:(Cohttp_async.Body.of_string body_text)
            info_url
        in
        let%bind body = Cohttp_async.Body.to_string body_stream in
        let status =
          Cohttp.Response.status response |> Cohttp.Code.code_of_status
        in
        return (status, body))
    with
    | Ok (status, body) when status >= 200 && status < 300 -> Ok body
    | Ok (status, _) ->
      Or_error.errorf "HTTP %d from %s" status (Uri.to_string info_url)
    | Error exn -> Or_error.of_exn exn
  ;;

  let fetch_tickers exchange =
    let%bind body = fetch ~body_text:(request_body exchange) in
    return (Or_error.bind body ~f:(snapshot_of_body ~exchange))
  ;;

  let fetch_trading_symbols exchange =
    let%bind body = fetch ~body_text:(request_body exchange) in
    return (Or_error.bind body ~f:trading_symbols_of_body)
  ;;
end

let fetch_tickers = Io.fetch_tickers
let fetch_trading_symbols = Io.fetch_trading_symbols
