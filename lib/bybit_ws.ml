open! Core

let ws_base = "wss://stream.bybit.com/v5/public"

let ws_url : Exchange.t -> Uri.t = function
  | Bybit -> Uri.of_string (ws_base ^ "/linear")
  | Bybit_spot -> Uri.of_string (ws_base ^ "/spot")
  | Bybit_inverse -> Uri.of_string (ws_base ^ "/inverse")
  | Binance | Binance_spot | Binance_inverse
  | Hyperliquid | Hyperliquid_spot | Bitget ->    failwith "Bybit_ws.ws_url: not a bybit variant"
;;

(* Upstream serialises every numeric field as a string. Treat the empty
   string and [`Null] as absent so an empty field doesn't collapse into
   a zero that would be indistinguishable from an honestly-zero value
   in a later sparse frame. *)
let string_opt_field json field =
  match Yojson.Safe.Util.member field json with
  | `String "" | `Null -> None
  | `String s -> Some s
  | _ -> None
;;

let float_field json field =
  string_opt_field json field |> Option.bind ~f:Float.of_string_opt
;;

let int_field json field =
  string_opt_field json field |> Option.bind ~f:Int.of_string_opt
;;

module Partial = struct
  (* A WS ticker frame only carries the fields that changed. Each field
     is [Some] when upstream touched it in this frame; [None] means
     "unchanged, don't clobber the stored value".

     [open_interest_raw] holds the upstream number verbatim — for
     inverse it's a USD-notional and gets normalised to base units on
     merge, using the current mark/last. [turnover] is similarly raw
     and is folded into [quote_volume] at merge time. *)
  type t =
    { bid : float option
    ; ask : float option
    ; last : float option
    ; mark : float option
    ; index : float option
    ; percentage : float option
    ; open_interest_raw : float option
    ; funding_rate : float option
    ; funding_time : int option
    ; volume : float option
    ; turnover : float option
    }
  [@@deriving sexp, compare, equal]

  let empty =
    { bid = None
    ; ask = None
    ; last = None
    ; mark = None
    ; index = None
    ; percentage = None
    ; open_interest_raw = None
    ; funding_rate = None
    ; funding_time = None
    ; volume = None
    ; turnover = None
    }
  ;;

  let apply ~(exchange : Exchange.t) (t : Ticker.t) partial =
    let bid = Option.value partial.bid ~default:t.bid in
    let ask = Option.value partial.ask ~default:t.ask in
    let last = Option.value partial.last ~default:t.last in
    let mark = Option.value partial.mark ~default:t.mark in
    let index = Option.value partial.index ~default:t.index in
    let percentage = Option.value partial.percentage ~default:t.percentage in
    let funding_rate =
      match exchange with
      | Bybit_spot -> 0.
      | Bybit | Bybit_inverse ->
        Option.value partial.funding_rate ~default:t.funding_rate
      | Binance | Binance_spot | Binance_inverse
      | Hyperliquid | Hyperliquid_spot | Bitget -> t.funding_rate
    in
    let funding_time = Option.value partial.funding_time ~default:t.funding_time in
    let volume = Option.value partial.volume ~default:t.volume in
    let quote_volume =
      match partial.turnover with
      | Some tv when Float.(tv > 0.) -> tv
      | Some _ | None ->
        if Float.(volume > 0. && last > 0.) then volume *. last else t.quote_volume
    in
    let open_interest =
      match partial.open_interest_raw with
      | None -> t.open_interest
      | Some raw -> Bybit_rest.normalize_open_interest ~exchange ~raw ~mark ~last
    in
    { Ticker.bid
    ; ask
    ; last
    ; mark
    ; index
    ; percentage
    ; open_interest
    ; funding_rate
    ; funding_time
    ; volume
    ; quote_volume
    }
  ;;
end

let partial_of_data ~(exchange : Exchange.t) json =
  { Partial.bid = float_field json "bid1Price"
  ; ask = float_field json "ask1Price"
  ; last = float_field json "lastPrice"
  ; mark = float_field json "markPrice"
  ; index = float_field json "indexPrice"
  ; percentage =
      string_opt_field json "price24hPcnt"
      |> Option.bind ~f:Float.of_string_opt
      |> Option.map ~f:(fun p -> p *. 100.)
  ; open_interest_raw = float_field json "openInterest"
  ; funding_rate =
      (match exchange with
       | Bybit_spot -> None
       | Bybit | Bybit_inverse -> float_field json "fundingRate"
       | Binance | Binance_spot | Binance_inverse
       | Hyperliquid | Hyperliquid_spot | Bitget -> None)
  ; funding_time = int_field json "nextFundingTime"
  ; volume = float_field json "volume24h"
  ; turnover = float_field json "turnover24h"
  }
;;

let parse_ticker_frame ~(exchange : Exchange.t) body =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error _ -> None
  | `Assoc _ as json ->
    let topic =
      match Yojson.Safe.Util.member "topic" json with
      | `String s -> s
      | _ -> ""
    in
    (match String.chop_prefix topic ~prefix:"tickers." with
     | None -> None
     | Some _ ->
       (match Yojson.Safe.Util.member "data" json with
        | `Assoc _ as data ->
          let symbol =
            match Yojson.Safe.Util.member "symbol" data with
            | `String s -> s
            | _ -> ""
          in
          if String.is_empty symbol
          then None
          else Some (symbol, partial_of_data ~exchange data)
        | _ -> None))
  | _ -> None
;;

let default_chunk_size = 10
let chunk_symbols ?(chunk_size = default_chunk_size) symbols =
  List.chunks_of symbols ~length:chunk_size
;;

let subscribe_frame symbols =
  Yojson.Safe.to_string
    (`Assoc
        [ "op", `String "subscribe"
        ; "args", `List (List.map symbols ~f:(fun s -> `String ("tickers." ^ s)))
        ])
;;

let subscribe_frames ?chunk_size symbols =
  chunk_symbols ?chunk_size symbols |> List.map ~f:subscribe_frame
;;

let ping_frame = Yojson.Safe.to_string (`Assoc [ "op", `String "ping" ])

module Io = struct
  open Async

  (* Bybit's v5 public stream allows roughly 10 subscribe ops per
     connection per second. Staggering by 10ms keeps us safely below
     that even when subscribing to thousands of symbols at reconnect.

     The 20s ping interval matches Bybit's recommendation; the server
     will close idle connections after 30s of silence. *)
  let subscribe_stagger = Time_float.Span.of_ms 10.

  let ping_interval = Time_float.Span.of_sec 20.
  let reconnect_delay = Time_float.Span.of_sec 5.

  let send_subscribes writer frames =
    Deferred.List.iter frames ~how:`Sequential ~f:(fun frame ->
      let%bind () = Pipe.write writer frame in
      Clock.after subscribe_stagger)
  ;;

  let ping_loop writer =
    let rec go () =
      match%bind Clock.with_timeout ping_interval (Pipe.closed writer) with
      | `Result () -> return ()
      | `Timeout ->
        if Pipe.is_closed writer
        then return ()
        else (
          let%bind () = Pipe.write writer ping_frame in
          go ())
    in
    go ()
  ;;

  let handle_frames ~exchange ~on_update reader =
    Pipe.iter reader ~f:(fun frame ->
      match parse_ticker_frame ~exchange frame with
      | None -> return ()
      | Some (symbol, partial) -> on_update ~symbol partial)
  ;;

  (* Bybit's WS edge requires an explicit SNI hostname on the TLS
     handshake — async_ssl's default SNI derivation trips the server's
     handshake_failure alert. Passing the host explicitly is the
     documented workaround.

     [Monitor.try_with] catches any remaining async_ssl / network
     exceptions that [Client.create]'s own Or_error plumbing doesn't
     wrap — they escape on a separate monitor and would crash the
     scheduler otherwise. *)
  let run_once ~exchange ~symbols ~on_update =
    let uri = ws_url exchange in
    let sni = Uri.host uri |> Option.value ~default:"" in
    match%bind
      Monitor.try_with (fun () ->
        Cohttp_async_websocket.Client.create
          ~force_ssl_overriding_SNI_hostname:sni
          ~opcode:`Text
          uri)
    with
    | Error exn -> return (Or_error.of_exn exn)
    | Ok (Error e) -> return (Error e)
    | Ok (Ok (_resp, ws)) ->
      let reader, writer = Websocket.pipes ws in
      don't_wait_for (ping_loop writer);
      Deferred.Or_error.try_with (fun () ->
        let%bind () = send_subscribes writer (subscribe_frames symbols) in
        handle_frames ~exchange ~on_update reader)
  ;;

  let run_forever ~exchange ~symbols ~on_update =
    let rec go () =
      let%bind result = run_once ~exchange ~symbols ~on_update in
      (match result with
       | Ok () -> ()
       | Error e ->
         eprintf
           "bybit ws [%s] disconnected: %s\n%!"
           (Exchange.to_string exchange)
           (Error.to_string_hum e));
      let%bind () = Clock.after reconnect_delay in
      go ()
    in
    go ()
  ;;
end

let run_once = Io.run_once
let run_forever = Io.run_forever
