open! Core

(* Single-ws endpoint per category. Subscribe happens via an in-band
   [{"method":"SUBSCRIBE","params":[...]}] frame after the connection
   opens — Binance's /stream?streams=... URL form has finicky quoting
   rules and sometimes rejects multi-stream combinations here that
   work via the subscribe-frame path. *)
let ws_url : Exchange.t -> Uri.t = function
  | Binance -> Uri.of_string "wss://fstream.binance.com/ws"
  | Binance_spot -> Uri.of_string "wss://stream.binance.com/ws"
  | Binance_inverse -> Uri.of_string "wss://dstream.binance.com/ws"
  | Bybit | Bybit_spot | Bybit_inverse | Hyperliquid | Hyperliquid_spot ->
    failwith "Binance_ws.ws_url: not a binance variant"
;;

(* Streams subscribed to once the WS is open. Futures and inverse need
   three kinds; spot fields are all in the 24h ticker stream. *)
let streams_for : Exchange.t -> string list = function
  | Binance | Binance_inverse ->
    [ "!ticker@arr"; "!bookTicker"; "!markPrice@arr@1s" ]
  | Binance_spot -> [ "!ticker@arr" ]
  | Bybit | Bybit_spot | Bybit_inverse | Hyperliquid | Hyperliquid_spot ->
    failwith "Binance_ws.streams_for: not a binance variant"
;;

let subscribe_frame streams =
  Yojson.Safe.to_string
    (`Assoc
        [ "method", `String "SUBSCRIBE"
        ; "params", `List (List.map streams ~f:(fun s -> `String s))
        ; "id", `Int 1
        ])
;;

module Partial = struct
  (* Per-field change from a Binance WS frame. Each stream only
     populates the fields it owns — bookTicker touches bid/ask,
     markPrice touches mark/index/funding, the 24h ticker touches
     last/percentage/volume/quote_volume (and bid/ask on spot).

     [apply] merges into an existing Ticker.t with non-None-overwrites
     semantics so a bookTicker frame for BTCUSDT doesn't wipe its
     last-known mark. *)
  type t =
    { bid : float option
    ; ask : float option
    ; last : float option
    ; mark : float option
    ; index : float option
    ; percentage : float option
    ; funding_rate : float option
    ; funding_time : int option
    ; volume : float option
    ; quote_volume : float option
    }
  [@@deriving sexp, equal]

  let empty =
    { bid = None
    ; ask = None
    ; last = None
    ; mark = None
    ; index = None
    ; percentage = None
    ; funding_rate = None
    ; funding_time = None
    ; volume = None
    ; quote_volume = None
    }
  ;;

  let apply (t : Ticker.t) p =
    { Ticker.bid = Option.value p.bid ~default:t.bid
    ; ask = Option.value p.ask ~default:t.ask
    ; last = Option.value p.last ~default:t.last
    ; mark = Option.value p.mark ~default:t.mark
    ; index = Option.value p.index ~default:t.index
    ; percentage = Option.value p.percentage ~default:t.percentage
    ; open_interest = t.open_interest (* WS doesn't carry OI for Binance *)
    ; funding_rate = Option.value p.funding_rate ~default:t.funding_rate
    ; funding_time = Option.value p.funding_time ~default:t.funding_time
    ; volume = Option.value p.volume ~default:t.volume
    ; quote_volume = Option.value p.quote_volume ~default:t.quote_volume
    }
  ;;
end

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
  match Yojson.Safe.Util.member field json with
  | `Int i -> Some i
  | `Intlit s -> Int.of_string_opt s
  | `Float f -> Some (Int.of_float f)
  | _ -> None
;;

(* Binance serialises symbols as strings; empty means we don't have a
   useful row. *)
let symbol_of json =
  match Yojson.Safe.Util.member "s" json with
  | `String "" -> None
  | `String s -> Some s
  | _ -> None
;;

let partial_of_ticker json =
  match symbol_of json with
  | None -> None
  | Some sym ->
    let p =
      { Partial.empty with
        last = float_field json "c"
      ; percentage = float_field json "P"
      ; volume = float_field json "v"
      ; quote_volume = float_field json "q"
      ; bid = float_field json "b"
      ; ask = float_field json "a"
      }
    in
    Some (sym, p)
;;

let partial_of_book_ticker json =
  match symbol_of json with
  | None -> None
  | Some sym ->
    let p =
      { Partial.empty with
        bid = float_field json "b"
      ; ask = float_field json "a"
      }
    in
    Some (sym, p)
;;

let partial_of_mark_price json =
  match symbol_of json with
  | None -> None
  | Some sym ->
    let p =
      { Partial.empty with
        mark = float_field json "p"
      ; index = float_field json "i"
      ; funding_rate = float_field json "r"
      ; funding_time = int_field json "T"
      }
    in
    Some (sym, p)
;;

(* Dispatch per [e] event type. [!ticker@arr] and [!markPrice@arr@1s]
   deliver arrays of events; [!bookTicker] delivers a single event per
   message. Subscribe acknowledgements ({result:null,id:1}) lack [e]
   and are silently dropped. *)
let dispatch_event json =
  match string_opt_field json "e" with
  | Some "24hrTicker" -> partial_of_ticker json
  | Some "bookTicker" -> partial_of_book_ticker json
  | Some "markPriceUpdate" -> partial_of_mark_price json
  | Some _ | None -> None
;;

let parse_frame body : (string * Partial.t) list =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error _ -> []
  | `List items -> List.filter_map items ~f:dispatch_event
  | `Assoc _ as json ->
    (match dispatch_event json with
     | Some x -> [ x ]
     | None -> [])
  | _ -> []
;;

module Io = struct
  open Async

  let ping_interval = Time_float.Span.of_sec 20.
  let reconnect_delay = Time_float.Span.of_sec 5.

  let ping_loop writer =
    let rec go () =
      match%bind Clock.with_timeout ping_interval (Pipe.closed writer) with
      | `Result () -> return ()
      | `Timeout ->
        if Pipe.is_closed writer
        then return ()
        else (
          let%bind () =
            Pipe.write
              writer
              (Yojson.Safe.to_string (`Assoc [ "method", `String "PING" ]))
          in
          go ())
    in
    go ()
  ;;

  let handle_frames ~on_update reader =
    Pipe.iter reader ~f:(fun frame ->
      let updates = parse_frame frame in
      Deferred.List.iter updates ~how:`Sequential ~f:(fun (symbol, partial) ->
        on_update ~symbol partial))
  ;;

  let run_once ~exchange ~on_update =
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
        let%bind () =
          Pipe.write writer (subscribe_frame (streams_for exchange))
        in
        handle_frames ~on_update reader)
  ;;

  let run_forever ~exchange ~on_update =
    let rec go () =
      let%bind result = run_once ~exchange ~on_update in
      (match result with
       | Ok () -> ()
       | Error e ->
         eprintf
           "binance ws [%s] disconnected: %s\n%!"
           (Exchange.to_string exchange)
           (Error.to_string_hum e));
      let%bind () = Clock.after reconnect_delay in
      go ()
    in
    go ()
  ;;
end

let run_forever = Io.run_forever
