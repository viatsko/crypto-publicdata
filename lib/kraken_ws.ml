open! Core

let ws_url = Uri.of_string "wss://futures.kraken.com/ws/v1"
let chunk_size = 50

let subscribe_frame product_ids =
  Yojson.Safe.to_string
    (`Assoc
        [ "event", `String "subscribe"
        ; "feed", `String "ticker"
        ; ( "product_ids"
          , `List (List.map product_ids ~f:(fun s -> `String s)) )
        ])
;;

let subscribe_frames ?(chunk_size = chunk_size) symbols =
  List.chunks_of symbols ~length:chunk_size |> List.map ~f:subscribe_frame
;;

module Partial = struct
  type t =
    { bid : float option
    ; ask : float option
    ; last : float option
    ; mark : float option
    ; index : float option
    ; percentage : float option
    ; open_interest : float option
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
    ; open_interest = None
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
    ; open_interest = Option.value p.open_interest ~default:t.open_interest
    ; funding_rate = Option.value p.funding_rate ~default:t.funding_rate
    ; funding_time = Option.value p.funding_time ~default:t.funding_time
    ; volume = Option.value p.volume ~default:t.volume
    ; quote_volume = Option.value p.quote_volume ~default:t.quote_volume
    }
  ;;
end

let string_of_field json field =
  match Yojson.Safe.Util.member field json with
  | `String s -> Some s
  | _ -> None
;;

let float_of_field json field =
  match Yojson.Safe.Util.member field json with
  | `Float f -> Some f
  | `Int i -> Some (Float.of_int i)
  | `String s -> Float.of_string_opt s
  | _ -> None
;;

let int_of_field json field =
  match Yojson.Safe.Util.member field json with
  | `Int i -> Some i
  | `Float f -> Some (Int.of_float f)
  | `String s -> Int.of_string_opt s
  | _ -> None
;;

(* [ticker] and [ticker_snapshot] feeds carry the same fields.
   [relative_funding_rate] is already fractional, so the WS merge
   overwrites the coarse REST estimate with the correct value.
   Heartbeats and subscription responses (feed ∈ {heartbeat,
   subscribed, unsubscribed, info}) produce nothing. *)
let parse_frame body : (string * Partial.t) option =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error _ -> None
  | `Assoc _ as json ->
    let feed =
      Option.value (string_of_field json "feed") ~default:""
    in
    (match feed with
     | "ticker" | "ticker_snapshot" ->
       (match string_of_field json "product_id" with
        | None | Some "" -> None
        | Some sym ->
          let p =
            { Partial.bid = float_of_field json "bid"
            ; ask = float_of_field json "ask"
            ; last = float_of_field json "last"
            ; mark = float_of_field json "mark_price"
            ; index = float_of_field json "index"
            ; percentage = float_of_field json "change"
            ; open_interest = float_of_field json "openInterest"
            ; funding_rate = float_of_field json "relative_funding_rate"
            ; funding_time = int_of_field json "next_funding_rate_time"
            ; volume = float_of_field json "volume"
            ; quote_volume = float_of_field json "volumeQuote"
            }
          in
          Some (sym, p))
     | _ -> None)
  | _ -> None
;;

module Io = struct
  open Async

  let subscribe_stagger = Time_float.Span.of_ms 10.
  let ping_interval = Time_float.Span.of_sec 15.
  let reconnect_delay = Time_float.Span.of_sec 5.

  let send_subscribes writer frames =
    Deferred.List.iter frames ~how:`Sequential ~f:(fun frame ->
      let%bind () = Pipe.write writer frame in
      Clock.after subscribe_stagger)
  ;;

  (* Kraken uses [challenge]/pong conventions on signed private feeds;
     public-ticker connections just need to stay busy. We send a noop
     [{"event":"ping"}] frame — the server either ignores or replies
     with [event: pong], both of which flow through the reader without
     affecting parse_frame (it only dispatches on ticker feeds). *)
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
              (Yojson.Safe.to_string (`Assoc [ "event", `String "ping" ]))
          in
          go ())
    in
    go ()
  ;;

  let handle_frames ~on_update reader =
    Pipe.iter reader ~f:(fun frame ->
      match parse_frame frame with
      | None -> return ()
      | Some (symbol, partial) -> on_update ~symbol partial)
  ;;

  let run_once ~symbols ~on_update =
    let sni = Uri.host ws_url |> Option.value ~default:"" in
    match%bind
      Monitor.try_with (fun () ->
        Cohttp_async_websocket.Client.create
          ~force_ssl_overriding_SNI_hostname:sni
          ~opcode:`Text
          ws_url)
    with
    | Error exn -> return (Or_error.of_exn exn)
    | Ok (Error e) -> return (Error e)
    | Ok (Ok (_resp, ws)) ->
      let reader, writer = Websocket.pipes ws in
      don't_wait_for (ping_loop writer);
      Deferred.Or_error.try_with (fun () ->
        let%bind () = send_subscribes writer (subscribe_frames symbols) in
        handle_frames ~on_update reader)
  ;;

  let run_forever ~symbols ~on_update =
    let rec go () =
      let%bind result = run_once ~symbols ~on_update in
      (match result with
       | Ok () -> ()
       | Error e ->
         eprintf "kraken ws disconnected: %s\n%!" (Error.to_string_hum e));
      let%bind () = Clock.after reconnect_delay in
      go ()
    in
    go ()
  ;;
end

let run_forever = Io.run_forever
