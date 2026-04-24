open! Core

let ws_url = Uri.of_string "wss://ws.bitget.com/v2/ws/public"
let product_type = "USDT-FUTURES"
let chunk_size = 50

(* Bitget wants one arg object per symbol in a subscribe frame. Chunk
   50 per message to stay within the server's per-frame size budget. *)
let subscribe_frame symbols =
  Yojson.Safe.to_string
    (`Assoc
        [ "op", `String "subscribe"
        ; ( "args"
          , `List
              (List.map symbols ~f:(fun s ->
                 `Assoc
                   [ "instType", `String product_type
                   ; "channel", `String "ticker"
                   ; "instId", `String s
                   ])) )
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

let partial_of_ticker_row json =
  match string_opt_field json "instId" with
  | None -> None
  | Some sym ->
    let p =
      { Partial.bid = float_field json "bidPr"
      ; ask = float_field json "askPr"
      ; last = float_field json "lastPr"
      ; mark = float_field json "markPrice"
      ; index = float_field json "indexPrice"
      ; percentage =
          string_opt_field json "change24h"
          |> Option.bind ~f:Float.of_string_opt
          |> Option.map ~f:(fun p -> p *. 100.)
      ; open_interest = float_field json "holdingAmount"
      ; funding_rate = float_field json "fundingRate"
      ; funding_time = int_field json "fundingTime"
      ; volume = float_field json "baseVolume"
      ; quote_volume = float_field json "quoteVolume"
      }
    in
    Some (sym, p)
;;

(* Bitget pushes frames shaped as [{"action":"snapshot",
   "arg":{"channel":"ticker",...}, "data":[...]}]. The [data] list can
   have one or many rows depending on batching. [arg.channel] routes
   us to ticker payloads. *)
let parse_frame body : (string * Partial.t) list =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error _ -> []
  | `Assoc _ as json ->
    let arg = Yojson.Safe.Util.member "arg" json in
    let channel =
      match Yojson.Safe.Util.member "channel" arg with
      | `String s -> s
      | _ -> ""
    in
    if not (String.equal channel "ticker")
    then []
    else (
      match Yojson.Safe.Util.member "data" json with
      | `List items -> List.filter_map items ~f:partial_of_ticker_row
      | _ -> [])
  | _ -> []
;;

module Io = struct
  open Async

  let subscribe_stagger = Time_float.Span.of_ms 10.
  let ping_interval = Time_float.Span.of_sec 20.
  let reconnect_delay = Time_float.Span.of_sec 5.

  let send_subscribes writer frames =
    Deferred.List.iter frames ~how:`Sequential ~f:(fun frame ->
      let%bind () = Pipe.write writer frame in
      Clock.after subscribe_stagger)
  ;;

  (* Bitget pings with a literal [ping] string (not JSON) and replies
     with literal [pong]. Keep the loop aligned. *)
  let ping_loop writer =
    let rec go () =
      match%bind Clock.with_timeout ping_interval (Pipe.closed writer) with
      | `Result () -> return ()
      | `Timeout ->
        if Pipe.is_closed writer
        then return ()
        else (
          let%bind () = Pipe.write writer "ping" in
          go ())
    in
    go ()
  ;;

  let handle_frames ~on_update reader =
    Pipe.iter reader ~f:(fun frame ->
      let pairs = parse_frame frame in
      Deferred.List.iter pairs ~how:`Sequential ~f:(fun (symbol, partial) ->
        on_update ~symbol partial))
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
         eprintf "bitget ws disconnected: %s\n%!" (Error.to_string_hum e));
      let%bind () = Clock.after reconnect_delay in
      go ()
    in
    go ()
  ;;
end

let run_forever = Io.run_forever
