open! Core

(* Hyperliquid serves all perp mids on a single [allMids] subscription.
   Every frame is a full map of coin → mid; unchanged coins still get
   their mid reprinted, which downstream is deduplicated by the
   aggregator's "upsert returns changed" contract.

   The WS tier doesn't carry mark / funding / OI / volume — those stay
   as the REST snapshot wrote them, and the mid-only merge path
   preserves them. *)

let ws_url = Uri.of_string "wss://api.hyperliquid.xyz/ws"

let subscribe_all_mids_frame =
  Yojson.Safe.to_string
    (`Assoc
        [ "method", `String "subscribe"
        ; "subscription", `Assoc [ "type", `String "allMids" ]
        ])
;;

let ping_frame =
  Yojson.Safe.to_string (`Assoc [ "method", `String "ping" ])
;;

let parse_all_mids body : (string * float) list =
  match Yojson.Safe.from_string body with
  | exception Yojson.Json_error _ -> []
  | `Assoc _ as json ->
    (match Yojson.Safe.Util.member "channel" json with
     | `String "allMids" ->
       let data = Yojson.Safe.Util.member "data" json in
       (match Yojson.Safe.Util.member "mids" data with
        | `Assoc entries ->
          List.filter_map entries ~f:(fun (sym, v) ->
            match v with
            | `String s ->
              Option.map (Float.of_string_opt s) ~f:(fun f -> sym, f)
            | _ -> None)
        | _ -> [])
     | _ -> [])
  | _ -> []
;;

(* Merge a mid price into an existing ticker. Bid / ask / last / index
   all collapse onto the mid; mark stays as the REST snapshot wrote
   it since upstream doesn't re-serve mark on the mid feed. *)
let apply_mid (t : Ticker.t) mid =
  { t with bid = mid; ask = mid; last = mid; index = mid }
;;

module Io = struct
  open Async

  let ping_interval = Time_float.Span.of_sec 15.
  let reconnect_delay = Time_float.Span.of_sec 5.

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

  let handle_frames ~on_update reader =
    Pipe.iter reader ~f:(fun frame ->
      let pairs = parse_all_mids frame in
      Deferred.List.iter pairs ~how:`Sequential ~f:(fun (symbol, mid) ->
        on_update ~symbol mid))
  ;;

  let run_once ~on_update =
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
        let%bind () = Pipe.write writer subscribe_all_mids_frame in
        handle_frames ~on_update reader)
  ;;

  let run_forever ~on_update =
    let rec go () =
      let%bind result = run_once ~on_update in
      (match result with
       | Ok () -> ()
       | Error e ->
         eprintf
           "hyperliquid ws disconnected: %s\n%!"
           (Error.to_string_hum e));
      let%bind () = Clock.after reconnect_delay in
      go ()
    in
    go ()
  ;;
end

let run_forever = Io.run_forever
