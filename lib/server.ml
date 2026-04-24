open! Core
open Async

let default_port = 9880

(* Build a stable, sorted snapshot for a client subscribe. No filter =>
   every exchange the aggregator currently carries; explicit list =>
   use that (including zero-entry exchanges, which serialise to an
   empty map — a useful "this exchange exists but is empty" signal
   versus an exchange the client didn't ask for at all).

   [~include_index] runs every ticker through [Ticker.apply_subscribe_opts]
   so the frame we emit and the copy we seed into the shadow agree on
   what the client has seen — otherwise a subsequent delta would think
   [index] changed on the first tick for every symbol. *)
let snapshot_data ~aggregator ~exchanges ~include_index =
  let selected =
    match exchanges with
    | None -> Aggregator.exchanges aggregator
    | Some xs -> xs
  in
  selected
  |> List.sort ~compare:Exchange.compare
  |> List.map ~f:(fun exchange ->
    let tickers =
      Aggregator.snapshot_exchange aggregator exchange
      |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
      |> List.map ~f:(fun (sym, t) ->
        sym, Ticker.apply_subscribe_opts ~include_index t)
    in
    exchange, tickers)
;;

let initial_snapshot ~aggregator ~exchanges ~include_index ~shadow =
  let snap = snapshot_data ~aggregator ~exchanges ~include_index in
  Shadow.seed shadow ~snapshot:snap;
  snap
;;

let handle_incoming ~aggregator msg : Protocol.Outgoing.t =
  match Protocol.Incoming.of_text msg with
  | Error e -> Error (Error.to_string_hum e)
  | Ok Ping -> Pong
  | Ok (Unsubscribe _) -> Error "unsubscribe not implemented yet"
  | Ok (Subscribe { channel; exchanges; rate = _; include_index }) ->
    if String.equal channel "tickers"
    then (
      let shadow = Shadow.create () in
      let data = initial_snapshot ~aggregator ~exchanges ~include_index ~shadow in
      Snapshot { channel; data })
    else Error (sprintf "unknown channel: %s" channel)
;;

(* Background tick loop per subscribed connection. Emits a Delta frame
   every [rate] seconds *if* something actually changed; empty ticks
   produce no wire traffic. Exits when the output pipe closes (i.e.
   client disconnected or handler tore down). *)
let run_tick_loop
      ~aggregator
      ~exchanges_filter
      ~include_index
      ~shadow
      ~output
      ~rate
  =
  let span = Time_float.Span.of_sec rate in
  let rec loop () =
    if Pipe.is_closed output
    then return ()
    else (
      let%bind () = Clock.after span in
      if Pipe.is_closed output
      then return ()
      else (
        let data, removed =
          Shadow.advance shadow ~aggregator ~exchanges_filter ~include_index
        in
        if List.is_empty data && List.is_empty removed
        then loop ()
        else (
          let%bind () =
            Pipe.write
              output
              (Protocol.Outgoing.to_string
                 (Delta { channel = "tickers"; data; removed }))
          in
          loop ())))
  in
  loop ()
;;

let handle_messages ~aggregator ~input ~output =
  let%bind () = Pipe.write output (Protocol.Outgoing.to_string Connected) in
  let subscribed = ref false in
  let%bind () =
    Pipe.iter input ~f:(fun msg ->
      match Protocol.Incoming.of_text msg with
      | Error e ->
        Pipe.write
          output
          (Protocol.Outgoing.to_string (Error (Error.to_string_hum e)))
      | Ok Ping -> Pipe.write output (Protocol.Outgoing.to_string Pong)
      | Ok (Unsubscribe _) ->
        Pipe.write
          output
          (Protocol.Outgoing.to_string
             (Error "unsubscribe not implemented yet"))
      | Ok (Subscribe { channel; exchanges; rate; include_index }) ->
        if !subscribed
        then
          Pipe.write
            output
            (Protocol.Outgoing.to_string
               (Error "already subscribed; disconnect to resubscribe"))
        else if not (String.equal channel "tickers")
        then
          Pipe.write
            output
            (Protocol.Outgoing.to_string
               (Error (sprintf "unknown channel: %s" channel)))
        else (
          subscribed := true;
          let shadow = Shadow.create () in
          let data =
            initial_snapshot ~aggregator ~exchanges ~include_index ~shadow
          in
          let%bind () =
            Pipe.write
              output
              (Protocol.Outgoing.to_string (Snapshot { channel; data }))
          in
          don't_wait_for
            (run_tick_loop
               ~aggregator
               ~exchanges_filter:exchanges
               ~include_index
               ~shadow
               ~output
               ~rate);
          return ()))
  in
  Pipe.close output;
  return ()
;;

let run_websocket ~aggregator websocket =
  let input, output = Websocket.pipes websocket in
  handle_messages ~aggregator ~input ~output
;;

let websocket_handler ~aggregator ~inet:_ ~subprotocol:_ (_req : Cohttp.Request.t) =
  return
    (Cohttp_async_websocket.Server.On_connection.create (run_websocket ~aggregator))
;;

let json_headers =
  Cohttp.Header.init_with "content-type" "application/json; charset=utf-8"
;;

let respond_json ?(status = `OK) body =
  Cohttp_async.Server.respond_string ~status ~headers:json_headers body
;;

let serve_tickers ~aggregator req =
  let uri = Cohttp.Request.uri req in
  match Uri.get_query_param uri "exchange" with
  | None ->
    Cohttp_async.Server.respond_string
      ~status:`Bad_request
      "missing exchange query parameter"
  | Some name ->
    (match Exchange.of_string name with
     | None ->
       Cohttp_async.Server.respond_string
         ~status:`Bad_request
         (sprintf "unknown exchange: %s" name)
     | Some exchange ->
       let tickers = Aggregator.snapshot_exchange aggregator exchange in
       respond_json
         (Yojson.Safe.to_string (Ticker_json.tickers_response tickers)))
;;

let non_ws_handler ~aggregator ~body:_ _addr req =
  match Uri.path (Cohttp.Request.uri req) with
  | "/health" -> Cohttp_async.Server.respond_string ~status:`OK "ok"
  | "/api/tickers" -> serve_tickers ~aggregator req
  | _ -> Cohttp_async.Server.respond_string ~status:`Not_found "not found"
;;

let accept_any_origin _addr _header ~is_websocket_request:_ = Or_error.return ()

let handler ~aggregator =
  Cohttp_async_websocket.Server.create
    ~non_ws_request:(non_ws_handler ~aggregator)
    ~opcode:`Text
    ~should_process_request:accept_any_origin
    (websocket_handler ~aggregator)
;;

let start ?(port = default_port) ~aggregator () =
  let%bind server =
    Cohttp_async.Server.create_expert
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (handler ~aggregator)
  in
  let bound_port = Cohttp_async.Server.listening_on server in
  printf "listening on :%d\n%!" bound_port;
  return server
;;
