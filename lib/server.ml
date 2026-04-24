open! Core
open Async

let default_port = 9880

(* Build a stable, sorted snapshot of the aggregator for a client. If
   the client supplied no exchange filter we take everything currently
   in the aggregator; if they supplied an explicit list we use it as-is
   (including zero-entry exchanges — those just serialise to an empty
   map, which is a useful "this exchange exists but is empty" signal). *)
let snapshot_data ~aggregator ~exchanges =
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
    in
    exchange, tickers)
;;

let handle_incoming ~aggregator msg : Protocol.Outgoing.t =
  match Protocol.Incoming.of_text msg with
  | Error e -> Error (Error.to_string_hum e)
  | Ok Ping -> Pong
  | Ok (Unsubscribe _) -> Error "unsubscribe not implemented yet"
  | Ok (Subscribe { channel; exchanges }) ->
    if String.equal channel "tickers"
    then Snapshot { channel; data = snapshot_data ~aggregator ~exchanges }
    else Error (sprintf "unknown channel: %s" channel)
;;

let handle_messages ~aggregator ~input ~output =
  let%bind () = Pipe.write output (Protocol.Outgoing.to_string Connected) in
  let%bind () =
    Pipe.iter input ~f:(fun msg ->
      let reply = handle_incoming ~aggregator msg in
      Pipe.write output (Protocol.Outgoing.to_string reply))
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
