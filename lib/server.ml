open! Core
open Async

let default_port = 9880

let handle_messages ~input ~output =
  let%bind () =
    Pipe.iter input ~f:(fun msg ->
      Pipe.write output (Protocol.(Outgoing.to_string (response_of_text msg))))
  in
  Pipe.close output;
  return ()
;;

let run_websocket websocket =
  let input, output = Websocket.pipes websocket in
  handle_messages ~input ~output
;;

let websocket_handler ~inet:_ ~subprotocol:_ (_req : Cohttp.Request.t) =
  return (Cohttp_async_websocket.Server.On_connection.create run_websocket)
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
    websocket_handler
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
