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

let non_ws_handler ~body:_ _addr req =
  match Uri.path (Cohttp.Request.uri req) with
  | "/health" -> Cohttp_async.Server.respond_string ~status:`OK "ok"
  | _ -> Cohttp_async.Server.respond_string ~status:`Not_found "not found"
;;

let accept_any_origin _addr _header ~is_websocket_request:_ = Or_error.return ()

let handler =
  Cohttp_async_websocket.Server.create
    ~non_ws_request:non_ws_handler
    ~opcode:`Text
    ~should_process_request:accept_any_origin
    websocket_handler
;;

let start ?(port = default_port) () =
  let%bind _server =
    Cohttp_async.Server.create_expert
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      handler
  in
  printf "listening on :%d\n%!" port;
  Deferred.never ()
;;
