open! Core
open Crypto_publicdata

let hit_tickers_api ~aggregator ~query =
  let open Async in
  (* Bind to port 0 for an ephemeral port — avoids collision with a
     real service on 9880 or any other test running in parallel. *)
  let%bind server =
    Cohttp_async.Server.create_expert
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port 0)
      (Server.handler ~aggregator)
  in
  let port = Cohttp_async.Server.listening_on server in
  let uri = Uri.of_string (sprintf "http://localhost:%d/api/tickers%s" port query) in
  let%bind resp, body = Cohttp_async.Client.get uri in
  let%bind body = Cohttp_async.Body.to_string body in
  let%bind () = Cohttp_async.Server.close server in
  let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
  return (status, body)
;;

let%expect_test "GET /api/tickers serves aggregator state for the requested exchange" =
  let open Async in
  Thread_safe.block_on_async_exn (fun () ->
    let aggregator = Aggregator.create () in
    let _changed : bool =
      Aggregator.upsert
        aggregator
        ~exchange:Bybit
        ~symbol:"BTCUSDT"
        ~ticker:{ Ticker.empty with bid = 50000.; ask = 50001.; last = 50000.5 }
    in
    let _changed : bool =
      Aggregator.upsert
        aggregator
        ~exchange:Bybit
        ~symbol:"ETHUSDT"
        ~ticker:{ Ticker.empty with bid = 3000.; ask = 3001.; last = 3000.5 }
    in
    let%bind status, body = hit_tickers_api ~aggregator ~query:"?exchange=bybit" in
    printf "status=%d\n" status;
    print_endline
      (Yojson.Safe.pretty_to_string (Yojson.Safe.from_string body));
    return ());
  [%expect
    {|
    status=200
    {
      "tickers": {
        "BTCUSDT": { "b": 50000.0, "a": 50001.0, "l": 50000.5 },
        "ETHUSDT": { "b": 3000.0, "a": 3001.0, "l": 3000.5 }
      }
    }
    |}]
;;

let%expect_test "GET /api/tickers without exchange param is 400" =
  let open Async in
  Thread_safe.block_on_async_exn (fun () ->
    let aggregator = Aggregator.create () in
    let%bind status, body = hit_tickers_api ~aggregator ~query:"" in
    printf "status=%d body=%s\n" status body;
    return ());
  [%expect {| status=400 body=missing exchange query parameter |}]
;;

let%expect_test "GET /api/tickers with unknown exchange is 400" =
  let open Async in
  Thread_safe.block_on_async_exn (fun () ->
    let aggregator = Aggregator.create () in
    let%bind status, body =
      hit_tickers_api ~aggregator ~query:"?exchange=nonesuch"
    in
    printf "status=%d body=%s\n" status body;
    return ());
  [%expect {| status=400 body=unknown exchange: nonesuch |}]
;;

let%expect_test "GET /api/tickers for unknown-but-valid exchange returns empty" =
  let open Async in
  Thread_safe.block_on_async_exn (fun () ->
    let aggregator = Aggregator.create () in
    (* Exchange valid but no tickers stored for it yet. *)
    let%bind status, body =
      hit_tickers_api ~aggregator ~query:"?exchange=bybit"
    in
    printf "status=%d body=%s\n" status body;
    return ());
  [%expect {| status=200 body={"tickers":{}} |}]
;;
