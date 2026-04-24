open! Core
open! Async
open Crypto_publicdata

let start_adapter name start =
  match%bind Monitor.try_with start with
  | Ok (Ok ()) -> return ()
  | Ok (Error e) ->
    eprintf "%s adapter startup failed: %s\n%!" name (Error.to_string_hum e);
    return ()
  | Error exn ->
    eprintf "%s adapter crashed at startup: %s\n%!" name (Exn.to_string exn);
    return ()
;;

let () =
  let aggregator = Aggregator.create () in
  don't_wait_for
    (start_adapter "bybit" (fun () ->
       Bybit_adapter.start ~exchange:Bybit ~aggregator));
  don't_wait_for
    (start_adapter "binance" (fun () ->
       Binance_adapter.start ~exchange:Binance ~aggregator));
  don't_wait_for
    (let%bind _server = Server.start ~aggregator () in
     Deferred.never ());
  never_returns (Scheduler.go ())
;;
