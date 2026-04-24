open! Core
open! Async
open Crypto_publicdata

let start_bybit_adapter ~aggregator =
  (* Monitor.try_with catches exceptions that escape the adapter's own
     Or_error plumbing (e.g. an SSL handshake failure thrown out of the
     cohttp-async Deferred) so a broken network doesn't crash the
     service — we just log and serve empty state until the next retry. *)
  match%bind
    Monitor.try_with (fun () ->
      Bybit_adapter.start ~exchange:Bybit ~aggregator)
  with
  | Ok (Ok ()) -> return ()
  | Ok (Error e) ->
    eprintf "bybit adapter startup failed: %s\n%!" (Error.to_string_hum e);
    return ()
  | Error exn ->
    eprintf "bybit adapter crashed at startup: %s\n%!" (Exn.to_string exn);
    return ()
;;

let () =
  let aggregator = Aggregator.create () in
  don't_wait_for (start_bybit_adapter ~aggregator);
  don't_wait_for
    (let%bind _server = Server.start ~aggregator () in
     Deferred.never ());
  never_returns (Scheduler.go ())
;;
