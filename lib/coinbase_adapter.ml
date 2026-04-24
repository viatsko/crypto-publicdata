open! Core
open Async

(* Coinbase INTX has no public WebSocket, so this adapter polls the
   instruments endpoint on a fixed interval and overwrites the
   aggregator with whatever came back. 5-second cadence keeps bid /
   ask / mark reasonably fresh without hammering the upstream. *)

let poll_interval = Time_float.Span.of_sec 5.

let apply_snapshot ~aggregator tickers =
  List.iter tickers ~f:(fun (symbol, ticker) ->
    let _changed : bool =
      Aggregator.upsert aggregator ~exchange:Coinbase ~symbol ~ticker
    in
    ())
;;

let poll_loop ~aggregator =
  let rec go () =
    (match%bind Coinbase_rest.fetch_tickers () with
     | Ok tickers ->
       apply_snapshot ~aggregator tickers;
       return ()
     | Error e ->
       eprintf "coinbase poll failed: %s\n%!" (Error.to_string_hum e);
       return ())
    >>= fun () ->
    Clock.after poll_interval >>= go
  in
  go ()
;;

let start ~aggregator =
  let open Deferred.Or_error.Let_syntax in
  let%bind tickers = Coinbase_rest.fetch_tickers () in
  apply_snapshot ~aggregator tickers;
  don't_wait_for (poll_loop ~aggregator);
  Deferred.Or_error.return ()
;;
