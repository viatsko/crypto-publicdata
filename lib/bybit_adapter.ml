open! Core
open Async

(* Startup path: fetch the REST snapshot so the aggregator is populated
   before the first WS frame lands, then subscribe to tickers for every
   Trading symbol and stream incremental updates. Sparse WS frames merge
   into whatever the REST snapshot wrote earlier — a first delta for a
   symbol we already have won't wipe its non-updated fields.

   Returns [Ok ()] once the REST snapshot is in and the WS loop is
   running in the background; REST/WS errors after that are handled by
   the WS loop's reconnect machinery and don't bubble back here. *)
let start ~(exchange : Exchange.t) ~aggregator =
  let on_update ~symbol partial =
    let base =
      Aggregator.get aggregator ~exchange ~symbol
      |> Option.value ~default:Ticker.empty
    in
    let ticker = Bybit_ws.Partial.apply ~exchange base partial in
    let _changed : bool =
      Aggregator.upsert aggregator ~exchange ~symbol ~ticker
    in
    Deferred.return ()
  in
  let open Deferred.Or_error.Let_syntax in
  let%bind tickers = Bybit_rest.fetch_tickers exchange in
  List.iter tickers ~f:(fun (symbol, ticker) ->
    let _changed : bool =
      Aggregator.upsert aggregator ~exchange ~symbol ~ticker
    in
    ());
  let%bind symbols = Bybit_rest.fetch_trading_symbols exchange in
  don't_wait_for (Bybit_ws.run_forever ~exchange ~symbols ~on_update);
  Deferred.Or_error.return ()
;;
