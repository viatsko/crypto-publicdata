open! Core
open Async

(* REST snapshot populates the aggregator, then the WS stream layers
   partials over it. The three Binance streams ([!ticker@arr],
   [!bookTicker], [!markPrice@arr@1s]) each own a disjoint set of
   fields, so the merge order is irrelevant — whichever arrives first
   paints its fields, the others fill in theirs. *)
let start ~(exchange : Exchange.t) ~aggregator =
  let on_update ~symbol partial =
    let base =
      Aggregator.get aggregator ~exchange ~symbol
      |> Option.value ~default:Ticker.empty
    in
    let ticker = Binance_ws.Partial.apply base partial in
    let _changed : bool =
      Aggregator.upsert aggregator ~exchange ~symbol ~ticker
    in
    Deferred.return ()
  in
  let open Deferred.Or_error.Let_syntax in
  let%bind tickers = Binance_rest.fetch_tickers exchange in
  List.iter tickers ~f:(fun (symbol, ticker) ->
    let _changed : bool =
      Aggregator.upsert aggregator ~exchange ~symbol ~ticker
    in
    ());
  don't_wait_for (Binance_ws.run_forever ~exchange ~on_update);
  Deferred.Or_error.return ()
;;
