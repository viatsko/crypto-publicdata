open! Core
open Async

let start ~aggregator =
  let exchange = Exchange.Bitget in
  let on_update ~symbol partial =
    let base =
      Aggregator.get aggregator ~exchange ~symbol
      |> Option.value ~default:Ticker.empty
    in
    let ticker = Bitget_ws.Partial.apply base partial in
    let _changed : bool =
      Aggregator.upsert aggregator ~exchange ~symbol ~ticker
    in
    Deferred.return ()
  in
  let open Deferred.Or_error.Let_syntax in
  let%bind tickers = Bitget_rest.fetch_tickers () in
  List.iter tickers ~f:(fun (symbol, ticker) ->
    let _changed : bool =
      Aggregator.upsert aggregator ~exchange ~symbol ~ticker
    in
    ());
  let%bind symbols = Bitget_rest.fetch_trading_symbols () in
  don't_wait_for (Bitget_ws.run_forever ~symbols ~on_update);
  Deferred.Or_error.return ()
;;
