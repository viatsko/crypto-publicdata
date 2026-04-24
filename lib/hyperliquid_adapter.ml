open! Core
open Async

(* Perp adapter: REST snapshot populates mark / funding / OI / volume /
   prev-day-based percentage, then the allMids WS keeps mid-derived
   bid / ask / last / index fresh.

   Spot support is REST-only for now — the allMids WS doesn't serve
   spot tokens without extra plumbing (HIP-3 dex argument, @-indexed
   symbol resolution). Wire it as a periodic REST refresh when
   revisiting; for now start/stop on Hyperliquid_spot is a no-op WS
   over the same perp stream and the adapter just serves the initial
   REST snapshot. *)
let start ~(exchange : Exchange.t) ~aggregator =
  let on_update ~symbol mid =
    let base =
      Aggregator.get aggregator ~exchange ~symbol
      |> Option.value ~default:Ticker.empty
    in
    let ticker = Hyperliquid_ws.apply_mid base mid in
    let _changed : bool =
      Aggregator.upsert aggregator ~exchange ~symbol ~ticker
    in
    Deferred.return ()
  in
  let open Deferred.Or_error.Let_syntax in
  let%bind tickers = Hyperliquid_rest.fetch_tickers exchange in
  List.iter tickers ~f:(fun (symbol, ticker) ->
    let _changed : bool =
      Aggregator.upsert aggregator ~exchange ~symbol ~ticker
    in
    ());
  (match (exchange : Exchange.t) with
   | Hyperliquid -> don't_wait_for (Hyperliquid_ws.run_forever ~on_update)
   | Hyperliquid_spot ->
     () (* REST only until we wire a spot-capable WS path. *)
   | Bybit | Bybit_spot | Bybit_inverse | Binance | Binance_spot | Binance_inverse
   | Bitget | Coinbase | Kraken ->
     failwith "Hyperliquid_adapter.start: not a hyperliquid variant");
  Deferred.Or_error.return ()
;;
