open! Core
open Crypto_publicdata

let show body =
  match Coinbase_rest.tickers_of_body body with
  | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e)
  | Ok tickers ->
    List.iter tickers ~f:(fun (sym, t) ->
      print_s [%sexp (sym : string), (t : Ticker.t)])
;;

let body_ok =
  {|[
    { "symbol": "BTC-PERP", "trading_state": "TRADING", "type": "PERP",
      "open_interest": "100", "qty_24hr": "5000", "notional_24hr": "250000000",
      "quote": { "best_bid_price": "50000", "best_ask_price": "50001",
                 "trade_price": "50000.5", "mark_price": "50000.3",
                 "index_price": "50000.2",
                 "predicted_funding": "0.0001" } },
    { "symbol": "ETH-PERP", "trading_state": "TRADING", "type": "PERP",
      "open_interest": "50", "qty_24hr": "1000", "notional_24hr": "3000000",
      "quote": { "best_bid_price": "3000", "best_ask_price": "3001",
                 "trade_price": "3000.5", "mark_price": "3000.3",
                 "predicted_funding": "0.0002" } },
    { "symbol": "SOL-USD", "trading_state": "TRADING", "type": "SPOT",
      "quote": { "best_bid_price": "100" } },
    { "symbol": "OLD-PERP", "trading_state": "EXPIRED", "type": "PERP" }
  ]|}
;;

let%expect_test "keep only PERP + TRADING instruments, map every quote field" =
  show body_ok;
  [%expect
    {|
    (BTC-PERP
     ((bid 50000) (ask 50001) (last 50000.5) (mark 50000.3) (index 50000.2)
      (percentage 0) (open_interest 100) (funding_rate 0.0001) (funding_time 0)
      (volume 5000) (quote_volume 250000000)))
    (ETH-PERP
     ((bid 3000) (ask 3001) (last 3000.5) (mark 3000.3) (index 0) (percentage 0)
      (open_interest 50) (funding_rate 0.0002) (funding_time 0) (volume 1000)
      (quote_volume 3000000)))
    |}]
;;

let%expect_test "trading_symbols_of_body filters the same way" =
  (match Coinbase_rest.trading_symbols_of_body body_ok with
   | Ok syms -> List.iter syms ~f:print_endline
   | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e));
  [%expect
    {|
    BTC-PERP
    ETH-PERP
    |}]
;;

let%expect_test "non-array body surfaces as Or_error" =
  (match Coinbase_rest.tickers_of_body "{}" with
   | Ok _ -> print_endline "unexpected ok"
   | Error e -> print_endline (Error.to_string_hum e));
  [%expect {| expected top-level array of instruments |}]
;;
