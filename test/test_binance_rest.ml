open! Core
open Crypto_publicdata

let show_tickers ~exchange ~tickers_body ~premium_body =
  match
    Binance_rest.snapshot_of_bodies ~exchange ~tickers_body ~premium_body
  with
  | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e)
  | Ok rows ->
    List.iter rows ~f:(fun (sym, t) ->
      print_s [%sexp (sym : string), (t : Ticker.t)])
;;

let%expect_test "usdt-m futures: 24h + premiumIndex merge into one ticker" =
  let tickers_body =
    {|[ { "symbol": "BTCUSDT",
          "priceChangePercent": "1.23",
          "lastPrice": "50000.5",
          "bidPrice": "50000.0",
          "askPrice": "50001.0",
          "volume": "100",
          "quoteVolume": "5000025" } ]|}
  in
  let premium_body =
    {|[ { "symbol": "BTCUSDT",
          "markPrice": "50000.3",
          "indexPrice": "50000.2",
          "lastFundingRate": "0.0001",
          "nextFundingTime": "1775766154000" } ]|}
  in
  show_tickers
    ~exchange:Exchange.Binance
    ~tickers_body
    ~premium_body:(Some premium_body);
  [%expect
    {|
    (BTCUSDT
     ((bid 50000) (ask 50001) (last 50000.5) (mark 50000.3) (index 50000.2)
      (percentage 1.23) (open_interest 0) (funding_rate 0.0001)
      (funding_time 1775766154000) (volume 100) (quote_volume 5000025)))
    |}]
;;

let%expect_test "spot has no premium index; 24h fields carry through" =
  let tickers_body =
    {|[ { "symbol": "ETHUSDC",
          "priceChangePercent": "0.5",
          "lastPrice": "3000.5",
          "bidPrice": "3000",
          "askPrice": "3001",
          "volume": "100",
          "quoteVolume": "300050" } ]|}
  in
  show_tickers
    ~exchange:Exchange.Binance_spot
    ~tickers_body
    ~premium_body:None;
  [%expect
    {|
    (ETHUSDC
     ((bid 3000) (ask 3001) (last 3000.5) (mark 0) (index 0) (percentage 0.5)
      (open_interest 0) (funding_rate 0) (funding_time 0) (volume 100)
      (quote_volume 300050)))
    |}]
;;

let%expect_test "symbol in 24h but missing from premium keeps its base ticker" =
  let tickers_body =
    {|[ { "symbol": "NEWUSDT", "priceChangePercent": "0.1",
          "lastPrice": "1", "volume": "10", "quoteVolume": "10" } ]|}
  in
  let premium_body = {|[ ]|} in
  show_tickers
    ~exchange:Exchange.Binance
    ~tickers_body
    ~premium_body:(Some premium_body);
  [%expect
    {|
    (NEWUSDT
     ((bid 0) (ask 0) (last 1) (mark 0) (index 0) (percentage 0.1)
      (open_interest 0) (funding_rate 0) (funding_time 0) (volume 10)
      (quote_volume 10)))
    |}]
;;

let%expect_test "exchangeInfo keeps only TRADING + PERPETUAL USDT/USDC futures" =
  let body =
    {|{ "symbols": [
      { "symbol": "BTCUSDT", "status": "TRADING", "contractType": "PERPETUAL",
        "marginAsset": "USDT" },
      { "symbol": "BTCUSDT_250927", "status": "TRADING",
        "contractType": "CURRENT_QUARTER", "marginAsset": "USDT" },
      { "symbol": "ETHBUSD", "status": "TRADING", "contractType": "PERPETUAL",
        "marginAsset": "BUSD" },
      { "symbol": "ETHUSDC", "status": "TRADING", "contractType": "PERPETUAL",
        "marginAsset": "USDC" },
      { "symbol": "PAUSEDUSDT", "status": "PAUSED", "contractType": "PERPETUAL",
        "marginAsset": "USDT" } ] }|}
  in
  (match Binance_rest.trading_symbols_of_body ~exchange:Exchange.Binance body with
   | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e)
   | Ok syms -> List.iter syms ~f:print_endline);
  [%expect
    {|
    BTCUSDT
    ETHUSDC
    |}]
;;

let%expect_test "spot exchangeInfo keeps every TRADING symbol (no margin check)" =
  let body =
    {|{ "symbols": [
      { "symbol": "BTCUSDT", "status": "TRADING" },
      { "symbol": "OLDUSDT", "status": "BREAK" },
      { "symbol": "ETHUSDC", "status": "TRADING" } ] }|}
  in
  (match
     Binance_rest.trading_symbols_of_body ~exchange:Exchange.Binance_spot body
   with
   | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e)
   | Ok syms -> List.iter syms ~f:print_endline);
  [%expect
    {|
    BTCUSDT
    ETHUSDC
    |}]
;;

let%expect_test "coin-m inverse exchangeInfo filters like futures but allows USD margin" =
  let body =
    {|{ "symbols": [
      { "symbol": "BTCUSD_PERP", "status": "TRADING",
        "contractType": "PERPETUAL", "marginAsset": "BTC" },
      { "symbol": "BTCUSD_250927", "status": "TRADING",
        "contractType": "CURRENT_QUARTER", "marginAsset": "BTC" } ] }|}
  in
  (match
     Binance_rest.trading_symbols_of_body ~exchange:Exchange.Binance_inverse body
   with
   | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e)
   | Ok syms -> List.iter syms ~f:print_endline);
  [%expect {| BTCUSD_PERP |}]
;;

let%expect_test "url routing across categories" =
  let show e =
    printf
      "%s markets=%s tickers=%s premium=%s\n"
      (Exchange.to_string e)
      (Uri.to_string (Binance_rest.exchange_info_url e))
      (Uri.to_string (Binance_rest.tickers_24h_url e))
      (match Binance_rest.premium_index_url e with
       | Some u -> Uri.to_string u
       | None -> "(none)")
  in
  show Exchange.Binance;
  show Exchange.Binance_spot;
  show Exchange.Binance_inverse;
  [%expect
    {|
    binance markets=https://fapi.binance.com/fapi/v1/exchangeInfo tickers=https://fapi.binance.com/fapi/v1/ticker/24hr premium=https://fapi.binance.com/fapi/v1/premiumIndex
    binancespot markets=https://api.binance.com/api/v3/exchangeInfo tickers=https://api.binance.com/api/v3/ticker/24hr premium=(none)
    binanceinverse markets=https://dapi.binance.com/dapi/v1/exchangeInfo tickers=https://dapi.binance.com/dapi/v1/ticker/24hr premium=https://dapi.binance.com/dapi/v1/premiumIndex
    |}]
;;
