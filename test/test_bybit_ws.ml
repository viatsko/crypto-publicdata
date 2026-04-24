open! Core
open Crypto_publicdata

let print_frame_parse ~exchange frame =
  match Bybit_ws.parse_ticker_frame ~exchange frame with
  | None -> print_endline "None"
  | Some (sym, p) ->
    print_s [%sexp (sym : string), (p : Bybit_ws.Partial.t)]
;;

let linear_frame =
  {|{"topic":"tickers.BTCUSDT","type":"snapshot","ts":1775766154000,
     "data":{"symbol":"BTCUSDT","bid1Price":"50000","ask1Price":"50001",
             "lastPrice":"50000.5","markPrice":"50000.3","indexPrice":"50000.2",
             "price24hPcnt":"0.0123","openInterest":"12345",
             "fundingRate":"0.0001","nextFundingTime":"1775766154000",
             "volume24h":"100","turnover24h":"5000025"}}|}
;;

let%expect_test "snapshot frame parses every field" =
  print_frame_parse ~exchange:Exchange.Bybit linear_frame;
  [%expect
    {|
    (BTCUSDT
     ((bid (50000)) (ask (50001)) (last (50000.5)) (mark (50000.3))
      (index (50000.2)) (percentage (1.23)) (open_interest_raw (12345))
      (funding_rate (0.0001)) (funding_time (1775766154000)) (volume (100))
      (turnover (5000025))))
    |}]
;;

let%expect_test "sparse delta frame only populates touched fields" =
  let frame =
    {|{"topic":"tickers.ETHUSDT","type":"delta",
       "data":{"symbol":"ETHUSDT","bid1Price":"3000.5"}}|}
  in
  print_frame_parse ~exchange:Exchange.Bybit frame;
  [%expect
    {|
    (ETHUSDT
     ((bid (3000.5)) (ask ()) (last ()) (mark ()) (index ()) (percentage ())
      (open_interest_raw ()) (funding_rate ()) (funding_time ()) (volume ())
      (turnover ())))
    |}]
;;

let%expect_test "spot frame drops fundingRate from the partial" =
  let frame =
    {|{"topic":"tickers.ETHUSDC",
       "data":{"symbol":"ETHUSDC","fundingRate":"0.9","lastPrice":"3000"}}|}
  in
  print_frame_parse ~exchange:Exchange.Bybit_spot frame;
  [%expect
    {|
    (ETHUSDC
     ((bid ()) (ask ()) (last (3000)) (mark ()) (index ()) (percentage ())
      (open_interest_raw ()) (funding_rate ()) (funding_time ()) (volume ())
      (turnover ())))
    |}]
;;

let%expect_test "unknown topic yields None" =
  let frame = {|{"topic":"orderbook.50.BTCUSDT","data":{}}|} in
  print_frame_parse ~exchange:Exchange.Bybit frame;
  [%expect {| None |}]
;;

let%expect_test "invalid JSON yields None" =
  print_frame_parse ~exchange:Exchange.Bybit "not json";
  [%expect {| None |}]
;;

let%expect_test "frame missing data yields None" =
  print_frame_parse ~exchange:Exchange.Bybit {|{"topic":"tickers.BTCUSDT"}|};
  [%expect {| None |}]
;;

let%expect_test "partial apply preserves untouched fields" =
  (* WS delta only says bid changed; ask/last/mark should stay put. *)
  let partial = { Bybit_ws.Partial.empty with bid = Some 99. } in
  let base =
    { Ticker.empty with
      bid = 100.
    ; ask = 100.5
    ; last = 99.5
    ; mark = 99.6
    ; volume = 1000.
    ; quote_volume = 99500.
    }
  in
  let merged = Bybit_ws.Partial.apply ~exchange:Exchange.Bybit base partial in
  print_s [%sexp (merged : Ticker.t)];
  [%expect
    {|
    ((bid 99) (ask 100.5) (last 99.5) (mark 99.6) (index 0) (percentage 0)
     (open_interest 0) (funding_rate 0) (funding_time 0) (volume 1000)
     (quote_volume 99500))
    |}]
;;

let%expect_test "inverse apply normalizes open interest against mark" =
  let partial = { Bybit_ws.Partial.empty with open_interest_raw = Some 500_000. } in
  let base = { Ticker.empty with mark = 50_000.; last = 49_999. } in
  let merged =
    Bybit_ws.Partial.apply ~exchange:Exchange.Bybit_inverse base partial
  in
  printf "%f\n" merged.open_interest;
  [%expect {| 10.000000 |}]
;;

let%expect_test "spot apply forces funding_rate to zero" =
  let partial = { Bybit_ws.Partial.empty with funding_rate = Some 0.0001 } in
  let base = { Ticker.empty with funding_rate = 123. } in
  let merged =
    Bybit_ws.Partial.apply ~exchange:Exchange.Bybit_spot base partial
  in
  printf "%f\n" merged.funding_rate;
  [%expect {| 0.000000 |}]
;;

let%expect_test "turnover=0 falls back to volume*last when both known" =
  let partial =
    { Bybit_ws.Partial.empty with
      last = Some 100.
    ; volume = Some 10.
    ; turnover = Some 0.
    }
  in
  let base = Ticker.empty in
  let merged = Bybit_ws.Partial.apply ~exchange:Exchange.Bybit base partial in
  printf "%f\n" merged.quote_volume;
  [%expect {| 1000.000000 |}]
;;

let%expect_test "subscribe_frames chunk at 10 by default" =
  let symbols = List.init 25 ~f:(sprintf "S%d") in
  let frames = Bybit_ws.subscribe_frames symbols in
  printf "%d frames\n" (List.length frames);
  List.iter frames ~f:print_endline;
  [%expect
    {|
    3 frames
    {"op":"subscribe","args":["tickers.S0","tickers.S1","tickers.S2","tickers.S3","tickers.S4","tickers.S5","tickers.S6","tickers.S7","tickers.S8","tickers.S9"]}
    {"op":"subscribe","args":["tickers.S10","tickers.S11","tickers.S12","tickers.S13","tickers.S14","tickers.S15","tickers.S16","tickers.S17","tickers.S18","tickers.S19"]}
    {"op":"subscribe","args":["tickers.S20","tickers.S21","tickers.S22","tickers.S23","tickers.S24"]}
    |}]
;;

let%expect_test "empty symbol list yields no frames" =
  let frames = Bybit_ws.subscribe_frames [] in
  printf "%d frames\n" (List.length frames);
  [%expect {| 0 frames |}]
;;

let%expect_test "ws_url routes to the right category" =
  print_endline (Uri.to_string (Bybit_ws.ws_url Exchange.Bybit));
  print_endline (Uri.to_string (Bybit_ws.ws_url Exchange.Bybit_spot));
  print_endline (Uri.to_string (Bybit_ws.ws_url Exchange.Bybit_inverse));
  [%expect
    {|
    wss://stream.bybit.com/v5/public/linear
    wss://stream.bybit.com/v5/public/spot
    wss://stream.bybit.com/v5/public/inverse
    |}]
;;
