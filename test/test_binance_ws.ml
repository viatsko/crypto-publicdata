open! Core
open Crypto_publicdata

let show_parsed frame =
  let parsed = Binance_ws.parse_frame frame in
  List.iter parsed ~f:(fun (sym, p) ->
    print_s [%sexp (sym : string), (p : Binance_ws.Partial.t)])
;;

let%expect_test "ticker arr frame yields per-symbol partials" =
  let frame =
    {|[
         {"e":"24hrTicker","s":"BTCUSDT","c":"50000.5","P":"1.23",
          "v":"100","q":"5000025","b":"0","a":"0"},
         {"e":"24hrTicker","s":"ETHUSDT","c":"3000","P":"-0.5",
          "v":"50","q":"150000","b":"0","a":"0"}
      ]|}
  in
  show_parsed frame;
  [%expect
    {|
    (BTCUSDT
     ((bid (0)) (ask (0)) (last (50000.5)) (mark ()) (index ())
      (percentage (1.23)) (funding_rate ()) (funding_time ()) (volume (100))
      (quote_volume (5000025))))
    (ETHUSDT
     ((bid (0)) (ask (0)) (last (3000)) (mark ()) (index ()) (percentage (-0.5))
      (funding_rate ()) (funding_time ()) (volume (50)) (quote_volume (150000))))
    |}]
;;

let%expect_test "bookTicker event carries only bid/ask" =
  let frame =
    {|{"e":"bookTicker","u":1,"s":"BTCUSDT","b":"50000","a":"50001"}|}
  in
  show_parsed frame;
  [%expect
    {|
    (BTCUSDT
     ((bid (50000)) (ask (50001)) (last ()) (mark ()) (index ()) (percentage ())
      (funding_rate ()) (funding_time ()) (volume ()) (quote_volume ())))
    |}]
;;

let%expect_test "markPrice array carries mark/index/funding/next time" =
  let frame =
    {|[
         {"e":"markPriceUpdate","s":"BTCUSDT","p":"50000.3",
          "i":"50000.2","r":"0.0001","T":1775766154000}
      ]|}
  in
  show_parsed frame;
  [%expect
    {|
    (BTCUSDT
     ((bid ()) (ask ()) (last ()) (mark (50000.3)) (index (50000.2))
      (percentage ()) (funding_rate (0.0001)) (funding_time (1775766154000))
      (volume ()) (quote_volume ())))
    |}]
;;

let%expect_test "subscribe acknowledgement is silently dropped" =
  show_parsed {|{"result":null,"id":1}|};
  [%expect {| |}]
;;

let%expect_test "unknown event is dropped" =
  show_parsed {|{"e":"kline","s":"BTCUSDT"}|};
  [%expect {| |}]
;;

let%expect_test "invalid json yields nothing" =
  show_parsed "not-json";
  [%expect {| |}]
;;

let%expect_test "partial apply folds into a ticker without clobbering untouched fields" =
  let base =
    { Ticker.empty with bid = 100.; ask = 101.; last = 99.5; mark = 99.6 }
  in
  let partial = { Binance_ws.Partial.empty with bid = Some 102. } in
  let merged = Binance_ws.Partial.apply base partial in
  print_s [%sexp (merged : Ticker.t)];
  [%expect
    {|
    ((bid 102) (ask 101) (last 99.5) (mark 99.6) (index 0) (percentage 0)
     (open_interest 0) (funding_rate 0) (funding_time 0) (volume 0)
     (quote_volume 0))
    |}]
;;

let%expect_test "partial apply never touches open_interest" =
  (* Binance's WS tier has no OI — the REST snapshot populates it (or
     leaves it at zero) and WS updates must preserve whatever is stored. *)
  let base = { Ticker.empty with open_interest = 12345. } in
  let partial = { Binance_ws.Partial.empty with bid = Some 99. } in
  let merged = Binance_ws.Partial.apply base partial in
  printf "open_interest=%f bid=%f\n" merged.open_interest merged.bid;
  [%expect {| open_interest=12345.000000 bid=99.000000 |}]
;;

let%expect_test "ws_url routes per category" =
  print_endline (Uri.to_string (Binance_ws.ws_url Exchange.Binance));
  print_endline (Uri.to_string (Binance_ws.ws_url Exchange.Binance_spot));
  print_endline (Uri.to_string (Binance_ws.ws_url Exchange.Binance_inverse));
  [%expect
    {|
    wss://fstream.binance.com/ws
    wss://stream.binance.com/ws
    wss://dstream.binance.com/ws
    |}]
;;

let%expect_test "streams_for includes bookTicker + markPrice on futures only" =
  let show e =
    printf
      "%s: %s\n"
      (Exchange.to_string e)
      (String.concat ~sep:"," (Binance_ws.streams_for e))
  in
  show Exchange.Binance;
  show Exchange.Binance_spot;
  show Exchange.Binance_inverse;
  [%expect
    {|
    binance: !ticker@arr,!bookTicker,!markPrice@arr@1s
    binancespot: !ticker@arr
    binanceinverse: !ticker@arr,!bookTicker,!markPrice@arr@1s
    |}]
;;

let%expect_test "subscribe_frame emits the standard SUBSCRIBE envelope" =
  print_endline (Binance_ws.subscribe_frame [ "!ticker@arr"; "!bookTicker" ]);
  [%expect
    {| {"method":"SUBSCRIBE","params":["!ticker@arr","!bookTicker"],"id":1} |}]
;;
