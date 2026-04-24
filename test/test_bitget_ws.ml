open! Core
open Crypto_publicdata

let show_parsed body =
  List.iter (Bitget_ws.parse_frame body) ~f:(fun (sym, p) ->
    print_s [%sexp (sym : string), (p : Bitget_ws.Partial.t)])
;;

let%expect_test "ticker frame from the public channel" =
  let body =
    {|{"action":"snapshot",
       "arg":{"instType":"USDT-FUTURES","channel":"ticker","instId":"BTCUSDT"},
       "data":[{
         "instId":"BTCUSDT",
         "lastPr":"50000.5","bidPr":"50000","askPr":"50001",
         "markPrice":"50000.3","indexPrice":"50000.2",
         "change24h":"0.0123",
         "holdingAmount":"12345",
         "fundingRate":"0.0001",
         "fundingTime":"1775766154000",
         "baseVolume":"100","quoteVolume":"5000025"
       }]}|}
  in
  show_parsed body;
  [%expect
    {|
    (BTCUSDT
     ((bid (50000)) (ask (50001)) (last (50000.5)) (mark (50000.3))
      (index (50000.2)) (percentage (1.23)) (open_interest (12345))
      (funding_rate (0.0001)) (funding_time (1775766154000)) (volume (100))
      (quote_volume (5000025))))
    |}]
;;

let%expect_test "non-ticker channel is dropped" =
  let body =
    {|{"action":"snapshot","arg":{"channel":"books5","instId":"BTCUSDT"},"data":[]}|}
  in
  show_parsed body;
  [%expect {| |}]
;;

let%expect_test "subscribe response is dropped" =
  show_parsed {|{"event":"subscribe","code":"0","arg":{"channel":"ticker"}}|};
  [%expect {| |}]
;;

let%expect_test "partial apply folds without touching untouched fields" =
  let base = { Ticker.empty with bid = 1.; mark = 99. } in
  let p = { Bitget_ws.Partial.empty with bid = Some 2. } in
  let merged = Bitget_ws.Partial.apply base p in
  printf "bid=%f mark=%f\n" merged.bid merged.mark;
  [%expect {| bid=2.000000 mark=99.000000 |}]
;;

let%expect_test "subscribe_frames chunks per default size" =
  let symbols = List.init 120 ~f:(sprintf "S%d") in
  let frames = Bitget_ws.subscribe_frames symbols in
  printf "%d frames\n" (List.length frames);
  (* Default chunk is 50: 50 + 50 + 20 *)
  [%expect {| 3 frames |}]
;;

let%expect_test "subscribe frame shape" =
  let frame = Bitget_ws.subscribe_frame [ "BTCUSDT" ] in
  print_endline frame;
  [%expect
    {| {"op":"subscribe","args":[{"instType":"USDT-FUTURES","channel":"ticker","instId":"BTCUSDT"}]} |}]
;;
