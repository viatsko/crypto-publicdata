open! Core
open Crypto_publicdata

let show body =
  match Kraken_ws.parse_frame body with
  | None -> print_endline "None"
  | Some (sym, p) -> print_s [%sexp (sym : string), (p : Kraken_ws.Partial.t)]
;;

let%expect_test "ticker frame carries every field" =
  let body =
    {|{"feed":"ticker","product_id":"PF_XBTUSD",
       "bid":50000,"ask":50001,"last":50000.5,
       "mark_price":50000.3,"index":50000.2,
       "change":-1.15,
       "relative_funding_rate":0.0001,
       "next_funding_rate_time":1775766154000,
       "volume":100,"volumeQuote":5000025,"openInterest":12345}|}
  in
  show body;
  [%expect
    {|
    (PF_XBTUSD
     ((bid (50000)) (ask (50001)) (last (50000.5)) (mark (50000.3))
      (index (50000.2)) (percentage (-1.15)) (open_interest (12345))
      (funding_rate (0.0001)) (funding_time (1775766154000)) (volume (100))
      (quote_volume (5000025))))
    |}]
;;

let%expect_test "ticker_snapshot feed also parses" =
  let body =
    {|{"feed":"ticker_snapshot","product_id":"PF_ETHUSD",
       "bid":3000,"ask":3001,"last":3000.5,"mark_price":3000.4}|}
  in
  show body;
  [%expect
    {|
    (PF_ETHUSD
     ((bid (3000)) (ask (3001)) (last (3000.5)) (mark (3000.4)) (index ())
      (percentage ()) (open_interest ()) (funding_rate ()) (funding_time ())
      (volume ()) (quote_volume ())))
    |}]
;;

let%expect_test "heartbeat and subscription responses are dropped" =
  show {|{"feed":"heartbeat","time":1775766154000}|};
  show {|{"event":"subscribed","feed":"ticker","product_ids":["PF_XBTUSD"]}|};
  [%expect
    {|
    None
    None
    |}]
;;

let%expect_test "subscribe frame shape, chunked at 50" =
  let symbols = List.init 120 ~f:(sprintf "PF_S%d") in
  let frames = Kraken_ws.subscribe_frames symbols in
  printf "%d frames\n" (List.length frames);
  [%expect {| 3 frames |}]
;;

let%expect_test "subscribe frame uses the [ticker] feed by name" =
  let frame = Kraken_ws.subscribe_frame [ "PF_XBTUSD" ] in
  print_endline frame;
  [%expect
    {| {"event":"subscribe","feed":"ticker","product_ids":["PF_XBTUSD"]} |}]
;;
