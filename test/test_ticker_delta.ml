open! Core
open Crypto_publicdata

let show d = print_s [%sexp (d : Ticker_delta.t)]

let%expect_test "diff of identical tickers is empty" =
  let t = { Ticker.empty with bid = 100.; ask = 100.5 } in
  let d = Ticker_delta.diff ~prev:(Some t) ~curr:t in
  printf "empty=%b\n" (Ticker_delta.is_empty d);
  show d;
  [%expect
    {|
    empty=true
    ((bid ()) (ask ()) (last ()) (mark ()) (index ()) (percentage ())
     (open_interest ()) (funding_rate ()) (funding_time ()) (volume ())
     (quote_volume ()))
    |}]
;;

let%expect_test "diff surfaces only changed fields" =
  let prev = { Ticker.empty with bid = 100.; ask = 100.5; last = 99.5 } in
  let curr = { prev with bid = 101. } in
  let d = Ticker_delta.diff ~prev:(Some prev) ~curr in
  printf "empty=%b\n" (Ticker_delta.is_empty d);
  show d;
  [%expect
    {|
    empty=false
    ((bid (101)) (ask ()) (last ()) (mark ()) (index ()) (percentage ())
     (open_interest ()) (funding_rate ()) (funding_time ()) (volume ())
     (quote_volume ()))
    |}]
;;

let%expect_test "diff of a brand new symbol carries every non-zero field" =
  let curr =
    { Ticker.empty with
      bid = 100.
    ; ask = 100.5
    ; last = 99.5
    ; mark = 99.6
    ; volume = 1000.
    }
  in
  let d = Ticker_delta.diff ~prev:None ~curr in
  show d;
  [%expect
    {|
    ((bid (100)) (ask (100.5)) (last (99.5)) (mark (99.6)) (index ())
     (percentage ()) (open_interest ()) (funding_rate ()) (funding_time ())
     (volume (1000)) (quote_volume ()))
    |}]
;;

let%expect_test "diff of all-zero new symbol is empty (wire-strip semantics)" =
  let d = Ticker_delta.diff ~prev:None ~curr:Ticker.empty in
  printf "empty=%b\n" (Ticker_delta.is_empty d);
  [%expect {| empty=true |}]
;;

let%expect_test "funding_time diff (integer compare)" =
  let prev = { Ticker.empty with funding_time = 1000 } in
  let curr = { prev with funding_time = 2000 } in
  let d = Ticker_delta.diff ~prev:(Some prev) ~curr in
  show d;
  [%expect
    {|
    ((bid ()) (ask ()) (last ()) (mark ()) (index ()) (percentage ())
     (open_interest ()) (funding_rate ()) (funding_time (2000)) (volume ())
     (quote_volume ()))
    |}]
;;

let%expect_test "to_json omits fields that are None" =
  let d = { Ticker_delta.empty with bid = Some 100.; last = Some 99.5 } in
  print_endline (Yojson.Safe.to_string (Ticker_delta.to_json d));
  [%expect {| {"b":100.0,"l":99.5} |}]
;;

let%expect_test "to_json on empty delta is an empty object" =
  print_endline (Yojson.Safe.to_string (Ticker_delta.to_json Ticker_delta.empty));
  [%expect {| {} |}]
;;

let%expect_test "apply folds a delta into a base ticker" =
  let base = { Ticker.empty with bid = 100.; ask = 101. } in
  let d = { Ticker_delta.empty with ask = Some 102. } in
  let merged = Ticker_delta.apply base d in
  printf "bid=%f ask=%f\n" merged.bid merged.ask;
  [%expect {| bid=100.000000 ask=102.000000 |}]
;;

let%expect_test "Some 0. collapses to absent on the wire" =
  (* Wire format treats zero as absent. A delta that walks a field to
     0 can't be encoded — that's an intentional tradeoff for payload
     size; symbols whose fields hit zero usually get removed via the
     [removed] map instead. *)
  let d = { Ticker_delta.empty with bid = Some 0.; funding_time = Some 0 } in
  print_endline (Yojson.Safe.to_string (Ticker_delta.to_json d));
  [%expect {| {} |}]
;;
