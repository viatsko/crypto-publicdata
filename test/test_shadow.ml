open! Core
open Crypto_publicdata

let upsert agg ~exchange ~symbol ~last =
  let _ : bool =
    Aggregator.upsert
      agg
      ~exchange
      ~symbol
      ~ticker:{ Ticker.empty with last }
  in
  ()
;;

let show_advance ?(include_index = true) ~shadow ~aggregator ~exchanges_filter () =
  let changed, removed =
    Shadow.advance shadow ~aggregator ~exchanges_filter ~include_index
  in
  print_s
    [%sexp
      { changed : (Exchange.t * (string * Ticker_delta.t) list) list
      ; removed : (Exchange.t * string list) list
      }]
;;

let%expect_test "first advance on empty shadow surfaces everything" =
  let agg = Aggregator.create () in
  upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~last:50000.;
  upsert agg ~exchange:Bybit ~symbol:"ETHUSDT" ~last:3000.;
  let shadow = Shadow.create () in
  show_advance ~shadow ~aggregator:agg ~exchanges_filter:None ();
  [%expect
    {|
    ((changed
      ((Bybit
        ((BTCUSDT
          ((bid ()) (ask ()) (last (50000)) (mark ()) (index ()) (percentage ())
           (open_interest ()) (funding_rate ()) (funding_time ()) (volume ())
           (quote_volume ())))
         (ETHUSDT
          ((bid ()) (ask ()) (last (3000)) (mark ()) (index ()) (percentage ())
           (open_interest ()) (funding_rate ()) (funding_time ()) (volume ())
           (quote_volume ())))))))
     (removed ()))
    |}]
;;

let%expect_test "second advance with no aggregator change is empty" =
  let agg = Aggregator.create () in
  upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~last:50000.;
  let shadow = Shadow.create () in
  let _ = Shadow.advance shadow ~aggregator:agg ~exchanges_filter:None ~include_index:true in
  show_advance ~shadow ~aggregator:agg ~exchanges_filter:None ();
  [%expect {| ((changed ()) (removed ())) |}]
;;

let%expect_test "seed then advance reports only post-seed changes" =
  let agg = Aggregator.create () in
  upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~last:50000.;
  let shadow = Shadow.create () in
  let snapshot =
    [ Exchange.Bybit, Aggregator.snapshot_exchange agg Exchange.Bybit ]
  in
  Shadow.seed shadow ~snapshot;
  (* After seeding, a fresh advance on unchanged state should be empty. *)
  show_advance ~shadow ~aggregator:agg ~exchanges_filter:None ();
  (* Now move the price — advance should surface only the last field. *)
  upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~last:50001.;
  show_advance ~shadow ~aggregator:agg ~exchanges_filter:None ();
  [%expect
    {|
    ((changed ()) (removed ()))
    ((changed
      ((Bybit
        ((BTCUSDT
          ((bid ()) (ask ()) (last (50001)) (mark ()) (index ()) (percentage ())
           (open_interest ()) (funding_rate ()) (funding_time ()) (volume ())
           (quote_volume ())))))))
     (removed ()))
    |}]
;;

let%expect_test "removing a symbol from the aggregator surfaces it in [removed]" =
  let agg = Aggregator.create () in
  upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~last:50000.;
  upsert agg ~exchange:Bybit ~symbol:"ETHUSDT" ~last:3000.;
  let shadow = Shadow.create () in
  let _ = Shadow.advance shadow ~aggregator:agg ~exchanges_filter:None ~include_index:true in
  let (_ : bool) =
    Aggregator.remove agg ~exchange:Bybit ~symbol:"ETHUSDT"
  in
  show_advance ~shadow ~aggregator:agg ~exchanges_filter:None ();
  [%expect {| ((changed ()) (removed ((Bybit (ETHUSDT))))) |}]
;;

let%expect_test "new symbol after seeding appears as a fresh delta" =
  let agg = Aggregator.create () in
  upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~last:50000.;
  let shadow = Shadow.create () in
  let _ = Shadow.advance shadow ~aggregator:agg ~exchanges_filter:None ~include_index:true in
  upsert agg ~exchange:Bybit ~symbol:"NEWUSDT" ~last:1.;
  show_advance ~shadow ~aggregator:agg ~exchanges_filter:None ();
  [%expect
    {|
    ((changed
      ((Bybit
        ((NEWUSDT
          ((bid ()) (ask ()) (last (1)) (mark ()) (index ()) (percentage ())
           (open_interest ()) (funding_rate ()) (funding_time ()) (volume ())
           (quote_volume ())))))))
     (removed ()))
    |}]
;;

let%expect_test "include_index=false hides the index field from diffs" =
  let agg = Aggregator.create () in
  let _ : bool =
    Aggregator.upsert
      agg
      ~exchange:Bybit
      ~symbol:"BTCUSDT"
      ~ticker:{ Ticker.empty with last = 50000.; index = 49_999. }
  in
  let shadow = Shadow.create () in
  show_advance
    ~include_index:false
    ~shadow
    ~aggregator:agg
    ~exchanges_filter:None
    ();
  [%expect
    {|
    ((changed
      ((Bybit
        ((BTCUSDT
          ((bid ()) (ask ()) (last (50000)) (mark ()) (index ()) (percentage ())
           (open_interest ()) (funding_rate ()) (funding_time ()) (volume ())
           (quote_volume ())))))))
     (removed ()))
    |}]
;;

let%expect_test "include_index=true keeps the index field in diffs" =
  let agg = Aggregator.create () in
  let _ : bool =
    Aggregator.upsert
      agg
      ~exchange:Bybit
      ~symbol:"BTCUSDT"
      ~ticker:{ Ticker.empty with last = 50000.; index = 49_999. }
  in
  let shadow = Shadow.create () in
  show_advance
    ~include_index:true
    ~shadow
    ~aggregator:agg
    ~exchanges_filter:None
    ();
  [%expect
    {|
    ((changed
      ((Bybit
        ((BTCUSDT
          ((bid ()) (ask ()) (last (50000)) (mark ()) (index (49999))
           (percentage ()) (open_interest ()) (funding_rate ()) (funding_time ())
           (volume ()) (quote_volume ())))))))
     (removed ()))
    |}]
;;

let%expect_test "filter restricts output to the named exchanges" =
  let agg = Aggregator.create () in
  upsert agg ~exchange:Bybit ~symbol:"BTCUSDT" ~last:50000.;
  upsert agg ~exchange:Bybit_spot ~symbol:"ETHUSDC" ~last:3000.;
  let shadow = Shadow.create () in
  show_advance
    ~shadow
    ~aggregator:agg
    ~exchanges_filter:(Some [ Exchange.Bybit ])
    ();
  [%expect
    {|
    ((changed
      ((Bybit
        ((BTCUSDT
          ((bid ()) (ask ()) (last (50000)) (mark ()) (index ()) (percentage ())
           (open_interest ()) (funding_rate ()) (funding_time ()) (volume ())
           (quote_volume ())))))))
     (removed ()))
    |}]
;;
