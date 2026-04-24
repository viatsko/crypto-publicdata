open! Core
open Crypto_publicdata

let seed_aggregator () =
  let agg = Aggregator.create () in
  let _ : bool =
    Aggregator.upsert
      agg
      ~exchange:Bybit
      ~symbol:"BTCUSDT"
      ~ticker:{ Ticker.empty with last = 50000. }
  in
  let _ : bool =
    Aggregator.upsert
      agg
      ~exchange:Bybit
      ~symbol:"ETHUSDT"
      ~ticker:{ Ticker.empty with last = 3000. }
  in
  let _ : bool =
    Aggregator.upsert
      agg
      ~exchange:Bybit_spot
      ~symbol:"ETHUSDC"
      ~ticker:{ Ticker.empty with last = 3001. }
  in
  agg
;;

let show_reply ~aggregator text =
  print_endline
    (Protocol.Outgoing.to_string (Server.handle_incoming ~aggregator text))
;;

let%expect_test "ping yields pong" =
  show_reply ~aggregator:(seed_aggregator ()) {|{"op":"ping"}|};
  [%expect {| {"type":"pong"} |}]
;;

let%expect_test "unknown channel yields an error" =
  show_reply
    ~aggregator:(seed_aggregator ())
    {|{"op":"subscribe","channel":"orderbook"}|};
  [%expect {| {"type":"error","reason":"unknown channel: orderbook"} |}]
;;

let%expect_test "subscribe with no exchange filter returns every exchange in the aggregator" =
  show_reply
    ~aggregator:(seed_aggregator ())
    {|{"op":"subscribe","channel":"tickers"}|};
  [%expect
    {| {"type":"snapshot","channel":"tickers","data":{"bybit":{"BTCUSDT":{"bid":0.0,"ask":0.0,"last":50000.0,"mark":0.0,"index":0.0,"percentage":0.0,"openInterest":0.0,"fundingRate":0.0,"fundingTime":0,"volume":0.0,"quoteVolume":0.0},"ETHUSDT":{"bid":0.0,"ask":0.0,"last":3000.0,"mark":0.0,"index":0.0,"percentage":0.0,"openInterest":0.0,"fundingRate":0.0,"fundingTime":0,"volume":0.0,"quoteVolume":0.0}},"bybitspot":{"ETHUSDC":{"bid":0.0,"ask":0.0,"last":3001.0,"mark":0.0,"index":0.0,"percentage":0.0,"openInterest":0.0,"fundingRate":0.0,"fundingTime":0,"volume":0.0,"quoteVolume":0.0}}}} |}]
;;

let%expect_test "exchange filter trims the snapshot" =
  show_reply
    ~aggregator:(seed_aggregator ())
    {|{"op":"subscribe","channel":"tickers","exchanges":["bybitspot"]}|};
  [%expect
    {| {"type":"snapshot","channel":"tickers","data":{"bybitspot":{"ETHUSDC":{"bid":0.0,"ask":0.0,"last":3001.0,"mark":0.0,"index":0.0,"percentage":0.0,"openInterest":0.0,"fundingRate":0.0,"fundingTime":0,"volume":0.0,"quoteVolume":0.0}}}} |}]
;;

let%expect_test "empty exchange filter selects nothing" =
  show_reply
    ~aggregator:(seed_aggregator ())
    {|{"op":"subscribe","channel":"tickers","exchanges":[]}|};
  [%expect {| {"type":"snapshot","channel":"tickers","data":{}} |}]
;;

let%expect_test "bad subscribe surfaces the parse error verbatim" =
  show_reply ~aggregator:(seed_aggregator ()) {|{"op":"subscribe"}|};
  [%expect {| {"type":"error","reason":"missing channel"} |}]
;;

let%expect_test "pipe round-trip emits connected first then responds to messages" =
  let open Async in
  Thread_safe.block_on_async_exn (fun () ->
    let agg = seed_aggregator () in
    let input_reader, input_writer = Pipe.create () in
    let output_reader, output_writer = Pipe.create () in
    don't_wait_for
      (Server.handle_messages
         ~aggregator:agg
         ~input:input_reader
         ~output:output_writer);
    let%bind () =
      match%map Pipe.read output_reader with
      | `Eof -> print_endline "unexpected eof"
      | `Ok msg -> printf "first frame: %s\n" msg
    in
    let%bind () = Pipe.write input_writer {|{"op":"ping"}|} in
    let%bind () =
      match%map Pipe.read output_reader with
      | `Eof -> print_endline "unexpected eof"
      | `Ok msg -> printf "ping response: %s\n" msg
    in
    Pipe.close input_writer;
    let%bind () = Pipe.closed output_reader in
    print_endline "output closed";
    return ());
  [%expect
    {|
    first frame: {"type":"connected"}
    ping response: {"type":"pong"}
    output closed
    |}]
;;
