open! Core
open Crypto_publicdata

let show_incoming text =
  match Protocol.Incoming.of_text text with
  | Ok t -> print_s [%sexp (t : Protocol.Incoming.t)]
  | Error e -> print_endline ("ERROR: " ^ Error.to_string_hum e)
;;

let%expect_test "subscribe without exchange filter takes the default rate" =
  show_incoming {|{"op":"subscribe","channel":"tickers"}|};
  [%expect {| (Subscribe (channel tickers) (exchanges ()) (rate 1)) |}]
;;

let%expect_test "subscribe with explicit exchange list" =
  show_incoming
    {|{"op":"subscribe","channel":"tickers","exchanges":["bybit","bybitspot"]}|};
  [%expect
    {| (Subscribe (channel tickers) (exchanges ((Bybit Bybit_spot))) (rate 1)) |}]
;;

let%expect_test "subscribe with empty exchange list" =
  show_incoming {|{"op":"subscribe","channel":"tickers","exchanges":[]}|};
  [%expect {| (Subscribe (channel tickers) (exchanges (())) (rate 1)) |}]
;;

let%expect_test "subscribe with null exchange list means 'all'" =
  show_incoming {|{"op":"subscribe","channel":"tickers","exchanges":null}|};
  [%expect {| (Subscribe (channel tickers) (exchanges ()) (rate 1)) |}]
;;

let%expect_test "subscribe rate is clamped low" =
  show_incoming {|{"op":"subscribe","channel":"tickers","rate":0.01}|};
  [%expect {| (Subscribe (channel tickers) (exchanges ()) (rate 0.1)) |}]
;;

let%expect_test "subscribe rate is clamped high" =
  show_incoming {|{"op":"subscribe","channel":"tickers","rate":60}|};
  [%expect {| (Subscribe (channel tickers) (exchanges ()) (rate 5)) |}]
;;

let%expect_test "subscribe accepts integer rate" =
  show_incoming {|{"op":"subscribe","channel":"tickers","rate":2}|};
  [%expect {| (Subscribe (channel tickers) (exchanges ()) (rate 2)) |}]
;;

let%expect_test "subscribe rejects non-numeric rate" =
  show_incoming {|{"op":"subscribe","channel":"tickers","rate":"fast"}|};
  [%expect {| ERROR: rate must be a number |}]
;;

let%expect_test "subscribe with unknown exchange is rejected" =
  show_incoming
    {|{"op":"subscribe","channel":"tickers","exchanges":["bybit","nope"]}|};
  [%expect {| ERROR: "unknown exchange: nope" |}]
;;

let%expect_test "subscribe without channel is rejected" =
  show_incoming {|{"op":"subscribe"}|};
  [%expect {| ERROR: missing channel |}]
;;

let%expect_test "unsubscribe parses" =
  show_incoming {|{"op":"unsubscribe","channel":"tickers"}|};
  [%expect {| (Unsubscribe (channel tickers)) |}]
;;

let%expect_test "ping parses" =
  show_incoming {|{"op":"ping"}|};
  [%expect {| Ping |}]
;;

let%expect_test "missing op is rejected" =
  show_incoming {|{"channel":"tickers"}|};
  [%expect {| ERROR: missing op |}]
;;

let%expect_test "unknown op is rejected" =
  show_incoming {|{"op":"frobnicate"}|};
  [%expect {| ERROR: unknown op: frobnicate |}]
;;

let%expect_test "invalid json is rejected" =
  show_incoming "{not-json";
  [%expect {| ERROR: invalid json |}]
;;

let%expect_test "non-object is rejected" =
  show_incoming "42";
  [%expect {| ERROR: expected object |}]
;;

let show_outgoing t = print_endline (Protocol.Outgoing.to_string t)

let%expect_test "connected encodes minimally" =
  show_outgoing Connected;
  [%expect {| {"type":"connected"} |}]
;;

let%expect_test "pong encodes minimally" =
  show_outgoing Pong;
  [%expect {| {"type":"pong"} |}]
;;

let%expect_test "error carries the reason" =
  show_outgoing (Error "bad op");
  [%expect {| {"type":"error","reason":"bad op"} |}]
;;

let%expect_test "snapshot with data serialises nested exchange→symbol map" =
  let data =
    [ ( Exchange.Bybit
      , [ "BTCUSDT", { Ticker.empty with last = 100. }
        ; "ETHUSDT", { Ticker.empty with last = 5. }
        ] )
    ]
  in
  let snap = Protocol.Outgoing.Snapshot { channel = "tickers"; data } in
  print_endline (Yojson.Safe.pretty_to_string (Protocol.Outgoing.to_json snap));
  [%expect
    {|
    {
      "type": "snapshot",
      "channel": "tickers",
      "data": {
        "bybit": {
          "BTCUSDT": {
            "bid": 0.0,
            "ask": 0.0,
            "last": 100.0,
            "mark": 0.0,
            "index": 0.0,
            "percentage": 0.0,
            "openInterest": 0.0,
            "fundingRate": 0.0,
            "fundingTime": 0,
            "volume": 0.0,
            "quoteVolume": 0.0
          },
          "ETHUSDT": {
            "bid": 0.0,
            "ask": 0.0,
            "last": 5.0,
            "mark": 0.0,
            "index": 0.0,
            "percentage": 0.0,
            "openInterest": 0.0,
            "fundingRate": 0.0,
            "fundingTime": 0,
            "volume": 0.0,
            "quoteVolume": 0.0
          }
        }
      }
    }
    |}]
;;

let%expect_test "snapshot with empty data encodes an empty map" =
  show_outgoing (Snapshot { channel = "tickers"; data = [] });
  [%expect {| {"type":"snapshot","channel":"tickers","data":{}} |}]
;;
