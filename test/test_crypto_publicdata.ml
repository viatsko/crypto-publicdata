open! Core
open Crypto_publicdata

let print_response text =
  print_endline (Protocol.(Outgoing.to_string (response_of_text text)))
;;

let%expect_test "subscribe" =
  print_response {|{"op":"subscribe"}|};
  [%expect {| {"type":"subscribed"} |}]
;;

let%expect_test "subscribe with extra fields is still accepted" =
  print_response {|{"op":"subscribe","channel":"tickers"}|};
  [%expect {| {"type":"subscribed"} |}]
;;

let%expect_test "invalid json" =
  print_response "{not-json";
  [%expect {| {"type":"error","reason":"invalid json"} |}]
;;

let%expect_test "unknown op" =
  print_response {|{"op":"unsubscribe"}|};
  [%expect {| {"type":"error","reason":"unknown op"} |}]
;;

let%expect_test "missing op" =
  print_response {|{"channel":"tickers"}|};
  [%expect {| {"type":"error","reason":"missing op"} |}]
;;

let%expect_test "not an object" =
  print_response "42";
  [%expect {| {"type":"error","reason":"expected object"} |}]
;;

let%expect_test "pipe round-trip forwards responses and closes output on input close" =
  let open Async in
  Thread_safe.block_on_async_exn (fun () ->
    let input_reader, input_writer = Pipe.create () in
    let output_reader, output_writer = Pipe.create () in
    don't_wait_for
      (Server.handle_messages ~input:input_reader ~output:output_writer);
    let%bind () = Pipe.write input_writer {|{"op":"subscribe"}|} in
    let%bind () =
      match%bind Pipe.read output_reader with
      | `Eof ->
        print_endline "unexpected eof";
        return ()
      | `Ok msg ->
        print_endline msg;
        return ()
    in
    Pipe.close input_writer;
    let%bind () = Pipe.closed output_reader in
    print_endline "output closed";
    return ());
  [%expect
    {|
    {"type":"subscribed"}
    output closed
    |}]
;;
