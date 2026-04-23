open! Core
open! Async

let () =
  don't_wait_for (Crypto_publicdata.Server.start ());
  never_returns (Scheduler.go ())
;;
