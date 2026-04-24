open! Core

(* Full-name JSON keys, camelCased to match the planned wire format.
   The compact single-letter representation comes later; for now the
   REST endpoint returns readable payloads so manual inspection is
   easy while the pipeline is being assembled. *)
let to_json (t : Ticker.t) =
  `Assoc
    [ "bid", `Float t.bid
    ; "ask", `Float t.ask
    ; "last", `Float t.last
    ; "mark", `Float t.mark
    ; "index", `Float t.index
    ; "percentage", `Float t.percentage
    ; "openInterest", `Float t.open_interest
    ; "fundingRate", `Float t.funding_rate
    ; "fundingTime", `Int t.funding_time
    ; "volume", `Float t.volume
    ; "quoteVolume", `Float t.quote_volume
    ]
;;

(* Sorted by symbol so the JSON response is order-independent of
   hashtable iteration; golden tests and manual diff stay stable. *)
let tickers_response tickers =
  let sorted =
    List.sort tickers ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
  `Assoc
    [ ( "tickers"
      , `Assoc (List.map sorted ~f:(fun (sym, t) -> sym, to_json t)) )
    ]
;;
