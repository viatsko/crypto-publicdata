open! Core

(* Compact wire format: single-letter JSON keys with zero-stripping.
   A field that is literally zero is omitted from the serialised
   object; the client-side convention is that absent fields default to
   0 (and to "unchanged" in a delta frame).

   Key mapping (same letters used for snapshot and delta):
   b = bid, a = ask, l = last, m = mark, i = index (opt-in),
   p = percentage (24h %), o = openInterest, f = fundingRate,
   t = fundingTime (ms), v = volume (24h base), q = quoteVolume (24h quote) *)
let to_json (t : Ticker.t) =
  let add_f k v = if Float.equal v 0. then None else Some (k, `Float v) in
  let add_i k v = if v = 0 then None else Some (k, `Int v) in
  `Assoc
    (List.filter_opt
       [ add_f "b" t.bid
       ; add_f "a" t.ask
       ; add_f "l" t.last
       ; add_f "m" t.mark
       ; add_f "i" t.index
       ; add_f "p" t.percentage
       ; add_f "o" t.open_interest
       ; add_f "f" t.funding_rate
       ; add_i "t" t.funding_time
       ; add_f "v" t.volume
       ; add_f "q" t.quote_volume
       ])
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
