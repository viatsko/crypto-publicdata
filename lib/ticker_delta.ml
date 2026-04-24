open! Core

(* Per-field change record. [Some v] means "this field moved to v since
   the last sent snapshot"; [None] means "unchanged, don't serialise".

   On the wire this collapses into an object with only the non-None
   keys — the client side is expected to treat an absent field as
   "preserve the previous value". *)
type t =
  { bid : float option
  ; ask : float option
  ; last : float option
  ; mark : float option
  ; index : float option
  ; percentage : float option
  ; open_interest : float option
  ; funding_rate : float option
  ; funding_time : int option
  ; volume : float option
  ; quote_volume : float option
  }
[@@deriving sexp, equal]

let empty =
  { bid = None
  ; ask = None
  ; last = None
  ; mark = None
  ; index = None
  ; percentage = None
  ; open_interest = None
  ; funding_rate = None
  ; funding_time = None
  ; volume = None
  ; quote_volume = None
  }
;;

let is_empty t = equal t empty

(* Diff [curr] against [prev]. When [prev] is None (first time we've
   seen this symbol) we diff against Ticker.empty, so the resulting
   delta carries every non-zero field — enough for the client to insert
   a fresh row. Zero-valued fields on a brand-new symbol still get
   omitted because they equal the zero in [empty]; the wire convention
   is that the client defaults absent fields to zero, so this produces
   the right shape. *)
let diff ~prev ~(curr : Ticker.t) =
  let prev = Option.value prev ~default:Ticker.empty in
  let f a b = if Float.equal a b then None else Some b in
  let fi a b = if a = b then None else Some b in
  { bid = f prev.bid curr.bid
  ; ask = f prev.ask curr.ask
  ; last = f prev.last curr.last
  ; mark = f prev.mark curr.mark
  ; index = f prev.index curr.index
  ; percentage = f prev.percentage curr.percentage
  ; open_interest = f prev.open_interest curr.open_interest
  ; funding_rate = f prev.funding_rate curr.funding_rate
  ; funding_time = fi prev.funding_time curr.funding_time
  ; volume = f prev.volume curr.volume
  ; quote_volume = f prev.quote_volume curr.quote_volume
  }
;;

let apply (t : Ticker.t) d =
  { Ticker.bid = Option.value d.bid ~default:t.bid
  ; ask = Option.value d.ask ~default:t.ask
  ; last = Option.value d.last ~default:t.last
  ; mark = Option.value d.mark ~default:t.mark
  ; index = Option.value d.index ~default:t.index
  ; percentage = Option.value d.percentage ~default:t.percentage
  ; open_interest = Option.value d.open_interest ~default:t.open_interest
  ; funding_rate = Option.value d.funding_rate ~default:t.funding_rate
  ; funding_time = Option.value d.funding_time ~default:t.funding_time
  ; volume = Option.value d.volume ~default:t.volume
  ; quote_volume = Option.value d.quote_volume ~default:t.quote_volume
  }
;;

(* Serialize with full-name camelCase keys to match Ticker_json. The
   compact single-letter representation lands in a later step. *)
let to_json t =
  let add name opt json_fn = Option.map opt ~f:(fun v -> name, json_fn v) in
  let fl v = `Float v in
  let i v = `Int v in
  `Assoc
    (List.filter_opt
       [ add "bid" t.bid fl
       ; add "ask" t.ask fl
       ; add "last" t.last fl
       ; add "mark" t.mark fl
       ; add "index" t.index fl
       ; add "percentage" t.percentage fl
       ; add "openInterest" t.open_interest fl
       ; add "fundingRate" t.funding_rate fl
       ; add "fundingTime" t.funding_time i
       ; add "volume" t.volume fl
       ; add "quoteVolume" t.quote_volume fl
       ])
;;
