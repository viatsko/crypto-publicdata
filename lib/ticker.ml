open! Core

(* Numeric fields default to 0.0 when absent — the wire format later
   strips zeros, so a never-populated field and an honestly-zero field
   are indistinguishable on the wire. Upstream adapters set only the
   fields they have and leave the rest at zero.

   [funding_time] is the next funding timestamp in unix milliseconds. *)
type t =
  { bid : float
  ; ask : float
  ; last : float
  ; mark : float
  ; index : float
  ; percentage : float
  ; open_interest : float
  ; funding_rate : float
  ; funding_time : int
  ; volume : float
  ; quote_volume : float
  }
[@@deriving sexp, compare, equal, fields]

let empty =
  { bid = 0.
  ; ask = 0.
  ; last = 0.
  ; mark = 0.
  ; index = 0.
  ; percentage = 0.
  ; open_interest = 0.
  ; funding_rate = 0.
  ; funding_time = 0
  ; volume = 0.
  ; quote_volume = 0.
  }
;;
