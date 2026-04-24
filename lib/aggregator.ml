open! Core

(* Per-exchange bucket. The version counter ticks whenever any stored
   value in this bucket changes, so the diffing layer further down can
   skip exchanges that haven't moved without traversing their tickers. *)
module Exchange_state = struct
  type t =
    { tickers : (string, Ticker.t) Hashtbl.t
    ; mutable version : int
    }

  let create () =
    { tickers = Hashtbl.create (module String); version = 0 }
  ;;

  let get t symbol = Hashtbl.find t.tickers symbol

  let upsert t ~symbol ~ticker =
    let changed =
      match Hashtbl.find t.tickers symbol with
      | None -> true
      | Some existing -> not (Ticker.equal existing ticker)
    in
    if changed
    then (
      Hashtbl.set t.tickers ~key:symbol ~data:ticker;
      t.version <- t.version + 1);
    changed
  ;;

  let remove t ~symbol =
    let removed = Hashtbl.mem t.tickers symbol in
    if removed
    then (
      Hashtbl.remove t.tickers symbol;
      t.version <- t.version + 1);
    removed
  ;;

  let to_alist t = Hashtbl.to_alist t.tickers
  let length t = Hashtbl.length t.tickers
  let version t = t.version
end

type t = { exchanges : (Exchange.t, Exchange_state.t) Hashtbl.t }

let create () = { exchanges = Hashtbl.create (module Exchange) }

let state_for t exchange =
  Hashtbl.find_or_add t.exchanges exchange ~default:Exchange_state.create
;;

let upsert t ~exchange ~symbol ~ticker =
  Exchange_state.upsert (state_for t exchange) ~symbol ~ticker
;;

let remove t ~exchange ~symbol =
  match Hashtbl.find t.exchanges exchange with
  | None -> false
  | Some state -> Exchange_state.remove state ~symbol
;;

let get t ~exchange ~symbol =
  match Hashtbl.find t.exchanges exchange with
  | None -> None
  | Some state -> Exchange_state.get state symbol
;;

let snapshot_exchange t exchange =
  match Hashtbl.find t.exchanges exchange with
  | None -> []
  | Some state -> Exchange_state.to_alist state
;;

let version t exchange =
  match Hashtbl.find t.exchanges exchange with
  | None -> 0
  | Some state -> Exchange_state.version state
;;

let ticker_count t exchange =
  match Hashtbl.find t.exchanges exchange with
  | None -> 0
  | Some state -> Exchange_state.length state
;;

let exchanges t = Hashtbl.keys t.exchanges
