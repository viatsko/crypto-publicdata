open! Core

(* Parsed representation of a client→server message. Everything on the
   wire is JSON; [of_text] does the JSON parse and the structural
   validation in one pass so the server loop only has to handle the
   well-typed cases. *)
module Incoming = struct
  type t =
    | Subscribe of
        { channel : string
        ; exchanges : Exchange.t list option
            (* [None] means "every exchange the aggregator knows about". *)
        }
    | Unsubscribe of { channel : string }
    | Ping
  [@@deriving sexp]

  let string_field fields name =
    match List.Assoc.find fields name ~equal:String.equal with
    | Some (`String s) -> Some s
    | _ -> None
  ;;

  let parse_exchanges fields =
    match List.Assoc.find fields "exchanges" ~equal:String.equal with
    | None | Some `Null -> Ok None
    | Some (`List items) ->
      let open Or_error.Let_syntax in
      let%map list =
        items
        |> List.map ~f:(function
          | `String s ->
            (match Exchange.of_string s with
             | Some e -> Ok e
             | None -> Or_error.errorf "unknown exchange: %s" s)
          | _ -> Or_error.error_string "exchanges must contain strings")
        |> Or_error.combine_errors
      in
      Some list
    | Some _ -> Or_error.error_string "exchanges must be an array"
  ;;

  let of_text text =
    let open Or_error.Let_syntax in
    let%bind json =
      try Ok (Yojson.Safe.from_string text) with
      | Yojson.Json_error _ -> Or_error.error_string "invalid json"
    in
    match json with
    | `Assoc fields ->
      (match string_field fields "op" with
       | None -> Or_error.error_string "missing op"
       | Some "ping" -> Ok Ping
       | Some "subscribe" ->
         let%bind channel =
           match string_field fields "channel" with
           | Some s -> Ok s
           | None -> Or_error.error_string "missing channel"
         in
         let%map exchanges = parse_exchanges fields in
         Subscribe { channel; exchanges }
       | Some "unsubscribe" ->
         let%map channel =
           match string_field fields "channel" with
           | Some s -> Ok s
           | None -> Or_error.error_string "missing channel"
         in
         Unsubscribe { channel }
       | Some op -> Or_error.errorf "unknown op: %s" op)
    | _ -> Or_error.error_string "expected object"
  ;;
end

(* Server→client message shapes. Separating [t] from the JSON encoder
   keeps business logic (building a Snapshot) distinct from wire
   concerns (how its fields serialise) — matters for the compact-wire
   switchover later. *)
module Outgoing = struct
  type snapshot_data = (Exchange.t * (string * Ticker.t) list) list
  [@@deriving sexp_of]

  type t =
    | Connected
    | Snapshot of
        { channel : string
        ; data : snapshot_data
        }
    | Pong
    | Error of string
  [@@deriving sexp_of]

  let snapshot_data_to_json data =
    `Assoc
      (List.map data ~f:(fun (exchange, tickers) ->
         ( Exchange.to_string exchange
         , `Assoc
             (List.map tickers ~f:(fun (sym, t) -> sym, Ticker_json.to_json t)) )))
  ;;

  let to_json = function
    | Connected -> `Assoc [ "type", `String "connected" ]
    | Snapshot { channel; data } ->
      `Assoc
        [ "type", `String "snapshot"
        ; "channel", `String channel
        ; "data", snapshot_data_to_json data
        ]
    | Pong -> `Assoc [ "type", `String "pong" ]
    | Error reason ->
      `Assoc [ "type", `String "error"; "reason", `String reason ]
  ;;

  let to_string t = Yojson.Safe.to_string (to_json t)
end
