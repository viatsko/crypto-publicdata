open Core

module Outgoing = struct
  type t =
    | Subscribed
    | Error of string

  let to_json = function
    | Subscribed -> `Assoc [ "type", `String "subscribed" ]
    | Error reason ->
      `Assoc [ "type", `String "error"; "reason", `String reason ]
  ;;

  let to_string t = Yojson.Safe.to_string (to_json t)
end

let response_of_text text =
  match Yojson.Safe.from_string text with
  | exception Yojson.Json_error _ -> Outgoing.Error "invalid json"
  | `Assoc fields ->
    (match List.Assoc.find fields "op" ~equal:String.equal with
     | Some (`String "subscribe") -> Outgoing.Subscribed
     | Some _ -> Outgoing.Error "unknown op"
     | None -> Outgoing.Error "missing op")
  | _ -> Outgoing.Error "expected object"
;;
