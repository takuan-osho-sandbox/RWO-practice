module Time_ns = Core_kernel.Time_ns

module Log_entry = struct
  type t = {
    important: bool;
    message: string;
  }
end

module HeartBeat = struct
  type t = {
    status_message: string;
  }
end

module Logon = struct
  type t = {
    user: string;
    credentials: string;
  }
end

type details =
  | Logon of Logon.t
  | HeartBeat of HeartBeat.t
  | Log_entry of Log_entry.t

module Common = struct
  type t = {
    session_id: string;
    time: Time_ns.t;
  }
end

let messages_for_user user (messages : (Common.t * details) list) =
  let (user_messages, _) =
    List.fold messages ~init:([], String.Set.empty)
      ~f:(fun ((messages, user_sessions) as acc) ((common, details) as message) ->
        match details with
        | Logon m ->
          if m.user = user then
            (message::messages, Set.add user_sessions common.session_id)
          else acc
        | HeartBeat _ | Log_entry _ ->
          if Set.mem user_sessions common.session_id then
            (message::messages, user_sessions)
          else acc
      )
  in
  List.rev user_messages