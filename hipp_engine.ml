let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

(* Abstract presentation of the computation engine *)
module type Spec = sig
  type command
  val compare_command : command -> command -> int
  val print_command : command -> string

  type hint
  type result

  val cached
    :  build:(?hint:hint -> command -> result Lwt.t)
    -> command
    -> result option Lwt.t

  val execute
    :  build:(?hint:hint -> command -> result Lwt.t)
    -> ?cached:result
    -> ?hint:hint
    -> command
    -> result Lwt.t
end

module Make
    (* Specification of the build backend *)
    (S : Spec)
  :
sig
  module CommandMap : Map.S with type key = S.command
  exception Cycle of S.command * S.command list

  val state : unit -> S.result CommandMap.t
  val build : ?hint:S.hint -> S.command -> S.result Lwt.t
end = struct

  module Command = struct
    type t = S.command
    let compare = S.compare_command
  end
  module CommandSet = Set.Make(Command)
  module CommandMap = Map.Make(Command)

  exception Cycle of S.command * S.command list
  let () = Printexc.register_printer (function
      | Cycle (cmd,cmds) ->
        Some (
          "Build cycle:\n" ^
          S.print_command cmd ^ " is required by: " ^
          String.concat "\n - " ("" :: List.map S.print_command cmds))
      | _ -> None)

  module Trace = struct
    type t = CommandSet.t * S.command list
    let add target (set,targets) =
      if CommandSet.mem target set then
        let rec deps acc = function
          | target' :: _ when S.compare_command target target' = 0 ->
            target' :: acc
          | target' :: rest -> deps (target' :: acc) rest
          | [] -> assert false in
        raise (Cycle (target, deps [] targets))
      else
        (CommandSet.add target set, target :: targets)

    let empty = (CommandSet.empty, [])
  end

  let result_database = ref CommandMap.empty
  let process_database = ref CommandMap.empty

  let rec rebuild ?hint ~trace target () =
    assert (not (CommandMap.mem target !result_database));
    let build = build ~trace in
    S.cached ~build target >>= fun cached ->
    S.execute ~build ?cached ?hint target >|= fun result ->
    result_database := CommandMap.add target result !result_database;
    result

  and build ?hint ~trace target =
    let trace = Trace.add target trace in
    try
      let result = CommandMap.find target !process_database in
      prerr_endline ("--- already built " ^ S.print_command target);
      result
    with Not_found ->
      prerr_endline ("--- building fresh " ^ S.print_command target);
      let result = Lwt_unix.yield () >>= rebuild ?hint ~trace target in
      process_database := CommandMap.add target result !process_database;
      result

  let build ?hint target = build ~trace:Trace.empty ?hint target
  let state () = !result_database
end


