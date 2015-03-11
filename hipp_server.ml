open Utils
open Hipp_common

module Make ( ) =
struct

  let keys : (?hint:unit -> Command.t -> Result.t Lwt.t) Builder.t =
    Builder.create ()

  (* Server loop, waiting for clients on unix socket *)
  module Runner = struct
    let server_path =
      Path.canonicalize (".hipp." ^ string_of_int (Unix.getpid ()))
    let () = at_exit (fun () -> Unix.unlink server_path)

    let server_socket = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0

    let () =
      Lwt_unix.set_close_on_exec server_socket;
      begin
        try Unix.unlink server_path
        with _ -> ()
      end;
      Lwt_unix.bind server_socket (Lwt_unix.ADDR_UNIX server_path);
      Lwt_unix.listen server_socket 0;
      Unix.putenv env_socket server_path

    let handle_client (fd,_addr) =
      let open Command in
      Lwt.catch (fun () -> connect_client fd)
        (fun exn -> Lwt_unix.close fd >>= fun () -> Lwt.fail exn)
      >>= fun command ->
      let finish () =
        Lwt_io.flush command.output >>= fun () ->
        close_command command
      in
      Lwt.finalize
        begin fun () ->
          let query = command.query in
          Builder.with_key keys query.key @@ function
          | None ->
            write_string (env_key ^ " is not valid") !(command.env.stderr)
          | Some builder ->
            builder query.request >>= fun result ->
            Result.dump_to !(command.env.stdout) result >>= fun status ->
            close_fd command.env.stdout >>= fun () ->
            status >>= fun status ->
            Lwt_io.write_value command.output status
        end
        finish

    let () =
      let rec loop () =
        Lwt_unix.accept server_socket >>= fun client ->
        Lwt.async (fun () -> handle_client client);
        loop () in
      Lwt.async loop
  end

  module Backend = struct
    type command = Command.t
    let compare_command = Command.compare
    let print_command = Command.to_string

    type hint = unit
    type result = Result.t

    let do_execute ?hint ~key {Command. exec_dir; exec_args} =
      let vars = Array.of_list ["HIPP_KEY=" ^ key] in
      let env = Array.append vars (Unix.environment ()) in
      (* Lwt_process don't allow specifying working directory ?! *)
      Unix.chdir exec_dir;
      let command = (exec_args.(0), exec_args) in
      let process = Lwt_process.open_process_in ~env command in
      let result = Result.of_process process in
      Lwt.async (fun () -> Builder.with_key keys
                    key (fun _ -> Result.exit_status result));
      result

    let execute ~build ?cached ?hint command =
      match cached with
      | Some result -> Lwt.return result
      | None ->
        Builder.with_value keys build @@ fun key ->
        Lwt.return (do_execute ~key ?hint command)
  end

  (* Empty database *)
  module DontResume = struct
    let cached _ = Lwt.return_none
  end

  module Hipp = Hipp_engine.Make (Backend)(DontResume)

  let join a = a >>= fun x -> x

  let main command  =
    let binary = Path.canonicalize Sys.executable_name in
    Unix.putenv env_binary binary;
    Hipp.build command >>= fun result ->
    join (Result.dump_to (Some Lwt_unix.stdout) result) >>= fun status ->
    begin match status with
      | Unix.WEXITED n ->
        prerr_endline ("-- exited with status " ^ string_of_int n)
      | Unix.WSIGNALED n ->
        prerr_endline ("-- killed by signal " ^ string_of_int n)
      | Unix.WSTOPPED n ->
        prerr_endline ("-- stopped by signal " ^ string_of_int n)
    end;
    Lwt.return status
end

(* Command line interface *)
open Cmdliner

let command_exec input arguments =
  let exec_dir = Path.canonicalize "." in
  let exec_args = Array.of_list arguments in
  let module M = Make () in
  match Lwt_main.run (M.main {Command. exec_dir; exec_args}) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> exit n
  | _ -> exit (-1)

let command_exec =
  let arguments = Arg.(non_empty & pos_all string [] & info [] ~docv:"ARGS") in
  let doc = "Execute a command and memoize its result" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) will execute the command represented by the rest of the arguments.";
    `P "This command might get executed again later, if the result changes the target will be recomputed.";
  ] in
  Term.(pure command_exec $ pure None $ arguments),
  Term.info "exec" ~version:"0.0.1" ~doc ~man

let commands = [command_exec]

let main () =
  match Term.eval command_exec with
  | `Error _ -> exit 1
  | _ -> exit 0
