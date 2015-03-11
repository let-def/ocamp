open Utils
open Hipp_common

module Make ( ) = struct

  open Command

  let make_query ?(env=[]) request =
    let key =
      try Sys.getenv "HIPP_KEY"
      with Not_found ->
        invalid_arg "Client.make_command: no HIPP_KEY in environment"
    in
    { key; cwd = Sys.getcwd (); request; vars = env }

  let connect_server () =
    let path =
      try Sys.getenv "HIPP_PATH"
      with Not_found ->
        invalid_arg "Client.connect_server: no HIPP_PATH in environment"
    in
    let socket = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
    Lwt.catch (fun () ->
      Lwt_unix.connect socket (Lwt_unix.ADDR_UNIX path) >>= fun () ->
      let io_vector s =
        { Lwt_unix. iov_buffer = s; iov_offset = 0; iov_length = String.length s } in
      let io_vectors = List.map io_vector [Command.signature;"i";"o";"e"] in
      let fds = [Unix.stdin; Unix.stdout; Unix.stderr] in
      Lwt_unix.send_msg ~socket ~io_vectors ~fds >>= fun _wrote -> (* FIXME *)
      Lwt.return socket)
      (fun exn -> Lwt_unix.close socket >>= fun () -> Lwt.fail exn)

  let execute_command (query : Command.query) =
    connect_server () >>= fun socket ->
    let input = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.input socket in
    let output = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.output socket in
    Lwt_io.write_value output query >>= fun () ->
    Lwt_io.flush output >>= fun () ->
    Lwt_io.read_value input >>= fun (status : Unix.process_status) ->
    Lwt.join [Lwt_io.close input; Lwt_io.close output] >>= fun () ->
    Lwt_unix.close socket >>= fun () ->
    Lwt.return status

  let main ?env actions =
    let query = make_query ?env actions in
    let status = Lwt_main.run (execute_command query) in
    exit (match status with
          | Unix.WEXITED n -> n
          | Unix.WSIGNALED n -> (-1)
          | Unix.WSTOPPED n -> (-1))
end

let main ?env actions =
  let module M = Make () in
  M.main ?env actions

let command_exec input arguments =
  let exec_dir = Path.canonicalize "." in
  let exec_args = Array.of_list arguments in
  main {exec_dir; exec_args}

(* Command line interface *)
open Cmdliner

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
  | `Error _ -> exit 1 | _ -> exit 0
