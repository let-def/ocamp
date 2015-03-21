open Utils
open Common

module Make ( ) = struct

  open Command

  let make_query ?(env=[]) action request =
    let key =
      try Sys.getenv env_key
      with Not_found ->
        invalid_arg ("Client.make_command: no " ^ env_key ^ " in environment")
    in
    { key; cwd = Sys.getcwd (); action; request; vars = env }

  let connect_server () =
    let path =
      try Sys.getenv env_socket
      with Not_found ->
        invalid_arg ("Client.connect_server: no " ^ env_socket ^ " in environment")
    in
    let socket = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
    Lwt.catch
      (fun () ->
         Lwt_unix.connect socket (Lwt_unix.ADDR_UNIX path) >>= fun () ->
         let io_vector s =
           { Lwt_unix. iov_buffer = s; iov_offset = 0; iov_length = String.length s } in
         let io_vectors = List.map io_vector [signature;"i";"o";"e"] in
         let fds = [Unix.stdin; Unix.stdout; Unix.stderr] in
         Lwt_unix.send_msg ~socket ~io_vectors ~fds >>= fun _wrote -> (* FIXME *)
         Lwt.return socket)
      (fun exn -> Lwt_unix.close socket >>= fun () -> Lwt.fail exn)

  let execute_command (query : Command.query) =
    Lwt.catch (fun () ->
        connect_server () >>= fun socket ->
        let input = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.input socket in
        let output = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.output socket in
        Lwt_io.write_value output query >>= fun () ->
        Lwt_io.flush output >>= fun () ->
        Lwt_io.read_value input >>= fun (status : Unix.process_status) ->
        Lwt.join [Lwt_io.close input; Lwt_io.close output] >>= fun () ->
        Lwt_unix.close socket >>= fun () ->
        Lwt.return (Some status)
      )
      (function
        | Unix.Unix_error (Unix.ENOENT, "connect", "") ->
          prerr_endline "--- cannot connect to server";
          Lwt.return_none
        | End_of_file ->
          prerr_endline "--- build error, server closed connection";
          Lwt.return_none
        | exn -> Lwt.fail exn)


  let main ?env action command =
    let query = make_query ?env action command in
    let status = Lwt_main.run (execute_command query) in
    exit (match status with
        | None -> (-1)
        | Some (Unix.WEXITED n) -> n
        | Some (Unix.WSIGNALED n) -> (-1)
        | Some (Unix.WSTOPPED n) -> (-1))
end

let main ?env action command =
  let module M = Make () in
  M.main ?env action command

(* Command line interface *)
open Cmdliner

let command_pull =
  let run input arguments =
    let exec_dir = Path.canonicalize "." in
    let exec_args = Array.of_list arguments in
    main `Pull {Command. exec_dir; exec_args}
  in
  let arguments = Arg.(non_empty & pos_all string [] & info [] ~docv:"ARGS") in
  let doc = "Execute a command and memoize its result" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) will execute the command represented by the rest of the arguments.";
    `P "This command might get executed again later, if the result changes the target will be recomputed.";
  ] in
  Term.(pure run $ pure None $ arguments),
  Term.info "pull" ~version:"0.0.1" ~doc ~man

let command_hipp =
  let run input arguments =
    let exec_dir = Path.canonicalize "." in
    let exec_args = Array.of_list arguments in
    main `Hipp {Command. exec_dir; exec_args}
  in
  let arguments = Arg.(non_empty & pos_all string [] & info [] ~docv:"ARGS") in
  let doc = "Execute a command and memoize its result" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) will execute the command represented by the rest of the arguments.";
    `P "This command might get executed again later, if the result changes the target will be recomputed.";
  ] in
  Term.(pure run $ pure None $ arguments),
  Term.info "hipp" ~version:"0.0.1" ~doc ~man

let command_stir =
  let run input arguments =
    let exec_dir = Path.canonicalize "." in
    let exec_args = Array.of_list arguments in
    main `Stir {Command. exec_dir; exec_args}
  in
  let arguments = Arg.(non_empty & pos_all string [] & info [] ~docv:"ARGS") in
  let doc = "Execute a command and memoize its result" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) will execute the command represented by the rest of the arguments.";
    `P "This command might get executed again later, if the result changes the target will be recomputed.";
  ] in
  Term.(pure run $ pure None $ arguments),
  Term.info "stir" ~version:"0.0.1" ~doc ~man

let command_is_valid =
  let run input arguments =
    let exec_dir = Path.canonicalize "." in
    let exec_args = Array.of_list arguments in
    main `Validate {Command. exec_dir; exec_args}
  in
  let arguments = Arg.(non_empty & pos_all string [] & info [] ~docv:"ARGS") in
  let doc = "Execute a command and memoize its result" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) will execute the command represented by the rest of the arguments.";
    `P "This command might get executed again later, if the result changes the target will be recomputed.";
  ] in
  Term.(pure run $ pure None $ arguments),
  Term.info "is-valid" ~version:"0.0.1" ~doc ~man

let command_follow =
  let run input arguments =
    let exec_dir = Path.canonicalize "." in
    let exec_args = Array.of_list arguments in
    main `Follow {Command. exec_dir; exec_args}
  in
  let arguments = Arg.(non_empty & pos_all string [] & info [] ~docv:"ARGS") in
  let doc = "Execute a command and memoize its result" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) will execute the command represented by the rest of the arguments.";
    `P "This command might get executed again later, if the result changes the target will be recomputed.";
  ] in
  Term.(pure run $ pure None $ arguments),
  Term.info "follow" ~version:"0.0.1" ~doc ~man

let command_unfollow =
  let run input arguments =
    let exec_dir = Path.canonicalize "." in
    let exec_args = Array.of_list arguments in
    main `Unfollow {Command. exec_dir; exec_args}
  in
  let arguments = Arg.(non_empty & pos_all string [] & info [] ~docv:"ARGS") in
  let doc = "Execute a command and memoize its result" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) will execute the command represented by the rest of the arguments.";
    `P "This command might get executed again later, if the result changes the target will be recomputed.";
  ] in
  Term.(pure run $ pure None $ arguments),
  Term.info "unfollow" ~version:"0.0.1" ~doc ~man

let commands = [command_pull; command_hipp; command_stir;
                command_is_valid; command_follow; command_unfollow]

let main () =
  match Term.eval_choice command_hipp commands with
  | `Error _ -> exit 1 | _ -> exit 0
