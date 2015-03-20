open Utils
open Ocamp_common

module Make (P : sig
               val cache : Result.t CommandMap.t
             end) =
struct

  type builder = {
    build_func : Command.action -> Command.t -> Result.t;
    build_heart : Heart.t;
  }

  let builders : builder Builder.t =
    Builder.create ()

  (* Server loop, waiting for clients on unix socket *)
  module Runner = struct
    let server_path =
      Path.canonicalize (socket_name ^ string_of_int (Unix.getpid ()))
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
          Builder.with_key builders command.query.key @@ function
          | None ->
            write_string (env_key ^ " is not valid") !(command.env.stderr)
          | Some builder ->
            let query = command.query in
            let result = builder.build_func query.action query.request in
            Heart.fragilize builder.build_heart [result.Result.heart];
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

    type result = Result.t

    (*let rec verify first ~(build: command -> result Lwt.t) command =
      print_endline ("-- checking " ^ print_command command);
      match Hashtbl.find P.cache command with
      | result ->
        result.Result.exit_status >>= begin function
          | ([],_) when first ->
            Lwt.return_none
          | ([],_) ->
            build command >>= fun result' ->
            Result.equal result result' >>= fun equal ->
            if equal then
              Lwt.return (Some result)
            else
              Lwt.return_none
          | (deps,_) ->
            Lwt_list.exists_p (fun cmd ->
                verify false ~build cmd >|= function
                | None -> true
                | Some _ -> false)
              deps
            >|= fun b -> if b then Some result else None
        end
      | exception Not_found -> Lwt.return_none*)

    (* Followers structure *)

    (*type followers = { mutable followers: CommandSet.t; mutable workers : int }

    let followed : followers CommandMap.t ref = ref CommandMap.empty
    let update_followed command followers =
      if CommandSet.is_empty followers.followers && followers.workers = 0 then
        followed := CommandMap.remove command !followed
      else if not (CommandMap.mem command !followed) then
        followed := CommandMap.add command followers !followed

    let update_followers command result' result =
      let deps x = Lwt.catch (fun () -> Lwt.map Result.exit_status x) (fun
      let before =

    let roots : CommandSet.t ref = ref CommandSet.empty*)

    (* Result cache *)
    let cache : Result.t CommandMap.t ref =
      ref CommandMap.empty

    let update_cache command result =
      cache := CommandMap.add command result !cache

    let do_execute builder result resultf build_deps command =
      (* Lwt_process doesn't allow specifying working directory ?! *)
      Lwt.ignore_result
        begin
          Builder.with_value builders builder @@ fun key ->
          let vars = Array.of_list [env_key ^ "=" ^ key] in
          let env = Array.append vars (Unix.environment ()) in
          Unix.chdir command.Command.exec_dir;
          let process = Lwt_process.open_process_in ~env
              (command.Command.exec_args.(0), command.Command.exec_args) in
          (* Wait for command to finish *)
          Result.of_process build_deps process resultf;
        end;
      result

    let dont_record _ _ _ = ()

    let record deps action result command =
      match action with
      | `Hipp -> deps := Result.Hipp result :: !deps
      | `Pull -> deps := Result.Pull command :: !deps
      | `Stir | `Validate -> ()

    let is_fresh result = function
      | `Hipp | `Pull -> not (Heart.is_broken result.Result.heart)
      | `Stir -> Heart.break result.Result.heart; false

    let rec rebuild record_dep command =
      let build_deps = ref [] in
      let builder = {
        build_func = build (record build_deps);
        build_heart = Heart.fresh ();
      } in
      let result, resultf = Result.fresh command builder.build_heart in
      record_dep result command;
      update_cache command result;
      do_execute builder result resultf build_deps command

    and build record_dep action command =
      match action with
      | `Validate ->
        let result, resultf = Result.fresh command Heart.broken in
        let status = match CommandMap.find command !cache with
          | result' when Heart.is_broken result'.Result.heart -> 1
          | result' -> 0
          | exception Not_found -> 1
        in
        Lwt.ignore_result
          (Result.of_status (Lwt.return ([], Unix.WEXITED status)) resultf);
        result
      | (`Hipp | `Pull | `Stir) as action ->
        let record_dep = record_dep action in
        begin match CommandMap.find command !cache with
          | result ->
            if is_fresh result action then result
            else if action = `Hipp then
              refresh record_dep command result
            else rebuild record_dep command
          | exception Not_found ->
            rebuild record_dep command
        end

    and refresh record_dep command result_old =
      let heart = Heart.fresh () in
      let result, resultf = Result.fresh command heart in
      record_dep result command;
      update_cache command result;
      Lwt.ignore_result
        begin
          result_old.Result.exit_status >>= fun (deps,status) ->
          let is_broken =
            (* First find a broken pull deps *)
            List.exists (function
                | Result.Pull c ->
                  begin match CommandMap.find command !cache with
                    | dep_result -> not (is_fresh dep_result `Pull)
                    | exception Not_found -> assert false
                  end
                | Result.Hipp _ -> false)
              deps
          in
          let rebuild_me () =
            let build_deps = ref [] in
            let builder = {
              build_func = build (record build_deps);
              build_heart = heart;
            } in
            do_execute builder result resultf build_deps command
          in
          let rec refresh_hipps acc = function
            | Result.Hipp dep_result_old :: xs
              when Heart.is_broken dep_result_old.Result.heart ->
              let dep_result_new =
                build dont_record `Hipp dep_result_old.Result.command in
              Result.equal dep_result_old dep_result_new >>= fun is_equal ->
              if is_equal then
                refresh_hipps (Result.Hipp dep_result_new :: acc) xs
              else
                Lwt.return (rebuild_me ())
            | x :: xs -> refresh_hipps (x :: acc) xs
            | [] ->
              result_old.Result.chunks >>= fun chunks ->
              Result.copy_chunks (List.rev acc) chunks resultf >|= fun _ ->
              result
          in
          if is_broken then
            Lwt.return (rebuild_me ())
          else
            refresh_hipps [] deps
        end;
      result

    let entry command =
      Builder.with_value builders {
        build_func = build dont_record;
        build_heart = Heart.fresh ();
      } @@ fun key ->
      let vars = Array.of_list [env_key ^ "=" ^ key] in
      let env = Array.append vars (Unix.environment ()) in
      Unix.chdir command.Command.exec_dir;
      let process = Lwt_process.open_process_none ~env
          (command.Command.exec_args.(0), command.Command.exec_args) in
      process#status

  end

  let join a = a >>= fun x -> x

  let main command =
    let binary = Path.canonicalize Sys.executable_name in
    Unix.putenv env_binary binary;
    let result = Backend.entry command in
    result >>= fun status ->
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

let rec command_fire cache input arguments =
  let exec_dir = Path.canonicalize "." in
  let exec_args = Array.of_list arguments in
  let module M = Make (struct let cache = cache end) in
  match Lwt_main.run (M.main {Command. exec_dir; exec_args}) with
  (*| _, Unix.WEXITED 0 ->
    ignore (read_line ());
    command_exec cache input arguments*)
  | Unix.WEXITED n -> exit n
  | _ -> exit (-1)

let command_fire input arguments = command_fire CommandMap.empty input arguments

let command_fire =
  let arguments = Arg.(non_empty & pos_all string [] & info [] ~docv:"ARGS") in
  let doc = "Execute a command and memoize its result" in
  let man = [
    `S "DESCRIPTION";
    `P "$(tname) will execute the command represented by the rest of the arguments.";
    `P "This command might get executed again later, if the result changes the target will be recomputed.";
  ] in
  Term.(pure command_fire $ pure None $ arguments),
  Term.info "fire" ~version:"0.0.1" ~doc ~man

let commands = [command_fire]

let main () =
  match Term.eval command_fire with
  | `Error _ -> exit 1
  | _ -> exit 0
