(*
  OCamp by Frédéric Bour <frederic.bour(_)lakaban.net>
  To the extent possible under law, the person who associated CC0 with
  OCamp has waived all copyright and related or neighboring rights to OCamp.

  You should have received a copy of the CC0 legalcode along with this
  work. If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

  Website: https://github.com/def-lkb/ocamp

  Version 0.1, April 2015
*)
open Utils
open Common

module Make (P : sig
               val cache : Result.t CommandMap.t
             end) =
struct

  type builder = Command.action -> Command.t -> Result.t

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
            let result = builder query.action query.request in
            Result.dump_to !(command.env.stdout) result >>= fun status ->
            close_fd command.env.stdout >>= fun () ->
            status >>= fun (_,status) ->
            Lwt_io.write_value command.output (status : Unix.process_status)
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

    (* Result cache *)
    let cache : Result.t CommandMap.t ref =
      ref CommandMap.empty

    let followed : CommandSet.t ref = ref CommandSet.empty

    let update_cache command result =
      cache := CommandMap.add command result !cache

    let do_execute builder result resultf build_deps command =
      Lwt.ignore_result
        begin
          Builder.with_value builders builder @@ fun key ->
          let vars = Array.of_list [env_key ^ "=" ^ key] in
          let env = Array.append vars (Unix.environment ()) in
          (* Lwt_process doesn't allow specifying working directory ?! *)
          Unix.chdir command.Command.exec_dir;
          let process = Lwt_process.open_process_in ~env
              (command.Command.exec_args.(0), command.Command.exec_args) in
          (* Wait for command to finish *)
          Result.of_process build_deps process resultf;
        end;
      result

    let dont_record _ _ = ()

    let record build_heart build_deps
        action ({Result. heart; command} as result)
      =
      match action with
      | `Hipp ->
        Heart.fragilize build_heart [heart];
        build_deps := Result.Hipp result :: !build_deps
      | `Pull ->
        Heart.fragilize build_heart [heart];
        build_deps := Result.Pull (heart, command) :: !build_deps
      | `Stir | `Validate -> ()

    let fresh_result command heart =
      let result = Result.fresh command heart in
      update_cache command (fst result);
      result

    let rec rebuild record_dep command =
      let build_deps = ref [] in
      let build_heart = Heart.fresh () in
      let builder = build (record build_heart build_deps) in
      let result, resultf = fresh_result command build_heart in
      record_dep result;
      do_execute builder result resultf build_deps command

    and build record_dep action command =
      match action with
      | `Validate ->
        let result, resultf = Result.fresh command Heart.broken in
        let status = match CommandMap.find command !cache with
          | result' ->
            Lwt.ignore_result
              begin
                result'.Result.exit_status >|= fun (deps,_) ->
                List.iter (function
                    | Result.Pull (_,cmd) ->
                      print_endline ("pull " ^ print_command cmd);
                    | Result.Hipp result ->
                      print_endline ("hipp " ^ print_command result.Result.command)
                  ) deps
              end;
            if Heart.is_broken result'.Result.heart then 1 else 0
          | exception Not_found -> 1
        in
        Lwt.ignore_result
          (Result.of_status (Lwt.return ([], Unix.WEXITED status)) resultf);
        result
      | (`Unfollow | `Follow) as action ->
        let status =
          if (CommandSet.mem command !followed) <> (action = `Follow) then
            0
          else
            1
        in
        followed :=
          (match action with
           | `Unfollow -> CommandSet.remove
           | `Follow -> CommandSet.add) command !followed;
        let result, resultf = Result.fresh command Heart.broken in
        Lwt.ignore_result
          (Result.of_status (Lwt.return ([], Unix.WEXITED status)) resultf);
        result
      | (`Hipp | `Pull | `Stir) as action ->
        let record_dep = record_dep action in
        begin match CommandMap.find command !cache with
          | result when action = `Stir ->
            Heart.break result.Result.heart;
            rebuild record_dep command
          | result when not (Heart.is_broken result.Result.heart) -> result
          | result -> refresh record_dep command result
          | exception Not_found -> rebuild record_dep command
        end

    and refresh record_dep command result_old =
      let build_heart = Heart.fresh () in
      let result, resultf = fresh_result command build_heart in
      record_dep result;
      Lwt.ignore_result
        begin
          result_old.Result.exit_status >>= fun (deps,status) ->
          (* Do we need to rebuild? *)
          let rebuild_me () =
            let build_deps = ref [] in
            let builder = build (record build_heart build_deps) in
            do_execute builder result resultf build_deps command
          in
          (* First find a broken pull deps *)
          let is_broken =
            List.exists (function
                | Result.Pull (heart,_) -> Heart.is_broken heart
                | Result.Hipp _ -> false)
              deps
          in
          if is_broken then
            Lwt.return (rebuild_me ())
          else
            (* Then refresh hipp deps *)
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
                List.iter (fun dep ->
                    let dep_result = match dep with
                      | Result.Hipp r -> r
                      | Result.Pull (_,c) ->
                        try CommandMap.find command !cache
                        with Not_found -> assert false
                    in
                    Heart.fragilize build_heart [dep_result.Result.heart])
                  acc;
                result_old.Result.chunks >>= fun chunks ->
                Result.copy_chunks (List.rev acc) chunks resultf >|= fun _ ->
                result
            in
            refresh_hipps [] deps
        end;
      result

    let entry command =
      Builder.with_value builders (build dont_record) @@ fun key ->
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
    let rec follow_jobs () =
      let refresh command =
        let need_rebuild = match CommandMap.find command !Backend.cache with
          | result -> Heart.is_broken result.Result.heart
          | exception Not_found -> true
        in
        if need_rebuild then
          ignore (Backend.build Backend.dont_record `Pull command);
      in
      CommandSet.iter refresh !Backend.followed;
      if Lwt.is_sleeping result then
      Lwt_unix.sleep 1.0 >>= follow_jobs
      else Lwt.return_unit
    in
    Lwt.async follow_jobs;
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
