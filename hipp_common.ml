open Utils

(* Shared constants *)

let signature        = "OCAMP001"
let signature_length = String.length signature
let env_socket       = "OCAMP_PATH"
let env_key          = "OCAMP_KEY"
let env_binary       = "OCAMP"
let socket_name      = ".ocamp."

(* Simple engine using unix and shell as backend *)

module Command = struct
  type t = {
    exec_dir: string;
    exec_args: string array
  }

  let compare (a : t) b = compare a b
  let hash (a : t) = Hashtbl.hash a
  let equal (a : t) b = a = b

  let to_string t =
    let is_special = function
      | '\'' | '"' | ' ' | '$' -> true
      | _ -> false
    in
    let is_special s =
      try
        for i = 0 to String.length s - 1 do
          if is_special s.[i] then
            raise Not_found
        done;
        false
      with Not_found -> true
    in
    let args = Array.map
        (fun s -> if is_special s then "\"" ^ String.escaped s ^ "\"" else s)
        t.exec_args
    in
    "[" ^ t.exec_dir ^ "] " ^ (String.concat " " (Array.to_list args))

  type query = {
    key : string;
    cwd : string;
    vars : string list;
    request : t;
  }

  type env = {
    stdin  : Lwt_unix.file_descr option ref;
    stdout : Lwt_unix.file_descr option ref;
    stderr : Lwt_unix.file_descr option ref;
  }

  type command = {
    query: query;
    env: env;
    socket: Lwt_unix.file_descr;
    input: Lwt_io.input_channel;
    output: Lwt_io.output_channel;
  }

  let unexpected_error fn exn =
    prerr_endline ("Unexpected error in " ^ fn);
    prerr_endline (Printexc.to_string exn)

  let try_close fd =
    try Lwt_unix.close fd
    with exn ->
      unexpected_error "Command.try_close" exn;
      Lwt.return_unit
  let close_fd r = match !r with
    | None -> Lwt.return_unit
    | Some fd -> r := None; try_close fd

  let close_command {env; socket; input} =
    Lwt.join [close_fd env.stdin; close_fd env.stdout; close_fd env.stderr]
    >>= fun () ->
    Lwt_io.close input >>= fun () ->
    Lwt.catch
      (fun () -> Lwt_unix.close socket)
      (fun exn ->
        unexpected_error "Command.close_command" exn;
        Lwt.return_unit)

  let connect_client socket =
    let io_vector n =
      let iov_buffer = String.make n '_' in
      { Lwt_unix. iov_buffer; iov_offset = 0; iov_length = n } in
    let header = io_vector signature_length in
    let fd_desc = [io_vector 1; io_vector 1; io_vector 1] in
    let io_vectors = header :: fd_desc in
    let sz = List.fold_left (fun s v -> s + v.Lwt_unix.iov_length) 0 io_vectors in
    Lwt_unix.recv_msg ~socket ~io_vectors >>= fun (sz',fds) ->
    let fds = List.map Lwt_unix.of_unix_file_descr fds in
    let fail exn =
      Lwt_list.iter_p try_close fds >>= fun () ->
      Lwt.fail exn in
    (if sz = sz' then Lwt.return_unit else
       fail (Invalid_argument
           (Printf.sprintf "Command.connect_client: expecting %d bytes, got %d" sz sz')))
    >>= fun () ->
    (if header.Lwt_unix.iov_buffer = signature then Lwt.return_unit else
       fail (Invalid_argument "Command.connect_client: Protocol mismatch"))
    >>= fun () ->
    let acc = List.map (fun v -> v.Lwt_unix.iov_buffer) fd_desc, fds in
    let pop s = function
      | (" " :: iovs),fds ->
        Lwt.return (None, (iovs, fds))
      | (s' :: iovs),(fd :: fds) when s = s' ->
        Lwt.return (Some fd, (iovs, fds))
      | _ -> fail (Invalid_argument "Command.connect_client: invalid file descriptors")
    in
    pop "i" acc >>= fun (stdin , acc) ->
    pop "o" acc >>= fun (stdout, acc) ->
    pop "e" acc >>= fun (stderr, acc) ->
    let env = { stdin = ref stdin; stdout = ref stdout; stderr = ref stderr } in
    let input = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.input socket in
    let output = Lwt_io.of_fd ~close:Lwt.return ~mode:Lwt_io.output socket in
    Lwt.catch (fun () ->
      Lwt_io.read_value input >|= fun query -> {query; env; input; output; socket})
      (fun exn ->
        Lwt.join [Lwt_io.close input; Lwt_io.close output]
        >>= fun () -> fail exn)

end

module CommandMap = Map.Make (Command)
module CommandSet = Set.Make (Command)
module CommandHash = Hashtbl.Make (Command)

(** Record the output of a process (stdout and exit status) to replay it later
    or concurrently *)
module Result = struct
  type status = Command.t list * Unix.process_status

  type chunks =
    | Chunk of string * chunks Lwt.t
    | Close of status Lwt.t

  type t = {
    mutable chunks: chunks Lwt.t;
    exit_status: status Lwt.t;
  }

  let equal_status t1 t2 =
    t1 >>= fun t1 -> t2 >|= fun t2 -> t1 = t2

  let equal_prefix p1 s1 p2 s2 =
    let l1 = String.length s1 - p1 and l2 = String.length s2 - p2 in
    try
      for i = 0 to min l1 l2 do
        if s1.[i] <> s2.[i] then
          raise Not_found
      done;
      if l1 = l2
      then `Equal
      else if l2 < l1
      then `S1 (p1 + l2)
      else `S2 (p2 + l1)
    with Not_found -> `Diff

  let rec equal_chunks p1 t1 p2 t2 = match t1, t2 with
    | Close t1, Close t2 -> equal_status t1 t2
    | Chunk ("",t1'), t2 ->
      t1' >>= fun t1 -> equal_chunks 0 t1 p2 t2
    | t1, Chunk ("",t2') ->
      t2' >>= fun t2 -> equal_chunks p1 t1 0 t2
    | Chunk (s1,t1'), Chunk (s2,t2') ->
      begin match equal_prefix p1 s1 p2 s2 with
        | `Equal ->
          t1' >>= fun t1 -> t2' >>= fun t2 ->
          equal_chunks 0 t1 0 t2
        | `S1 p1 ->
          t2' >>= fun t2 -> equal_chunks p1 t1 0 t2
        | `S2 p2 ->
          t1' >>= fun t1 -> equal_chunks 0 t1 p2 t2
        | `Diff -> Lwt.return_false
      end
    | _ -> Lwt.return_false

  let equal t1 t2 =
    t1.chunks >>= fun t1 ->
    t2.chunks >>= fun t2 ->
    equal_chunks 0 t1 0 t2

  let rec pack buffer = function
    | Chunk (s, next) ->
      Buffer.add_string buffer s;
      if Buffer.length buffer >= 4096 then
        let s = Buffer.contents buffer in
        Buffer.clear buffer;
        Lwt.return (Chunk (s, next >>= pack buffer))
      else
        next >>= pack buffer
    | Close status ->
      let s = Buffer.contents buffer in
      if s = "" then
        Lwt.return (Close status)
      else
        Lwt.return (Chunk (s, Lwt.return (Close status)))
  let pack chunks = pack (Buffer.create 4096) chunks

  let of_chunks ?(packed=true) chunks =
    let rec exit_status = function
      | Chunk (_,next) -> next >>= exit_status
      | Close status -> status in
    let result = { chunks; exit_status = chunks >>= exit_status } in
    let pack () =
      result.exit_status >|= fun _ ->
        result.chunks >|= fun chunks ->
          result.chunks <- pack chunks in
    if packed then Lwt.async pack;
    result

  let of_status status =
    of_chunks ~packed:false (Lwt.return (Close status))

  let none = of_status (Lwt.return ([], Unix.WEXITED 255))
  let ok = of_status (Lwt.return ([], Unix.WEXITED 0))

  let of_process deps p =
    let buffer = Bytes.to_string (Bytes.create 1024) in
    let rec aux () =
      Lwt_io.read_into p#stdout buffer 0 1024 >>= fun got ->
      if got = 0 then
        Lwt.return (Close (Lwt.map (fun status -> !deps, status) p#close))
      else
        Lwt.return (Chunk (String.sub buffer 0 got, aux ())) in
    of_chunks ~packed:true (aux ())

  let chunks t = t.chunks
  let exit_status t = t.exit_status

  let rec dump_chunks output = function
    | Chunk ("",next) -> next >>= dump_chunks output
    | Chunk (s,next) ->
      write_string s output >>= fun () ->
      next >>= dump_chunks output
    | Close exit_status -> Lwt.return exit_status

  let dump_to output {chunks} = chunks >>= dump_chunks output
end

(* We need to keep some context associated with unix processes.
   To carry this information through user scripts, we use an environment
   variable.
   The Builder structure gives a unique string key to arbitrary ocaml values,
   and allows to retrieve the value of a key while the associated lwt process
   is alive. *)
module Builder : sig
  type 'a t
  type key = string

  val create : unit -> 'a t
  val with_key : 'a t -> key -> ('a option -> 'b Lwt.t) -> 'b Lwt.t
  val with_value : 'a t -> 'a -> ?on_release:('a -> unit Lwt.t) -> (key -> 'b Lwt.t) -> 'b Lwt.t
end = struct
  type key = string
  type 'a cell = 'a * int ref * ('a -> unit Lwt.t) option
  type 'a t = (string, 'a cell) Hashtbl.t

  let create () : 'a t = Hashtbl.create 7

  let fresh_key =
    let counter = ref 0 in
    fun () -> incr counter; string_of_int !counter

  let get builder key =
    try Some (Hashtbl.find builder key)
    with Not_found -> None

  let decr_cell builder (a,counter,release_action) key () =
    decr counter;
    if !counter = 0 then
      begin
        Hashtbl.remove builder key;
        match release_action with
        | None -> ()
        | Some f -> Lwt.async (fun () -> f a)
      end;
    Lwt.return_unit

  let with_key builder key f =
    match get builder key with
    | Some (t, counter, _ as cell) ->
      incr counter;
      Lwt.finalize
        (fun () -> f (Some t))
        (decr_cell builder cell key)
    | None -> f None

  let with_value builder value ?on_release f =
    let key = fresh_key () in
    let cell = (value, ref 1, on_release) in
    Hashtbl.add builder key cell;
    Lwt.finalize
      (fun () -> f key)
      (decr_cell builder cell key)
end

