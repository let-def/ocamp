let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

(* Write complete string *)
let rec write_string s i l fd =
  if l <= 0 then Lwt.return_unit
  else begin
    Lwt_unix.write fd s i l >>= fun wrote ->
    write_string s i (l - wrote) fd
  end

let write_string s = function
  | None -> Lwt.return_unit
  | Some fd -> write_string s 0 (String.length s) fd

(* Manipulate file system paths *)
module Path = struct
  (** [split path] turns [path] into a pair of a directory and a filename *)
  let split path = Filename.dirname path, Filename.basename path

  (** [split_all path acc] turns [path] into a list of
      components prepended to [acc] *)
  let rec split_all path acc =
    match split path with
    | dir, _ when dir = path -> dir :: acc
    | dir, base -> split_all dir (base :: acc)

  (** [flatten parts] turns a list of filesystem components into one path
      flatten [] = ""
      flatten ["usr";"share"] = "usr/share"
    *)
  let flatten = function
    | [] -> ""
    | root :: subs -> List.fold_left Filename.concat root subs

  (** [canonicalize ?cwd path] turns [path] into an absolute directory,
      resolving relative paths from ?cwd or Sys.getcwd() *)
  let canonicalize ?cwd path =
    let parts =
      match split_all path [] with
      | dot :: rest when dot = Filename.current_dir_name ->
        split_all (match cwd with None -> Sys.getcwd () | Some c -> c) rest
      | parts -> parts in
    let goup path = function
      | dir when dir = Filename.parent_dir_name ->
        (match path with _ :: t -> t | [] -> [])
      | dir when dir = Filename.current_dir_name ->
        path
      | dir -> dir :: path in
    flatten (List.rev (List.fold_left goup [] parts))

  let mtime filename =
    Lwt.catch
      (fun () -> Lwt.map (fun st -> st.Lwt_unix.st_mtime)
          (Lwt_unix.stat filename))
      (fun _exn -> Lwt.return nan)
end
