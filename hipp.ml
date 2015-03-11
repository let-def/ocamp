open Utils

let main () =
  let is_client =
    try ignore (Sys.getenv "HIPP_PATH" : string); true
    with Not_found -> false
  in
  if is_client then
    Hipp_client.main ()
  else
    Hipp_server.main ()

let () = main ()
