open Utils

let main () =
  let is_client =
    try ignore (Sys.getenv Common.env_socket : string); true
    with Not_found -> false
  in
  if is_client then
    Client.main ()
  else
    Server.main ()

let () = main ()
