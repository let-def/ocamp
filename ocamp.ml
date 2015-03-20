open Utils

let main () =
  let is_client =
    try ignore (Sys.getenv Ocamp_common.env_socket : string); true
    with Not_found -> false
  in
  if is_client then
    Ocamp_client.main ()
  else
    Ocamp_server.main ()

let () = main ()
