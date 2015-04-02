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
