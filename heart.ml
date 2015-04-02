(*
  OCamp by Frédéric Bour <frederic.bour(_)lakaban.net>
  To the extent possible under law, the person who associated CC0 with
  OCamp has waived all copyright and related or neighboring rights to OCamp.

  You should have received a copy of the CC0 legalcode along with this
  work. If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

  Website: https://github.com/def-lkb/ocamp

  Version 0.1, April 2015

  --

  The concept of 'Heart' is inspired from Jenga,
  https://github.com/janestreet/jenga
*)
type t = { mutable deps: t list; mutable broken: bool }

let fresh () = { deps = []; broken = false }

let is_broken t = t.broken

let rec break t =
  let deps = t.deps in
  t.deps <- []; t.broken <- true;
  List.iter break deps

let rec register_dep g = function
  | [] -> ()
  | t :: ts ->
    t.deps <- g :: t.deps;
    register_dep g ts

let fragilize g ts =
  if not (is_broken g) then begin
    if List.exists is_broken ts then
      break g
    else
      register_dep g ts
  end

let broken = { deps = []; broken = true }

let join ts =
  if List.exists is_broken ts then
    broken
  else
    let g = fresh () in
    register_dep g ts;
    g

