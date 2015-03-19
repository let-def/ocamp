type t = { mutable deps: t list; mutable broken: bool }
type glass = t

type t and glass

let fresh () = { deps = []; broken = false }
let heart x = x

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
  if List.exists is_broken ts then
    break g
  else
    register_dep g ts

let broken = { deps = []; broken = true }

let join ts =
  if List.exists is_broken ts then
    broken
  else
    let g = fresh () in
    register_dep g ts;
    g

