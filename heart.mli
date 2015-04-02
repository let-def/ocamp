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
type t

val fresh : unit -> t

val is_broken : t -> bool

val break : t -> unit
val fragilize : t -> t list -> unit

val broken : t
val join : t list -> t
