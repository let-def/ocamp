type t

val fresh : unit -> t

val is_broken : t -> bool
val break : t -> unit
val fragilize : t -> t list -> unit

val broken : t
val join : t list -> t
