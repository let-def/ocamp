type t and glass

val fresh : unit -> glass
val heart : glass -> t

val is_broken : t -> bool
val break : glass -> unit
val fragilize : glass -> t list -> unit

val broken : t
val join : t list -> t
