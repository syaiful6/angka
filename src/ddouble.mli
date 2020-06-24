type t

val create : float -> float -> t

val of_float : float -> t
val of_float_exp : float -> int -> t

val neg : t -> t

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t

val decode : t -> float * float