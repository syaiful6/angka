type t
(** The decimal type *)

val zero : t
(** The decimal zero *)

val is_zero : t -> bool

val of_zarith : Z.t -> int -> t
(** Create a decimal with the given integer and exponent *)

val neg : t -> t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val div_with : t -> t -> ?min_prec:int -> unit -> t

type round =
    HalfEven
  | HalfCeil
  | HalfFloor
  | HalfTruncate
  | HalfAwayFromZero
  | Ceil
  | Floor
  | Truncate
  | AwayFromZero

val round_to_prec : t -> ?prec:int -> ?round:round -> unit -> t