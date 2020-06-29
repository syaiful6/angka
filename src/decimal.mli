type t
(** The decimal type *)

val zero : t
(** The decimal zero *)

val is_zero : t -> bool

val of_zarith : Z.t -> ?exp:int -> unit -> t
(** Create a decimal with the given integer and exponent *)

val of_int : int -> ?exp:int -> unit -> t

val of_float : float -> ?prec:int -> unit -> t

val neg : t -> t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val div_with : t -> t -> ?min_prec:int -> unit -> t

val succ : t -> t

val pred : t -> t

val sign : t -> int
(** Return -1, 0 and 1, when the argument is negative, zero or positive *)

val compare : t -> t -> int
(** Comparison. compare x y returns 0 if x equals y, -1 if x is smaller than y,
  and 1 if x is greater than y *)

val equal : t -> t -> bool

val leq : t -> t -> bool

val geq : t -> t -> bool

val lt : t -> t -> bool

val gt : t -> t -> bool

val min : t -> t -> t

val max : t -> t -> t

val abs : t -> t

val is_even : t -> bool

val is_odd : t -> bool

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

val get_exponent : t -> int

val to_string : t -> ?prec:int -> unit -> string

val to_string_fixed : t -> ?prec:int -> unit -> string

val to_string_exponent : t -> ?prec:int -> unit -> string
