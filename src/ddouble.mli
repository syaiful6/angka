type t
(** The type of ddouble *)

val zero : t
(** The ddouble 0 *)

val one : t
(** The ddouble 1 *)

val two : t
(** The ddouble 2 *)

val ten : t
(** The ddouble 10 *)

val nan : t
(** The Not a Number of ddouble *)

val of_float : float -> t
(** Convert the given floating point number to ddouble *)

val of_int : int -> int -> t
(** Craete a ddouble x such that x equal i*10e *)

val abs : t -> t
(** Absoulte value *)

val succ : t -> t
(** Returns its argument plus one *)

val pred : t -> t
(** Return its argument minus one *)

val neg : t -> t
(** Unary negation *)

val add : t -> t -> t
(** Addition. *)

val sub : t -> t -> t
(** Subtraction. *)

val mul : t -> t -> t
(** Multiplication. *)

val sqr : t -> t
(** Multiply `x` with itself *)

val div : t -> t -> t
(** Division *)

val rem : t -> t -> t
(** ddouble remainder *)

val div_rem : t -> t -> t * t
(** Compute both the ddouble quotien and ramainder *)

val compare : t -> t -> int
(** Comparison. compare x y returns 0 if x equals y, -1 if x is smaller than y,
  and 1 if x is greater than y *)

val equal : t -> t -> bool
(** Equality test. *)

val nearly_equal : t -> t -> ?epsilon:t -> unit -> bool
(** Return if two `ddouble`s are nearly equal with respect to some `epsilon` *)

val leq : t -> t -> bool
(** Less than or equal *)

val geq : t -> t -> bool
(** Greater than or equal *)

val lt : t -> t -> bool
(** Less than (and not equal) *)

val gt : t -> t -> bool
(** Greater than (and not equal) *)

val min : t -> t -> t
(** Returns the minimum of its arguments. *)

val max : t -> t -> t
(** Returns the maximum of its arguments *)

val decode : t -> float * float
(** Decode a ddouble `d` into two floats, `(hi,lo)` such that `d` equals  `hi`+`lo`. *)

val encode : float -> float -> t
(**  Encode a ddouble `d` from two floats `(hi,lo)` such that `d` equals  `hi`+`lo`. *)

val to_float : t -> float
(** Convert ddouble to float, losing the precision *)

val round : t -> t
(** Round a ddouble *)

val floor : t -> t
(** Round a ddouble *)

val ceil : t -> t
(** Ceil a ddouble *)

val truncate : t -> t
(** Round toward zero *)

val fraction : t -> t

val ffraction : t -> t

val round_to_prec : t -> int -> t
(** Round a `:ddouble` to a specified precision. *)

val ldexp : t -> int -> t

val to_string : t -> ?prec:int -> unit -> string
(**  Show a `:ddouble` `x` with a given precision `prec` (=`-31`) *)

val parse_ddouble : string -> t option
(** Parse a :ddouble string to  *)