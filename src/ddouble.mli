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

val neg : t -> t
(** Unary negation *)

val add : t -> t -> t
(** Addition. *)

val sub : t -> t -> t
(** Subtraction. *)

val mul : t -> t -> t
(** Multiplication. *)

val div : t -> t -> t
(** Division *)

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

val to_string : t -> ?prec:int -> unit -> string
(**  Show a `:ddouble` `x` with a given precision `prec` (=`-31`) *)
