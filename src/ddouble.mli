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

val of_float : float -> t
(** Convert the given floating point number to ddouble *)

val of_float_exp : float -> int -> t
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