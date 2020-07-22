type t
(** The decimal type *)

val zero : t
(** The decimal zero *)

val is_zero : t -> bool
(** The if the argument passed is decimal zero *)

val of_zarith : Z.t -> ?exp:int -> unit -> t
(** Create a decimal with the given integer and exponent *)

val of_int : int -> ?exp:int -> unit -> t
(** Create a decimal from an integer i with optional exponent exp:0 such that
  the result equal ix10exp *)

val of_float : float -> ?prec:int -> unit -> t
(** Create a decimal from a float with a specified maximu precision. Use a negative
  maximu precision to create a decimal that precisely represents the float. Note: creating
  a decimal from a float may lose precision and give surprising results as many decimal fractions
  cannot be represented precisely by a float. *)

val neg : t -> t
(** Negate a decimal *)

val abs : t -> t
(** Absoulte value *)

val add : t -> t -> t
(** Add two decimal *)

val sub : t -> t -> t
(** Substract two decimal *)

val mul : t -> t -> t
(** Multiply two decimals with full precision *)

val div : t -> t -> t
(** Divide two decimals using 15 digits of extra precision *)

val div_with : t -> t -> ?min_prec:int -> unit -> t
(** Divide two decimal with a given extrac precision min_prec:15. The min_prec is the number of extra
  digitis used to calculate inexact divisions *)

val succ : t -> t
(** Returns its argument plus one *)

val pred : t -> t
(** Return its argument minus one *)

val sign : t -> int
(** Return -1, 0 and 1, when the argument is negative, zero or positive *)

val compare : t -> t -> int
(** Comparison. compare x y returns 0 if x equals y, -1 if x is smaller than y,
  and 1 if x is greater than y *)

val equal : t -> t -> bool
(** Equality test. *)

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

val is_even : t -> bool
(** Is this an even decimal? *)

val is_odd : t -> bool
(** Is this an odd decimal? *)

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

val round_to_prec : t -> ?prec:int -> ?rnd:round -> unit -> t
(** Round the decimal-point to number x to a specified number of digits
  behind the dot prec:0 with an optional rounding mode round:HalfEven *)

val round : t -> ?rnd:round -> unit -> t
(** Round a `:decimal` number to a whole number with an optional rounding mode (=`Half-even`). *)

val ceil : t -> t
(** Round a decimal x to the smallest integer that is not less than x *)

val floor : t -> t
(** Round a decimal x using the largest integer that is not larger than x*)

val truncate : t -> t
(** Round a decimal x to an integer by rounding towards zero *)

val get_exponent : t -> int

val to_string : t -> ?prec:int -> unit -> string
(** Show a decimal with a given precision prec:-1000. The precision specifies the
  number of digitis after the dot. If the precision is negative, at most prec digits are
  displayed, while for a positive precision, exactly prec digits behind the dot are displayed. *)

val to_string_fixed : t -> ?prec:int -> unit -> string

val to_string_exponent : t -> ?prec:int -> unit -> string

val parse_decimal : string -> t Option.t
(** Parse a decimal number *)