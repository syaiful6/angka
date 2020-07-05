module Iutils = Utils.Int
module Zutils = Utils.Z
module Strutils = Utils.Str

type t =
  { num : Z.t
  ; exp : int
  }

let create num exp =
  { num = num
  ; exp = exp
  }

let zero = create (Z.of_int 0) 0

let is_zero x = Z.equal x.num Z.zero

let sign x = Z.sign x.num

let round_exp exp =
  if exp = 0 then exp else 7 * (Iutils.int_div exp 7)

let of_zarith i ?exp:(exp=0) () =
  let x = round_exp exp in
  let diff = exp - x in
  if diff = 0 then create i exp else create (Zutils.mul_exp10 i diff) x

let of_int i = of_zarith (Z.of_int i)

let reduce x =
  let p = Zutils.is_exp10 x.num in
  if p <= 0 then x else begin
    let exp = x.exp + p in
    let rexp = round_exp exp in
    if rexp = x.exp then x else of_zarith (Zutils.cdiv_exp10 x.num p) ~exp:exp ()
  end

let expand x e =
  if x.exp <= e then x else of_zarith (Zutils.mul_exp10 x.num (x.exp - e)) ~exp:e ()

let add x y =
  let e = min x.exp y.exp in
  let xx = expand x e in
  let yy = expand y e in
  of_zarith (Z.add xx.num yy.num) ~exp:e ()

let neg x = create (Z.neg x.num) x.exp

let sub x y = add x (neg y)

let mul x y =
  let z = of_zarith (Z.mul x.num y.num) ~exp:(x.exp + y.exp) () in
  if z.exp < 0 then reduce z else z

let div_with x y ?min_prec:(min_prec=15) () =
  if is_zero x || is_zero y then zero else begin
    let exp = x.exp - y.exp in
    let xdigits = Zutils.count_digits x.num in
    let ydigits = Zutils.count_digits y.num in
    let extra = max 0 (ydigits - xdigits) + min_prec in

    if extra > 0 then reduce (of_zarith (Z.div (Zutils.mul_exp10 x.num extra) y.num) ~exp:(exp - extra) ())
    else reduce (of_zarith (Z.div x.num y.num) ~exp:(exp - extra) ())
  end

let div x y = div_with x y ()

let succ x = create (Z.succ x.num) x.exp

let pred x = create (Z.pred x.num) x.exp

let pow x n =
  let m = Int.abs n in
  let y = of_zarith (Z.pow x.num m) ~exp:(x.exp * m) () in
  if n < 0 then div_with (of_int 1 ()) y ~min_prec:(3 + m) () else y

let compare x y =
  let e = min x.exp y.exp in
  let xx = expand x e in
  let yy = expand y e in
  Z.compare xx.num yy.num

let equal a b = compare a b = 0

let leq a b = compare a b <= 0
let geq a b = compare a b >= 0
let lt a b = compare a b < 0
let gt a b = compare a b > 0

let min a b = if compare a b <= 0 then a else b

let max a b = if compare a b >= 0 then a else b

let abs a = if sign a = -1 then neg a else a

let is_even x = Z.is_even x.num

let is_odd x = Z.is_odd x.num

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

let round_to_prec x ?prec:(prec=0) ?round:(round=HalfEven) () =
  if x.exp >= Int.neg prec then x
  else begin
    let cx = reduce x in
    let p = (Int.neg x.exp) - prec in
    if p < 0 then cx else begin
      let (q, r) = Zutils.div_mod_exp10 cx.num p in

      let round_half keep =
        let half = Zutils.div (Zutils.exp10 p) (Z.of_int 2) in
        match Z.compare r half with
          | 0 when keep -> q
          | 0 -> Z.succ q
          | 1 -> Z.succ q
          | _ -> q
      in

      let q1 = if r = Z.of_int 0 then q else match round with
          HalfEven  -> round_half (Z.is_even q)
        | HalfFloor -> round_half true
        | HalfCeil  -> round_half false
        | HalfTruncate -> round_half (Z.sign q > 0)
        | HalfAwayFromZero -> round_half (Z.sign q < 0)
        | Floor -> q
        | Ceil  -> Z.succ q
        | Truncate -> if Z.sign q > 0 then q else Z.succ q
        | AwayFromZero -> if Z.sign q < 0 then q else Z.succ q
      in


      of_zarith q1 ~exp:(Int.neg prec) ()
    end
  end

let get_exponent d = Zutils.count_digits d.num + d.exp - 1

let show_frac frac prec =
  let trimmed = Str.global_replace (Str.regexp "0+$") "" frac in
  let fractFul = if prec >= 0 then Strutils.pad_right trimmed prec ~fill:"0" () else trimmed in
  if fractFul = "" then "" else "." ^ fractFul

let to_string_fixed d ?prec:(prec=(-1000)) () =
  let x = round_to_prec d ~prec:(Int.abs prec) () in
  if x.exp >= 0 then begin
    let frac = if prec <= 0 then "" else "." ^ String.make prec '0' in
    Z.to_string x.num ^ String.make x.exp '0' ^ frac
  end else begin
    let digits = Int.neg x.exp in
    let sign = if Z.sign x.num < 0 then "-" else "" in
    let i = Z.abs x.num in
    let man = Zutils.cdiv_exp10 i digits in
    let frac = Z.sub i (Zutils.mul_exp10 man digits) in
    sign ^ Z.to_string man ^ show_frac (Strutils.pad_left (Z.to_string frac) digits ~fill:"0" ()) prec
  end

let to_string_exponent d ?prec:(prec=(-1000)) () =
  let x = round_to_prec d ~prec:((Int.abs prec) - get_exponent d) () in
  let s = Z.abs x.num |> Z.to_string in
  let digits = String.length s in
  let exp = x.exp + digits - 1 in
  let sign = if sign x = -1 then "-" else "" in
  let exponent = if exp = 0 then "" else "e" ^ (if exp > 0 then "+" else "") ^ Int.to_string exp in
  sign ^ String.sub s 0 1 ^ show_frac (String.sub s 1 (digits - 1)) prec ^ exponent
  
let to_string d ?prec:(prec=(-1000)) () =
  let exp = get_exponent d in
  let is_prec_negative = Int.compare prec 0 = -1 in
  if exp > -5 && exp < (if is_prec_negative then 15 else prec)
    then to_string_fixed d ~prec:prec ()
    else to_string_exponent d ~prec:prec ()

let decode_float x =
  let (man, exp) = Float.frexp x in
  (Float.to_int (man /. 1.1102230246251565e-16), exp - 53)

let of_float f ?prec:(prec=(-1)) () =
  let (man,exp) = decode_float f in
  if (exp >= 0) then of_int (man * (Utils.Int.pow 2 exp)) () else begin
    let prec = if prec < 0 then Int.neg exp else Stdlib.min prec (Int.neg exp) in
    div_with (of_int man ()) (pow (of_int 2 ()) (Int.neg exp)) ~min_prec:prec ()
  end

let rx_decimal = Re.Pcre.regexp "^([\\-\\+]?)(\\d+)(?:\\.(\\d*))?(?:[eE]([\\-\\+]?\\d+))?$"

let rx_group groups x =
  try
    Re.Group.get groups x |> Option.some
  with Not_found -> Option.none

let parse_decimal s =
  let groups = Re.Pcre.exec ~rex:rx_decimal ~pos:0 (String.trim s) in
  try
    let sign = Re.Group.get groups 1 in
    let whole = Re.Group.get groups 2 in
    let frac = Re.Group.get groups 3 in
    let frac2 = Option.value (rx_group groups 4) ~default:"" in
    let exp = (Int.neg (String.length frac)) + Option.value (int_of_string_opt frac2) ~default:0 in
    let f = of_zarith (Zutils.parse_int_default (whole ^ frac) ()) ~exp:exp () in
    if sign = "-" then Option.some (neg f) else Option.some f
  with Not_found -> Option.none