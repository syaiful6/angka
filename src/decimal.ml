module Zutils = struct
  let ten = Z.of_int 10

  let mul_exp10 i n =
    Z.mul i (Z.pow ten n)

  let is_zero x = Z.equal x Z.zero

  let cdiv_exp10 i n =
    Z.div i (Z.pow ten n)

  let cdiv_mod_exp10 i n =
    if n <= 0 then (i, 0) else begin
      let cq = cdiv_exp10 i n in
      let cr = Z.sub i (mul_exp10 cq n) in
      (cq, Z.to_int cr)
    end

  let div_mod_exp10 i n =
    let (cq, cr) = cdiv_mod_exp10 i n in
    if cr > 0 then (cq, cr) else (Z.pred cq, cr - 1)

  let rec is_exp10_rec a j =
    if is_zero a then j
    else begin
      let (quotien, remainder) = Z.div_rem a ten in
      if is_zero remainder then is_exp10_rec quotien (j + 1)
      else j
    end

  let is_exp10 x = is_exp10_rec x 0
  
  let count_digits x =
    Z.to_string x |> String.length
end

module Iutils = struct
  let is_even n =  n mod 2 = 0

  let pow base exponent =
    if exponent < 0 then invalid_arg "exponent can not be negative" else
    let rec aux accumulator base = function
      | 0 -> accumulator
      | 1 -> base * accumulator
      | e when is_even e -> aux accumulator (base * base) (e / 2)
      | e -> aux (base * accumulator) (base * base) ((e - 1) / 2) in
    aux 1 base exponent

  let mul_exp10 i n = i * (pow 10 n)

  let exp10 = mul_exp10 1
end

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

let round_exp exp =
  if exp = 0 then exp else 7 * (exp / 7)

let of_zarith i exp =
  let x = round_exp exp in
  let diff = exp - x in
  if diff = 0 then create i exp else create (Zutils.mul_exp10 i diff) x

let reduce x =
  let p = Zutils.is_exp10 x.num in
  if p <= 0 then x else begin
    let exp = x.exp + p in
    let rexp = round_exp exp in
    if rexp = x.exp then x else of_zarith (Zutils.cdiv_exp10 x.num p) exp
  end

let expand x e =
  if x.exp <= e then x else of_zarith (Zutils.mul_exp10 x.num (x.exp - e)) e

let add x y =
  let e = min x.exp y.exp in
  let xx = expand x e in
  let yy = expand y e in
  of_zarith (Z.add xx.num yy.num) e

let neg x = create (Z.neg x.num) x.exp

let sub x y = add x (neg y)

let mul x y =
  let z = of_zarith (Z.mul x.num y.num) (x.exp + y.exp) in
  if z.exp < 0 then reduce z else z

let div_with x y ?min_prec:(min_prec=15) () =
  if is_zero x || is_zero y then zero else begin
    let exp = x.exp - y.exp in
    let xdigits = Zutils.count_digits x.num in
    let ydigits = Zutils.count_digits y.num in
    let extra = max 0 (ydigits - xdigits) + min_prec in

    if extra > 0 then reduce (of_zarith (Z.div (Zutils.mul_exp10 x.num extra) y.num) (exp - extra))
    else reduce (of_zarith (Z.div x.num y.num) (exp - extra))
  end

let div x y = div_with x y ()

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
        let half = Iutils.exp10 p / 2 in
        if r = half then
          if keep then q else Z.succ q
        else if r > half then Z.succ q
        else q
      in

      let q1 = if r = 0 then q else match round with
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

      of_zarith q1 (Int.neg prec)
    end
  end

let get_exponent d = Zutils.count_digits d.num + d.exp - 1

let of_string_fixed d ?prec:(prec=(-1000)) () =
  let x = round_to_prec d ~prec:(Int.abs prec) () in
  if x.exp >= 0 then begin
    let frac = if prec <= 0 then "" else "." ^ String.make prec '0' in
    Z.to_string x.num ^ String.make x.exp '0' ^ frac
  end else begin
    let digits = Int.neg x.exp in
    let sign = if Z.sign x.num < 0 then "-" else "" in
    let i = Z.abs x.num in
    let man = Zutils.cdiv_exp10 i digits in
    (* let frac = Z.sub i (Zutils.mul_exp10 man digits) in *)
    sign ^ Z.to_string man
  end