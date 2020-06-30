module Edouble = struct
  type t =
    { num : float
    ; err : float
    }

  let create num err =
    { num = num
    ; err = err
    }

  let sum x y =
    let z = x +. y in
    let diff = z -. x in
    let err = (x -. (z -. diff)) +. (y -. diff) in
    create z (if Float.is_finite z then err else z)

  let quicksum x y =
    let z = x +. y in
    let err = if Float.is_finite z then y -. (z -. x) else z in
    create z err

  let prod x y =
    let z = x *. y in
    let err = Float.fma x y (Float.neg z) in
    create z err

  let prodsqr x = prod x x
end

type t =
  { hi : float
  ; lo : float
  }

let create hi lo =
  { hi = hi
  ; lo = lo
  }

let of_float d = create d 0.0

let zero = of_float 0.0
let one = of_float 1.0
let two = of_float 2.0
let ten = of_float 10.0
let nan = of_float Float.nan

let neg dd =
  create (Float.neg dd.hi) (Float.neg dd.lo)

let quicksum x y =
  if not (Float.is_finite x) then of_float x
  else begin
    let z = x +. y in
    let err = y -. (z -. x) in
    create z (if Float.is_finite z then err else z)
  end

let is_finite x = Float.is_finite x.hi && Float.is_finite x.lo

let add x y =
  let z1 = Edouble.sum x.hi y.hi in
  let lo = Edouble.sum x.lo y.lo in
  let e1 = z1.err +. lo.num in
  let z2 = Edouble.quicksum z1.num e1 in
  let e2 = z2.err +. lo.err in
  quicksum z2.num e2

let sub x y = add x (neg y)

let mul x y =
  let z = Edouble.prod x.hi y.hi in
  let e = z.err +. (x.hi*.y.lo +. x.lo*.y.hi) in
  quicksum z.num e

let sqr x =
  let z = Edouble.prodsqr x.hi in
  let e = (z.err +. (2.0*.x.hi*.x.lo)) +. (x.lo*.x.lo) in
  quicksum z.num e

let div x y =
  let q1 = of_float (x.hi /. y.hi) in
  if (not (is_finite q1) || not (Float.is_finite y.hi)) then q1
  else begin
    let r1 = sub x (mul y q1) in
    let q2 = of_float (r1.hi /. y.hi) in
    let r2 = sub r1 (mul y q2) in
    let q3 = of_float (r2.hi /. y.hi) in
    let q = quicksum q1.hi q2.hi in
    add q q3
  end

let is_zero x = x.hi = 0.0

let rec npwr_acc x acc n =
  if n <= 0 then acc
  else begin
    if Int.logand n 1 <> 1 then npwr_acc (sqr x) acc (n / 2)
    else npwr_acc x (mul x acc) (n - 1)
  end

let npwr x n =
  if Int.equal n 0 then
    if is_zero x then nan else one
  else
    if Int.equal n 1 then x else npwr_acc x one n

let powi x n =
  let p = npwr x (Int.abs n) in
  if n < 0 then div one p else p

let powi10 exp = powi ten exp

let mul_exp10 x exp = if exp = 0 then x else mul x (powi10 exp)

let to_float d = d.hi

let decode x = (x.hi, x.lo)

let encode hi lo = add (of_float hi) (of_float lo)

let round x =
  let r = Float.round x.hi in
  let diff = r -. x.hi in
  if diff = 0.0 then quicksum r (Float.round(x.lo))
  else if diff = 0.5 && x.lo < 0. then of_float (r -. 1.0)
  else if diff = -0.5 && x.lo > 0. then of_float (r +. 1.0)
  else of_float r

let floor x =
  let r = Float.floor x.hi in
  if r = x.hi then quicksum r (Float.floor x.lo) else create r 0.0

let ceil x =
  let r = Float.ceil x.hi in
  if r = x.hi then quicksum r (Float.floor x.lo) else create r 0.0

let maxprecise = 9007199254740991
let minprecise = Int.neg maxprecise

let is_precise i =
  let a = Int.compare i minprecise in
  let b = Int.compare i maxprecise in
  a >= 0 && b <= 0

let small_exp i exp =
  let d = of_float (Int.to_float i) in
  if exp = 0 then d else mul_exp10 d exp

let of_int i exp =
  if is_precise i then
    small_exp i exp
  else begin
    let p = Z.of_int i|> Utils.Z.count_digits in
    let px = p - 14 in
    let (hi, y) = Utils.Z.cdiv_mod_exp10 (Z.of_int i) px in
    let py = px - 14 in
    if py <= 0 then
      small_exp (Z.to_int hi) (px + exp)
    else begin
      let (mid, z) = Utils.Z.cdiv_mod_exp10 y py in
      let pz = py - 14 in
      let (lo, plo) = if pz <= 0 then (z, 0) else (Utils.Z.cdiv_exp10 z pz, pz) in

      let (<+>) = add in
      small_exp (Z.to_int hi) (px + exp) <+> small_exp (Z.to_int mid) (py + exp) <+> small_exp (Z.to_int lo) (plo + exp)
    end
  end

let to_decimal x ?prec:(prec=(-1)) () =
  if not (is_finite x) then Decimal.zero else begin
    let (<+>) = Decimal.add in
    Decimal.of_float x.hi ~prec:prec () <+> Decimal.of_float x.lo ~prec:prec ()
  end

let to_string x ?prec:(prec=(-31)) () =
  if not (is_finite x) then Float.to_string x.hi else Decimal.to_string (to_decimal x () ~prec:prec) ~prec:prec ()
