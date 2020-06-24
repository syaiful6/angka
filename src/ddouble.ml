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

    let splitbound = 6.696928794914171e299
    let splitter   = 1.34217729e8
    let two28      = 2.68435456e8

    let split x =
        if x > splitbound || x < Float.neg splitbound then
            let y = x *. 3.725290298461914e-9 in
            let t = y *. splitter in
            let hi = t -. (t -. y) in
            let lo = y -. hi in
            (hi *. two28, lo *. two28)
        else
            let t = x *. splitter in
            let hi = t -. (t -. x) in
            let lo = x -. hi in
            (hi, lo)
    
    let prod x y =
        let z = x *. y in
        let (xhi, xlo) = split x in
        let (yhi, ylo) = split y in
        let err = ((xhi*.yhi -. z) +. (xhi*.ylo +. xlo*.ylo)) +. (xlo *.ylo) in
        create z (if Float.is_finite z then err else z)

    let prodsqr x =
        let z = x*.x in
        let (hi, lo) = split x in
        let err = ((hi*.hi -. z) +. (2.0*.hi*.lo)) +. (lo*.lo) in
        create x (if Float.is_finite z then err else z)
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
    else
        let z = x +. y in
        let err = y -. (z -. x) in
        create z (if Float.is_finite z then err else z)

let is_finite x =
    Float.is_finite x.hi && Float.is_finite x.lo

let add x y =
    let z1 = Edouble.sum x.hi y.hi in
    let lo = Edouble.sum x.lo y.lo in
    let e1 = z1.err +. lo.num in
    let z2 = Edouble.quicksum z1.num e1 in
    let e2 = z2.err +. lo.err in
    quicksum z2.num e2

let sub x y =
    add x (neg y)

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
    else
        let r1 = sub x (mul y q1) in
        let q2 = of_float (r1.hi /. y.hi) in
        let r2 = sub r1 (mul y q2) in
        let q3 = of_float (r2.hi /. y.hi) in
        let q = quicksum q1.hi q2.hi in
        add q q3

let is_zero x = x.hi = 0.0

let rec npwr_acc x acc n =
    if n <= 0 then acc
    else
        if Int.logand n 1 <> 1 then npwr_acc (sqr x) acc (n / 2)
        else npwr_acc x (mul x acc) (n - 1)

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

let of_float_exp d exp = mul_exp10 (of_float d) exp

let to_float d = d.hi

let decode x = (x.hi, x.lo)

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