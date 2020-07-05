module Z = struct
  let ten = Z.of_int 10

  let mul_exp10 i n =
    Z.mul i (Z.pow ten n)

  let is_zero x = Z.equal x Z.zero

  let cdiv_exp10 i n =
    Z.div i (Z.pow ten n)

  let exp10 = mul_exp10 (Z.of_int 1)

  let cdiv_mod_exp10 i n =
    if n <= 0 then (i, Z.of_int 0) else begin
      let cq = cdiv_exp10 i n in
      let cr = Z.sub i (mul_exp10 cq n) in
      (cq, cr)
    end

  let div_mod_exp10 i n =
    let (cq, cr) = cdiv_mod_exp10 i n in
    if cr > Z.of_int 0 then (cq, cr) else (Z.pred cq, Z.add cr (exp10 n))

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

  let div x y =
    let (q, r) = Z.div_rem x y in
    if Z.sign r < 0 then
      if Z.sign q > 0 then Z.pred q else Z.succ q
    else q

  let parse_int s =
    try
      Z.of_string s |> Option.some
    with Invalid_argument _ -> Option.none

  let parse_int_default s ?default:(default=Z.zero) () =
    Option.value (parse_int s) ~default:default

end

module Int = struct
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

  let int_div x y =
    if y = 0 then 0 else begin
      let q = truncate ((Int.to_float x) /. (Int.to_float y)) in
      let r = Int.rem x y in
      if r < 0 then
        if y > 0 then q - 1 else q + 1
      else q
    end
end

module Str = struct
  let repeat s n =
    if n <= 0 then ""
    else if n = 1 then s
    else if n = 2 then s ^ s
    else begin
      let rec loop i r s =
        if i <= 0 then r else begin
          let x = i land 1 in
          if x > 0 then loop (i asr 1) (r ^ s) (s ^ s)
          else loop (i asr 1) r (s ^ s)
        end
      in
      loop n "" s
    end
  
  let pad_left s w ?fill:(fill=" ") () =
    let n = String.length s in
    if w <= n then s else repeat fill (w - n) ^ s

  let pad_right s w ?fill:(fill=" ") () =
    let n = String.length s in
    if w <= n then s else s ^ repeat fill (w - n)
end