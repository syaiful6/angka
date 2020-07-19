open OUnit2
open Angka

let ($) f a = f a

let test_ddouble_show _ =
  let () = assert_equal (Ddouble.to_string $ Ddouble.of_float 0.1 $ ()) "0.1000000000000000055511151231258" in
  let () = assert_equal (Ddouble.to_string $ Ddouble.div Ddouble.one Ddouble.ten $ ())  "0.1" in

  let x1 = Ddouble.of_float 5.123456 in
  let () = assert_equal (Ddouble.to_string x1 ()) "5.1234560000000000101749719760846" in
  let () = assert_equal (Ddouble.to_string x1 ~prec:4 ()) "5.1234" in
  let () = assert_equal (Ddouble.to_string x1 ~prec:1 ()) "5.1" in
  let () = assert_equal (Ddouble.to_string x1 ~prec:0 ()) "5" in

  let x2 = Ddouble.of_float 0.000123 in
  let () = assert_equal (Ddouble.to_string x2 ()) "0.0001230000000000000081983031475" in
  let () = assert_equal (Ddouble.to_string x2 ~prec:8 ()) "0.00012300" in
  assert_equal (Ddouble.to_string x2 ~prec:5 ()) "0.00012"

let test_ddouble_addition _ =
  let a = Ddouble.of_int 1 (-1) in
  let b = Ddouble.of_int 2 (-1) in
  let c = Ddouble.add a b in
  assert_equal (Ddouble.to_string c ()) "0.3"

let test_ddouble_substraction _ =
  let a = Ddouble.of_int 3 (-1) in
  let b = Ddouble.of_int 1 (-1) in
  let c = Ddouble.sub a b in
  assert_equal (Ddouble.to_string c ()) "0.2"

let test_ddouble_multiplication _ =
  let a = Ddouble.of_int 3 (-1) in
  let b = Ddouble.of_int 2 (-1) in
  assert_equal (Ddouble.to_string (Ddouble.mul a b) ()) "0.06"

let test_ddouble_division _ =
  let a = Ddouble.of_int 10 0 in
  let b = Ddouble.of_int 3 0 in
  let c = Ddouble.div a b in
  (* let () = Printf.printf "results: %s" (Ddouble.to_string c ()) in *)
  assert_equal (Ddouble.to_string c ()) "3.3333333333333333333333333333333"

let test_ddouble_min_max _ =
  let a = Ddouble.of_int 3 (-1) in
  let b = Ddouble.of_int 2 (-1) in
  let () = assert_bool "min function returns min value" (Ddouble.equal b (Ddouble.min a b)) in

  assert_bool "max function returns max value" (Ddouble.equal a (Ddouble.max a b))

let test_parse_ddouble _ =
  let x = Option.value (Ddouble.parse_ddouble "0.221") ~default:Ddouble.zero in
  let () = Printf.printf "results: %s\\n" (Ddouble.to_string x ()) in
  let () = assert_bool "the parsed ddouble should not equal zero" (not (Ddouble.equal x Ddouble.zero)) in
  let () = assert_bool "the parsed ddouble should equal" (Ddouble.equal x (Ddouble.of_int 221 (-3))) in

  let y = Option.value (Ddouble.parse_ddouble "-0.2") ~default:Ddouble.zero in
  let () = Printf.printf "results: %s\\n" (Ddouble.to_string y ()) in
  assert_bool "minus ddouble successfully parsed" (Ddouble.equal y (Ddouble.of_int 2 (-1) |> Ddouble.neg))

let test_ddouble_ordering _ =
  let a = Ddouble.of_int 2 (-1) in
  let b = Ddouble.of_int 3 (-1) in
  
  let () = assert_bool "leq return true if a < b" (Ddouble.leq a b) in
  let () = assert_bool "leq return true if a = b" (Ddouble.leq a a) in
  let () = assert_bool "leq return false if a > b" (Ddouble.leq b a = false) in

  let () = assert_bool "geq return true if b > a" (Ddouble.geq b a) in
  let () = assert_bool "geq return true if b = a" (Ddouble.geq b b) in
  let () = assert_bool "geq return false if b < a" (Ddouble.geq a b = false) in

  let () = assert_bool "lt return true if a < b" (Ddouble.lt a b) in
  let () = assert_bool "lt return false if a > b" (Ddouble.lt b a = false) in

  let () = assert_bool "gt return true if b > a" (Ddouble.gt b a) in
  assert_bool "gt return true if b < a" (Ddouble.gt a b = false)

let suite =
  "DDoubleTest" >::: [
      "test_ddouble_show" >:: test_ddouble_show
    ; "test_ddouble_addition" >:: test_ddouble_addition
    ; "test_ddouble_substraction" >:: test_ddouble_substraction
    ; "test_ddouble_multiplication" >:: test_ddouble_multiplication
    ; "test_ddouble_division" >:: test_ddouble_division
    ; "test_ddouble_min_max" >:: test_ddouble_min_max
    ; "test_ddouble_ordering" >:: test_ddouble_ordering
    ; "test_parse_ddouble" >:: test_parse_ddouble
  ] 

let () = run_test_tt_main suite
