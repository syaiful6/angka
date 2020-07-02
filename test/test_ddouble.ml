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

let suite =
  "DDoubleTest" >::: [
      "test_ddouble_show" >:: test_ddouble_show
    ; "test_ddouble_addition" >:: test_ddouble_addition
    ; "test_ddouble_substraction" >:: test_ddouble_substraction
    ; "test_ddouble_multiplication" >:: test_ddouble_multiplication
    ; "test_ddouble_division" >:: test_ddouble_division
  ] 

let () = run_test_tt_main suite
