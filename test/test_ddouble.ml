open OUnit2
open Angka

let ($) f a = f a

let test_ddouble_show _ =
  let () = assert_equal (Ddouble.to_string $ Ddouble.of_float 0.1 $ ()) "0.1000000000000000055511151231258" in
  assert_equal (Ddouble.to_string $ Ddouble.div Ddouble.one Ddouble.ten $ ())  "0.1"

let test_ddouble_addition _ =
  let a = Ddouble.of_int 1 (-1) in
  let b = Ddouble.of_int 2 (-1) in
  let c = Ddouble.add a b in
  assert_equal (Ddouble.to_string c ()) "0.3"

let suite =
  "DDoubleTest" >::: [
      "test_ddouble_show" >:: test_ddouble_show
    ; "test_ddouble_addition" >:: test_ddouble_addition
  ] 

let () = run_test_tt_main suite