open OUnit2
open Angka


let test_decimal_of_string_fixed _ =
  let x = Decimal.of_int 1 (-1) in
  let () = assert_equal (Decimal.of_string_fixed x ()) "0.1" in
  let y = Decimal.of_int 2 (-2) in
  assert_equal (Decimal.of_string_fixed y ()) "0.02"

let suite =
  "DecimalTest" >::: [
    "test_decimal_of_string_fixed" >:: test_decimal_of_string_fixed
  ]

let () = run_test_tt_main suite