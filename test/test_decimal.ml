open OUnit2
open Angka

let test_decimal_to_string_exponent _ =
  let x = Decimal.of_int 1 (-1) in
  assert_equal (Decimal.to_string_exponent x ()) "1e-1"

let test_decimal_to_string_fixed _ =
  let x = Decimal.of_int 1 (-1) in
  let () = assert_equal (Decimal.to_string_fixed x ()) "0.1" in
  let y = Decimal.of_int 2 (-2) in
  assert_equal (Decimal.to_string_fixed y ()) "0.02"

let test_decimal_addition_operation _ =
  let d = Decimal.add (Decimal.of_int 1 (-1)) (Decimal.of_int 2 (-1)) in
  assert_equal (Decimal.to_string_fixed d ()) "0.3"

let test_decimal_substract_operation _ =
  let d = Decimal.sub (Decimal.of_int 3 (-1)) (Decimal.of_int 1 (-1)) in
  assert_equal (Decimal.to_string_fixed d ()) "0.2"

let suite =
  "DecimalTest" >::: [
      "test_decimal_to_string_fixed" >:: test_decimal_to_string_fixed
    ; "test_decimal_to_string_exponent" >:: test_decimal_to_string_exponent
    ; "test_decimal_addition_operation" >:: test_decimal_addition_operation
    ; "test_decimal_substract_operation" >:: test_decimal_substract_operation
  ]

let () = run_test_tt_main suite