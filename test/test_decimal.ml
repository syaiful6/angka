open OUnit2
open Angka

let test_decimal_to_string_exponent _ =
  let x = Decimal.of_int 1 ~exp:(-1) () in
  assert_equal (Decimal.to_string_exponent x ()) "1e-1"

let test_decimal_to_string_fixed _ =
  let x = Decimal.of_int 1 ~exp:(-1) () in
  let () = assert_equal (Decimal.to_string_fixed x ()) "0.1" in
  let y = Decimal.of_int 2 ~exp:(-2) () in
  assert_equal (Decimal.to_string_fixed y ()) "0.02"

let test_decimal_addition_operation _ =
  let (<+>) = Decimal.add in
  let d = Decimal.of_int 1 ~exp:(-1) () <+> Decimal.of_int 2 ~exp:(-1) () in
  assert_equal (Decimal.to_string_fixed d ()) "0.3"

let test_decimal_substract_operation _ =
  let (<->) = Decimal.sub in
  let d = Decimal.of_int 3 ~exp:(-1) () <-> Decimal.of_int 1 ~exp:(-1) () in
  assert_equal (Decimal.to_string_fixed d ()) "0.2"

let test_create_decimal_from_float _ =
  let d = Decimal.of_float 1.1 () in
  let () = assert_equal (Decimal.to_string d ()) "1.100000000000000088817841970012523233890533447265625" in
  let dd = Decimal.of_float 1.1 ~prec:17 () in
  assert_equal (Decimal.to_string dd ()) "1.10000000000000008"

let test_parse_decimal _ =
  let x = Option.value (Decimal.parse_decimal "0.221") ~default:Decimal.zero in
  let () = assert_bool "the parsed decimal should not equal zero" (not (Decimal.equal x Decimal.zero)) in
  let () = assert_bool "the parsed decimal should equal" (Decimal.equal x (Decimal.of_int 221 ~exp:(-3) ())) in

  let y = Option.value (Decimal.parse_decimal "-0.2") ~default:Decimal.zero in
  assert_bool "minus decimal successfully parsed" (Decimal.equal y (Decimal.of_int 2 ~exp:(-1) () |> Decimal.neg))

let test_decimal_ordering _ =
  let a = Decimal.of_int 2 ~exp:(-1) () in
  let b = Decimal.of_int 3 ~exp:(-1) () in
  
  let () = assert_bool "leq return true if a < b" (Decimal.leq a b) in
  let () = assert_bool "leq return true if a = b" (Decimal.leq a a) in
  let () = assert_bool "leq return false if a > b" (Decimal.leq b a = false) in

  let () = assert_bool "geq return true if b > a" (Decimal.geq b a) in
  let () = assert_bool "geq return true if b = a" (Decimal.geq b b) in
  let () = assert_bool "geq return false if b < a" (Decimal.geq a b = false) in

  let () = assert_bool "lt return true if a < b" (Decimal.lt a b) in
  let () = assert_bool "lt return false if a > b" (Decimal.lt b a = false) in

  let () = assert_bool "gt return true if b > a" (Decimal.gt b a) in
  assert_bool "gt return true if b < a" (Decimal.gt a b = false)


let suite =
  "DecimalTest" >::: [
      "test_decimal_to_string_fixed" >:: test_decimal_to_string_fixed
    ; "test_decimal_to_string_exponent" >:: test_decimal_to_string_exponent
    ; "test_decimal_addition_operation" >:: test_decimal_addition_operation
    ; "test_decimal_substract_operation" >:: test_decimal_substract_operation
    ; "test_create_decimal_from_float" >:: test_create_decimal_from_float
    ; "test_parse_decimal" >:: test_parse_decimal
    ; "test_decimal_ordering" >:: test_decimal_ordering
  ]

let () = run_test_tt_main suite
