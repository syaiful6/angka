open OUnit2
open Angka

let test_ddouble_of_float _ =
  assert_equal 0.1 (Ddouble.of_float 0.1 |> Ddouble.decode |> fst)

let test_ddouble_of_float2 _ =
  assert_equal 0.1 (Ddouble.of_float 0.1 |> Ddouble.decode |> fst)

let test_ddouble_addition _ =
  let a = Ddouble.of_int 1 (-1) in
  let b = Ddouble.of_int 2 (-1) in
  let c = Ddouble.add a b in
  assert_equal (Ddouble.to_string c ()) "0.3"

let suite =
  "DDoubleTest" >::: [
      "test_ddouble_of_float" >:: test_ddouble_of_float
    ; "test_ddouble_of_float2" >:: test_ddouble_of_float2
    ; "test_ddouble_addition" >:: test_ddouble_addition
  ] 

let () = run_test_tt_main suite