open OUnit2
open Angka

let test_ddouble_of_float _ =
  assert_equal 0.1 (Ddouble.of_float 0.1 |> Ddouble.decode |> fst)

let test_ddouble_of_float2 _ =
  assert_equal 0.1 (Ddouble.of_float 0.1 |> Ddouble.decode |> fst)

let suite =
  "DDoubleTest" >::: [
      "test_ddouble_of_float" >:: test_ddouble_of_float
    ; "test_ddouble_of_float2" >:: test_ddouble_of_float2
  ] 

let () = run_test_tt_main suite