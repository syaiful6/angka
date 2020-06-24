open OUnit2

let test_ddouble_of_float _ =
  assert_equal 0.1 (Ddouble.of_float 0.1 |> Ddouble.decode |> fst)

let suite =
  "DDoubleTest" >::: [
    "test_ddouble_of_float" >:: test_ddouble_of_float
  ] 

let () = run_test_tt_main suite