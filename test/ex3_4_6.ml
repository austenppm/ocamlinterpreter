open OUnit
open EvalTestGenerator
open Miniml.Eval

let dataset_for_eval = [
  { input = "let fact = fun n -> n + 1 in let fact = fun n -> if n < 1 then 1 else n * fact (n + -1) in fact 5;;"; expected = IntV 25};
  { input = "let fact = dfun n -> n + 1 in let fact = fun n -> if n < 1 then 1 else n * fact (n + -1) in fact 5;;"; expected = IntV 25};
  { input = "let fact = fun n -> n + 1 in let fact = dfun n -> if n < 1 then 1 else n * fact (n + -1) in fact 5;;"; expected = IntV 120};
  { input = "let fact = dfun n -> n + 1 in let fact = dfun n -> if n < 1 then 1 else n * fact (n + -1) in fact 5;;"; expected = IntV 120};
  ];;

let () = ignore(run_test_tt_main (
    "ex3.4.6" >:::
    gen_eval_tests dataset_for_eval
  ))
