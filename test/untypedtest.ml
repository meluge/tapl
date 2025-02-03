open OUnit2
open Untyped  


let rec term_to_string (t : term) : string =
  match t with
  | Var x -> x
  | Abs (x, body) -> "λ" ^ x ^ ". " ^ term_to_string body
  | App (t1, t2) -> "(" ^ term_to_string t1 ^ " " ^ term_to_string t2 ^ ")"

let freeVarTest _ =
  let term1 = Abs ("x", Var "y")  in
  let result = freeVar term1 in
  let expected = ["y"] in
  assert_equal expected result

let substituteTest _ =
  let term1 = Abs ("z", Var "x") in
  let result = substitute "z" (Var "x") term1 in
  let expected =Abs ("z", Var "x") in
  assert_equal expected result

let substituteTest2 _ =
  (* Original term: λy. x *)
  let original = Abs ("y", Var "x") in

  (* Substituting x with y: x := y *)
  let result = substitute "x" (Var "y") original in

  (* Expected alpha-renamed result: λy1. y *)
  let expected = Abs ("y1", Var "y") in

  print_endline ("Result: " ^ term_to_string result);
  print_endline ("Expected: " ^ term_to_string expected);

  assert_equal (term_to_string expected) (term_to_string result)

let churchTest _ =  
   let result1 = toChurch 3 in
   let result2 = fromChurch result1 in
   let reduced = betaRedLOMult (App(churchSucc, (toChurch 3))) in 

   print_endline ("Reduced for R3: " ^ term_to_string reduced);

   let result3 = fromChurch reduced in
    print_endline ("Result1: " ^ term_to_string result1);
    print_endline ("Result2: " ^ string_of_int result2);
    
    print_endline ("Result3: " ^ string_of_int result3);


    assert_equal true true


let suite =
  "Lambda Calculus Tests" >::: [
    "freeVarTest" >:: freeVarTest;
    "substituteTest" >:: substituteTest;
    "substituteTest2" >:: substituteTest2;
    "church" >:: churchTest;
  ]

let () = run_test_tt_main suite
