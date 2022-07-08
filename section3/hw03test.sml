  use "hw03.sml";   
(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

(*     use "hw03test.sml";     *)


(*     use "hw03.sml";     *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test21 = longest_string1 [] = ""
val test22 = longest_string1 ["A"] = "A"
val test23 = longest_string1 ["A","bc","C"] = "bc"
val test24 = longest_string1 ["A","bc","Cc"] = "bc"
val test25 = longest_string1 ["A","bc","Ccc"] = "Ccc"

val test31 = longest_string2 ["A","bc","C"] = "bc"

val test4a1 = longest_string3 ["A","bc","C"] = "bc"
val test4a2 = longest_string1 [] = ""
val test4a3 = longest_string1 ["A"] = "A"
val test4a4 = longest_string1 ["A","bc","C"] = "bc"
val test4a5 = longest_string1 ["A","bc","Cc"] = "bc"
val test4a6 = longest_string1 ["A","bc","Ccc"] = "Ccc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test51 = longest_capitalized ["A","bc","C"] = "A"
val test52 = longest_capitalized ["afsfs", "A","bc","C", "Sf"] = "Sf"

val test61 = rev_string "" = ""
val test62 = rev_string "a" = "a"
val test63 = rev_string "abc" = "cba"

val test71 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

(*val test81 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = []*)   (* why does this test fail??*)
val test82 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2] = NONE
val test83 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1] = SOME [1]
val test84 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,1,1] = SOME [1,1,1]
val test85 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test86 = all_answers (fn x => if x = 2 then SOME [x] else NONE) [7,2,2,2,2] = NONE
val test87 = all_answers (fn x => if x = 2 then SOME [x] else NONE) [2,2,2,2,7] = NONE


val test9a1 = count_wildcards Wildcard = 1
val test9a2 = count_wildcards (ConstructorP ("abc",Wildcard)) = 1

val test9b1 = count_wild_and_variable_lengths (Variable("abcde")) = 5
val test9b2 = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c1 = count_some_var ("", Variable("x")) = 0
val test9c2 = count_some_var ("x", Variable("x")) = 1



val test101 = check_pat (Variable("x")) = true
val test102 = check_pat (TupleP[(Variable ("x")), (Variable ("y")), (Variable("x"))]) = false


val test111 = match (Const(1), UnitP) = NONE
val test112 = match (Tuple [(Const 5)], TupleP [(ConstP 5)]) = SOME []
val test113 = match (Unit, UnitP) = SOME []
val test114 = match (Tuple [(Const 5), (Constructor ("cname", (Const 3)))], TupleP [(ConstP 5), (ConstructorP ("cname", (ConstP 3)))]) = SOME []
val test115 = match (Tuple [(Const 5), (Const 5), (Const 5)], TupleP [(ConstP 5), (Variable ("a?")), (ConstP 5)]) = SOME [("a?", Const 5)]
val test116 = match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],   
          TupleP[Wildcard,Wildcard]) 


val test121 = first_match Unit [UnitP] = SOME []


