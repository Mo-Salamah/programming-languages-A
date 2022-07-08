use "hw02-1.sml";
(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
(*       use "hw2-test.sml";       *)

val test11 = all_except_option ("string", ["string"]) = SOME []
val test12 = all_except_option ("a", ["b", "a", "c"]) = SOME ["c" ,"b"]   
val test13 = all_except_option ("e", ["b", "a", "c", "d", "e"]) = SOME ["d","c","a","b"]  
val test14 = all_except_option ("Fred", ["Freddie" , "Fred", "F"]) = SOME ["F" ,"Freddie"]


val test21 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test22 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"]],"Fred") = ["Fredrick"]

val test23 = get_substitutions1([["Elizabeth","Betty"],["Fred","Fredrick"]],"Fred") =["Fredrick"]

val test24 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],
                                ["Freddie","Fred","F"]],
                                "Fred") 
                                = ["Fredrick", "F","Freddie"]

val test25 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],
                                ["Freddie","Fred","F"], ["Fred"], ["Bob", "Fred"]],
                                "Fred") 
                                = ["Fredrick", "F","Freddie", "Bob"]





val test31 = get_substitutions2 ([["foo"],["there"]], "foo") = []


val test32 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"]],"Fred") = ["Fredrick"]

val test33 = get_substitutions2([["Elizabeth","Betty"],["Fred","Fredrick"]],"Fred") =["Fredrick"]

val test34 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],
                                ["Freddie","Fred","F"]],
                                "Fred") 
                                = ["Fredrick", "F","Freddie"]

val test35 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],
                                ["Freddie","Fred","F"], ["Fred"], ["Bob", "Fred"]],
                                "Fred") 
                                = ["Fredrick", "F","Freddie", "Bob"]


val test41 = similar_names ([], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", middle="W", last="Smith"}]

val test42 = similar_names ([["F", "Freddie"], ["Bettey"]], {first="Fred", middle="W", last="Smith"}) = [{first="Fred", middle="W", last="Smith"}]

val test43 = similar_names ([["F", "Freddie", "Fred"], ["Bettey"], ["Fara", "Fred", "Fredrick", "F"]], {first="Fred", middle="W", last="Smith"}) =
 [{first="Fred", middle="W", last="Smith"},
 {first="F", middle="W", last="Smith"},
 {first="Freddie", middle="W", last="Smith"},
 {first="Fara", middle="W", last="Smith"},
 {first="Fredrick", middle="W", last="Smith"},
 {first="F", middle="W", last="Smith"}]

val test44 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test51 = card_color (Clubs, Num 2) = Black
val test52 = card_color (Hearts, Num 2) = Red
val test53 = card_color (Spades, Num 2) = Black
val test54 = card_color (Diamonds, Num 2) = Red

val test61 = card_value (Clubs, Num 2) = 2
val test62 = card_value (Clubs, Ace) = 11
val test63 = card_value (Clubs, Jack) = 10
val test64 = card_value (Clubs, Queen) = 10

exception IllegalMove

(*val test71 = remove_card ([], (Hearts, Ace), IllegalMove) handle IllegalMove true=  true
val test72 = remove_card ([(Hearts, Num 2)], (Hearts, Ace), IllegalMove) =  IllegalMove     *)
val test73 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test74 = remove_card ([(Hearts, Num 3), (Hearts, Ace), (Diamonds, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Num 3), (Diamonds, Ace)]
val test75 = remove_card ([(Hearts, Num 3), (Diamonds, Ace), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = [(Hearts, Num 3), (Diamonds, Ace)]


val test81 = all_same_color [] = true
val test82 = all_same_color [(Hearts, Ace)] = true
val test83 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test84 = all_same_color [(Hearts, Ace), (Hearts, Num 2)] = true
val test85 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Hearts, Ace), (Hearts, Ace)] = true
val test86 = all_same_color [(Diamonds, Ace), (Hearts, Ace)] = true
val test87 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Diamonds, Num 2)] = true
val test88 = all_same_color [(Diamonds, Ace), (Spades, Ace)] = false
val test89 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Spades, Num 2)] = false

val test91 = sum_cards [] = 0
val test92 = sum_cards [(Clubs, Num 2),(Clubs, Num 2), (Hearts, Ace)] = 15



val test101 = score ([],10) = 0
val test102 = score ([(Spades, Jack)],10) = 5
val test103 = score ([(Spades, Ace)],10) = 5
val test104 = score ([(Spades, Ace), (Spades, Jack)],10) = 10
val test105 = score ([(Spades, Ace), (Spades, Jack), (Spades, Ace), (Spades, Jack)],10) = 21



val test111 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) 

val test112 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             

(*val test113 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true) 6 3 *)
             
