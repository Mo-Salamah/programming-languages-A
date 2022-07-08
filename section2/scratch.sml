val m = 4
val n = 4
val e= m + n


datatype mytype = Str of string 
                | twoInts of int*int
                | empty 


(* mytype -> ???) *)
fun evaluate x = 
    case x of 
            Str s => 1
        |   twoInts (i1, i2) => 2
        |   empty           => 3






        (* Programming Languages, Dan Grossman *)
(* Section 2: Type Synonyms *)

datatype suit = Club | Diamond | Heart | Spade

datatype rank = Jack | Queen | King | Ace | Num of int

type card = suit * rank

type name_record = { student_num : int option, 
                     first       : string, 
                     middle      : string option, 
                     last        : string }

fun is_Queen_of_Spades (c:card) = 
    #1 c = Spade andalso #2 c = Queen

val c1 : card = (Diamond,Ace)
val c2 : suit * rank = (Heart,Ace)
val c3 = (Spade,Ace)
val c4 : card = {1=Heart, 2=Jack}

(* can call is_Queen_of_Spades with any of c1, c2, c3 *)

(* and once we learn more pattern-matching, we can leave the type off
   function arguments too -- here is a teaser we cannot explain quite yet *)
fun is_Queen_of_Spades2 c =
    case c of
	(Spade,Queen) => true
      | _ => false


(* REPL prints 
  val is_Queen_of_Spades2 = fn : suit * rank -> bool
but that is also
  val is_Queen_of_Spades2 = fn : card -> bool
*)



fun nondecreasing (xs) = 
    case xs of
        [] => true
    |   x1::x2::xs' => x1<=x2 andalso nondecreasing(x2::xs')
    |   x::xs' => true 













