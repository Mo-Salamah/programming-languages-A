(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* string*string list*constructor *)
(* acc is boolean *)

(* produce SOME string list if str is in strlist else NONE *)

fun all_except_option (str:string, strlst:string list) =
    let 
        fun all_except_option (str:string, strlst:string list, acc:bool, aux:string list) =
            case (str,strlst,acc,aux) of
                (_, [], bool, _) => if bool then SOME aux else NONE
            |   (word, x::xs, bool, aux) => if same_string(word, x) 
                                                then all_except_option(str, xs, true, aux) 
                                                else all_except_option(str, xs, bool, x::aux)
            

    in
        all_except_option(str, strlst, false, [])
    end






fun get_substitutions1(lst, str) = 
    case lst of
        [] => []
    |   hlst::tlst => case all_except_option(str ,hlst) of
                        SOME nlst => nlst @ get_substitutions1(tlst, str)
                    |   NONE      => [] @ get_substitutions1(tlst, str)
                    
                    
fun get_substitutions2(lst, str) = 
    let fun get_substitutions2(lst, str, rsf) =
        case lst of
            [] => rsf
        |   hlst::tlst => case all_except_option(str ,hlst) of
                            SOME nlst => get_substitutions2(tlst, str, rsf @ nlst)
                        |   NONE      => get_substitutions2(tlst, str, rsf)
    in
        get_substitutions2(lst, str, [])
    end




fun similar_names(lst, {first=fname, middle=mname, last=lname}) = 
    let
        val substitutions = get_substitutions2(lst, fname)
        fun make_full_names(lst, rsf) =
            case lst of
                [] => rsf
            |    hlst::tlst => make_full_names(tlst, rsf @ [{first=hlst, middle=mname, last=lname}])

    in
        make_full_names(substitutions, [{first=fname,middle=mname,last=lname}])
    end



(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(c) = 
    case c of
        (Diamonds, _) => Red
    |   (Hearts, _)   => Red
    |   _             => Black

fun card_value c = 
    case c of 
        (_, Num x) => x
    |   (_, Ace)   => 11
    |   _          => 10



    fun remove_card (cs, c, exp) = 
    case cs of
        [] => raise exp
    |   c1::cs' => if c=c1 then cs' else c1::remove_card(cs', c, exp)


fun all_same_color cs = 
    let 
        fun same_color (c1, c2) =
            card_color(c1) = card_color(c2)
    in
    case cs of
        [] => true
    |   _::[] => true
    |   head::nick::tale => same_color(head, nick) 
                    andalso all_same_color(nick::tale)
    end



fun sum_cards cs = 
    let
        fun sum_cards(cs, sum) = 
            case cs of
                [] => sum
            |   head::cs' => sum_cards(cs', sum+ card_value(head))
    in
    sum_cards(cs, 0)
    end


fun score (cs, goal) = 
    let 
        val sum = sum_cards(cs)
        fun score (cs, goal) =
            if sum > goal
                then (3 * (sum - goal)  ) 
                else (goal - sum)
    in 
    if all_same_color cs 
        then (sum div 2 )
        else score(cs, goal)
    end

(*
fun officiate (cs, ms, goal) = 
let
    fun officiate (cs, ms, hcs) = 
        case ms of
            [] => score(hcs, goal)
        |   Discard c::ms' => officiate(cs, ms', remove_card(hcs, c, IllegalMove)) 
        |   Draw::ms' => case cs of
                            [] => score(hcs, goal)
                        |    c::cs' => if sum_cards(c::hcs) > goal 
                                        then score(c::hcs, goal)
                                        else officiate(cs', ms', c::hcs)
in
    officiate(cs, ms, [])
end

*)
fun officiate (cards,plays,goal) =
    let 
        fun loop (current_cards,cards_left,plays_left) =
            case plays_left of
                [] => score(current_cards,goal)
              | (Discard c)::tail => 
                loop (remove_card(current_cards,c,IllegalMove),cards_left,tail)
              | Draw::tail =>
                (* note: must score immediately if go over goal! *)
                case cards_left of
                    [] => score(current_cards,goal)
                  | c::rest => if sum_cards (c::current_cards) > goal
                               then score(c::current_cards,goal)
                               else loop (c::current_cards,rest,tail)
    in 
        loop ([],cards,plays)
    end





