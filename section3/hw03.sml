(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern
(*Template: 
fun fn_for_pattern p = 
    case p of
        Wildcard   => (...)
    |   Variable s => (...s)
    |   UnitP      => (...)
    |   ConstP i   => (...i)
    |   TupleP lst => (...fn_for_pattern lst)
    | ConstructorP (s, p) => (...s p)
    |   _                 => (...)*)

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu
(*Template: 
fun fn_for_valu v = 
    case v of
        Const i => (...i)
    |   Unit    => (...)
    |   Tuple vlst => (...vlst)
    |   Constructor (s, v) => (...s v)*)

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string
(*Template
fun fn_for_typ t =
    case t of 
        Anaything => (...)
    |    UnitT    => (...)
    |   IntT      => (...)
    |   TupleT tlst => (...tlst)
    |   Datatype s  => (...s)*)


infix !>
fun x !> f = f x 
fun only_capitals xs = List.filter (fn l => String.sub(l, 0) !> Char.isUpper) xs


fun longest_string1 xs = List.foldl (fn (x, acc) => if String.size x > String.size acc then x else acc) "" xs

fun longest_string2 xs = List.foldl (fn (x, acc) => if String.size x >= String.size acc then x else acc) "" xs


fun longest_string_helper f xs = List.foldl (fn (x, acc) => if f(String.size x, String.size acc) then x else acc) "" xs


val longest_string3 = longest_string_helper (fn (i1, i2) => i1 > i2)

val longest_string4 = longest_string_helper (fn (i1, i2) => i1 >= i2)

val longest_capitalized =  longest_string3 o only_capitals     


fun rev_string s = s !> String.explode !> rev !> String.implode  

fun first_occurence xs = 
    case xs of
        NONE::xs' => first_occurence xs'
    |   SOME v::_ => v

fun first_answer f xs = 
    case xs of
        [] => raise NoAnswer
    |   x::xs' => case f x of
                    NONE => first_answer f xs'
                |   SOME v => v


(* a' -> b' list option) -> a' list -> b' list option*)
(* all_answer produces NONE if f returns NONE for any call, else
it returns a b' list option (f's results appended together)  *)

fun all_answers f0 xs0 = 
(* acc: b' list *)
    let 
        fun all_answer acc f xs = 
            case xs of 
                [] => SOME acc
            |   x::xs' => case f x of
                            NONE     => NONE
                        |   SOME lst => let
                                            val ans = all_answer (lst @ acc) f xs'
                                        in
                                            if ans <> NONE
                                                then ans
                                                else NONE
                                        end                                        
    in 
        all_answer [] f0 xs0
    end


fun count_wildcards p = g (fn () => 1) (fn _ => 0) p


fun count_wild_and_variable_lengths p = g (fn() => 1) (fn x => String.size x) p

fun count_some_var (s, p) = g (fn() => 0) (fn x => if x = s then 1 else 0) p  


fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


fun check_pat x = 
    let
        fun get_names p = 
            case p of
                Variable s => [s]
            |   TupleP lst => foldl (fn (p', rsf) => ((get_names p') @ rsf)) [] lst 
            | ConstructorP (s, p) => get_names p
            |   _          => []
        
            fun repeated_any xs = 
            case xs of
                [] => false
            |   x::xs' => ((foldl (fn (y,rsf) => (rsf orelse x=y)) false xs') 
                        orelse repeated_any xs')
    in
        (*not o repeated_any o get_names o x*)
         x !> get_names !> repeated_any !> not
    end


fun match (v, p) = 
    case (v, p) of
        (_, Wildcard)                   => SOME []
    |   (va, Variable s)               => SOME [(s, va)]     
    |   (Unit, UnitP)                   => SOME []
    |   (Const i1, ConstP i2)          => if i1 <> i2
                                             then NONE
                                              else SOME []
    |   (Tuple vs, TupleP ps)           =>  if length ps <> length vs
                                                then NONE 
                                                else
                                                    (all_answers match
                                                    (ListPair.zip(vs, ps)))
    |   ((Constructor  (s1, va)), 
         (ConstructorP (s2, patt)))       => if s1 = s2
                                            then match(va, patt) 
                                            else NONE
    |   (_, _)                           => NONE


fun first_match va lstp = 
    let 
        fun make_list (v, lst, rsf) = 
            case lst of
                [] => rsf
            |    x::xs' => make_list (v, xs', (v::rsf)) 
        val ans = first_answer match (ListPair.zip(make_list(va, lstp, []), lstp))
    in
    case ans of
        [] => SOME []
    |    (x,y)::xs  => SOME ((x,y)::xs)
   (* |    _    => NONE*)

    
    end

