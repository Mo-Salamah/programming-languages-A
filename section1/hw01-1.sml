
fun is_older (date1 : (int*int*int), date2 : int*int*int) =
    if (#1 date1) < (#1 date2)
    then true
    else if (#1 date1) > (#1 date2)
    then false
    else if (#2 date1) < (#2 date2)
    then true
    else if (#2 date1) > (#2 date2)
    then false
    else if (#3 date1) < (#3 date2)
    then true
    else false

fun number_in_month (xs : (int*int*int) list, m : int) =
    if null xs
    then 0
    else
	if ((#2 (hd xs)) = m)
	then 1 +  number_in_month ((tl xs), m)
	else number_in_month ((tl xs), m)


	     
fun number_in_months (xs : (int*int*int) list, ms : int list) =
    if null ms
    then 0
    else number_in_month(xs, (hd ms)) +  number_in_months(xs, (tl ms))

fun dates_in_month (xs : (int*int*int) list, m : int) =
    
    if null xs
    then []
    else
	if ((#2 (hd xs)) = m)
	then (hd xs)::  dates_in_month ((tl xs), m)
	else dates_in_month((tl xs), m)

			  
fun dates_in_months (xs : (int*int*int) list, ms : int list) =
    if null ms
    then []
    else dates_in_month(xs, (hd ms)) @ dates_in_months(xs, (tl ms))


(* String list*int -> String *)
fun get_nth (ws0 : string list, n : int) =
    (* counter: a contex preserving  accumulator; (hd ws) is in the counter'th
       position in ws0, in 1-based indexing *)
    let
	fun get_nth_counter (ws : string list, counter : int) =
	    if n = counter
	    then (hd ws)
	    else get_nth_counter((tl ws), counter+1)
    in
	get_nth_counter(ws0, 1)
	       
    end

	
	
fun date_to_string (date : (int*int*int)) =
   
    let
	val months = ["January", "February", "March", "April", "May", "June", "July",                             "August", "September", "October", "November", "December"]
    in
	get_nth(months, (#2 date))^ " "^
	Int.toString(#3 date)^", "^
	Int.toString(#1 date)

    end

(* int*int list -> int 
produces an index, n, such that the first n items of the list add up to less than s and the first n+1 elements add up to greater than or equal to s *)

fun number_before_reaching_sum (s : int, ints0 : int list) =
    (* n : int; a contex preserving accumulator; (hd ints) is the nth element in ints0,            1-based indexing
      rsf : int; a result so far accumulator; rsf = the sum of the first n elements *)
    
    let
	fun number_before_reaching_sum_r (ints : int list, n : int, rsf : int) =
	     if rsf >= s
	     then n-1
	     else number_before_reaching_sum_r((tl ints), n+1, rsf+(hd ints))

    in
	number_before_reaching_sum_r (ints0, 0, 0)
		 

    end
    


(* int -> int
produces 1 for values between 1 and 31
         2 for values between 32 and 
 *)
	
fun what_month (day : int) =
    let
	val months = [0 ,31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day, months)
    end


(* int*int -> int list 
produce the month of each day from day1 and day2 incl. *)

fun month_range (day1 : int, day2 : int) =
    let
	fun create_range (n1 : int, n2 : int) =
	    if n1 > n2
	    then []
	    else n1 :: create_range (n1+1, n2)

	val days = create_range(day1, day2)

	fun month_range (days : int list) =
	    if null days
	    then []
	    else what_month(hd days) :: month_range(tl days)
    in
	month_range(days)
    end


(* (int*int*int) list -> (int*int*int)
produce the oldest date; produce NONE if xs is empty *)
	
fun oldest (xs : (int*int*int) list) =
    if null xs
    then NONE
    else
	let
	    fun oldest (xs : (int*int*int) list) =
		if null (tl xs)
		then (hd xs)
		else
		    let
			val tl_ans = oldest(tl xs)
		    in
		    if is_older((hd xs), tl_ans)
		    then (hd xs)
		    else tl_ans
		    end
	in
	    SOME (oldest(xs))
	end
