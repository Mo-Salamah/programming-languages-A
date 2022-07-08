use "hw01-1.sml";

val test1 = is_older ((1,2,3),(2,3,4)) = true;

val test11 = is_older((2000, 10, 20), (1990, 5, 10)) = false;
val test12 = is_older((2000, 10, 20), (2000,10,20)) = false;
val test13 = is_older((2000, 5, 10), (2000, 5, 5)) = false;
val test14 = is_older((1990, 7, 7), (2100, 3, 4)) = true;



val test21 = number_in_month ([(2000, 7, 10), (1000, 3, 7), (1999, 7, 30)], 7) = 2
val test22 = number_in_month([], 3) = 0;
val test23 = number_in_month([(2000, 4, 13), (1990, 3, 30)], 8) = 0
								       



val test31 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[7,8,9,10,2]) = 1
val test32 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

											  
val test41 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test42 = dates_in_month ([(2000, 7, 10), (1000, 3, 7), (1999, 7, 30)], 7) = [(2000,7,10), (1999,7,30)]
val test43 = dates_in_month ([], 3) = []
val test44 = dates_in_month ([(2000, 4, 13), (1990, 3, 30)], 8) = []
								       
val test51 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[7,8,9,10,2]) = [(2012,2,28)]
val test52 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28), (2011,3,31), (2011,4,28)]

											       
val test61 = get_nth (["A", "B", "C", "D", "E"], 1) = "A"
val test62 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test71 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test72 = date_to_string (1999, 10, 31) = "October 31, 1999"

						 
val test81 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test82 = number_before_reaching_sum (10, [0, 0, 0, 1, 2, 3, 4, 5]) = 6
val test83 = number_before_reaching_sum (11, [0, 0, 0, 1, 2, 3, 4, 5]) = 7
val test84 = number_before_reaching_sum (14, [0, 0, 0, 1, 2, 3, 4, 5]) = 7
val test85 = number_before_reaching_sum (1,  [1, 2, 3, 4, 5]) = 0

								 
val test91 = what_month 70 = 3
val test92 = what_month 1 = 1
val test93 = what_month 355 = 12
val test94 = what_month 365  = 12
val test95 = what_month 31  = 1
val test96 = what_month 181  = 6
val test97 = what_month 183  = 7
			
val test101 = month_range (31, 34) = [1,2,2,2]
val test102 = month_range (35, 34) = []
val test103 = month_range (180, 185) = [6,6,7,7,7,7]					
					
val test111 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test112 = oldest([(1012,2,28),(2011,3,31),(2011,4,28)]) = SOME (1012,2,28)
val test113 = oldest([(2012,2,28),(2011,8,31),(2011,4,28)]) = SOME (2011,4,28)
