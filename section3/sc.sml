val x = 2


fun sqrt_abs i = 
    Math.sqrt(Real.fromInt(abs i))



fun sqrt_abs2 i = 
    (Math.sqrt o Real.fromInt o abs) i


val sqrt_abs3 = Math.sqrt o Real.fromInt o abs


val x = sqrt_abs3 16





fun fold f = fn acc => fn xs =>
  case xs of
    []     => acc
  | x::xs' => fold f (f(acc,x)) xs'


val sum_list = fold (fn (x, y) => x + y) 0 

















