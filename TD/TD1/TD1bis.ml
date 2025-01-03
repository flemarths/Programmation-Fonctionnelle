(* 1 *)



(* 2

1. int -> int
2. error ( float -> float )
3. error
4. bool -> int -> int -> int
5. bool -> 'a -> 'a -> 'a
6. 'a -> 'b -> 'a -> 'b
7. 'a -> 'b
8. 'a -> int *)


(* 3.1 *)
let max_of_three x y z = if x>y then
                           if x>z then x
                           else z
                         else if y > z then y
                         else z ;;

(*3.2*)
let is_even x = x mod 2=0;;

(*3.3*)
let is_leap_year y = if y mod 4 <> 0 then false
                     else if y mod 100 <> 0 then true
                     else if y mod 400 <> 0 then false
                     else true;;
                                     
(*4.1*)
let rec sum_range a b = if a>b then 0 else b + sum_range a (b-1) ;;


(*4.2*)
let rec num_digits x = if abs x<10 then 1 else 1 + num_digits (x/10) ;;

(*4.3*)
let rec nth_digit x i = if x = 0 then 0 else if i =1 then x mod 10 else nth_digit (x/10) (i-1);;

(*4.4*)
let rec coeff_bin n k = if k < 0 || k>n then 0 else
                          if k = n || k = 0 then 1 else coeff_bin (n-1) k + coeff_bin (n-1) (k-1);;
