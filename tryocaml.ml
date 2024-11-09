(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)


let max_of_three x y z = if x > y && x > z then x 
  else if y > x && y > z then y 
  else z ;;


let is_even x = if x mod 2 = 0 then true else false  ;;

let is_leap_year y = if y mod 4 <>  0 then false
  else if y mod 100 <> 0 then true 
  else if y mod 400 <> 0 then false
  else true ;;

let rec sum_range a b = if a = b then a
  else sum_range a (b-1) + b ;;

let rec num_digit x = if x / 10 < 1 then 1
  else num_digit (x / 10) + 1;;

let rec nth_digit i x = if i > num_digit x then 0
  else if i = 1 then x mod 10
  else nth_digit (i-1) (x / 10) ;;

let rec binom n k = if k = 0 then 1 
  else if k = n then 1
  else binom (n-1) k + binom (n-1) (k-1) ;;
 