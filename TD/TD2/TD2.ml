(*1*)

let rec pgcd a b = if a=b then a
               else if a<b then pgcd a (b-a)
               else pgcd (a-b) b;;
                      
(*2*)

let move org dst = print_string "Déplacer un disque du pillier ";
                   print_int org;
                   print_string " vers le pillier ";
                   print_int dst;
                   print_string "\n";;

let rec  hanoi n org int dst = if n = 1 then move org dst 
                               else begin
                                   hanoi (n-1) org dst int;
                                   move org dst;
                                   hanoi (n-1) int org dst;
                               end;;
                               
(*3.1*)

let min a  b = if a<b then a else b;;

(*3.2*)

let plafonne_a n = min n ;;

(*3.3*)

let permute_args f a b = f b a ;;


(*3.4*)

let rec syracuse n = if n=1 then 0
                     else if n mod 2 = 0 then 1 + syracuse (n/2)
                     else 1+syracuse (1+3*n);;
