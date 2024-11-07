(* 2 partiels un mtn et un autre en décembre de 8h à 10h (à priori)
   si pas satisfait -> examen terminal 3eme note*)

(*fonction d'ordre 1*)
let double x = 2*x;;
let square x = x*x;;

(*fonction d'ordre 2*)
let quad x = double(double(x));;

List.map sin [2. ; 2.4 ; 3.];;
List.map (fun x -> x+1) [82 ; 24 ; 5 ; 18 ];;

List.filter (fun x -> x<10) [2;24;5;18];;

List.fold_right (+) [2;4;6;8] 0;;
List.fold_right (+) [] 0;;
List.fold_right ( * ) [2;4;6;8] 1;;

(* Exercices du cours *)

let rec repeat f n x = if n > 0 then repeat f (n-1) ( f x ) else x;;

let rec range a b = if b>=a then a::(range (a+1) b) else [];;

let fives n = List.map (fun x->5*x) (range 1 (n/5)) ;;

let makeintpos c = List.filter (fun x -> x>0 )
                      (List.map int_of_string (String.split_on_char ' ' c));;

let power x n = repeat (fun a -> a * x) n 1;;
let power_of x n = List.map (power x) (range 0 n);;

List.map (x-> f(g x)) lst;;

let rec makeints c s = match c with
  |[]->""
  |a::b::tl-> (a ^ s)^makeints (b::tl) s
  |b::tl->b;;

let join_with strs sep =
  match strs with
  | [] -> ""
  |x :: xs ->
    List.fold_left
      (fun combined s -> combined ^ sep ^ s) x xs;;
