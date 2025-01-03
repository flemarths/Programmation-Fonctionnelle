let rec insert1 l v = match l with
  |[]-> [v]
  |[a]->if v>a then a::[v] else v::[a]
  |a::b::tl -> if v<a then v::l else (if v>b then a::b::(insert1 tl v) else a::v::b::tl) ;;


let rec insert2 l v = match l with
  |[]->[v]
  |h::t->if h>v then v::l else h::(insert2 t v);;


let sort l =let rec sortacc l acc = match l with
  |[]->acc
  |hd::tl-> sortacc tl (insert2 acc hd) in sortacc l [];;

let rec remove l a = match l with
  |[]->[]
  |hd::tl-> if hd = a then remove tl a else hd::remove tl a;;

let remove2 l a = List.filter(fun x-> x<>a) l;;

let rec oneofeach l = match l with
  |[]->[]
  |hd::tl-> hd::oneofeach(remove tl hd);;

let  union l m = oneofeach( l @ m);;

let rec union2 l m = match l with
  |[]->m
  |h::t->h::union t (remove m h);;

let rec couple l x = match l with
  |[]->[]
  |h::t->(h,x)::couple t x;;

let rec cartesienacc l1 l2 acc = match l1 with
  |[]->acc
  |h::tl->cartesienacc tl l2 ((couple l2 h)@ acc);;

let cartesien l1 l2 = cartesienacc l1 l2 [];;

let rec cartesien2 l1 l2 = match l1 with
  |[]->[]
  |h::t->(couple l2 h)@(cartesien2 t l2);;

(* autre variant avec list.map

let rec cartesien l1 l2 = match l with
    |[]->[]
    |h::t-> (List.map(fun a->(h,a)) l2 )@(cartesien t l2);;*)

(*let l = [1;2;3];;
List_fold_left (+) 0 (List.map (fun x -> x*x) l);;
List_fold_left (fun x a -> x + a*a) 0 l;;*)



