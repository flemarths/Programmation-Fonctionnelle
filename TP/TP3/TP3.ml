(*1.1*)
type 'a nelist =
  |Nil of 'a
  |Cons of 'a * 'a nelist;;

(*1.2*)
type point = { x : int ; y : int };;

let move_left p = {x=p.x-1;y=p.y};;


(*1.3*)
type card = N | S | E | W;;

(*1.4*)
let move p card = match card with
  |N -> {x=p.x ; y=p.y+1}
  |S -> {x=p.x ; y=p.y-1}
  |E -> {x=p.x+1 ; y=p.y}
  |W -> {x=p.x-1 ; y=p.y};;

let rec multi_move p l = match l with
  |[]->p
  |hd::tl-> multi_move (move p hd) tl;;

(*2*)
type student = {first_name : string; last_name : string; gpa : float};;

let get_name student = student.first_name, student.last_name;;

let create_student first last g = {first_name=first; last_name=last ; gpa=g};;


(*3*)
let rec take l n = if n=0 then []
                   else match l with
                        |[]->[]
                        |hd::tl->hd::(take tl (n-1));;


(*4*)
let rec drop l n = if n=0 then l
                   else match l with
                        |[]->[]
                        |hd::tl -> drop tl (n-1);;



(*5*)

let rec take_acc l n acc = if n=0 then acc
                           else match l with
                                |[]->acc
                                |hd::tl-> take_acc tl (n-1) (hd::acc);;

let take2 l n = take_acc l n [];;



(*6.1*)

type 'a bintree =
  |Nil
  |Node of 'a * 'a bintree * 'a bintree ;;

let rec height tree = match tree with
  |Nil -> 0
  |Node(_,t1,t2)-> 1 + max (height t1) (height t2);;

(*6.2*)

let rec maxtree tree = match tree with
  |Nil -> None
  |Node(x,t1,t2)->match (maxtree t1, maxtree t2) with
                  |(None,None)-> Some x
                  |(None, Some y) -> Some (max x y)
                  |(Some y, None)-> Some (max x y)
                  |(Some y, Some z)-> Some (max x (max y z ));;

(*6.3*)

let rec to_list tree = match tree with
  |Nil -> []
  |Node(x,t1,t2)->x::(to_list t1)@(to_list t2);;


(*6.4*)

let rec to_tree l= match l with
  |[]-> Nil
  |hd::tl-> Node(hd,Nil,to_tree tl);;
