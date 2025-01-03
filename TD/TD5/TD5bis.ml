(*1.1*)

type 'a tree =
  |Null
  |Node of 'a*'a tree*'a tree;;


let rec pre_order t = match t with
  |Null -> []
  |Node(v,l,r)-> [v]@(pre_order l)@(pre_order r);;

let rec in_order t = match t with
  |Null -> []
  |Node(v,l,r)-> (in_order l)@[v]@(in_order r);;

let rec post_order t = match t with
  |Null -> []
  |Node(v,l,r)-> (post_order l)@(post_order r)@[v];;

(*1.2*)

(* Non il ne s'agit pas d'un arbre de recherche binaire car le Z est à gauche du Y alors qu'il devrait être à droite*)


(*1.3*)
(*A,E,L,O,N,I,Y,S,C*)

(*1.4*)
(*C,A,S,I,E,N,L,O,Y*)

(*1.5*)
(*A,C,E,I,L,N,O,S,Y*)


(*2*)
(*                                           B
                                         /      \
                                      A            R
                                                /     \
                                              N         S
                                            /
                                          C
                                           \
                                            H
                                            /
                                          E
 *)



(*4.1*)

let suc x t= let rec suc_acc x l = match l with               
               |a::b::tl-> if a=x then Some b else suc_acc x (b::tl)
               |_->None in suc_acc x (in_order t);;

(*4.2*)

let pre x t=let rec pre_acc x l = match l with
              |a::b::tl-> if b=x then Some a else pre_acc x (b::tl)
              |_->None in pre_acc x (in_order t);;

(*4.3*)

(* Soit S(x) le succésseur de x il est plus grand que x donc il est dans la branche droite de x. Si S(x) avait un enfant à gauche alors il serait entre x et S(x) ce qui est impossible car S(x) est le plus petit nombre plus grand q x.
De même si P(X) est le prédécésseur de x il n'a pas d'enfant à droite*)




       
