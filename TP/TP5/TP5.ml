(*1*)
type 'a tree =
    |Null
    |Node of 'a*'a tree*'a tree;;

let rec is_st tree = match tree with
  |Null->true
  |Node(v,l,r)->let m = match l,r with
                  |Null,Null->true                            
                  |Null,Node(y,_,_)->y>v
                  |Node(x,_,_),Null->x<v                                   
                  |Node(x,_,_),Node(y,_,_)->x<v && y>v in (m && is_st l && is_st r);;

(*2*)


let rec is_in t x = match t with
  |Null->false
  |Node(v,l,r)-> v=x || (is_in l x ) || (is_in r x) ;;

let rec min_anc a b t = let x,y = min a b,max a b in match t with
                                                     |Null-> None                                                           
                                                     |Node(v,l,r)-> match is_in l x && is_in l y,is_in r x && is_in r y with
                                                                    |false,false->Some v
                                                                    |true,false->min_anc x y l
                                                                    |false,true->min_anc x y r
                                                                    |true,true->None;;


(*3*)

let rec in_order t = match t with
  |Null -> []
  |Node(v,l,r)-> in_order l @ [v] @ in_order r;;

let length l = let rec length_acc l acc = match l with
                 |[]->acc
                 |hd::tl-> length_acc tl (acc+1) in length_acc l 0;;

let k_bigger k t = let rec k_bigger_list k l =  match l with
  |[]->None
  |hd::tl->if length l = k then Some hd else k_bigger_list k tl in k_bigger_list k (in_order t);;


(*4*)

let rec in_list x l = match l with
  |[]->false
  |hd::tl-> hd = x || in_list x tl;;

let leave t = match t with
  |Node(v,Null,Null)-> true
  |_->false;;

let dead t = let rec dead_end t list = match t with
  |Null->false
  |Node(v,l,r)->((leave t) && (in_list (v+1) list) && (in_list (v-1) list))|| dead_end l list || dead_end r list
in dead_end t (0::in_order t);;


(*5*)

let common t1 t2 = let rec commonl l1 l2 acc = match l1 with
                     |[]->acc
                     |hd::tl when in_list hd l2 -> commonl tl l2 (hd::acc)
                     |hd::tl -> commonl tl l2 acc in commonl (in_order t1) (in_order t2) [];;
