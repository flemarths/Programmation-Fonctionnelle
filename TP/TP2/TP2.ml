(*1*)

let rec produit l = match l with
  |[]-> 1
  |0::tl->0
  |hd::tl-> hd*produit tl;;


(*2*)

let rec count l a = match l with
  |[]-> 0
  |hd::tl -> (if hd =a then 1 else 0) + count tl a;;


(*3*)

let rec is_equal l = match l with
  |a::b::c-> a=b && is_equal (b::c)
  |_->true;;


(*4*)

let rec append l1 l2 = match l1 with
  |[]->l2
  |hd::tl-> hd::(append tl l2);;


(*5*)
let rec invert_acc l acc = match l with
  |[]->acc
  |hd::tl-> invert_acc tl (hd::acc);;

let invert l =  invert_acc l [];;


(*6*)

let rec is_in l a = match l with
  |[]-> false
  |hd::tl-> hd=a || is_in tl a;;

let rec clean_acc l acc = match l with
  |[]->acc
  |hd::tl -> if is_in acc hd then clean_acc tl acc else clean_acc tl (hd::acc);;
             
let clean l = invert (clean_acc l []);;


(*7*)

let rec slice l a b = match l,a,b with
  |[],_,_-> []
  |_,0,0->[]
  |hd::tl,0,y-> hd::(slice tl 0 (y-1))
  |hd::tl,x,y-> slice tl (x-1) (y-1);;
 
