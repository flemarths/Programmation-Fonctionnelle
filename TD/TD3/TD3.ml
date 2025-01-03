(*1*)
let rec sum l = match l with
  |[]->0
  |hd::tl->hd+sum tl;;


(*2*)
let rec contains_zero l = match l with
  |[]->false
  |hd::tl-> (hd=0)||contains_zero tl;;

(*3*)
let rec contains l n = match l with
  |[]-> false
  |hd::tl-> (hd=n) || contains tl n;;


(*4*)
let rec is_sorted l = match l with
  |a::b::c-> (a<=b) && is_sorted (b::c)
  |_->true;;

(*5*)
let rec contains_pair l = match l with
  |a::b::c-> a=b || contains_pair (b::c)
  |_->false;;

(*6*)
let rec increment_all l = match l with
  |[]->[]
  |hd::tl-> (hd+1)::(increment_all tl);;

(*7*)
let rec parite l = match l with
  |[]->[]
  |hd::tl-> ((hd mod 2)=1)::parite tl;;

(*8*)
let rec keep_even l = match l with
  |[]->[]
  |hd::tl-> if (hd mod 2)=0 then hd::(keep_even tl) else keep_even tl ;;

(*9*)
let rec insert_after n l = match l with
  |[]->[]
  |hd::tl-> hd::100::(insert_after n tl);;

(*10*)
let rec insert_between n l = match l with
  |a::b::c-> a::n::(insert_between n (b::c))
  |_->l;;

(*11*)
let rec interleave l1 l2 = match l1,l2 with
  |[],_->l2
  |_,[]->l1
  |a::b,c::d->a::c::(interleave b d);;

(*12*)
let rec flatten l = match l with
  |[]->[]
  |hd::tl->match hd with
           |[]->flatten tl
           |a::b-> a::flatten (b::tl);;

