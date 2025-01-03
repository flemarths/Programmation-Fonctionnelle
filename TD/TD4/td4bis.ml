(*1*)

let min a b = if a < b then a else b;;

let rec min_list l = match l with
  |[]->9999999
  |hd::tl -> min hd (min_list tl);;


(*2*)

let index v l = let rec index_acc v l acc = match l with
                  |[]->None
                  |hd::tl-> if hd=v then Some acc else index_acc v tl (acc+1) in index_acc v l 0;;

(*3*)

let rec min_list2 = function
  |[]->None
  |hd::tl-> begin match min_list2 tl with
            |None-> Some hd
            |Some x-> Some (min hd x)
            end;;



(*4*)

let list_min l = let rec list_min_acc l acc = match l with
                 |[]-> acc
                 |hd::tl -> match acc with
                            |None -> list_min_acc tl (Some hd)
                            |Some x -> list_min_acc tl (Some (min x hd))
                 in (list_min_acc l None);;


(*5*)

let rec square_roots l = match l with
                       |[]->[]
                       |hd::tl -> if hd >= 0. then (Some (sqrt hd))::square_roots tl
                                  else None::square_roots tl;;
                     

  
(*6*)

let list_max l = let rec list_max_acc l acc = match l with
                   |[]->acc
                   |hd::tl-> let x = match acc with
                               |None-> Some hd
                               |Some y-> Some (max y hd)
                             in list_max_acc tl x
                 in list_max_acc l None;;

let list_max2 l1 l2 = match list_max l1 with
  |None-> list_max l2
  |Some x -> match list_max l2 with
             |None-> Some x
             |Some y-> Some(max x y);;

(*7*)

let max_incr l = let rec max_incr_acc l acc = match l with
                   |a::b::tl-> if b>a then let x =match acc with
                                             |None-> (b-a)
                                             |Some x -> max (b-a) x
                                           in max_incr_acc (b::tl) (Some x)
                               else max_incr_acc tl acc
                   |_->acc
                 in max_incr_acc l None;;

                 
(*8.1*)

type date = int*int*int;;

let get_day d = match d with |(x,_,_) -> x;;
let get_month d = match d with (_,x,_)-> x;;
let get_year d = match d with (_,_,x)-> x;;

let today = (31,12,2024);;


(*8.2*)

let is_leap_year y = if (y mod 4)<> 0 then false
                     else if (y mod 100)<>0 || ( (y mod 100)=0 && (y mod 400=0)) then true
                     else false;;

let rec contains l n = match l with
  |[]->false
  |hd::tl-> (hd=n) || contains tl n;;

let is_valid date = match date with  |(d,m,y)-> let x = if m = 2 then if is_leap_year y then 29 else 28
                                                        else if contains [4;6;9;11]  m then 30 else 31
                                                in if d > 0 && d <= x then true else false;;
                                                                                           


(*8.3*)


let is_before date1 date2 = if not (is_valid date1) || not( is_valid date2) then None
                      else match date1,date2 with
                           |(d1,m1,y1),(d2,m2,y2)->if y2>=y1 && m2>=m1 && d2>=d1 then Some true else Some false;;
                        
                        

                            
                       

                     

                    
                                                 



