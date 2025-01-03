(*1.1*)

let rec map l f = match l with
  |[]->[]
  |hd::tl-> (f hd)::(map tl f);;

(*1.2*)

let increment_all l = let f x = x+1 in map l f;;


(*1.3*)

let parity l = let f x = (x mod 2 =1) in map l f;;


(*2.1*)

let rec filter l f = match l with
  |[]->[]
  |hd::tl-> if (f hd )=true then hd::(filter tl f)
            else filter tl f;;

(*2.2*)

let keep_positive l = let f x = if x>= 0 then true else false in filter l f;;

(*2.3*)

let keep_even l = let f x = if (x mod 2 = 0) then true else false in filter l f;;

(*3.1*)

let rec fold_right f l acc = match l with
  |hd::tl -> f hd (fold_right f tl acc)
  |[]->acc;;

let rec fold_left f acc l = match l with
  |hd::tl-> fold_left f (f acc hd) tl
  |[]->acc;;


(*3.2*)


let print_pair l = let f acc x = if (acc mod 2) = 0 then (print_int x;print_string "\n"); acc+1 in fold_left f 0 l;;


(*3.3*)

(*let slice l a b = let rec slice_acc l a b acc = match l with
                    |[]->acc
                    |hd::tl->if b=0 then acc 
                             else if a=1 then slice_acc tl a (b-1) (acc@[hd])
                             else slice_acc tl (a-1) (b-1) acc in slice_acc l a b [];; *)


let slice l a b = match fold_right (fun x (i,acc) -> if a<=i && b>i then (i-1,x::acc) else (i-1,acc)) l (List.length l -1, []) with
  |(x,y)->y;;

(*4*)

type 'a tree = |Null
               |Node of 'a*'a tree*'a tree;;

let rec suffix f t v = match t with
  |Null-> v
  |Node(x,t1,t2)-> f x (suffix f t1 v) (suffix f t2 v);;


let height t = suffix (fun _ t1 t2 -> 1 + max t1 t2) t 0;;
let countnodes t = suffix (fun _ t1 t2 -> 1 + t1 + t2) t 0;;

                                     
                              
    
                                                                                    
                                                                 
                                                                                    

  
                                                                               
                                                        
                     
