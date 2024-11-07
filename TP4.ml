(* Exercice 1 *)

let rec map f l = match l with
  |[]->[]
  |h::t -> (f h )::map f t;;

let increment_all l = map (fun x -> x+1) l;;

let parity l = map (fun x-> (x mod 2)=1) l;;


(* Exercice 2 *)

let rec filter f l = match l with
  |[]->[]
  |h::d-> if f h then h::(filter f d) else filter f d;;

let keep_positive l = filter (fun x-> x>0) l;;

let keep_even l = filter (fun x-> (x mod 2)=0) l;;

(* Exercice 3 *)

let rec fold_right f l acc = match l with
  |[]-> acc
  |h::t-> f h (fold_right f t acc);;

let rec fold_left f acc l = match l with
  |[]-> acc
  |h::t -> fold_left f (f acc h) t;;

let impripair l = fold_left (fun x acc -> if (acc mod 2)=0 then print_int x; print_newline(); acc+1) 0 l;;

let rec sliceacc l a b acc = match l with
  |[]->[]
  |h::t -> if acc>=a && acc<b then h::(sliceacc t a b (acc+1)) else sliceacc t a b (acc+1);;

let slice l a b = sliceacc l a b 0;;

let slice2 l a b  = snd (fold_right (fun x (acc,res) -> if acc>=a && acc<b then (acc-1,x::res) else (acc-1,res)) l ((List.length l)-1,[]));;


(* Exercice 4 *)

