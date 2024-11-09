let rec filter f l = match l with
  |[]->[]
  |h::d-> if f h then h::(filter f d) else filter f d;;

let keep_positive l = filter (fun x-> x>0) l;;

let keep_even l = filter (fun x-> (x mod 2)=0) l;;
