let rec map f l = match l with
  |[]->[]
  |h::t -> (f h )::map f t;;

let increment_all l = map (fun x -> x+1) l;;

let parity l = map (fun x-> (x mod 2)=1) l;;
