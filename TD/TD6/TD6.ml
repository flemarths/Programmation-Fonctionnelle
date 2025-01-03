(*1.1*)

let moyenne t =
  let n = Array.length t in if n=0 then invalid_arg "moyenne"
                            else let sum = ref 0. in
                                 for i=0 to n-1 do
                                   sum:=!sum+.t.(i)
                                 done;
                                 !sum/.float(n);;


(*1.2*)


let prodscal x y = let res = ref 0. in for i=0 to (Array.length x)-1 do
                                         res:=!res+.x.(i)*.y.(i)                                       
                                       done;                                       
                                       !res;;

(*2.1*)


let swap t i j =
  let temp = t.(i) in
  t.(i)<- t.(j);
  t.(j)<-temp;;


(*2.2*)


let argmin k t = let res = ref k in
                 for i=k+1 to Array.length t -1 do
                   if t.(i) < t.(!res)
                   then res:= i
                 done;
                 !res;;


(*2.3*)

let tri_selection t = for i=0 to Array.length t -1 do
                        swap t i (argmin i t)
                      done;;



(*3.1*)


let factorielle n = let res = ref 1 in
                    for i=2 to n do
                      res := !res*i;
                    done;
                    !res;;

(*3.2*)

let sommel l =
  let sum = ref 0 in
  let lbis = ref l in
  while !lbis <> [] do
    sum := !sum + List.hd !lbis;
    lbis := List.tl !lbis;
    done;
  !sum;;


(*3.3*)

let argminl l = if l = [] then invalid_arg "argminl"
                else begin
                    let li = ref l in
                    let res,min = ref 0 ,ref (List.hd l) in
                    let i  = ref 0 in
                    while !li <> [] do
                      if List.hd !li < !min then begin
                          min:= List.hd !li ;
                          res:= !i
                        end;
                      i:=!i+1;
                      li:=List.tl !li;
                    done;
                    !res
                  end;;


(*3.4*)

let fac n =
  let rec fac_acc n acc =
    if n=1 then acc
    else fac_acc (n-1) (n*acc)
  in fac_acc n 1;;

let sommelr l =
  let rec somme_acc l acc =
    match l with
    |[]-> acc
    |hd::tl-> somme_acc tl (hd+acc)
  in somme_acc l 0;;

let argminr l = match l with
  |[]-> invalid_arg "argmin"
  |a::b->let rec argmin_acc l acc i res =
           match l with
           |[]->res
           |hd::tl-> if hd<acc then argmin_acc tl hd (i+1) i
                     else argmin_acc tl acc (i+1) res;
         in argmin_acc l a 0 0;;


(*4.1*)
type mlist  =
  |Empty
  |Cons of int ref * mlist ref;;
         
let rec somme mlist = match mlist with
  |Empty->0
  |Cons(i,mlist2)-> !i+ somme !mlist2;;


(*4.2*)

let rec set l i n = match l with
  |Empty->invalid_arg "set"
  |Cons(x,l2)-> if i=0 then x:=n
                else set !l2 (i-1) n ;;

(*4.3*)

let cycle ml =
let rec cyc = function
| Empty -> invalid_arg "cycle"
| Cons(_,r) -> if !r=Empty then r:=ml else cyc !r
in cyc ml;;


(*4.4*)
 
let clength ml =
  
let rec clen i = function
| Empty -> i
| Cons(_,mll) -> if ml == !mll then i+1 else clen (i+1) !mll
in clen 0 ml;;



                                                         
                           
                  
