(*1*)
let add x y = x + y;;


(*2*)
let date_fun d m = if d>31 || d<0 then false
                   else if d<=28 && m ="Feb" then true
                   else if d<=30 && (m="Apr" || m="Jun" || m="Sept" ||m="Nov") then true
                   else if d<31 && (m="Jan" || m="Mar" || m="May" || m="Jul" || m="Aug" || m="Oct" || m="Dec") then true
                   else false;;
                                                                          
(*3*)
let absf x = if x<0.0 then -.x else x;;


(*4*)
let signe x = if x =0 then 0
              else if x < 0 then (-1)
              else 1;;

(*5*)
let rec fac n =if n < 0 then 0
               else if n =0 then 1
               else n * fac (n-1);;

(*6*)
let rec est_diviseur n d = n mod d = 0;;


(*7*)
let est_premier n =
  let rec inter d =
    d*d>n || (not (est_diviseur n d) && inter (d+1)) in inter 2;;


(*8*)
let rec plus a b = if b=0 then a else 1 + plus a (b-1);;


(*9*)
let rec prod a b = if b=1 then a else a + prod a (b-1);;