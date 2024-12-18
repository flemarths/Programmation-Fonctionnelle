
   Pre-order : on commence par les racines  (R,G,D) (préfixe en fr)
   In-order : on prend les valeurs dans l'ordre croissant (G,R,D) (Infixe)
   Post-order : on prend les feuilles en premier (G,D,R) (postfixe) *)


type t  =
  |Nil
  |Node of int*t*t;;

let rec prefixe t = match t with
  |Nil->[]
  |Node(r,g,d) -> (r::(prefixe g))@(prefixe d);;

let rec infixe t = match t with
  |Nil->[]
  |Node(r,g,d) -> (infixe g)@(r::(infixe d));;

let rec postfixe t = match t with
  |Nil->[]
  |Node(r,g,d) -> (postfixe g)@(postfixe d)@[r];;

(*2- Il ne s'agit pas d'un arbre de recherche binaire car z est avant y sur l'arbre alors qu'il est après dans l'alphabet

3- pre - ( CASIENLOY )
4- in - ( ACEILNOSY )
5- post - ( AELONIYSC )*)


(*type tchar =
  |Nil
  |Node of tchar*char*tchar;;*)


let rec insert x t = match t with
  |Nil->Node(Nil,x,Nil)
  |Node(a,b,c)->if( x>=b) then Node(a,b,insert x c) else Node(insert x a,b,c);;

let rec list_insert l t = match l with
  |[]-> t
  |h::d-> list_insert d (insert h t);;

(* Les deux descriptions ne donnent pas le même arbre

                                13
                             /      \
                           5          19
                         /   \           \
                       3      11          23
                     /       /
                   2        7

                                13
                             /      \
                           11        19
                         /              \
                      7                   23
                    /
                 5
               /
              3
            /
         2                      *)


let rec next l x = match l with
  |[]-> None
  |a::b::l'-> when a=x then some b else next b::l' x;;

   let succ x t = next (infixe t) x;;

   
