type 'a arbre =
  | Nil
  | Node of 'a * 'a arbre * 'a arbre ;;

let arbre = Node(3, Node(2,Nil,Nil) ,Node(5, Node(4,Nil,Nil),Nil));;

let max x y = if x > y then x else y;;

let rec height t = match t with
  |Nil->0
  |Node(_,b,c)-> 1 + max (height b) (height c);;

