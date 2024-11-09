type 'a  arbre =
  |Null
  |Node of 'a*'a arbre*'a arbre*'a arbre;;

let rec suffixe f t v 
