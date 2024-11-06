(* Exercice n°1 *)

let rec list_min l = match l with
  |[a]->a
  |h::t->
    let m = list_min t in
              if m > h then h
              else m
  |[]->(-1);;