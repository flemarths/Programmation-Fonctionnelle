(*CM du jeudi 14 Novembre 2024

 28 Nov pas de TD et 04 Dec pas de CM 

   Racine
    / \
   0   0 Feuille
      / \
     0   0
    nil  nil   

un arbre est dit équilibré si tous les noeuds ont 2 enfants ou sont une feuille.

Propriété arbre de recherche binaireff :Soit x un noeud d'un arbre T,
     *si y est un noeud dans y sous-arbre à gauche de x alors val(y) <= val(x)
     *si z est un noeud dans y sous-arbre à droite de x alors val(z) >= val(x)

                             7
                            / \
                           5   9
                          / \ / \
                         2  6 8 10


Plusieurs manières de visiter un arbre : - PRE on commence par la racine
                                         - POST on commence par les feuilles
                                         - In dans l'ordre croissant des éléments

Ex: soit la liste [ 1, 4, 5, 10, 16, 17, 21 ] faire des arbres de recherche binaires de hauteur 2, 3, 4, 5, 6


                                     10
                                   /   \
                                 4       17
                               /  \     /  \
                              1    5   16   21

                              1
                                   4
                                       5
                                           10    
                                               16
                                                 17
                                                   21

                                    4
                                 1     5
                                           10    
                                               16
                                                 17
                                                   21

                                    4
                                 1     5
                                           10    
                                               17
                                             16   21
                                                
                                    5
                                 4     10
                              1            17
                                         16   21

Ex: On cherche une valeur x dans un arbre de recherche
    ( on suppose qu'elle est toujours présente dans cet arbre )*)

type t =
  |Nil
  |Node of t*int*t;;

let rec x_in_tree x t = match t with
  |Nil -> false
  |Node(a,b,c) -> if x=b then true else if x>b then x_in_tree x c else x_in_tree x a ;;

(* on cherche a determiner si la liste suivante correspond à un arbre parcouru par l'algorythme précédent
 2 252 401 398 330 344 597 363 -> oui
 125 202 911 240 912 245 363 -> non

let rec is_tree l acc =

écrire un algoritme pour isérer un élément dans un arbre de recherche binaire :*)

let rec insert x t = match t with
  |Nil->Node(Nil,x,Nil)
  |Node(a,b,c)->if x=>b then Node(a,b,insert x c) else Node(insert x a,b,c);;

(* écrire un algorithme pour effacer le noeud x dans un arbre de recherche binaire

1er cas     x      il suffit de remplacer le noeud x par Nil
           / \
         Nil Nil 

2eme cas        x       on remplace x par z puis z par Nil
               / \
              z   Nil


3eme cas     x    on va chercher le successeur de x  ( plus petit élément plus grand que x )
            / \   ( le fils gauche de gauche de ce successeur est forcément Nil)
           y   z   On remplace ensuite x par succ(x) puis on ajoute les fils y et z (sans le succ(x))
                   Et y' (le fils droite de succ(x) prend la place de succ(x) *)

let rec delete x t = match t with
  |Nil ->Nil
  |Node(Nil,y,Nil)-> if x=y then Nil else Node(Nil,y,Nil)
  |Node(t1,y,Nil)-> if x=y then t1 else if x>y then Node(t1,y,Nil) else Node(delete x t1,y,Nil)
  |Node(Nil,y,t2)->if x=y then t2 else if x<y then Node(Nil,y,t1) else Node(Nil,y,delete x t2)
  |Node(t1,y,t2)-> if x=y then Node(t1,(succ x t1),(delete t2 succ x))
                   else if x<y then Node((delete t1 x),y,t2)
                                      else Node(t1,y,delete(t2 x));;
