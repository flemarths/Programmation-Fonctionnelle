let swap arr i j =
  if i >= 0 && i < Array.length arr && j >= 0 && j < Array.length arr then
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  else
    invalid_arg "Indices out of bounds";;


let shuffle arr =
  let n = Array.length arr in
  if n > 0 then
    for i = 0 to n - 1 do
      let j = Random.int n in
      swap arr i j
    done;;

let dimmension arr =
  let l = Array.length arr in
  if arr = [||] then
    (0,0)
  else
    (l,Array.length arr.(1));;

let make_matrix m n e =
  if m > 0 && n > 0 then
    let res = Array.make m [||] in
    for i = 0 to m - 1 do
      res.(i) <- Array.make n e
    done;
    res
  else
    invalid_arg "Indices out of bounds";;

let (+~) a b =
  let m = Array.length a in
  let n = Array.length a.(0) in
  if m = Array.length b && n = Array.length b.(0) then
    let res = make_matrix m n 0 in
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        res.(i).(j) <- a.(i).(j) + b.(i).(j)
      done
    done;
    res
  else
    invalid_arg "Matrix dimensions must match for addition";;

let ( *~ ) a b =
  let m = Array.length a in
  let n = Array.length a.(0) in
  let p = Array.length b.(0) in
  if n = Array.length b then
    let res = make_matrix m p 0 in
    for i = 0 to m - 1 do
      for j = 0 to p - 1 do
        for k = 0 to n - 1 do
          res.(i).(j)<- res.(i).(j) + (a.(i).(k) * b.(k).(j))
        done
      done
    done;
    res
  else
    invalid_arg "Matrix dimensions must match for addition";;
                      

type 'a cell = {
    content : 'a ;
    mutable next : 'a cell option ;
    }]

type 'a queue = {
    mutable first : 'a cell option ;
    mutable last : 'a cell option ;
  }

