(*
 * Working with Lists
 *)

(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)

let rec last l =
  match l with
  | [] -> None
  | [x] -> Some x
  | _ :: tl -> last tl

let () = assert (last [1; 2; 3;] = Some 3)

(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)

let rec last_two l =
  match l with
  | [] | [_] -> None
  | [x1;x2] -> Some(x1, x2)
  | _ :: tl -> last_two tl

let () = assert (last_two [1; 2; 3;] = Some(2, 3))

(* 3. Find the k'th element of a list. (easy) *)

let rec at k l =
  match l with
  | [] -> None
  | hd :: tl -> if k = 1 then Some hd else at (k-1) tl;;

let () = assert (at 2 [1; 2; 3;] = Some 2)

(* 4a. Find the number of elements of a list. (easy) *)

let rec length l =
  match l with
  | [] -> 0
  | _ :: tl -> 1 + length tl

let () = assert (length [1; 2; 3;] = 3)

(* 4b. Find the number of elements of a list. (easy) TAIL RECURSION VERSION *)

let length_tail_rec l =
  let rec aux n = function
    | [] -> n
    | _ :: tl -> aux (n+1) tl
  in aux 0 l;;

let () = assert (length_tail_rec [1; 2; 3;] = 3)

(* 5. Reverse a list. (easy) *)

let reverse l =
  let rec aux reversed = function
    | [] -> reversed
    | head :: tail -> aux (head::reversed) tail in
  aux [] l

let () = assert (reverse [1; 2; 3;] = [3; 2; 1;])

(* 6. Find out whether a list is a palindrome. (easy) *)

let is_pal l =
  l = reverse l

let () = assert (is_pal [1;2;2;1;])
let () = assert (is_pal [1; 2; 3;] = false)

(* 7. Flatten a nested list structure. (medium) *)

(* There is no nested list type in OCaml, so we need to define one
   first. A node of a nested list is either an element, or a list of
  nodes. *)

type 'a node =
  | One of 'a
  | Many of 'a node list;;

let flatten l =
  let rec aux acc = function
    | [] -> acc
    | One head :: tail ->  aux (head :: acc) tail
    | Many l :: tail -> aux (aux acc l) tail in
  reverse (aux [] l);;

let () = assert (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] = ["a"; "b"; "c"; "d"; "e"])

(* 8. Eliminate consecutive duplicates of list elements. (medium) *)

let rec compress l =
  match l with
  | [] -> []
  | [x] -> [x]
  | x :: (y :: _ as tail) -> if x = y then compress tail else x :: compress tail

let () = assert(compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a"; "b"; "c"; "a"; "d"; "e"])
let () = assert(compress ["a";] = ["a";])
let () = assert(compress ["a"; "a";] = ["a";])

(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)

let pack l =
  let rec aux pack acc = function
    | [] -> []
    | [x] -> (x :: pack) :: acc
    | x :: (y :: _ as tail) -> if x = y
      then aux (x :: pack) acc tail
      else aux [] ((x :: pack) :: acc) tail in
    reverse (aux [] [] l)

let () = assert(
  pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] =
  [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
