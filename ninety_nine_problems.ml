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
    reverse (aux [] [] l);;

let () = assert(
  pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] =
  [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
)

(* 10. Run-length encoding of a list. (easy) *)

let encode l =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count + 1, x) :: acc
    | x :: ( y :: z as tail ) -> if x = y
      then aux (count + 1) acc tail
      else aux 0 (((count+1), x) :: acc) tail in
  reverse (aux 0 [] l);;

let () = assert(
  encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
  [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
)

(* 11. Duplicate the elements of a list. (easy) *)

let duplicate l =
  let rec aux acc = function
    | [] -> []
    | [head] -> head :: head :: acc
    | head :: tail -> aux (head :: head :: acc) tail in
  reverse (aux [] l)

let () = assert(duplicate ["a";"b";"c";"c";"d";] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d";])


(* 12. Replicate the elements of a list a given number of times. (medium) *)

let replicate l n =
  let rec prepend n acc x =
    if n = 0 then acc else prepend (n-1) (x :: acc) x in
  let rec aux acc = function
    | [] -> acc
    | head :: tail -> aux (prepend n acc head) tail in
  aux [] (List.rev l);;

let () = assert(replicate ["a";"b";"c";] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"])

(* 13. Drop every N'th element from a list. (medium) *)

let drop l n =
  let rec aux acc count = function
    | [] -> acc
    | head :: tail -> if count = 0
      then aux acc (n-1) tail
      else aux (head :: acc) (count-1) tail in
  List.rev (aux [] (n-1) l)

let () = assert(
  drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 =
  ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
)

(* 14. Split a list into two parts; the length of the first part is given. (easy) *)

let split l n =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | head :: tail as l -> if i = 0
      then List.rev acc, l
      else aux (i-1) (head :: acc) tail in
    aux n [] l;;

let () = assert(
  split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 =
  (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
)

(* 15. Extract a slice from a list. (medium) *)
