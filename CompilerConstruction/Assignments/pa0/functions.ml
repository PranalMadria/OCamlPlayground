(*Author: Pranal Madria*)
(*PA0*)

(* You will fill your implementations in this file *)

(*Imports*)
open Printf
open String
(*Printing*)
let message = "Hello World!";;
(printf "%s\n" message)

(*Functions*)
let max (n :int) (m : int) : int = 
    if n > m then n else m;;

(*Testing Function*)

(printf "Should be 5: %d\n" (max 5 4));;
(printf "Should be 4: %d\n" (max 3 4));;

(printf "Should be 4: %d\n" (max 4 4));;


(*Recursive Functions*)
let rec factorial (n : int) : int = 
    if n <= 1 then 1
    else n * (factorial (n - 1));;
(printf "Should be 5: %d\n" (factorial 5));;


let rec fibonacci (n :int) : int = 
    if n <= 1 then 1
    else if n = 2 then 2
    else fibonacci(n-1) + fibonacci(n-2);;



(printf "Should be 5: %d\n" (fibonacci 5));;

(*binary tree node*)
type btnode =
  | Leaf
  | Node of string * btnode * btnode

  (* get size of binary tree; number of nodes*)
let rec size (btn: btnode) : int = 
    match btn with
       | Leaf -> 0
       | Node(s, left, right) -> 
            1 + (size left) + (size right);;



(* get height of binary tree; number of nodes*)
let rec height (btn: btnode) : int = 
    match btn with
        | Leaf -> 0
        | Node(s, left, right) ->
                1 + max(height left) (height right);;


let rec inc_all (l: int list) : int list = 
    match l with 
        | [] -> []
        | first::rest -> first+1 :: inc_all(rest);;

let rec long_strings (l: string list) (require: int) : string list =
    match l with
        | [] -> []
        | first::rest -> 
                if String.length first > require then first::(long_strings rest require)
                else (long_strings rest require) 

                    
let rec everyOther (l: 'a list) : 'a list = 
    match l with 
        | [] | [_] -> []
        | first::rest::last -> rest::everyOther last;;

(* Tuples can only hold a fix amount of stuff but of different types, list can only hold one type but unlimited amounts*)

let tup = (1, "a", []);;
let (one, a, empty_list) = tup;;


let increment_snd (t : (string * int)) : (string * int) =
    (fst t,  1 + (snd t));;

 (*returns the pair ("a", 6) *)
(increment_snd ("a", 5))

(*Sum of squares*)

let rec sum_squares (l: (int * int) list) : int = 
    match l with
        | [] -> 0
        | first::rest ->
                (fst first) * (fst first) + (snd first) * (snd first) + sum_squares rest;;
