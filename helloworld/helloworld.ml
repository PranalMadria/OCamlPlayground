let () = print_endline "Hello World!";;

let x = 50;;
let z = x * x;;



let square x = x * x ;;


let y = print_int (square 50);;

let square_is_even x = square x mod 2 = 0;;

let ordered a b c = a <= b && b <= c ;;

let average_flt a b = (a +. b ) /. 2.0;;
let w = ordered 1 1 2;;
 
print_endline " ";;
let we = print_float( average_flt 2.0 1.5);; 

let rec range a b = 
  if a > b then [] 
  else a + 5 :: range (a + 1) b;;



let rec factorial n = 
  match n with
  | 0 | 1 -> 1 
  | _ -> n * factorial (n-1);;
  print_endline " ";;
  let wz = print_int(factorial 5)

let rec factorialfn = function
  | 0 | 1 -> 1
  | n -> n * factorialfn (n-1);;


(**  Lists *)

(**  Add one element to front of list with :: *)

  1 :: [2; 3];;

(**  concat two lists with @ *)

[1] @ [2; 3];;

let rec total l = 
  match l with 
  | [] -> 0
  | h :: t -> h + total t;;

  let rec length l = 
    match l with
    | [] -> 0
    | _ :: t -> 1 + length t

