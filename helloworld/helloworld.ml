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

let digits = print_int (range 0 9);;



    

