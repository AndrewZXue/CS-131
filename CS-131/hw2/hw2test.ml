type numerical_operations =
  | EXPR | TERM | OP | NUM

let accept_all derivation string = Some (derivation, string)
let accept_wierd = fun d -> function 
| "+"::xs -> Some (d,"+"::xs) 
| _ -> None
let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None


let naive_grammar =
  (EXPR,
   function
     | EXPR ->
         [[N TERM; N OP; N EXPR];
          [N TERM]]
     | TERM ->
       [[N NUM];
        [T"("; N EXPR; T")"]]
     | OP ->
       [[T"+"];
        [T"-"];
        [T"*"];
        [T"/"]]
     | NUM ->
       [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
        [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])
   
let test_accept_all = ((parse_prefix naive_grammar accept_all ["1";"-";"3";"*";"5"]) = Some
  ([(EXPR, [N TERM; N OP; N EXPR]); (TERM, [N NUM]); (NUM, [T "1"]);
   (OP, [T "-"]); (EXPR, [N TERM; N OP; N EXPR]); (TERM, [N NUM]);
   (NUM, [T "3"]); (OP, [T "*"]); (EXPR, [N TERM]); (TERM, [N NUM]);
   (NUM, [T "5"])],
  []))
  
let test_accept_wierd = ((parse_prefix naive_grammar accept_wierd ["1";"+";"3";"-";"5"]) = Some
 ([(EXPR, [N TERM]); (TERM, [N NUM]); (NUM, [T "1"])], ["+";"3";"-";"5"]))

let test_accept_empty = ((parse_prefix naive_grammar accept_empty_suffix ["1";"+";"$";"-"]) = None)