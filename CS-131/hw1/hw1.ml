let rec ifcontain x lis = match lis with
| [] -> false
| h::t -> if x == h
			then true
			else ifcontain x t


let rec subset a b = match a with
| [] -> true
| x::xs -> if not (ifcontain x b) 
			then false
			else subset xs b


let equal_sets a b = subset a b && subset b a


let rec set_union a b = match a with
| [] -> b
| x::xs -> if not (ifcontain x b)
			then x::set_union xs b
		else set_union xs b


let rec set_intersection a b = match a with
| [] -> a
| x::xs -> if (ifcontain x b)
			then x::set_intersection xs b
		else set_intersection xs b


let rec set_diff a b = match a with
| [] -> a
| x::xs -> if not (ifcontain x b)
			then x::set_diff xs b
		else set_diff xs b


let rec computed_fixed_point eq f x =
	let v = f x in if (eq v x) then x
else computed_fixed_point eq f (f x)


let rec compute_p f p x = match p with
| 0 -> x
| _ -> f (compute_p f (p-1) x)


let rec computed_periodic_point eq f p x =
	let v = compute_p f p x in
	if (eq x v) then x
else computed_periodic_point eq f p (f x)


let rec while_away s p x =
	if not (p x) then []
else x::while_away s p (s x)


let rec decoding (n, v) = match n with
| 0 -> []
| _ -> v::(decoding (n - 1, v))


let rec append l1 l2 = match l1 with
| [] -> l2
| x::xs -> x :: append xs l2


let rec rle_decode lp = match lp with
| [] -> []
| x::xs -> append (decoding x) (rle_decode xs)


type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal

(* Check if each element of the list is terminal *)
let rec word w non_list = match w with
| T _ -> true
| N t -> if (ifcontain t non_list) then true
			else false

(* Check if the rule has a terminal list *)
let rec ifterminal rule non_list = match rule with
| [] -> true
| x::xs -> if not (word x non_list) then false
		else ifterminal xs non_list

let rec create lis non_list = match lis with
| [] -> non_list
| x::xs -> if ifterminal (snd x) non_list then 
			if not (ifcontain (fst x) non_list) then create xs ((fst x)::non_list) else create xs non_list
				else create xs non_list

let rec filter lis non_list = match lis with
| [] -> []
| x::xs -> if (ifterminal (snd x) non_list) then x::(filter xs non_list)
			else filter xs non_list

let rec fixed_point eq f x y =
	let v = f x y in if (eq v y) then y
else fixed_point eq f x (f x y)

let rec filter_blind_alleys g =
	(fst g), filter (snd g) (fixed_point equal_sets create (snd g) [])













