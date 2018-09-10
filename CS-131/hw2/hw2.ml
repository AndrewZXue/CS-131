(*Zhouyang Xue*)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* warm-up part, converting hw1 formatted grammar to hw2 formatted grammar *)

let rec each_non_terminal non_terminal grammar_list = match grammar_list with
| []->[]
| x::xs-> if (fst x) == non_terminal then
			(snd x)::(each_non_terminal non_terminal xs)
		else
			each_non_terminal non_terminal xs

let convert_grammar gram1 = 
	(fst gram1, function nt -> each_non_terminal nt (snd gram1))

(* actual parser *)
(* Note that the parser returns a function, which takes in acceptor and fragment as parameters. *)
let rec append list1 list2 = match list1 with
| [] -> list2
| x::xs -> x::(append xs list2)

(* start with the starting nonterminal, if the first expr is terminal, call terminal_matcher *)
(* otherwise, recursively look for the next possible matching pattern *)
(* if nothing matches, return a function that always returns None *)
let rec nonterminal_matcher rule_fun non_terminal rules = match rules with
| [] -> (fun accept derivation frag -> None) 
| x::xs -> (fun accept derivation frag ->
let parsed = terminal_matcher rule_fun x 
	in let result = parsed accept (append derivation [non_terminal, x]) frag
	in match result with
		| None -> (nonterminal_matcher rule_fun non_terminal xs) accept derivation frag
		| _ -> result
)

(* if it's a nonterminal, call nonterminal_matcher back *)
(* if it's a termina, do the matching and pass the result to acceptor to check *)
and terminal_matcher rule_fun rule = match rule with
| [] -> (fun accept derivation frag -> accept derivation frag)
| x::xs -> match x with
	| (N nt) -> (fun accept derivation frag ->
		let accept_moveon = terminal_matcher rule_fun xs accept in
		(nonterminal_matcher rule_fun nt (rule_fun nt)) accept_moveon derivation frag
		)
	| (T t) -> (fun accept derivation frag -> match frag with
		| [] -> None
		| h::tail -> if h = t then
						(terminal_matcher rule_fun xs) accept derivation tail
					else
						None
		)

let parse_prefix gram = 
	let start = fst gram
	and func = snd gram
	in
	fun accept frag -> nonterminal_matcher func start (func start) accept [] frag


















