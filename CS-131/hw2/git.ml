type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

	let rec append_array rhs = function
		| [] -> rhs
		| h::t -> h::(append_array rhs t)

	let rec match_nonterminal prod_func nt = function
		
		| [] -> (fun accept derivation frag -> None)
		| rules_head::rules_tail ->
		(fun accept derivation frag ->
			let ormatch = (match_terminal prod_func rules_head) accept (append_array [(nt, rules_head)] derivation) frag
			in match ormatch with
				| None -> (match_nonterminal prod_func nt rules_tail) accept derivation frag
				| _ -> ormatch)


	and match_terminal prod_func = function
		
		| [] -> (fun accept derivation frag -> accept derivation frag)
		| (T t)::rules_tail ->
			(fun accept derivation frag -> match frag with
				| [] -> None
				| h::tail -> if h = t then
						(match_terminal prod_func rules_tail) accept derivation tail
					else
						None
			)
		| (N nt)::rules_tail ->
			(fun accept derivation frag ->
				let new_acceptor = match_terminal prod_func rules_tail accept
				in (match_nonterminal prod_func nt (prod_func nt)) new_acceptor derivation frag)
	
	let parse_prefix gram =
	let start_sym = fst gram
	and prod_func = snd gram
	in
	fun accept frag -> match_nonterminal prod_func start_sym (prod_func start_sym) accept [] frag

















	