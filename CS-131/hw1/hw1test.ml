let my_subset_test0 = subset [] []
let my_subset_test1 = subset [] [1; 2; 3]
let my_subset_test2 = subset [2; 1] [1; 2; 3]
let my_subset_test3 = not (subset [1; 4] [1; 2; 3])

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1; 2] [1; 2]
let my_equal_sets_test2 = equal_sets [2; 1] [1; 2]
let my_equal_sets_test3 = not (equal_sets [1; 2; 3] [1; 2])

let my_set_union_test0 = equal_sets (set_union [] []) []
let my_set_union_test1 = equal_sets (set_union [1; 2] [1; 2]) [1; 2]
let my_set_union_test2 = equal_sets (set_union [1; 3] [1; 2]) [1; 2; 3]
let my_set_union_test3 = equal_sets (set_union [1; 2] [3; 4]) [1; 2; 3; 4]

let my_set_intersection_test0 = equal_sets (set_intersection [] [1; 2; 3]) []
let my_set_intersection_test2 = equal_sets (set_intersection [1; 2] [2; 3]) [2]
let my_set_intersection_test3 = equal_sets (set_intersection [1; 2] [3; 4]) []

let my_set_diff_test0 = equal_sets (set_diff [1; 2] []) [1; 2]
let my_set_diff_test1 = equal_sets (set_diff [1; 2] [2; 3]) [1]
let my_set_diff_test2 = equal_sets (set_diff [1; 2; 2; 3] [1; 2; 3; 4]) []

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 2) 10000 = 0
let my_computed_fixed_point_test1 = computed_fixed_point (=) (sqrt) 30. = 1.

let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x / 2) 0 (50) = 50
let my_computed_periodic_point_test1 = computed_periodic_point (=) (fun x -> x / 2) 10 (10) = 0

let my_while_away_test0 = equal_sets (while_away ((+) 4) ((>) 10) 0 ) [0; 4; 8]
let my_while_away_test1 = equal_sets (while_away ((+) 2) ((>) 9) 0 ) [0; 2; 4; 6; 8]

let my_rle_decode_test0 = rle_decode [1,"w"; 2,"o"; 0,"x"; 1,"d"] = ["w"; "o"; "o"; "d"]

type grammar_nonterminals = 
| S | A | B 

let grammar = 
	S,
	[S, [N A];
	S, [N B];
	A, [T "a"];
	A, [N B];
	B, [N B]]

let my_filter_blind_alleys_test0 = 
filter_blind_alleys grammar = 
	(S,
	[S, [N A];
	A, [T "a"]])


