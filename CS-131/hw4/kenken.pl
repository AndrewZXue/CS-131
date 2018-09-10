kenken_testcase(
  6,
  [
   +(11, [[1|1], [2|1]]),
   /(2, [1|2], [1|3]),
   *(20, [[1|4], [2|4]]),
   *(6, [[1|5], [1|6], [2|6], [3|6]]),
   -(3, [2|2], [2|3]),
   /(3, [2|5], [3|5]),
   *(240, [[3|1], [3|2], [4|1], [4|2]]),
   *(6, [[3|3], [3|4]]),
   *(6, [[4|3], [5|3]]),
   +(7, [[4|4], [5|4], [5|5]]),
   *(30, [[4|5], [4|6]]),
   *(6, [[5|1], [5|2]]),
   +(9, [[5|6], [6|6]]),
   +(8, [[6|1], [6|2], [6|3]]),
   /(2, [6|4], [6|5])
  ]
).

kenken_testcase_trivial_2(
  2,
  [
    /(2, [1|1], [1|2]),
    /(2, [2|2], [2|1])
  ]
).

kenken_testcase_trivial(
  2,
  [
    *(4, [[2|2], [1|2], [2|1]])
  ]
).

transpose([], []).
transpose([H|T], Ret) :-
    transpose(H, [H|T], Ret).

transpose([], _, []).
transpose([_|Rows], Col, [T|Ts]) :-
    first_elements(Col, T, Cols),
    transpose(Rows, Cols, Ts).

first_elements([], [], []).
first_elements([[H|T]|Rows], [H|H_rec], [T|T_rec]) :-
    first_elements(Rows, H_rec, T_rec).

/* given number of row and column, retrieve the entry */
locate(T, Row, Column, Entry):-
	nth(Row, T, Rows),
	nth(Column, Rows, Entry).

/* constraints */
constraint(Cons,M):-
  	Cons = +(Tar, L),
  	sum(Tar, L, M).

constraint(Cons,M):-
  	Cons = -(Tar, Op1, Op2),
  	diff(Tar, Op1, Op2, M).

constraint(Cons,M):-
   	Cons = *(Tar, L),
   	product(Tar, L, M).

constraint(Cons,M):-
   	Cons = /(Tar, Op1, Op2),
   	division(Tar, Op1, Op2, M).

/* matching constraints */
sum(0, [], M).
sum(Tar, [[R|C]|Tail], M):-
	locate(M, R, C, Entry),
	sum(Temp, Tail, M),
	Tar #= Temp + Entry.

diff(Tar, [R1|C1], [R2|C2], M):-
	locate(M, R1, C1, Entry1),
	locate(M, R2, C2, Entry2),
	(Tar #= Entry1 - Entry2 ; Tar #= Entry2 - Entry1).

product(1, [], M).
product(Tar, [[R|C]|Tail], M):-
	locate(M, R, C, Entry),
	product(Temp, Tail, M),
	Tar #= Temp * Entry.

division(Tar, [R1|C1], [R2|C2], M):-
	locate(M, R1, C1, Entry1),
	locate(M, R2, C2, Entry2),
	(Tar #= Entry1 / Entry2 ; Tar #= Entry2 / Entry1).

label([]).
label([X|Xs]) :-
	fd_labeling(X),
	label(Xs).

/* basic kenken rules */
valid_row(N,Row):-
    length(Row,N), 
    fd_domain(Row, 1, N), 
    fd_all_different(Row).

check_valid_row(N, []).
check_valid_row(N, [H|T]):-
	check_valid_row(N, T),
	valid_row(N, H).

valid_matrix(N, T):-
	length(T, N),
	check_valid_row(N, T),
	transpose(T, T_tran),
	check_valid_row(N, T_tran).

solution([], T).
solution([H|Tail], T):-
	solution(Tail, T),
	constraint(H, T).
	

/*
N, a nonnegative integer specifying the number of cells on each side of the KenKen square.
C, a list of numeric cage constraints as described below.
T, a list of list of integers. All the lists have length N. This represents the N×N grid.
*/

kenken(N, C, T) :- 
	valid_matrix(N, T),
	solution(C, T),
	label(T).




/* plain kenken starts here */
plain_constraint(Cons,M):-
  	Cons = +(Tar, L),
  	psum(Tar, L, M).

plain_constraint(Cons,M):-
  	Cons = -(Tar, Op1, Op2),
  	pdiff(Tar, Op1, Op2, M).

plain_constraint(Cons,M):-
   	Cons = *(Tar, L),
   	pproduct(Tar, L, M).

plain_constraint(Cons,M):-
   	Cons = /(Tar, Op1, Op2),
   	pdivision(Tar, Op1, Op2, M).

/* matching constraints */
psum(0, [], M).
psum(Tar, [[R|C]|Tail], M):-
	locate(M, R, C, Entry),
	/*Tar #= Temp + Entry.*/
	Temp is (Tar - Entry),
	psum(Temp, Tail, M).

pdiff(Tar, [R1|C1], [R2|C2], M):-
	locate(M, R1, C1, Entry1),
	locate(M, R2, C2, Entry2),
	(Tar is Entry1 - Entry2 ; Tar is Entry2 - Entry1).

pproduct(1, [], M).
pproduct(Tar, [[R|C]|Tail], M):-
	locate(M, R, C, Entry),
	pproduct(Temp, Tail, M),
	Tar is (Temp * Entry).
	/*Tar #= Temp * Entry.*/

pdivision(Tar, [R1|C1], [R2|C2], M):-
	locate(M, R1, C1, Entry1),
	locate(M, R2, C2, Entry2),
	(Entry1 is Tar * Entry2 ; Entry2 is Tar * Entry1).

plain_range(N, Range) :-
  findall(V, between(1, N, V), Range).

plain_valid_row(N,Row):-
    plain_range(N, Range), 
    permutation(Range, Row).

plain_check_valid_row(N, []).
plain_check_valid_row(N, [H|T]):-
	plain_check_valid_row(N, T),
	plain_valid_row(N, H).

plain_valid_matrix(N, T):-
	length(T, N),
	plain_check_valid_row(N, T),
	transpose(T, T_tran),
	plain_check_valid_row(N, T_tran).

plain_solution([], T).
plain_solution([H|Tail], T):-
	plain_solution(Tail, T),
	plain_constraint(H, T).

plain_kenken(N, C, T) :- 
	plain_valid_matrix(N, T),
	plain_solution(C, T),
	label(T).

/*performance recorded in report.txt*/
/*third step solved in report.txt*/

