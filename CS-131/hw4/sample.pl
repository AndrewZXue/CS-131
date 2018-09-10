kenken_testcase(
	6,
	[
	    +(11, [1-1, 2-1]),
	    /(2, 1-2, 1-3),
	    *(20, [1-4, 2-4]),
	    *(6, [1-5, 1-6, 2-6, 3-6]),
	    -(3, 2-2, 2-3),
	    /(3, 2-5, 3-5),
	    *(240, [3-1, 3-2, 4-1, 4-2]),
	    *(6, [3-3, 3-4]),
	    *(6, [4-3, 5-3]),
	    +(7, [4-4, 5-4, 5-5]),
	    *(30, [4-5, 4-6]),
	    *(6, [5-1, 5-2]),
	    +(9, [5-6, 6-6]),
	    +(8, [6-1, 6-2, 6-3]),
	    /(2, 6-4, 6-5)
	]
	    ).

kenken_testcase_trivial(
  2,
  [
    *(4, [1-1, 1-2, 2-1])
  ]
).

/*Statistics for mytest:
kenken: 0.1 seconds
plain_kenken: 2.340 seconds
*/

kenken_mytest(
	4,
	[
	    +(7, [1-1,1-2]),
	    -(3, 1-3,2-3),
	    *(6, [2-1,2-2,3-2]),
	    /(2, 3-1, 4-1),
	    *(12,[3-3,3-4]),
	    *(24,[4-2,4-3,4-4])

	]
    ).


%find_entry and check length to be N
find_entry(Row,Col,T,Entry):-
    nth(Row, T, List_Row), nth(Col, List_Row, Entry).

find_sum(+(Sum,[]), T, 0). 
find_sum(+(Sum,[Row-Col|Tail1]), T, MySum):-
    find_entry(Row, Col,T,E), find_sum( +(Sum,Tail1),T,TempSum), MySum #=TempSum+E. 


find_prd( *(Prd,[]),T,1).
find_prd( *(Prd,[Row-Col|Tail1]), T, MyPrd):-
    find_entry(Row,Col,T,E), find_prd( *(Prd,Tail1),T,TempPrd), MyPrd #= TempPrd*E.


find_sub( -(Sub,Row1-Col1 ,Row2-Col2), T, MySub):-
    find_entry(Row1,Col1,T,E1), find_entry(Row2,Col2,T,E2), (MySub #= E1-E2 ; MySub #=E2-E1).

find_div(/(Div,Row1-Col1, Row2-Col2), T, MyDiv):-
    find_entry(Row1,Col1,T,E1), find_entry(Row2,Col2,T,E2), (MyDiv #= E1/E2 ; MyDiv #= E2/E1). 
    
%CONSTRAINTS
constraint(+(Sum,Li) ,T):-
    find_sum( +(Sum,Li), T, Sum ).

constraint( *(Prd,Li), T):-
    find_prd( *(Prd,Li),T,Prd).

constraint( -(Sub,R1-C1,R2-C2), T):-
    find_sub( -(Sub,R1-C1,R2-C2),T,Sub).

constraint( /(Div,R1-C1,R2-C2), T):-
    find_div( /(Div,R1-C1,R2-C2), T, Div).

/*Code to transpose a matrix. Need it to check for valid columns, using the code for rows*/
trans([],[]).
trans([[]|_], []):-!.
trans([S|R], [L|L1]) :-
    trans(S, R, L, M),
    trans(M, L1).
trans([], _,[],[]).
trans([S1|S2], [], [S1|L1], [S2|M]):-
    trans([], [], L1, M).
trans([S1|S2], [R1|R2], [S1|L1], [S2|M]):-
    trans(R1, R2, L1, M).



good_domain(N,L):-
    fd_domain(L,1,N). %Each value of L belongs in the interval [1,N].

%A good row satisfies length=N, it has valid values and all are different.
good_row(N,L):-
    length(L,N), good_domain(N,L), fd_all_different(L).

check_rows(N,[]).
check_rows(N,[Head|Tail]):-
    check_rows(N,Tail) ,good_row(N,Head).

good_matrix(N,T):-
    length(T,N), check_rows(N,T), trans(T, Trans), check_rows(N,Trans).

meet_constraints([],T).
meet_constraints([Head|Tail],T):- meet_constraints(Tail,T),  constraint(Head,T).


label([]).
label([Head|Tail]):-fd_labeling(Head), label(Tail).

%kenken(N, C, T):- good_matrix(N,T), meet_constraints(C,T),maplist(fd_labeling, T).

kenken(N, C, T):- good_matrix(N,T), meet_constraints(C,T), label(T).

/*
for the pain_kenken just rewrite the methods that use fd domain variables 
*/


%Acceptable lists are permutation of this one
plain_good_domain(N,L):-findall(Num, between(1, N, Num), L).

plain_good_row(N,L):-plain_good_domain(N, Perm), permutation(Perm,L).

plain_check_rows(N,[]).
plain_check_rows(N,[Head|Tail]):-
       plain_check_rows(N,Tail) ,plain_good_row(N,Head).


plain_good_matrix(N,T):-
        length(T,N), plain_check_rows(N,T), trans(T, Trans), plain_check_rows(N,Trans).

plain_meet_constraints([],T).
plain_meet_constraints([Head|Tail],T):- plain_meet_constraints(Tail,T),  constraint(Head,T).

plain_kenken(N, C, T):- plain_good_matrix(N,T), plain_meet_constraints(C,T).