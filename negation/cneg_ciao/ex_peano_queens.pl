:- module(ex_peano_queens,_,[.(cneg)]).
% :- module(ex_peano_queens,_,[.(cneg), .(debugger_pkg)]).
% :- module(queensPeano, [queens/2], [.(cneg)]).

cneg_ignores_preds([tests/2, test_queens_1/2, test_queens_2/2, echo_fail/0, echo_test/2, echo_nl/0]).
cneg_choosen_negation(cneg_rt_Chan).

echo_nl :- cneg_diseq_echo(1, 'nl', 'ex_peano_queens', '', '').
echo_fail :-
		cneg_diseq_echo(1, 'aux', 'ex_peano_queens', ' -- FAILED', ''),
		echo_nl,
		!,
		fail.

echo_test(Msg1, Msg2) :-
	cneg_diseq_echo(1, 'nl', 'ex_peano_queens', '', ''),
	cneg_diseq_echo(1, 'aux', 'ex_peano_queens', Msg1, Msg2).


tests(N, C) :- 
	queens_list(N, _Unused_List), % This avoids an infinite number of sols problem.
	echo_nl,
	echo_test('tests(N, C) :: value for N :: ', N),
	(
	    test_queens_1(N, C)
	;
	    test_queens_2(N, C)
	).

test_queens_1(N, Columns) :- % Let's see if we have invalid results.
	cneg([], queens(N, Columns)), % First constraints.
 	echo_test('test_queens_1 :: queens(N, Columns) :: ', queens(N, Columns)),
	(
	    queens(N, Columns) % Secondly values generator.
	;
	    echo_fail
	).

test_queens_2(N, Columns) :- % Let's see if we have invalid results.
	queens(N, Columns), % First values generator.
 	echo_test('test_queens_2 :: queens(N, Columns) :: ', queens(N, Columns)),
	(
	    cneg([], queens(N, Columns)) % Second constraints.
	;
	    echo_fail
	).

% queens(N,Columns) returns in Columns the column where we must place each of N
% queens in a Checkerboard of NxN assuming each position in the list 
% is a different row. For example :   
%        queens(s(s(s(s(0)))),[s(s(0)),s(s(s(s(0)))),s(0),s(s(s(0)))])
% means that the 4 queens are placed in positions (1,2),(2,4),(3,1) and (4,3). 

queens(N, Columns):- 
        queens_list(N, Ns),
        queens1(Ns, [], Columns).     % To be placed, placed, result

% queens_list(N, List) generates a list of N queens in List.
% The order does not matter yet.
queens_list(0, []).
queens_list(s(N), [s(N)|List]) :-
        queens_list(N, List).

% queens1(Ns,[],Columns) returns in Columns a permutation of the columns represented
% in Ns such as there will be secure position for placing queens in them.
% queens1(Unplaced, Placed, Columns) appends to Placed columns the columns of 
% Unplaced in a secure way for all the queens.
queens1([], Columns, Columns). 
queens1([X|Unplaced], Placed, Columns):-
        select_column(Column, [X|Unplaced], NewUnplaced),
        no_attack(Placed, Column, Column),
        queens1(NewUnplaced, [Column|Placed], Columns).
 
% select(X, Ys, Zs) X is an element of Ys and Zs is Ys except X
select_column(X, [X|Ys], Ys).
select_column(X, [Y|Ys], [Y|Zs]):-
        select_column(X, Ys, Zs).

% no_attack(Placed, Q, Q) checks that 
% a queen in the next row to the ones in Placed 
% doesn't attack any queen there.
no_attack([], _Col_Add, _Col_Subst).
no_attack([Y|Ys], Col_Add, 0):-
	add(Col_Add, s(0), New_Col_Add),
        disequality(New_Col_Add, Y, []),
        no_attack(Ys, New_Col_Add, 0).
no_attack([Y|Ys], Col_Add, Col_Subst):-
	add(Col_Add, s(0), New_Col_Add),
        disequality(New_Col_Add, Y, []),
	subst(Col_Subst, s(0), New_Col_Subst),
	disequality(New_Col_Subst, Y, []),
        no_attack(Ys, New_Col_Add, New_Col_Subst).

add(0,X,X). 
add(s(X),Y,s(Z)) :-
	add(X,Y,Z).

subst(X,0,X).
subst(s(X),s(Y),Z) :-
	subst(X,Y,Z).





