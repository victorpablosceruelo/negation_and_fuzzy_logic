:- module(ex_peano_queens,_,[cneg]).
% :- module(ex_peano_queens,_,[.(cneg), .(debugger_pkg)]).
% :- module(queensPeano, [queens/2], [.(cneg)]).

cneg_ignores_preds([tests/2, test_queens_1/2, test_queens_2/2, echo_fail/0, echo_test/2, echo_nl/0]).

tests :- 
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, '', 'Tests that should succeed', ''),
	tests_succeed(Logo, Vars, First_Part, Second_Part), 
	test_execution(Logo, Vars, First_Part, Second_Part, 'should_succeed'),
	fail.

tests :- 
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, '', 'Tests that should fail', ''), 
	tests_fail(Logo, Vars, First_Part, Second_Part), 
	test_execution(Logo, Vars, First_Part, Second_Part, 'should_fail'),
	fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tests_succeed('s01', [C], (queens(0, C)), true).
tests_fail('0 queens +-', [C], (queens(0, C)), (cneg(queens(0, C)))).
tests_fail('0 queens -+', [C], (cneg(queens(0, C))), (queens(0, C))).
tests_fail('1 queen +-', [C], (queens(s(0), C)), (cneg(queens(s(0), C)))).
tests_fail('1 queen -+', [C], (cneg(queens(s(0), C))), (queens(s(0), C))).
tests_fail('2 queens +-', [C], (queens(s(s(0)), C)), (cneg(queens(s(s(0)), C)))).
tests_fail('2 queens -+', [C], (cneg(queens(s(s(0)), C))), (queens(s(s(0)), C))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

complex_tests(N, Columns) :- 
	queens_list(N, List), % This avoids an infinite number of sols problem.
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, 'aux', 'tests(N, List) :: (N, List) :: ', (N, List)),
	print_msg(1, 3, 'nl', '', ''),
	(
	    (
		print_msg(1, 3, '', ' -- STARTED (-) -- ', cneg(queens(N, Columns))), 
		cneg(queens(N, Columns)), % First constraints.
		print_msg(1, 3, 'aux', ' -- READY (-) -- ', cneg(queens(N, Columns))),
		print_msg(1, 3, 'aux', '', ' '),
		print_vars_diseqs(1, '', (N, Columns)),
		print_msg(1, 3, 'nl', '', ''),
		complex_tests_aux_for_negative(N, Columns)
	    )
	;
	    (
		print_msg(1, 3, '', ' -- STARTED (+) -- ', cneg(queens(N, Columns))), 
		queens(N, Columns), % First values generator.
		print_msg(1, 3, '', ' -- READY (+) -- ', cneg(queens(N, Columns))),
		complex_tests_aux_for_positive(N, Columns)
	    )
	).

complex_tests_aux_for_negative(N, Columns) :-
	(
	    (
		queens(N, Columns), % Secondly values generator.
		print_msg(1, 3, 'aux', ' -- FAILED (-) -- ', queens(N, Columns)),
		print_msg(1, 3, 'aux', '', ' '),
		print_vars_diseqs(1, '', (N, Columns)),
		print_msg(1, 3, 'nl', '', ''),
		!, fail
	    )
	;
	    (
		print_msg(1, 3, '', ' -- PASSED (-) -- ', queens(N, Columns)),
		!, fail
	    )
	).

complex_tests_aux_for_positive(N, Columns) :-
	(
	    (
		cneg(queens(N, Columns)), % Second constraints.
		print_msg(1, 3, 'aux', ' -- FAILED (+) -- ', queens(N, Columns)), 
		print_msg(1, 3, 'aux', '', ' '),
		print_vars_diseqs(1, '', (N, Columns)),
		print_msg(1, 3, 'nl', '', ''),
		!, fail
	    )
	;
	    (
		print_msg(1, 3, '', ' -- PASSED (+) -- ', queens(N, Columns)),
		!, fail
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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





