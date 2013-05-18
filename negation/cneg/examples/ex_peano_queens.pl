:- module(ex_peano_queens,_,[cneg]).
% :- module(ex_peano_queens,_,[.(cneg), .(debugger_pkg)]).
% :- module(queensPeano, [queens/2], [.(cneg)]).

cneg_ignores_preds(
	[
	    tests/0, complex_tests/2, 
	    complex_tests_aux_for_negative/2,
	    complex_tests_aux_for_positive/2
	]).

tests :- 
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, 'nl', '', ''),
	test(Logo, Part_1, Part_1_Should_What, Part_2, Part_2_Should_What), 
	test_execution(Logo, Part_1, Part_1_Should_What, Part_2, Part_2_Should_What, Result),
	(
	    (
		Result = true,
		fail % Go for more tests
	    )
	;
	    (
		Result = fail,
		!, fail % Stop testing.
	    )
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test('0 queens', (queens(0, C)), 'should_succeed', fail_and_forget_it(C), 'should_fail').
test('1 queens', (queens(s(0), C)), 'should_succeed', fail_and_forget_it(C), 'should_fail').
test('2 queens', (queens(s(s(0)), C)), 'should_fail', fail_and_forget_it(C), 'should_fail').
test('3 queens', (queens(s(s(s(0))), C)), 'should_fail', fail_and_forget_it(C), 'should_fail').

test('0 queens +-', (queens(0, C)), 'should_succeed', (cneg(queens(0, C))), 'should_fail').
test('0 queens -+ (this one depends on using types for queens_list or not)', 
	(cneg(queens(0, C))), 'should_succeed', (queens(0, C)), 'should_fail').
test('1 queen +-', (queens(s(0), C)), 'should_succeed', (cneg(queens(s(0), C))), 'should_fail').
test('1 queen -+ (this one depends on using types for queens_list or not)', 
	(cneg(queens(s(0), C))), 'should_succeed', (queens(s(0), C)), 'should_fail').
test('2 queens +-', (queens(s(s(0)), C)), 'should_fail', fail_and_forget_it(C), 'should_fail').
test('2 queens -+', (cneg(queens(s(s(0)), C))), 'should_succeed', (queens(s(s(0)), C)), 'should_fail').
test('3 queens +-', (queens(s(s(s(0))), C)), 'should_succeed', (cneg(queens(s(s(s(0))), C))), 'should_fail').
test('3 queens -+', (cneg(queens(s(s(s(0))), C))), 'should_succeed', (queens(s(s(s(0))), C)), 'should_fail').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

complex_tests(N, Columns) :- 
	queens1_list(N, List), % This avoids an infinite number of sols problem.
	print_msg(1, 3, 'nl', '', ''),
	print_msg(1, 3, 'aux', 'tests(N, List) :: (N, List) :: ', (N, List)),
	print_msg(1, 3, 'nl', '', ''),
	(
	    (
		print_msg(1, 3, '', ' -- STARTED (-) -- ', cneg(queens1(N, Columns))), 
		cneg(queens1(N, Columns)), % First constraints.
		print_msg_with_diseqs(1, 3, ' -- READY (-) -- ', cneg(queens1(N, Columns))),
		complex_tests_aux_for_negative(N, Columns)
	    )
	;
	    (
		print_msg(1, 3, '', ' -- STARTED (+) -- ', cneg(queens1(N, Columns))), 
		queens1(N, Columns), % First values generator.
		print_msg(1, 3, '', ' -- READY (+) -- ', cneg(queens1(N, Columns))),
		complex_tests_aux_for_positive(N, Columns)
	    )
	).

complex_tests_aux_for_negative(N, Columns) :-
	(
	    (
		queens1(N, Columns), % Secondly values generator.
		print_msg_with_diseqs(1, 3, ' -- FAILED (-) -- ', queens1(N, Columns))
		% !, fail
	    )
	;
	    (
		print_msg(1, 3, '', ' -- PASSED (-) -- ', queens1(N, Columns)),
		!, fail
	    )
	).

complex_tests_aux_for_positive(N, Columns) :-
	(
	    (
		cneg(queens1(N, Columns)), % Second constraints.
		print_msg_with_diseqs(1, 3, ' -- FAILED (+) -- ', queens1(N, Columns))
		% !, fail
	    )
	;
	    (
		print_msg(1, 3, '', ' -- PASSED (+) -- ', queens1(N, Columns)),
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

queens1(N, Columns):- 
        queens1_list(N, Ns),
        queens1_aux(Ns, [], Columns).     % To be placed, placed, result

% queens_list(N, List) generates a list of N queens in List.
% The order does not matter yet.
queens1_list(0, []).
queens1_list(s(N), [s(N)|List]) :-
        queens1_list(N, List).

% queens1(Ns,[],Columns) returns in Columns a permutation of the columns represented
% in Ns such as there will be secure position for placing queens in them.
% queens1(Unplaced, Placed, Columns) appends to Placed columns the columns of 
% Unplaced in a secure way for all the queens.
queens1_aux([], Columns, Columns). 
queens1_aux([X|Unplaced], Placed, Columns):-
        queens1_reorder_queens(Column, [X|Unplaced], NewUnplaced),
        queens1_no_attack(Placed, Column, Column),
        queens1_aux(NewUnplaced, [Column|Placed], Columns).
 
% select(X, Ys, Zs) X is an element of Ys and Zs is Ys except X
queens1_reorder_queens(X, [X|Ys], Ys).
queens1_reorder_queens(X, [Y|Ys], [Y|Zs]):-
        queens1_reorder_queens(X, Ys, Zs).

% no_attack(Placed, Q, Q) checks that 
% a queen in the next row to the ones in Placed 
% doesn't attack any queen there.
queens1_no_attack([], _Col_Add, _Col_Subst).
queens1_no_attack([Y|Ys], Col_Add, 0):-
	add(Col_Add, s(0), New_Col_Add),
        disequality(New_Col_Add, Y, []),
        queens1_no_attack(Ys, New_Col_Add, 0).
queens1_no_attack([Y|Ys], Col_Add, Col_Subst):-
	add(Col_Add, s(0), New_Col_Add),
        disequality(New_Col_Add, Y, []),
	subst(Col_Subst, s(0), New_Col_Subst),
	disequality(New_Col_Subst, Y, []),
        queens1_no_attack(Ys, New_Col_Add, New_Col_Subst).

add(0,X,X). 
add(s(X),Y,s(Z)) :-
	add(X,Y,Z).

subst(X,0,X).
subst(s(X),s(Y),Z) :-
	subst(X,Y,Z).

