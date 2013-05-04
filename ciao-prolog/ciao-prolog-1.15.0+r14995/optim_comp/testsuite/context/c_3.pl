% A test for context variables
%
% Author: Jose F. Morales

:- module(_, [main/0], [compiler(complang)]).

:- '$begin_context'(idx).
:- '$incctx'(indexed(single(op))).
idx(X,X1) :- op(inc), !, X1 is X + 1.
idx(X,X1) :- op(dec), !, X1 is X - 1.
:- '$end'.

:- '$begin_context'(idx2).
:- '$incctx'(indexed(single(op))).
:- '$incctx'(pair(x)).
idx2 :- op(inc), !, X1 is ~x + 1, x <- X1.
idx2 :- op(dec), !, X1 is ~x - 1, x <- X1.
:- '$end'.

:- '$begin_context'(idx3).
:- '$incctx'(single(op)).
:- '$incctx'(pair(x)).
idx3 :- op(inc), !, X1 is ~x + 1, x <- X1.
idx3 :- op(dec), !, X1 is ~x - 1, x <- X1.
:- '$end'.

main :-
	push(op, inc) '$ctx_on' idx(0,A), chk(A,1),
	(push(op, dec), push_pair(x, 0)) '$ctx_on' (idx2, ~x = B), chk(B,-1),
	(push(op, dec), push_pair(x, 0)) '$ctx_on' (idx3, ~x = B), chk(B,-1).

chk(X, Expected) :- X == Expected, !.
chk(X, Expected) :-
	display(user_error, 'X = '),
	display(user_error, X),
	display(user_error, ' Expected = '),
	display(user_error, Expected),
	nl(user_error),
	fail.

	