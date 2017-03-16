:- module(testbuiltin, [p/1, testbuiltin/2], [
		assertions,
		ciaopp(tests(resources)),
		predefres(res_all)]).

:- entry p/1 : var.

p(a).
p(b).
p(c).

:- entry testbuiltin/2 : list(gnd) * var.

testbuiltin([],     []).
testbuiltin([X|Xs], [Y|Ys]) :-
	(
	    X == a,
	    X \== b,
	    X is 2 +1, X =:= 1 /2, X =\= 1 /2, X < 1 +2, X > 1 * 2, X =< 1 /3,
	    X >= 3.0, atom(X), integer(X), number(X), atm(X), int(X), num(X),
	    gnd(X), functor(a(b, c), Name, Arity), X = Name / Arity
%	,   findall(X0, p(X0), X), p(X), arithexpression(X)
	),
	Y = X,
% 	ttynl,
% 	ttyput(c),
% 	writeq(b),
% 	display(a),
% 	print(b),
% 	true,
% 	\+ fail,
% 	\+ false,
% %	check(true),
% 	indep(a),
% 	indep(a,b),
% 	nl,
	testbuiltin(Xs, Ys).
