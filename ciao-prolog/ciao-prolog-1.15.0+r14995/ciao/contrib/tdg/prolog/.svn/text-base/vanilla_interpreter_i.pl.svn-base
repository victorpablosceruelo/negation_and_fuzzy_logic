solve([]).
solve([A|As]) :-
	clause_(A,B,_),!,
	solve(B),
	solve(As).
solve([\+A|As]) :- !,
	\+ solve([A]),
	solve(As).
solve([A|As]) :-
	catch(call(A),_,fail),
	solve(As).
