:- use_package([assertions]).

:- entry append/3.

unify(X,X).
%unify(X,Y):- X=Y.

append(A_0_0, A_0_1, A_0_2) :-
	unify(A_0_0, []),
	unify(A_0_1, A_0_2).
append(A_0_0, A_0_1, A_0_2) :-
	unify(A_0_0, [T_0|A_1_0]),
	unify(A_0_1, A_1_1),
	unify(A_0_2, [T_0|A_1_2]),
	user:append(A_1_0, A_1_1, A_1_2).

