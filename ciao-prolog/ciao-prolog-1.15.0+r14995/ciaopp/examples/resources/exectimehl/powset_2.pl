:- module(powset_2, [powset/2], [assertions, regtypes,
		nativeprops, ciaopp(examples(resources(exectimehl)))]).

:- entry powset(A, B) : list(int) * var.

powset(A, B) :-
	powset2(A, [[]], B).

:- doc(bug, "Currently the analyzer is unable to see that the 3th
	argument of append_elem/4 can be safely ignored. It has been
	marked as void but it is not being ignored when creating the
	adg. -- EMM").

% This is the optimized version of powerset:
:- trust comp powset2(A, B, C) + size_metric(B, void).
powset2([],    X,  X).
powset2([X|L], P0, P) :-
	append_elem(P0, X, P1, P0),
	powset2(L, P1, P).

:- trust comp append_elem(A, B, C, D) + size_metric(C, void).
append_elem([],     _X, T,          T).
append_elem([L|Ls], X,  [[X|L]|Rs], T) :-
	append_elem(Ls, X, Rs, T).
