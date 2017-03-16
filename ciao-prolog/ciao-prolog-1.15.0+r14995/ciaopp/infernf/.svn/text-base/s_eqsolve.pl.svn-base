/********************************************************************
*                                                                   *
*  File:    eqsolve.pl                                              *
*  Purpose: Solve a set of equations and disequations over the      *
*           Herbrand domain.                                        *
*  Author:  Saumya Debray                                           *
*  Date:    03 Aug 1995                                             *
*  Acknowledgements: The algorithm used is based on that described  *
*           in "Unification Revisited", by J.-L. Lassez, M. Maher   *
*           and K. Marriott, in "Foundations of Deductive Databases *
*           and Logic Programming, ed. J. Minker, Morgan Kaufman,   *
*           1988, but I've made no effort to be efficient.  In      *
*           particular, when checking whether a disequation is      *
*           consistent with a set of equations E, this code will    *
*           reprocess all of E afresh instead of simply looking at  *
*           its solution.  This could (and should) be improved.     *
*                                                                   *
********************************************************************/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                 %%
%%  DATA REPRESENTATION: A set of equations and disequations is    %%
%%  represented as a list: an equation between t1 and t2 is        %%
%%  represented as a term '='(t1,t2), while a disequation between  %%
%%  t1 and t2 is represented as a term '$noteq$'(t1,t2).           %%
%%                                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% solvable(+S) is true iff the system of equations and disequations S
% is solvable.
%
% From Theorem 10 of [Lassez, Maher & Marriott 1988] (see above for
% complete citation), a system S consisting of a set of equations E
% and a set of disequations {I_1,...,I_n} is solvable iff each
% {E \union I_j} is solvable.  So it suffices to verify that
%
% (i)  E is solvable; and
% (ii) for each j in [1,...,n], {E \union E_j} has no solution, where
%      the equation E_j is the complement of the disequation I_j.

/*
solvable(S) :-
	separate_eqs_and_diseqs(S, Eqs, DisEqs),
	solvable_eqs(Eqs),
	check_diseqs(DisEqs, Eqs).
*/

% solvable_eqs(+Eqs) is true iff the set of equations Eqs is solvable.

solvable_eqs([]).
solvable_eqs(['='(LHS, RHS)|Eqs0]) :-
	rewrite_eq(LHS, RHS, Eqs0, Eqs1),
	solvable_eqs(Eqs1).

rewrite_eq(X, Y, Eqs, Eqs) :-
	var(X),
	var(Y),
	!,
	X = Y.
rewrite_eq(X, Y, Eqs, Eqs) :-
	var(X), /* nonvar(Y) */
	!,
	\+ occurs_in(X, Y),
	X = Y.
rewrite_eq(X, Y, Eqs0, Eqs1) :-
	var(Y), /* nonvar(X) */
	!,
	Eqs1 = ['='(Y, X)|Eqs0].
rewrite_eq(X, Y, Eqs0, Eqs1) :-
	functor(X, F, N),
	functor(Y, F, N),
	unwrap_eqs(N, X, Y, Eqs0, Eqs1).

/*
% check_diseqs(+DisEqs, +Eqs) is true iff each disequation in the set
% DisEqs is consistent with the equation set Eqs.

check_diseqs([],                       _).
check_diseqs(['$noteq$'(X, Y)|DisEqs], Eqs) :-
	\+ solvable_eqs(['='(X, Y)|Eqs]),
	check_diseqs(DisEqs, Eqs).
*/

/********************************************************************
*                                                                   *
*                             Utilities                             *
*                                                                   *
********************************************************************/

separate_eqs_and_diseqs([],        [],       []).
separate_eqs_and_diseqs([Eq|Rest], [Eq|Eqs], DisEqs) :-
	functor(Eq, '=', 2),
	!,
	separate_eqs_and_diseqs(Rest, Eqs, DisEqs).
separate_eqs_and_diseqs([DisEq|Rest], Eqs, [DisEq|DisEqs]) :-
	functor(DisEq, '$noteq$', 2),
	separate_eqs_and_diseqs(Rest, Eqs, DisEqs).


occurs_in(X, Y) :- var(Y), !, X == Y.
occurs_in(X, Y) :- functor(Y, _, N), occ_in_args(N, X, Y).

occ_in_args(N, X, Y) :- N > 0, arg(N, Y, Yn), occurs_in(X, Yn).
occ_in_args(N, X, Y) :- N > 0, N1 is N-1, occ_in_args(N1, X, Y).

% unwrap_eqs/5 "unwraps" an equation f(t1,...,tn) = f(u1,...,un) 
% to yield n equations ti = ui.

unwrap_eqs(I, X, Y, Eqs0, Eqs1) :-
	I > 0,
	!,
	arg(I, X, Xi),
	arg(I, Y, Yi),
	I1 is I-1,
	unwrap_eqs(I1, X, Y, ['='(Xi, Yi)|Eqs0], Eqs1).
unwrap_eqs(0, _, _, Eqs, Eqs).
