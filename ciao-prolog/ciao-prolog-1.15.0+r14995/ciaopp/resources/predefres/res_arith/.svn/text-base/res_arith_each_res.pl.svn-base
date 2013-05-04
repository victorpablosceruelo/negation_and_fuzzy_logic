:- module(res_arith_each_res, _, [assertions]).

:- use_module(resources(resources_basic)).

:- discontiguous resource_arith/2.

:- include(res_arith(res_arith_res_auto)).

arith_operator(F, LitInfo, Cost) :-
	litinfo_get_lit(LitInfo, Lit),
	valid_arith_builtin(Lit),
	!,
	count_arith_operator(Lit, F, 0, Cost).
arith_operator(_, _, 0).

:- test count_arith_operator(A, B, C0, C) :
	(
	    A = (X is (2 +3 /4) +1),
	    B = '+'/2,
	    C0 = 0
	) => (C = 1) # "Counts the number of +/2 operators.".

:- test count_arith_operator(A, B, C0, C) :
	(
	    A = (X is (2 +3 /4 +1) +1 -1),
	    B = '++'/1,
	    C0 = 0
	) => (C = 2) # "Counts the number of ++/1 operators.".

count_arith_operator(Var, _, Count, Count) :-
	var(Var),
	!.
count_arith_operator(Num, _, Count, Count) :-
	number(Num),
	!.
count_arith_operator(A+B, '+'/2, Count0, Count) :-
	ground(B),
	B =:= 1,
	!,
	count_arith_operator(A, '+'/2, Count0, Count).
count_arith_operator(A+B, '++'/1, Count0, Count) :-
	ground(B),
	B =:= 1,
	!,
	Count1 is Count0 + 1,
	count_arith_operator(A, '++'/1, Count1, Count).
count_arith_operator(A-B, '-'/2, Count0, Count) :-
	ground(B),
	B =:= 1,
	!,
	count_arith_operator(A, '-'/2, Count0, Count).
count_arith_operator(A-B, '--'/1, Count0, Count) :-
	ground(B),
	B =:= 1,
	!,
	Count1 is Count0 + 1,
	count_arith_operator(A, '--'/1, Count1, Count).
count_arith_operator(Term, A/N, Count0, Count) :-
	functor(F, A, N),
	(
	    Term = F ->
	    Count1 is Count0 + 1
	;
	    Count1 = Count0
	),
	Term =.. [_|Terms],
	count_arith_operator_list(Terms, A/N, Count1, Count).

count_arith_operator_list([],           _F, Count,  Count).
count_arith_operator_list([Term|Terms], F,  Count0, Count) :-
	count_arith_operator(Term, F, Count0, Count1),
	count_arith_operator_list(Terms, F, Count1, Count).

valid_arith_builtin('arithmetic:is'(_,  _)).
valid_arith_builtin('arithmetic:=:='(_, _)).
valid_arith_builtin('arithmetic:=\='(_, _)).
valid_arith_builtin('arithmetic:<'(_,   _)).
valid_arith_builtin('arithmetic:=<'(_,  _)).
valid_arith_builtin('arithmetic:>='(_,  _)).
valid_arith_builtin('arithmetic:>'(_,   _)).
