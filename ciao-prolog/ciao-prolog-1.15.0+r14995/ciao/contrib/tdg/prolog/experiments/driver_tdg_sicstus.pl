:- module(driver_tdg_sicstus,[tdg/1,collect_vars/2]).

%:- use_module(library(clpfd)).

:- use_module(example_list).

tdg(_).


% Code adapted from ciao/library/terms_vars.pl
collect_vars(Term,Vars) :- collect_vars_(Term,Vars,[]).
collect_vars_(X,Vars,Tail) :- 
	var(X),!,
	Vars = [X|Tail].
collect_vars_([H|T],Vars,Tail) :- !,
	collect_vars_from_list([H|T],Vars,Tail).
collect_vars_(Term,Vars,Tail) :-
	functor(Term,_,A),
	go_inside(A,Term,Vars,Tail).

collect_vars_from_list(L,Tail,Tail) :- L == [].
collect_vars_from_list(L,Tail,Tail) :- var(L),!.
collect_vars_from_list(L,EVars,Tail) :- 
	L =..['.',X,Lrest],
	(var(X) -> EVars = [X|EVars_r] ; EVars = EVars_r),
	collect_vars_from_list(Lrest,EVars_r,Tail).

go_inside(0,_,Tail,Tail) :- !.
go_inside(N,T,Bag,Tail) :-
	Nth is N-1,
	arg(N,T,ARG),
	collect_vars_(ARG,Bag,Tail0),
	go_inside(Nth,T,Tail0,Tail).
