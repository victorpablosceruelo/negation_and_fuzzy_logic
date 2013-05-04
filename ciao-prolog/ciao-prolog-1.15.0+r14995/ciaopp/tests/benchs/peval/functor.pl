:- module(_,[p/1],[]).

p(Term):-
	functor(a,F,A),
	copy_term(here,H),
	functor(Term,f,Arity),
	functor(Term2,1,f).
