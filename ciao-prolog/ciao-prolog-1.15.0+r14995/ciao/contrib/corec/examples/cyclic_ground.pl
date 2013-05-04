:- module(cyclic_ground, [ground/1], [corec]).

% Plain copy of the Prolog version of term_typing:ground/1 that works
% (for free) with cyclic terms. Note that the implemented C version
% does not work with cyclic terms yet.

:- corecursion ground(===) with !.

ground(Term):-
	nonvar(Term),
	functor(Term,_,N),
	ground_(N,Term).

ground_(0,_) :- !.
ground_(N,Term):-
	N > 0,
	arg(N,Term,Arg),
	ground(Arg),
	N1 is N-1,
	ground_(N1,Term).



