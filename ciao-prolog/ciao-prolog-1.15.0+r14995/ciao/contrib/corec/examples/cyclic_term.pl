:- module(cyclic_term, [cyclic_term/1, 
	                acyclic_term/1,
			term_size/2, 
			term_flatten/3,
			term_flatten2/3],
	[corec, assertions]).

/* acyclic_term/1 */
:- true pred acyclic_term(Term) 
# "Succeeds if @var{Term} is an acyclic (i.e., finite) term.".

:- corecursion acyclic_term(===) with (!, fail).

acyclic_term(X) :- 
	var(X), !.
acyclic_term(X) :- 
	atomic(X), !.
acyclic_term(X) :-
	X =.. [_|Args],
	acyclic_term_args(Args).

acyclic_term_args([]).
acyclic_term_args([H|T]) :- 
	acyclic_term(H),
	acyclic_term_args(T).

/* cyclic_term/1 */
:- true pred cyclic_term(Term) 
# "Succeeds if @var{Term} is a cyclic (i.e., infinite) term.".

cyclic_term(X) :- 
	\+ acyclic_term(X).



:- corecursion term_size(===, X) with (!, X=1).

:- true pred term_size(Term, Size) => int(Size) 
# "@var{Size} is the size of the regular term @var{Term}.".


term_size(A, 1):-
	var(A), !.
term_size(A, 1):-
	atomic(A), !.
term_size(X, N) :-
	X =.. [_|Args],
	term_size_args(Args, 1, N).

term_size_args([], N, N).
term_size_args([H|T], N, M):-
	term_size(H, L), 
	K is N + L, 
	term_size_args(T, K, M).



:- true pred term_flatten(Term, X, Goal) 
# "Decomposes the term @var{Term} into a conjunction of flat
   unifications (of the form @pred{Y = f(A_1, ... A_n)} where the
   @var{A_i}'s are variables or atomic terms).".

term_flatten(X, Y, G):-
	term_flatten_(X, Y, G, true).
	
:- corecursion term_flatten_(===, =, G1, G2) with (!, G1=G2).

term_flatten_(X, X, G, G):-
	var(X), !.
term_flatten_(X, X, G, G):-
	atomic(X).
term_flatten_(X, X_, (X_= X__, G1), G2):-
	X =.. [F|Args],
	term_flatten_args(Args, Args_, G1, G2), 
	X__ =.. [F|Args_].

term_flatten_args([], [], G, G).
term_flatten_args([X|Args], [X_|Args_], G1, G3):-
	term_flatten_(X, X_, G1, G2), 
	term_flatten_args(Args, Args_, G2, G3).


:- true pred term_flatten2(Term, X, Goal) 
# "Decomposes the term @var{Term} into a conjunction of flat
   unifications (unifications of the form @pred{Y = f(A_1, ... A_n)}
   where the @var{A_i}'s are variables or atomic terms). This version
   detects more sharing than @pred{term_flatten/3}.".

/* Same version as previous but in continuation passing style */

term_flatten2(X, Y, G):-
	term_flatten2_(X, Y, G, true, true).
	
:- corecursion term_flatten2_(===, =, G1, G2, Cont) with (!, G1=G2, call(Cont)).

term_flatten2_(X, X, G, G, Cont):-
	var(X), !, call(Cont).
term_flatten2_(X, X, G, G, Cont):-
	atomic(X), !, call(Cont).
term_flatten2_(X, X_, (X_= X__, G1), G2, Cont):-
	X =.. [F|Args],
	term_flatten2_args(Args, Args_, G1, G2, Cont), 
	X__ =.. [F|Args_].

term_flatten2_args([], [], G, G, Cont):- call(Cont).
term_flatten2_args([X|Args], [X_|Args_], G1, G3, Cont):-
	term_flatten2_(X, X_, G1, G2, (
		      term_flatten2_args(Args, Args_, G2, G3, Cont))) .




