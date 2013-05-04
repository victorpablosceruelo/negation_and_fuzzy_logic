:- entry subterm2(A,B,C) : ( ground(A), var(B), var(C) ) .
:- entry subterm2(A,B,C) : ( var(A), ground(B), ground(C) ) .

subterm2(Term,Term,_).
subterm2(Sub,Term,N):- 
%    functor(Term,_F,N), 
	length(Term,N),
	subterm3(N,Sub,Term).

:- entry which_position(A,B,C) : (ground([A,B]), var(C)).

which_position(Sub,TermList,N):-
	subterm3(N,Sub,TermList).
