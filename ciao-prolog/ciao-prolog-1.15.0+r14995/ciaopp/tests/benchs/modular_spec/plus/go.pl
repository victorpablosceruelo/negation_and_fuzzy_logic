:- module(go, [ go/2 ],[assertions]).
:- use_module(plus, [plus/3]).

:- entry go(A,B) : ( ground(A), ground(B) ).
go(A,B):-
 	p(A,_,B),
 	p(A,B,_).

p(A,B,C):-
	plus(A,B,C).
p(A,B,C):-
	plus(A,B,C).







