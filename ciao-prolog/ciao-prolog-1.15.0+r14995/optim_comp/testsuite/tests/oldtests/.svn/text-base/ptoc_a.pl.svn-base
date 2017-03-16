% Little example to measure grow factor when compiling to C

:- module(_, _, []).

:- use_module(library(strings)).

:- '$preddef'(p/1, ptoc).
:- '$ptoc_prop'('ptoc_a:p'/1, [imp = semidet, register = true]).
p(X) :-
%	X = f(f(1),f(1,f(2,f(3,e)))).
% "esto es un string desde C"              9430
% "esto es un string desde C1234"         10370  (940)
% "esto es un string desde C12345678"     11310  (940)
% "esto es un string desde C12345678abcd" 12250  (940)
	X = "esto es un string desde C12345678abcd".

q(X) :-
% "esto es un string desde C"             2972  
% "esto es un string desde C1234"         3036  (64)
% "esto es un string desde C12345678"     3100  (64)
% "esto es un string desde C12345678abcd" 3159  (59)
	X = "esto es un string desde C12345678abcd".

foo :-
	display(calling_foo), nl,
	p(X),
	write_string(X), nl,
	display(called_foo(X)), nl.
