% prolog_sys:statistics/2 is not properly handled using 'terms',
% although it works using 'eterms'.  If the following predicate is
% analyzed, the 'terms' domain should infer an assertion like
% 
% :- true pred foo(A)
%          : term(A)
%         => p(A).
% 
% However, it fails when analyzing the call to statistics, generating
% 
% :- true pred foo(A)
%          : term(A)
%          + fails.
% 
% It works using 'eterms'.
:- module(_,[foo/1],[assertions,regtypes]).
:- use_module(library(prolog_sys), [statistics/2]).

:- entry foo(A).

foo(X):-
	statistics(runtime,[GT,_]),
	p(X),
	statistics(runtime,[_,GT2]),
	display(times([GT,GT2])),nl.

:- prop p(A) + regtype.

p(f(a)).
