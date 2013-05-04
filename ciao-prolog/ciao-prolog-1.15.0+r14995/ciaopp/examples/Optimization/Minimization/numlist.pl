:- module( numlist, [main/2], [assertions] ).

:- use_module(library(write), [write/1]).

main(T,T2):- 
        write('Hello world'),
	numlist([1,3|T]), 
	numlist([2,4|T2]).

numlist([]).
numlist([H|T]):- 
	num(H),
	numlist(T).
