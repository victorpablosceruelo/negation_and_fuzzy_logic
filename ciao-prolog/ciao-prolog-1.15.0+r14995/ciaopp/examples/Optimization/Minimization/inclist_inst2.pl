:- module( inclist_inst2, [main/4], [assertions] ).

:- use_module(library(write), [write/1]).

main(T,R,T2,R2):- 
        write('Hello world'),
	inclist([1,3|T],2,R), 
	inclist([2,4|T2],3,R2).

inclist([],_,[]).
inclist([H|X],F,[H2|Y]):- 
	H2 is F+H,
	inclist(X,F,Y).
