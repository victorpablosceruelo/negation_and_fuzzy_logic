:- module( inclist, [main/4], [] ).

:- use_module(library(write)).

main(T,R,T2,R2):- 
	write(hello),
	inclist([1,3|T],R), 
%	inclist([2,4,8|T2],R2).
	inclist([2,3|T2],R2).

inclist([],[]).
inclist([H|X],[H2|Y]):- 
	H2 is 1+H,
	inclist(X,Y).
