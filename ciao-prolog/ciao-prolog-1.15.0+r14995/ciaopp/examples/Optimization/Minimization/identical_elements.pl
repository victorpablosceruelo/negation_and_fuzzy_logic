:- module( _, [main/2], [assertions] ).

:- use_module(library(write), [write/1]).

:- entry main(A,B): (ground(A), ground(B)).

main(T,T2):- 
        write('Hello world'),
	id_elements(T,E), 
	id_elements(T2,E).

id_elements([],_).
id_elements([H|T],Element):- 
	ground(H),
	H = Element,
	id_elements(T,H).
