:- module( addlists, [main/15], [] ).

:- use_module(library(write)).

main(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O):-
        write(hello),
        addlists([4,4|A],[0,3|B],[4,7|C]),
        addlists([3,3|D],[1,4|E],[4,7|F]),
        addlists([3,3|G],[1,4|H],I),
        addlists([1,1|J],[3,6|K],L),
	addlists([7,8|M],[4,5|N],O).

addlists([],[],[]).
addlists([H1|T1],[H2|T2],[HR|TR]):- 
	HR is H1+H2,
	addlists(T1,T2,TR).
