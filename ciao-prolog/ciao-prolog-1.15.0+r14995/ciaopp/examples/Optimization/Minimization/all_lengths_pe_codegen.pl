:- module( _all_lengths, [main/4], [assertions] ).


:- use_module(library(write), [write/1]).

:- use_module(library(lists), [length/2]).

:- true pred main(A,B,C,D).

main(A,B,C,D) :-
        write(hello),
        all_lengths_1([[a,b]|A],C),
        all_lengths_3([[b,a]|B],D) .

:- true pred all_lengths_1(A,B).

all_lengths_1([[a,b]],[2]).
all_lengths_1([[a,b],A|B],[2,C|D]) :-
        length(A,C),
        all_lengths_2(B,D) .

:- true pred all_lengths_2(A,B).

all_lengths_2([],[]).
all_lengths_2([A|B],[C|D]) :-
        length(A,C),
        all_lengths_2(B,D) .

:- true pred all_lengths_3(A,B).

all_lengths_3([[b,a]],[2]).
all_lengths_3([[b,a],A|B],[2,C|D]) :-
        length(A,C),
        all_lengths_2(B,D) .
