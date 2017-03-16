:- module( _all_lengths, [main/4], [assertions] ).


:- use_module(library(write), [write/1]).

:- use_module(library(lists), [length/2]).

main(A,B,C,D) :-
        write(hello),
        all_lengths_3([[a,b]|A],C),
        all_lengths_3([[b,a]|B],D) .

all_lengths_2([],[]).
all_lengths_2([A|B],[C|D]) :-
        length(A,C),
        all_lengths_2(B,D) .

all_lengths_3([[_1,_2]],[2]).
all_lengths_3([[_1,_2],A|B],[2,C|D]) :-
        length(A,C),
        all_lengths_2(B,D) .
