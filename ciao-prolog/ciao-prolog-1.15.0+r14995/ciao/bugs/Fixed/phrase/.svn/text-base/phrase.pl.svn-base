:- module(phrase, [test/0]).
:- use_package(library(dcg(dcg_phrase))).

a --> {Seq=([a],{true},[],([b];{false}))}, Seq.

% test/0 must succeed
test :-
	phrase(a,L),
	L = [a,b].
