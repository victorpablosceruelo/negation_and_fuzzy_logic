:- module( fib_lemma, [lfib/2], [assertions] ).

:- use_module(library(dynamic)).

lfib(N, F):-  lemma_fib(N, F), !. 
lfib(N, F):- 
        N > 1,   
        N1 is N - 1,  
        N2 is N1 - 1, 
        lfib(N1, F1), 
        lfib(N2, F2), 
        F is F1 + F2,     
        assert(lemma_fib(N, F)). 

:- trust success lemma_fib(A,B) => (num(A), num(B) ).
:- dynamic lemma_fib/2.
lemma_fib(0, 0). 
lemma_fib(1, 1). 
