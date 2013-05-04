:- module(clpq_examples, [main/0], [assertions]).

:- use_module(library(format)).

:- use_module(laplace).
:- use_module(fib_q).
% :- use_module(fib_r).



fibnum(15).

main:-
        format("Solving Dirichlet problem for Laplace's equation~n", []),
        laplace:go1,
        fibnum(N),
        format("Solving direct and inverse Fibonacci~n", []),
        fib_q:fib(N, FQ),
        fib_q:fib(M, FQ),
        (
            N = M ->
            format("Ok, correct~n", [])
        ;
            format("Uh oh: problems with Fibonnaci on Q~n", [])
        ).

 %% Hum, it seems that clp(R) and clp(Q) cannot be mixed in the same execution
 %%  (there is a module which is imported twice).
 %%         fib_r:fib(N, FR),
 %%         fib_r:fib(R, FR),
 %%         (
 %%             N = R ->
 %%             true
 %%         ;
 %%             format("Uh oh: problems with Fibonnaci on R~n", [])
 %%         ).
