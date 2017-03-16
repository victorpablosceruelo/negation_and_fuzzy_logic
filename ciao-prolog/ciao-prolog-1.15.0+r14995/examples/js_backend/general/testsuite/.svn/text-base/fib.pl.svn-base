:- module(fib, [], [benchmark]).

% TODO: Requires bignums (we could use some bignum library for JS)
name("Fib (only floating point)").
repeat_count(100).
solve(Out) :-
	Data = 1000,
	fib(Data, Out).

fib(N,F):-
        fibaux(N,0,1,F).
fibaux(0, Fact, _Fpost, Fact) :- !.
fibaux(N, Fact, Fpost, F) :-
        N1 is N - 1,
        Nfib is Fact + Fpost,
        fibaux(N1, Fpost, Nfib, F).

