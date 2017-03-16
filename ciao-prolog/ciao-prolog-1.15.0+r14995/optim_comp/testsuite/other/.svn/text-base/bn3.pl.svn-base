:- module(_, [main/0], []).

main :-
        N = 73,
        Exp = 30111,
        exponential_naive(N, Exp, _R).

exponential_naive(_Base, 0, 1).
exponential_naive(Base, Exp, Res):-
        Exp > 0,
        NewExp is Exp - 1,
        exponential_naive(Base, NewExp, PartRes),
        Res is PartRes * Base.
