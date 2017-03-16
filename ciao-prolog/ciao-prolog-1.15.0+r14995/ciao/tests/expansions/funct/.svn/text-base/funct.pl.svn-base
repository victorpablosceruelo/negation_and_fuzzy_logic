:- module(funct, [main/0], [fsyntax,hiord,assertions]).

:- use_module(library(format)).
:- use_module(library(write)).

:- fun_eval(arith(true)).

:- test main.

main:-
        n(N),
        format("Fact of ~d is ~d~n", [N, ~fact(N)]),
        format("Type testing with H.O (1)... ", []),
        type_test(1, Test1),
        (
            Test1 ->
            write('Ok!'),
            nl
        ;
            format("Uh! Oh!~n ~w failed!~n", [Test1])
        ),
        type_test(2, Test2),
        format("Type testing with H.O (2)... ", []),
        (
            Test2 ->
            format("Uh! Oh!~n ~w suceeded!~n", Test2)
        ;
            write('Ok!'),
            nl
       ),
       hopredfun(HOPF),
       hopredfunname(HOPFN),
       format("Combining metapredicates and functions: ~w = ~w~n", [HOPFN,HOPF]),
       !.

n(100).
        
type_test(1,funct:list([[a,b,c,t],[t,g,t]], list(atom))).
type_test(2,funct:list([[a,b,c,t],[t,[g],t]], list(atom))).

%% Infix operator :=/2 defines relations as funciones.
%% Prefix operator ~ defines use of relations as functions.

fact(0) := 1.
fact(N) := N * ~fact(--N) :- N > 0.


:- prop list(L,T) + regtype # "@var{L} is a list of @var{T}s.".
:- meta_predicate list(?, pred(1)).

list([],_).
list([X|Xs], T) :-
        T(X),
        funct:list(Xs, T).


%% Combining meta predicates and functions

:- meta_predicate map(_, pred(2), _).
:- fun_eval map/2.

map([], _) := [].
map([X|Xs], P) := [~P(X)|~map(Xs,P)].

hopredfun(map([1,3,2], arg(f(a,b,c,d)))).
hopredfunname(^map([1,3,2], arg(f(a,b,c,d)))).
