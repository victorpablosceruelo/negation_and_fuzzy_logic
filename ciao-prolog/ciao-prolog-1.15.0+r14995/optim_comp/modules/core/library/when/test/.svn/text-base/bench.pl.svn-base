:- use_package(assertions).

:- use_module(library(when)).
:- use_module(library(format)).
:- use_module(library(lists)).

:- use_module(basic_stat).

%% Compare time and memory usage with non-delay, non-tail-recursive versions.

size(10000).

test_addition:-
        size(N),
        ta(N).

ta(N):-
        state(initial, State),
        add_from_1_to_n(N, R),
        (
            R =:= (N * (N+1)) / 2 ->
            true
        ;
            format("Wrong result in number addition~n", [])
        ),
        state(final, State).

add_from_1_to_n(0, 0).
add_from_1_to_n(N, R):-
        N > 0,
        N1 is --N,
        when(ground(R1), add(R1, N, R)),
        add_from_1_to_n(N1, R1).

add(A, B, C):- C is A + B.



test_fib(N):-
        state(initial, State),
        fib(N, _F),
        state(final, State).

fib(N, F):-
        length(L, N),
        build_fibo(L, F),
        L = [0, 0 | _].

build_fibo([F1, F2, F3], F3):- !,
        when((ground(F1), ground(F2)), add(F1, F2, F3)).
build_fibo([F1, F2, F3 | Rest], Result):-
        when((ground(F1), ground(F2)), add(F1, F2, F3)),
        build_fibo([F2, F3 | Rest], Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Call queens_constrain/2 to see the goals.

test_queens(N):-
        state(initial, State),
        allqueens(N),
        state(final, State).

queens(N, Qs):-
        queens_constrain(N, Qs), !, %% Constraint diagonals
        queens_search(N, Qs).       %% Nondet. put a queen in each column

queens_search(0, []).               %% Finished
queens_search(ThisQueen, ToPlace):-
        ThisQueen > 0,
        select(ThisQueen, ToPlace, NewToPlace),
        OtherQueen is ThisQueen - 1,
        queens_search(OtherQueen, NewToPlace).

queens_constrain(0, []).
queens_constrain(N, [Q|Qs]):-
        N > 0,
        N1 is N - 1,
        no_attack(N1, Qs, Q, 1),
        queens_constrain(N1, Qs).

no_attack(0, [],    _Queen, _Nb).
no_attack(N, [Y|Ys], Queen,  Nb):-
        N > 0,
        when((ground(Queen), ground(Nb), ground(Y)), 
              test_no_attack(Queen, Y, Nb)),
        Nb1 is Nb + 1,
        N1 is N - 1,
        no_attack(N1, Ys, Queen, Nb1).

test_no_attack(Queen, Y, Nb):-
        Queen + Nb =\= Y,
        Queen - Nb =\= Y.

allqueens(N):-
        queens(N, _),
        fail.
allqueens(_N).
