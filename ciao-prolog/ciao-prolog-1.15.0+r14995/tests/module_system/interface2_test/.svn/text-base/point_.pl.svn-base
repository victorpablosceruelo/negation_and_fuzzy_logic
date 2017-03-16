% TODO: avoid module wrapper
:- module(point_, [], [compiler(complang)]).

:- use_module(.(printable)).

:- public point.
:- class point {
    :- extends printable. % TODO: No error is detected if this line is missing
    :- attr x :: any.
    :- attr y :: any.

    :- constructor new_/2.
    new_(X,Y) :- ~x = X, ~y = Y.

    :- public show/0.
    show :-
        display('point<'), display(~x), display(','), display(~y), display('>'). 
}.

