% TODO: avoid module wrapper
:- module(circle_, [], [compiler(complang)]).

:- use_module(.(printable_)).

:- public circle.
:- class circle {
    :- extends printable.
    :- attr x :: any.
    :- attr y :: any.
    :- attr r :: any.

    :- constructor new_/3.
    new_(X,Y,R) :- ~x = X, ~y = Y, ~r = R.

    :- public show/0.
    show :-
        display('circle<'), display(~x), display(','), display(~y), display(','), display(~r), display('>'). 
}.
