:- module(_, _, _).

:- use_module(engine(ql_inout)).

main([X]) :-
    catch(a(X), E, e(E)).

a(X) :-
    ( X = a -> true ; throw(no) ).

e(E) :-
    display(E), nl.
