:- module(t, [list/2, lst/2],[types]).

:- use_module(engine(basictypes)).

:- type list(L,T) # "...".

list([],_).
list([X|Xs], T) :-
        T(X),
        list(Xs, T).

:- type lst(L,T) # "...".

lst([],_).
lst([X|Xs], T) :-
        regtype(X,T),
        lst(Xs, T).
