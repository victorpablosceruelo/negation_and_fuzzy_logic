:- module(_,[call_list/1],[]).

:- meta_predicate call_list(list(goal)).

call_list([]).
call_list([G|Gs]) :-
        call(G),
        call_list(Gs).
