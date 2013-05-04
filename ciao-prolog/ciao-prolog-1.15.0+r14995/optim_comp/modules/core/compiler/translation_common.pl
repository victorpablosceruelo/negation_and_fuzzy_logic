:- module(_, [], [pure]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(hiord_rt), ['$meta_call'/1, call/1]).
:- use_package(compiler(complang_mini)).

:- public create_trans/4.
create_trans(2, T,_M, T).
create_trans(3, T, M, Tr) :-
        functor(Tr, T, 1),
        arg(1, Tr, M).
create_trans(4, T, M, Tr) :-
        functor(Tr, T, 2),
        arg(1, Tr, M).

:- public do_translations/4.
% do_translation(Ts, X, Dict, Y) :: list(keyval) * term * dict * term
do_translations([], X, _, X).
do_translations([T|Ts], X, Dict, Y) :-
        do_translation(T, X, Dict, Xt),
        do_translations(Ts, Xt, Dict, Y).

:- public do_translation/4.
do_translation(T, X, Dict, Y) :-
        comp_goal(T, Dict, G),
        arg(1, G, X),
        arg(2, G, Y),
        '$meta_call'(G), !.
do_translation(_, X, _, X).

comp_goal(T,_Dict, G) :-
        atom(T), !,
        functor(G, T, 2).
comp_goal(T,_Dict, G) :-
        functor(T, F, 1), !,
        arg(1, T, M),
        functor(G, F, 3),
        arg(3, G, M).
comp_goal(T, Dict, G) :-
        functor(T, F, 2), !,
        arg(1, T, M),
        functor(G, F, 4),
        arg(3, G, M),
        arg(4, G, Dict).
