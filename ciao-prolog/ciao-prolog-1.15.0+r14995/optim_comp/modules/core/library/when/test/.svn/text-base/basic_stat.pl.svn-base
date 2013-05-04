:- module(basic_stat, [state/2], []).

:- use_module(library(format)).

:- use_module(library(prolog_sys)).

state(When, State):-
        gather_info(ThisState),
        (
            When = initial ->
            State = ThisState
        ;
            State = [Tin, Gin, Lin, Trin,Chin],
            ThisState = [Tfin, Gfin, Lfin, Trfin,Chfin],
            TDelta is Tfin - Tin,
            GDelta is Gfin - Gin,
            LDelta is Lfin - Lin,
            TrDelta is Trfin - Trin,
            ChDelta is Chfin - Chin,
            format("Time used: ~w ms~n", [TDelta]),
            format("Global stack initial ~w, final ~w, used ~w~n",
                   [Gin, Gfin, GDelta]),
            format("Local stack initial ~w, final ~w, used ~w~n", 
                   [Lin, Lfin, LDelta]),
            format("Trail stack initial ~w, final ~w, used ~w~n", 
                   [Trin, Trfin, TrDelta]),
            format("Choice stack initial ~w, final ~w, used ~w~n", 
                   [Chin, Chfin, ChDelta]),
            format("-----------------------------------------------~n", [])
        ).

            

gather_info([T, G, L, Tr,Ch]):-
        statistics(runtime, [T,_]),
        statistics(global_stack, [G, _]),
        statistics(local_stack,  [L, _]),
        statistics(trail, [Tr, _]),
        statistics(choice, [Ch, _]).
