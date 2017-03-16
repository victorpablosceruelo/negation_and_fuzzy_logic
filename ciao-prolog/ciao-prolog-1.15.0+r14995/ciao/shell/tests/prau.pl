:- module(prau, [a/1,b/3], []).

:- use_module(engine(internals)).

a(X) :-
        del_stumps(prau),
        load_lib(prau, prau),
        a(X).

b(X,Y,Z) :-
        del_stumps(prau),
        load_lib(prau, prau),
        b(X,Y,Z).

:- meta_predicate(stump(?, fact)).
:- multifile stump/2.
:- data stump/2.

stump(prau, a(_)).
stump(prau, b(_,_,_)).

del_stumps(Mod) :-
        retract_fact(stump(Mod, Pred)),
        term_to_meta(T, Pred),
        '$abolish'(T),
        fail.
del_stumps(Mod) :- retract_fact(current_module(Mod)). % Delete this later 
