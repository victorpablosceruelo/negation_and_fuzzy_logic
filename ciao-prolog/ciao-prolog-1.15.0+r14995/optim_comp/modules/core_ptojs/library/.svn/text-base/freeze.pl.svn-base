:- module(freeze, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Delaying predicates (freeze)").
:- doc(author, "Remy Haemmerle").
:- doc(author, "Jose F. Morales (adapted to pl2js)").

:- doc(bug, "[Incomplete version of library(freeze) for pl2js]").

:- use_package(attr).

:- export(freeze/2).
% freeze Goal on X
freeze(X, Goal) :-
        ( nonvar(X) ->
            call(Goal)
        ; get_attr_local(X, Fb) ->
	    % rescue conjunction   
            put_attr_local(X, (Goal, Fb))
        ; put_attr_local(X, Goal)
        ).

:- export(attr_unify_hook/2). % TODO: we would need a package for this
attr_unify_hook(Fa, Other) :-
        ( nonvar(Other) ->
            call(Fa)
        ; get_attr_local(Other, Fb) ->
	    % rescue conjunction
            put_attr_local(Other, (Fa, Fb))
        ; put_attr_local(Other, Fa)
        ).

