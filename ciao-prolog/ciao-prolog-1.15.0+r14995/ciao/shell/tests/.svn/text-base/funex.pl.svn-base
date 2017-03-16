:- use_package([types,fsyntax]).

% :- fun_eval append/2.

append([], L) := L.
append([X|Xs], L) := [X|~append(Xs,L)].

%:- fun_eval length/1.

length([]) := 0.
length([_|L]) := ~length(L)+1.

%:- fun_eval fact/1.

fact(0) := 1.
fact(N) := N * ~fact(--N) :- N > 0.

:- fun_eval arith(false).

af(2 + ~append([2+3],[5])).

:- fun_eval arith(true).

at(2 + ~fact(2+3)).

p(~fact(~length(~append([a,b],[c,d])))).


% This exists in LIFE

:- fun_eval {}/1.
:- fun_eval choice/2.

{E} := X :- var(E), !, X = E.
{E,S} := choice(E,S) :- !.
{E} := E.

choice(E,_) := E.
choice(_,S) := {S}.

addition := ^(~int + ~int).

:- type list/2.

list(T) := [].
list(T) := [regtype(T)|~list(T)].

list(T) := X :-
        X = []
      ;
        X = [regtype(T)|~list(T)].
