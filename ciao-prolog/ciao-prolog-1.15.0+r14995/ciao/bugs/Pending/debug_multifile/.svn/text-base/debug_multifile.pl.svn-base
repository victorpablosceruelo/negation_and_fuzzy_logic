:- module(_,_,[]).

% Load file for source debugging and trace "go".
% m is not trace because it is in a ficticious module

go :-
        s(Y),
        display(Y),
        m(X),
        display(X).

s(X) :-
        display(m),
        nl,
        X = a.

:- multifile m/1.

m(X) :-
        display(m),
        nl,
        X = a.
