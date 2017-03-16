:- module(subst, [substitute/4], [assertions]).

% :- use_module(library(lists), [list/2, list1/2]).

:- entry substitute(A, B, C, D):
         (ground(A), ground(B), ground(C), list(C, term), var(D)).

substitute(_X,_Y,[],[]).
substitute(X,Y,[X|T],[Y|Ts]) :-
  substitute(X,Y,T,Ts).
substitute(X,Y,[F|T],[F|Ts]) :-
  \+ X = F,
  substitute(X,Y,T,Ts).

%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:

