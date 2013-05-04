:- module(subst, [substitute/4], [assertions]).
%:- use_module(engine(basic_props), [list/2,gnd/1]).
:- use_module(engine(basiccontrol), [(\+)/1]).
:- use_module(engine(term_basic), [(=)/2]).
%:- use_module(library(assertions(native_props)),[var/1]).

:- entry substitute(A, B, C, D): gnd * gnd * list(gnd) * var.

substitute(_X,_Y,[],[]).
substitute(X,Y,[X|T],[Y|Ts]) :-
  substitute(X,Y,T,Ts).
substitute(X,Y,[F|T],[F|Ts]) :-
  \+ X = F,
  substitute(X,Y,T,Ts).

