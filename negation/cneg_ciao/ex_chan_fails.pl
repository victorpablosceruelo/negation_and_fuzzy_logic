:- module(ex_chan_fails,_,[.(cneg), debugger_pkg]).


q(V, W) :- disequality((Z, W), (3, a), []), equality(V, Z, []).
r(V) :- q(V, _W).