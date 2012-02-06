:- module(ex_error_in_chan,_,[.(cneg)]).
% :- module(ex_error_in_chan,_,[.(cneg), .(debugger_pkg)]).


q(V, W) :- disequality((Z, W), (3, a), []), equality(V, Z, []).
r(V) :- q(V, _W).

p(X) :- disequality(struct(X, _Y), struct(a, b), []).

