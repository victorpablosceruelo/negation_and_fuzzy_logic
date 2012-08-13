:- module(ex_error_in_chan,_,[.(cneg)]).
% :- module(ex_error_in_chan,_,[.(cneg), .(debugger_pkg)]).

cneg_ignores_preds([tests/0]).

tests :- cneg_rt_Chan([], r(Y)), portray_term_with_attributes('', cneg_rt([], r(Y))).

q(V, W) :- disequality((Z, W), (3, W), []), equality(V, Z, []).
r(V) :- q(V, _W).

p(X) :- disequality(struct(X, _Y), struct(a, b), []).

