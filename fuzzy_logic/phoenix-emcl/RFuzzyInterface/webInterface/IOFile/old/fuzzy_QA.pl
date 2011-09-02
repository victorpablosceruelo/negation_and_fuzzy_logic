:- module(query,_,[clpr,rfuzzy,debugger_pkg]).
:- use_module(engine(hiord_rt)).
% Fuzzy concept generated from database 
expensive(FLAT_NUMBER,V) :- house(PRICE,_,_),expensive_func(PRICE,V).
cheap(FLAT_NUMBER,V) :- house(PRICE,_,_),cheap_func(PRICE,V).
small(FLAT_NUMBER,V) :- house(_,_,SIZE),small_func(SIZE,V).
% Fuzzy Concept Functions 
expensive_func(X,Y) :- X.>.500, X.=<.600, Y.=.X/600.
expensive_func(X,Y) :- X.>=.300, X.=<.400, Y.=.0.1.
% Neagtion Functions 
not_func(X,Y) :- X.>=.0, X.=<.1, Y.=.1-X.
something_func(X,Y) :- X.>=.0, X.=<.1, Y.=.1-X.
% Quantification Functions 
very_func(X,Y) :- X.>=.0, X.=<.0.5, Y.=.X.
very_func(X,Y) :- X.>=.0, X.=<.0.5, Y.=.X.
very_func(X,Y) :- X.>.0.5, X.=<.1, Y.=.1-X.
% Query List 
not_very_expensive(FLAT_NUMBER, V) :- expensive(FLAT_NUMBER, V1), very_func(V1, V2), not_func(V2, V).
something_very_cheap(FLAT_NUMBER, V) :- cheap(FLAT_NUMBER, V1), very_func(V1, V2), something_func(V2, V).
local_write_vars([]).
local_write_vars([Var|Vars]) :-
	write_attribute(Var), display(' , '),
	local_write_vars(Vars).

local_write_answers(Vars, Dict) :- 
%	display('display: '), display(Q), nl,
%	display('write_term: '), write_term(Q, [portrayed(true)]), nl, 
%	write_out(Q, options(_,_,_,true,_), _,_,_,_,_,_), nl,
%	display('write_attribute: [ '), local_write_vars(Vars), display(' ] '), nl. 
%	prettyvars(Vars).
	display('dump_constraints: '), dump_constraints(Vars, Dict, C), 
	write(C), nl.

:- multifile portray_attribute/2.
portray_attribute(X) :- X = eqn_var(_,_,_,_,_), !, print(X).

show([]).
show([(Y,Vars)|Left]) :- display('Y='), write(Y),nl, local_write_answers(Vars, ['X']), show(Left).q1 :- findall((FLAT_NUMBER,[V]),not_very_expensive(FLAT_NUMBER, V),Ans),show(Ans).
q2 :- findall((FLAT_NUMBER,[V]),something_very_cheap(FLAT_NUMBER, V),Ans),show(Ans).
main :- q1,nl,q2,nl.
