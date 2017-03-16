:- module(_,_,[clpr,rfuzzy,debugger_pkg]).
:- use_module(engine(hiord_rt)).
:- use_module(library(aggregates)).
:- use_module(library(write)).
:- use_module(library(terms_vars)).
:- use_module(library(pretty_print)).
house(lfs2168,'apartment',114,5,630000,2,5700).
house(lfs2144,'apartment',77,3,420000,7,3500).
house(lfs2147,'apartment',80,2,675000,12,200).
house(lfs2145,'apartment',224,8,790000,20,100).
house(c358,'apartment',74,3,340000,5,3100).
house(lfs2110,'apartment',415,9,2500000,8,2400).
house(lfs2124,'apartment',63,2,275000,15,450).
house(lfs2123,'apartment',62,3,285000,6,1000).

house(lfs2111,'villa',700,10,1100000,9,4500).
house(lfs2047,'villa',1750,11,1650000,15,1000).
house(lfs2041,'villa',4000,13,2500000,4,1800).
house(es13462,'villa',600,6,4000000,6,1500).
house(lfs1942,'villa',900,10,3100000,3,3400).
house(lfs1917,'villa',210,5,590000,13,5000).
house(lfb143,'villa',1200,9,2750000,7,4000).
house(5607/152,'town_house',161,7,815000,6,1200).
house(es13340,'town_house',1025,8,2800000,25,7000).
house(lfs1939,'town_house',860,9,1800000,14,2400).
house(lfs1938,'town_house',520,11,1990000,19,80).
house(lfs2155,'villa',2300,9,3000000,13,800).

% FUZZY FUNCTIONS OVER THE DATABASE
expensive(X,Y):- house(X,_,_,_,P,_,_),expensive_func(P,Y).

% Fuzzy Concept Functions 
%expensive_func(X,Y) :- X.>.50000, X.=<.100000, Y.=.X/500000.
%expensive_func(X,Y) :- X.>.100000, X.=<.250000, Y.=.X/1500000.
%expensive_func(X,Y) :- X.>.250000, X.=<.350000, Y.=.X/1000000.
%expensive_func(X,Y) :- X.>.350000, X.=<.450000, Y.=.X/500000.
%expensive_func(X,Y) :- X.>.450000, X.=<.800000, Y.=.X/1250000.
%expensive_func(X,Y) :- X.>.800000, X.=<.1000000, Y.=.X/2000000.
%expensive_func(X,Y) :- X.>.1000000, X.=<.1500000, Y.=.X/5000000.
%expensive_func(X,Y) :- X.>.1500000, X.=<.2500000, Y.=.X/10000000.

expensive_func :# ([(50000,0),(100000,0.1),(250000,0.2),(350000,0.3),(450000,0.5),(550000,0.6),
	            (800000,0.7),(1000000,0.8),(1500000,0.9),(2500000,1)]).

% Neagtion Functions 
not_func(X,Y) :- X.>=.0, X.=<.1, Y.=.1-X.

% Quantification Functions 
very_func(X,Y) :- X.>=.0, X.=<.0.5, Y.=.X.
very_func(X,Y) :- X.>.0.5, X.=<.1, Y.=.1-X.
% Query List 
not_very_expensive(FLAT_NUMBER, V) :- expensive(FLAT_NUMBER, V1), very_func(V1, V2), not_func(V2, V).

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
%	display('dump_constraints: '),
	dump_constraints(Vars, Dict, C), 
	write(C), nl.
:- multifile portray_attribute/2.
portray_attribute(X) :- X = eqn_var(_,_,_,_,_), !, print(X).

show([]).
show([(Y,Vars)|Left]) :- write(Y),nl, local_write_answers(Vars, ['X']), show(Left).
query(not_very_expensive(Y,X), Y, [X]).

q1 :- findall((Y,[X]),not_very_expensive(Y,X),Ans),show(Ans).
q2 :- findall((Y,[X]),not_very_expensive(Y,X),Ans),show(Ans).



main :- q1,nl,q2,nl.



