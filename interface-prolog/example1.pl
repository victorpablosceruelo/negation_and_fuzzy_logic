:- module(example1, _, [clpr]).

:-use_module(library(write)).

% This example works as expected,  
% thanks to Jose F. Morales ( jfran )

p(X) :- X .>. 0.

query(p(X), [X]).

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

main :- query(Q, Vars), call(Q),
	local_write_answers(Vars, ['X']).

:- multifile portray_attribute/2.
portray_attribute(X) :- X = eqn_var(_,_,_,_,_), !, print(X).

