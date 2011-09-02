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
show([(Y,Vars)|Left]) :- display('Y='), write(Y),nl, local_write_answers(Vars, ['X']), show(Left).