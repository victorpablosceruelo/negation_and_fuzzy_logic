:- package(attr).

:- use_module(library(attr(attr_rt))).

% TODO: implement 'addmodule' or 'context module' so that this is not necessary

get_attr_local(Var, Value) :-
	'$module'(M),
	get_attr(Var, M, Value).

put_attr_local(Var, Value) :-
	'$module'(M),
	put_attr(Var, M, Value).

del_attr_local(_) :- '$nodef'.

