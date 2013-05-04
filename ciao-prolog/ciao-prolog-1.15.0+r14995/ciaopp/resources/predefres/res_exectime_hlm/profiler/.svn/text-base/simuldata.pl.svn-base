:- module(simuldata, [check_types/2], [assertions]).

check_types([],     []).
check_types([T|Ts], EE) :-
	T =.. [E0|Tail],
	(
	    \+ member(E0, ['native_props:mshare']) ->
	    Tail = [_Value|OtherInfo],
	    E =.. [E0|OtherInfo],
	    EE = [E|Es]
	;
	    EE = Es
	),
	check_types(Ts, Es).
