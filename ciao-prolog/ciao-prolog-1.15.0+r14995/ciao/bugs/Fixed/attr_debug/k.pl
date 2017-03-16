:- module(k, [test/1]).
:- use_module(engine(attributes)).

test(X) :-
%	X = '$emilio'.
	attach_attribute(X, '$emilio'(X)).

