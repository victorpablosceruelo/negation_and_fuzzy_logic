% A test for context variables
%
% Author: Jose F. Morales
:- module(_, [main/0], [compiler(complang)]).

% This program checks a solved bug in context variables

:- '$begin_context'(test).
:- '$incctx'(pair(status)). % also with pair_wo
test1 :- !,
	Status0 = conttrue,
	% in a buggy translation, Status and BUGStatus are unified to the same variable at compile time
	( Status0 = conttrue ->
	    display('If this is not a free variable, this is a bug:'(BUGStatus)), nl,
	    status <- BUGStatus
	; status <- Status0
	).
test2 :- !,
	Status0 = conttrue,
	( Status0 = conttrue ->
	    display('If this is not a free variable, this is a bug:'(BUGStatus)), nl,
	    V1 = BUGStatus, status <- V1
	; V2 = Status0, status <- V2
	).
:- '$end'.

main :-
	push_pair(status, _) '$ctx_on' (test1, display(~status), nl),
	push_pair(status, _) '$ctx_on' (test2, display(~status), nl).



