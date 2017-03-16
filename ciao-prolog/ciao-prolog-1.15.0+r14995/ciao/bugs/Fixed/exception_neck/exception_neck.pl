:- module(exception_neck, [main/0], []).

% The exception handling code (low-level) requires complete choice
% points at the time of invoking the handler. When exceptions are
% thrown from C builtins while doing shallow backtracking, the engine
% breaks.

% The bug appears at least when executed from the the toplevel (both
% in optim_comp and Ciao).

main :-
	catch(main1, _, true).

main1 :-
	( main0 -> display(a), nl ; display(b), nl ).

main0 :-
	p.
main0.

p :- _X < _Y.
p.
