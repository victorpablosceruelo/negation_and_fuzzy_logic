:- module(exec, [main/0], []).
% BUG: Buffer overflow in system:exec/?
%
% (Jose F. Morales)

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(system), [exec/3]).
:- use_module(library(strings), [write_string/1, get_line/2]).

main :-
	repeat(10).

% Break the engine by repeating the process several times
repeat(0).
repeat(I) :- I > 0, test, I1 is I - 1, repeat(I1).

% This test overrides memory
test :-
	Args = ['h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h ',
	        'h h h h h h h h h h'],
	atom_concat(['echo '|Args], Cmd),
	exec(Cmd, _In, Out),
	( get_line(Out, L),
	    write_string(L),
	    nl,
	    fail
	; true
	).
