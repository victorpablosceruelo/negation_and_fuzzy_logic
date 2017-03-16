:- module(prior_trans, _, []).

% Test for translation hooks (with priorities)
%
% Author: Jose F. Morales

% :- use_module(library(write)).

main :-
	test(sentence),
	test(term),
	test(clause),
	test(goal).

:- discontiguous do/1.

test(X) :-
	( do(X) ->
	    true
	; display('failed expansion test for hook \''),
	  display(X),
	  display('\'!'),
	  nl
	).

% ---------------------------------------------------------------------------
% Setup translation modules

:- load_compilation_module(+(mytr1)).
:- load_compilation_module(+(mytr2)).
:- load_compilation_module(+(mytr3)).

:- add_term_trans(mytr3:t_term/2, 770).
:- add_sentence_trans(mytr3:t_sentence/2, 770).
:- add_clause_trans(mytr3:t_clause/2, 770).
:- add_goal_trans(mytr3:t_goal/2, 770).

:- add_term_trans(mytr2:t_term/2, 760).
:- add_sentence_trans(mytr2:t_sentence/2, 760).
:- add_clause_trans(mytr2:t_clause/2, 760).
:- add_goal_trans(mytr2:t_goal/2, 760).

:- add_term_trans(mytr1:t_term/2, 750).
:- add_sentence_trans(mytr1:t_sentence/2, 750).
:- add_clause_trans(mytr1:t_clause/2, 750).
:- add_goal_trans(mytr1:t_goal/2, 750).

% ---------------------------------------------------------------------------
% Test for sentence translations

do(sentence) :-
	s(Z),
	% write(Z), nl,
	Z = (3,2,1,nil). % expected output
	
s(nil).

% ---------------------------------------------------------------------------
% Test for term translations

do(term) :-
	% --
	% Term
	X = t(nil),
	% write(X), nl,
	X =.. [_,(3,2,1,nil)].

% ---------------------------------------------------------------------------
% Test for clause translations

do(clause) :-
	c(Y),
%	write(Y), nl,
	Y = (3,2,1,nil).

c(nil) :- true.

% ---------------------------------------------------------------------------
% Test for goal translations

do(goal) :-
	g(1, nil).

g(Step, X) :-
%	write(g_v(Step, X)), nl,
	Step = 4,
	X = (3,2,1,nil).





	