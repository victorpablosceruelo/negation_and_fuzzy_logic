%
%  top.pl			Nai-Wel Lin			December, 1991
%
%  This file contains the top level procedures for the system.
%

%
%  Top-level predicate of CASLOG in interactive mode.
%

:- module(top,
	[ add/3,
	  addition/3,
	  append/3,
	  close_list/1,
	  compound/1,
	  empty_queue/2,
	  error_message/3,
	  get_queue/3,
	  init_queue/2,
	  intersection/3,
	  ith_list_element/3,
%	  list/1,
	  maximum/3,
%	  member/2,
	  minimum/3,
	  minus/2,
	  multiply/3,
	  noncompound/1,
	  nonempty_queue/2,
	  noninteger/1,
	  nonlist/1,
	  nonsequence/1,
	  opened_set_equivalent/2,
	  opened_set_inclusion/2,
	  opened_set_insertion/2,
	  opened_set_member/2,
	  opened_set_union/2,
	  pop/3,
	  push/3,
	  set_put_queue/3,
	  set_worst_result/1,
	  set_zero_if_lower_bot/2,
	  sub/3,
	  subterm/2,
	  subtraction/3,
	  union/3
% top_.pl
%	  analysis/4,
%	  analysis/7
	],
	[]).

:- use_module('..'(algebraic)).
:- use_module('..'(init)).

:- use_module('..'(database), [approximation/1]).
:- use_module('..'(undefined)).

:- push_prolog_flag(multi_arity_warnings,off).

:- include(error).
:- include(output).
% :- include(top_).
:- include(utility).

:- pop_prolog_flag(multi_arity_warnings).

%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:

