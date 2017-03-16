:- module(top, [], [assertions]).

%
%  top.pl			Nai-Wel Lin			December, 1991
%
%  This file contains the top level procedures for the system.
%

%
%  Top-level predicate of CASLOG in interactive mode.
%

:- reexport(infercost(top(error)), [error_message/3]).
:- reexport(infercost(top(utility)),
	[
	    add/3,
	    addition/3,
	    close_list/1,
	    compound/1,
	    empty_queue/2,
	    get_queue/3,
	    init_queue/2,
	    intersection/3,
	    ith_list_element/3,
	    list/1,
	    maximum/3,
	    member/2,
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
	    sub/3,
	    subterm/2,
	    subtraction/3,
	    union/3
	]).

% top_.pl
%	  analysis/4,
%	  analysis/7
