:- module(nfbool,
	    [bool_normalize/2,
		push_conj/2,
		push_neg_in_test/2,
		put_negation/3,
		remove_negation/2,
		translate_test/2,
		wff2list/2
	    ],
	    [assertions]).

:- use_module(library(idlists), [member_0/2]).
:- use_module(library(messages)).

:- include(infernf(p_bool)).
:- include(infernf(s_bool)).
