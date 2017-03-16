:- module(trace_terms,_,[assertions]).

:- doc(title,"Utilities for trace terms").

:- doc(author, "M. Zamalloa").

:- doc(module,"This module provides a set of utilities to manipulate trace terms").

:- use_module(library(lists), [length/2]).

:- data label_info/2.

cleanup_label_info :-
	retractall_fact(label_info(_,_)).

set_label_info(L,Ar) :-
	retractall_fact(label_info(L,_)),
	assertz_fact(label_info(L,Ar)).

tr_to_trterm(Trace,TrTerm) :- tr_to_trterm_(Trace,TrTerm,[]).
tr_to_trterm_([L|Ls],TrTerm,RemTr) :-
	label_info(L,Ar), % This has to provided from outside
	length(SubTrTerms,Ar),
	TrTerm =..[L|SubTrTerms],
	to_trterms(Ar,Ls,SubTrTerms,RemTr).

to_trterms(0,Ls,[],Ls).
to_trterms(N,Trace,[TrTerm|TrTerms],RemTr) :-
	N_p is N - 1,
	tr_to_trterm_(Trace,TrTerm,Trace_p),
	to_trterms(N_p,Trace_p,TrTerms,RemTr).
