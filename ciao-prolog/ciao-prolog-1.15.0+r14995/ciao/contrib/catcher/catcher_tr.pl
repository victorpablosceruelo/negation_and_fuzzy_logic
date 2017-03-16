:- module(catcher_tr, [catcher_sentence_tr/3], []).

:- use_module(library(compiler(c_itf_internal)), [location/1]).

catcher_sentence_tr(end_of_file, end_of_file, _).
catcher_sentence_tr((:- Decl),   (:- Decl),   _).
catcher_sentence_tr((H :- B0),   (H :- B),    _) :-
	expand_body(B0, B).
% catcher_sentence_tr(H, H, _).

expand_body(A, catcher_call(A, Loc)) :-
	var(A),
	!,
	location(Loc).
expand_body(!,        !) :- !.
expand_body((A0, B0), (A, B)) :-
	!,
	expand_body(A0, A),
	expand_body(B0, B).
expand_body((A0; B0), (A; B)) :-
	!,
	expand_body(A0, A),
	expand_body(B0, B).
expand_body((A0 -> B0), (A -> B)) :-
	!,
	expand_body(A0, A),
	expand_body(B0, B).
expand_body(A, catcher_call(A, Loc)) :-
	location(Loc).
