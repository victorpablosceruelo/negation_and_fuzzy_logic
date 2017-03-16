:- module(_, [], [compiler(complang)]).

:- use_module(library(lists), [append/3]).

% ---------------------------------------------------------------------------
% Semantic expansion
% TODO: change name? it is clause and goal translations

:- use_module(compiler(srcdbg)).

:- public expand_init/2.
expand_init(Errs, Module) :-
	% Translator initialization
	expand_clause(Errs, 0, 0, Module, _, _, _).

:- public expand_end/1.
expand_end(Module) :-
	del_srcdbg_expand(Module),
        del_trans_hook(Module).

:- public expand_clause/7.
expand_clause(Errs, H, B, M, Dict, H1, B1) :-
	expand_clause__1(H, B, M, Dict, H0, B0),
	( use_srcdbg_expand(M) ->
	    srcdbg_expand(Errs, H0, B0, H1, B1, Dict)
	; H1 = H0,
	  B1 = B0
	).

expand_clause__1(H, B, M, Dict, H1, B1) :-
	m :: any <- M,
        get_translation_hook(clause, KVs), !,
	pqueue_values(KVs, Ts),
        do_translations(Ts, clause(H,B), Dict, clause(H1,B1)).
expand_clause__1(H, B,_M,_Dict, H, B).

% ---------------------------------------------------------------------------
% TODO: add priority and remove the special treatment of srcdbg case

:- data use_srcdbg_expand/1.

del_srcdbg_expand(Module) :-
	retractall_fact(use_srcdbg_expand(Module)).

:- public add_srcdbg_expand/1.
add_srcdbg_expand(Module) :-
	asserta_fact(use_srcdbg_expand(Module)).

% ---------------------------------------------------------------------------
% translation hooks (duplicated in compiler/frontend.pl)

:- public add_trans_hook/4.
:- public del_trans_hook/1.
:- public pqueue_values/2.

:- include(compiler(trans_hook_db)).
%     [add_trans_hook/4, del_trans_hook/1, pqueue_values/2]
:- data translation_hook/3.
{
:- fluid m :: any.
:- public get_translation_hook/2. % (for compiler/frontend.pl, mexpand)
get_translation_hook(Kind, Tr) :-
	current_fact(translation_hook(~m, Kind, Tr)).
set_translation_hook(Kind, Tr) :-
	M = ~m,
	retractall_fact(translation_hook(M, Kind, _)),
	assertz_fact(translation_hook(M, Kind, Tr)).
add_translation_hook(Kind, Tr) :-
	assertz_fact(translation_hook(~m, Kind, Tr)).
del_translation_hook(Kind) :-
	retractall_fact(translation_hook(~m, Kind, _)).
mark_trans_hook(_).
unmark_trans_hook.
}.

:- use_module(compiler(translation_common)).
