%:- module(syntactic_translation, [], [assertions]).

:- use_module(library(lists), [append/3]).

% ---------------------------------------------------------------------------
% Sentence and term expansions (syntactic)
% note: used in compiler/frontend and in toplevel

% (exported)
% (used by the toplevel)
syntr__expand_query(V, _, Query) :- var(V), !, Query = call(V). % TODO: incorrect, expansions could treat this
syntr__expand_query((:- Decl), VarNames, Query) :-
        syntr__expand_sentence((:- Decl), VarNames, Expansion),
        Query = ( Expansion = [] ? true | Expansion ).
syntr__expand_query(RawQuery, VarNames, Query) :-
        syntr__expand_sentence(('TOPLEVEL':-RawQuery), VarNames, Expansion),
        ( Expansion = [('TOPLEVEL':-Query)], !
        ; Query = fail,
	  % TODO: bug or error message?
          message(error, ['unexpected answer from expansion: ',Expansion])
        ).

% (exported)
% Perform syntactic translations ('sentence' and 'term')
syntr__expand_sentence(Data0, Dict, Data) :-
        syntr__sentence_translation(Data0, Dict, Data1), % not recursive
        syntr__term_translation(Data1, Dict, Data2),     % recursive
	!,
	Data = ( var(Data2) ? []
	       | Data2 = [_|_] ? Data2
	       | Data2 = [] ? Data2
	       | [Data2]
	       ).
syntr__expand_sentence(_, _, []).

% TODO: add a end of file callback expansion... instead of expanding the fact end_of_file
% TODO: add the same for expansion initialization (now it is done expanding 0)
% (exported)
syntr__expand_end(Data) :-
	syntr__expand_sentence(end_of_file, [], Data0),
        % TODO: expansions should not generate end_of_file ...
	( append(Data, [end_of_file], Data0) -> true ; Data = Data0 ).

% ---------------------------------------------------------------------------

% (exported)
:- meta_predicate syntr__init(+, primitive(spec)).
% Initialize transl. for this module
syntr__init(M, T/A) :-
        atom(T),
        create_trans(A, T, M, Tr),
        do_translation(Tr, 0, [], _).

% ----------------------------------------------------------------

% Perform sentence translations (given the translation hooks)
syntr__sentence_translation(X, Dict, Y) :-
        nonvar(X),
        get_translation_hook(sentence, KVs), !,
	pqueue_values(KVs, [T|Ts]),
        do_sent_trans(Ts, T, X, Dict, Y).
syntr__sentence_translation(X, _, X).

do_sent_trans([], T, X, Dict, Xt) :-
        do_translation(T, X, Dict, Xt).
do_sent_trans([T|Ts], T0, X, Dict, Y) :-
        do_translation(T0, X, Dict, Xt),
        do_sent_trans2(Xt, T, Ts, Dict, Y).

do_sent_trans2([], _, _, _, []) :- !.
do_sent_trans2([S1|S2], T, Ts, Dict, St) :- !,
        do_sent_trans(Ts, T, S1, Dict, S1t),
        append_clauses(S1t, S2t, St),
        do_sent_trans2(S2, T, Ts, Dict, S2t).
do_sent_trans2(S, T, Ts, Dict, St) :-
        do_sent_trans(Ts, T, S, Dict, St).

append_clauses([], L, L) :- !.
append_clauses([C|Cs], L, [C|R]) :- !, append(Cs, L, R).
append_clauses(C, L, [C|L]).

% ---------------------------------------------------------------------------

% Perform term translations (given the translation hooks)
syntr__term_translation(X, Dict, Y) :-
        get_translation_hook(term, KVs), !,
	pqueue_values(KVs, Ts),
        syntr__term_translation_clauses(X, Ts, Dict, Y).
syntr__term_translation(X, _, X).

syntr__term_translation_clauses([], _, _, []) :- !.
syntr__term_translation_clauses([C|Cs], Ts, Dict, [D|Ds]) :- !,
        syntr__term_translation_t(C, Ts, Dict, D),
        syntr__term_translation_clauses(Cs, Ts, Dict, Ds).
syntr__term_translation_clauses(C, Ts, Dict, D) :-
        syntr__term_translation_t(C, Ts, Dict, D).

syntr__term_translation_t(X, _, _, Y) :- var(X), !, Y = X.
syntr__term_translation_t(X, Ts, Dict, Y) :-
        do_translations(Ts, X, Dict, Xt),
        functor(Xt, F, A),
        functor(Y, F, A),
        syntr__term_trans_args(A, Xt, Ts, Dict, Y).

syntr__term_trans_args(0, _, _, _, _) :- !.
syntr__term_trans_args(N, X, Ts, Dict, Y) :-
        arg(N, X, Xn),
        arg(N, Y, Yn),
        N1 is N-1,
        syntr__term_translation_t(Xn, Ts, Dict, Yn),
        syntr__term_trans_args(N1, X, Ts, Dict, Y).

:- use_module(compiler(translation_common)).
