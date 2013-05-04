:- module(monad_tr, [monad_sentence_trans/3], []).

monad_sentence_trans((:- _), _, _) :- !, fail.
monad_sentence_trans((?- _), _, _) :- !, fail.
monad_sentence_trans(end_of_file, _, _) :- !, fail.
monad_sentence_trans(Sentence0, Sentence, Mod) :-
	trans_term(Sentence0, Mod, Sentence).

trans_term(T, _Mod, NT) :- var(T), !, NT = T.
trans_term(do(Monad, Do), Mod, NT) :-
	trans_do(Do, Monad, Mod, NT),
	display(transdo(NT)), nl.
trans_term(T, Mod, NT) :-
	trans_term_args_of(T, Mod, NT).

trans_term_args_of(T, Mod, NT) :-
        functor(T, F, A),
        functor(NT, F, A),
        trans_term_args(A, T, Mod, NT).

trans_term_args(0, _, _Mod, _ ) :- !.
trans_term_args(N, T0, Mod, T1) :-
        arg(N, T0, A0),
        arg(N, T1, A1),
        N1 is N-1,
        trans_term(A0, Mod, A1),
        trans_term_args(N1, T0, Mod, T1).

% Translate from 'do' notation to bind operations
% [[V1 <- F1, Rest]] ====> F1 >>= ''(V1 := [[Rest]])
trans_do([], Monad, _Mod, NT) :- NT = '~'(return(Monad, '()')). % todo: unsure... error instead?
trans_do([G], Monad, Mod, NT) :- !,
	add_monad(G, Monad, G1),
	trans_term(G1, Mod, G2),
	NT = G2.
trans_do(['<-'(Var,G)|Gs], Monad, Mod, NT) :- !,
	functor(Head, '', 1),
	arg(1, Head, Var),
	add_monad(G, Monad, G1),
	trans_term(G1, Mod, G2),
	NT = '>>='(G2, ':='(Head, NT0)),
	trans_do(Gs, Monad, Mod, NT0).
trans_do([G|Gs], Monad, Mod, NT) :- !,
	trans_do(['<-'(_,G)|Gs], Monad, Mod, NT).
	
add_monad(G, Monad, G2) :-
	G =.. [N|Xs],
	G1 =.. [N, Monad|Xs],
	( G1 = do(_,_) ->
	    G2 = G1
	; G2 = '~'(G1)
	).
