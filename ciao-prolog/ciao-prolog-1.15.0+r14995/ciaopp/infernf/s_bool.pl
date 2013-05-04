/********************************************************************
*                                                                   *
*  File:    bool.pl                                                 *
*  Purpose: Manipulation of Boolean formulae                        *
*  Author:  Saumya Debray                                           *
*  Date:    4 Aug 1995                                              *
*                                                                   *
*  Notes:   I've chosen simplicity of code over efficiency here,    *
*    and if performance becomes an issue this would be a good place *
*    to look for improvements.                                      *
*                                                                   *
********************************************************************/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                                                 %%
%%  DATA REPRESENTATION: Conjunction is represented by ','/2,      %%
%%    disjunction by ';'/2, negation by not/1.                     %%
%%                                                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% normalize(+F0, -F1) takes a Boolean formula F0 and transforms it to
% an equivalent formula F1 that is a disjunction of conjunctions.
/*
normalize(Fin, Fout) :-
	push_negation(Fin, F0),
	push_conj(F0, Fout).

% push_negation pushes negations inwards using De Morgan's rules.

push_negation(not(','(F1, F2)), ';'(G1, G2)) :-
	!,
	push_negation(not(F1), G1),
	push_negation(not(F2), G2).
push_negation(not(';'(F1, F2)), ','(G1, G2)) :-
	!,
	push_negation(not(F1), G1),
	push_negation(not(F2), G2).
push_negation(not(not(F)), G) :-
	!,
	push_negation(F, G).
push_negation(','(F1, F2), ','(G1, G2)) :-
	!,
	push_negation(F1, G1),
	push_negation(F2, G2).
push_negation(';'(F1, F2), ';'(G1, G2)) :-
	!,
	push_negation(F1, G1),
	push_negation(F2, G2).
push_negation(not('='(T1, T2)),       '$noteq$'(T1, T2)) :- !.
push_negation(not('$noteq$'(T1, T2)), '='(T1, T2)) :- !.
push_negation(X,                      X).
*/

% push_conj repeatedly pushes (i.e., distributes) conjunctions into
% disjunctions. It assumes that negations have already been pushed
% in as far as possible.

push_conj(F, G) :-
	push_conj_3(F, H, Flag),
	( nonvar(Flag) ->
	    push_conj(H, G) ;
	    G = H
	).

push_conj_3((P, (Q;R)), (A1;A2), y) :-
	!,
	push_conj_3((P, Q), A1, _),
	push_conj_3((P, R), A2, _).
push_conj_3(((Q;R), P), (A1;A2), y) :-
	!,
	push_conj_3((Q, P), A1, _),
	push_conj_3((R, P), A2, _).
push_conj_3((P, Q), (A1, A2), Flag) :-
	!,
	push_conj_3(P, A1, Flag),
	push_conj_3(Q, A2, Flag).
push_conj_3((P;Q), (A1;A2), Flag) :-
	!,
	push_conj_3(P, A1, Flag),
	push_conj_3(Q, A2, Flag).
push_conj_3(G, G, _).

% wff2list(+Fmla, -Lst) takes a formula that is a disjunction of
% conjunctions, and transforms this to a list of lists Lst, where
% each element of the top-level list Lst is a list that represents
% a disjunct whose elements represent conjuncts.

wff2list(F, L) :- disj2list(F, L, []).

disj2list(';'(A1, A2), Hd, Tl) :-
	!,
	disj2list(A1, Hd,  Mid),
	disj2list(A2, Mid, Tl).
disj2list(A, [CList|Tl], Tl) :-
	conj2list(A, CList0, []),
	rem_dups(CList0, [true], CList).

conj2list(','(A1, A2), Hd, Tl) :-
	!,
	conj2list(A1, Hd,  Mid),
	conj2list(A2, Mid, Tl).
conj2list(A, [A|Tl], Tl).

% rem_dups(+Lin, +Seen, -Lout) removes duplicate elements from the list
% Lin to produce the list Lout.  Seen is a list of elements that have
% been encountered already.

rem_dups([X|L], Seen, Lout) :-
	( member_0(X, Seen) ->
	    rem_dups(L, Seen, Lout) ;
	    (Lout = [X|Lout1], rem_dups(L, [X|Seen], Lout1)) ).
rem_dups([], _, []).
