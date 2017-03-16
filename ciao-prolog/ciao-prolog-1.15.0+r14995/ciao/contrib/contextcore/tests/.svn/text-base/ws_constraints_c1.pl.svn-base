% ws_constraints with contexts - version 1
%
%   The 'permute' predicate is using the workshop list and Poss as
%   contextual variables. The 'constraint', 'par', 'seq', 'not_par',
%   and 'not_seq' predicates use explicit contexts, but notation is
%   not shorter.
%   -- jose

:- module(_, [main/0], [contextcore]).

:- use_module(library(lists)).
:- use_module(library(write)).
:- use_module(library(sort)).
:- use_module(library(aggregates)).

main :-
	( setof(P, t(P), L),
	  member(X, L),
	  write(X), nl, fail
	; true
	).

t(Poss):-
	Poss = [_,_,_,_],
	ws(W),
	permute[+(^W), +(^Poss)],
	poss:constrain[+(^Poss)],
	sort(Poss, Poss).

permute[+I,+Poss] :-
	I = [],
	closed[+Poss].
permute[+I,+Poss] :-
	accum(S)[Poss],
	permute_(S)[I],
	permute[+I,+Poss].

permute_(S)[I] :- pick(S)[I].
permute_(S)[I] :- 
	(S = X+Y ; S = X*Y),
	pick(X)[I],
	pick(Y)[I],
	X @> Y.

pick(X)[L] :- select(X, L, L2), L<-L2.
accum(X)[L] :- L = [X|Rest], L<-Rest.
closed[+L] :- L = [].

ws([pstd,chr,culp,dc,wlpe,cic,asp]).

:- sub_module poss {
  :- implicit_context(+poss).
  constrain[+poss]:-
  %% workshops which are (probably) large enough that they cannot reuse
  %% another ws' room in the same day.
	not_seq(pstd,   _)[+poss],
	not_seq(culp,   _)[+poss],
	not_seq(chr,    _)[+poss],
	not_seq(dc,     _)[+poss],       % Unsure - but let's play safe
	not_seq(wlpe,   _)[+poss],       % Unsure - but let's play safe
  %%
  %% Workshops which should not go in parallel because of
  %% recommendations, preferences, etc.
  %%
	par(     dc, pstd)[+poss],       % pstd not a research wp
	not_par(cic, wlpe)[+poss],       % WLPE is trad. interesting for ciclops
	not_par(cic, pstd)[+poss],       % Many cic cont. are pstd. cont
	not_par(cic, chr)[+poss],        % Probably the same for CHR
	not_par(culp, _)[+poss].         % Promote CULP to a central position?

  not_par(A, B)[+poss] :- \+ par(A, B)[+poss].
  not_seq(A, B)[+poss] :- \+ seq(A, B)[+poss].

  par(A, B)[+poss] :-
	c(A*B)[+poss] ; c(B*A)[+poss].
  seq(A, B)[+poss] :-
	c(A+B)[+poss] ; c(B+A)[+poss].

  c(V)[+poss] :-
	member(V, poss@).
}.


