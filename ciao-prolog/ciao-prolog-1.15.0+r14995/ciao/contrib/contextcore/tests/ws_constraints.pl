% (from a Manuel Carro's email)
%
% > Related to negation: I have what i think is a nice (and natural)
% > example of use of contexts without state change.  Look at the program
% > attached: it is a program to schedule meetings with different topics
% > which can go in parallel or sequentially in a series of days.  Some
% > constraints have to be respected regarding which meetings can go or
% > not in parallel (due to too much overlapping) or sequentially (because
% > they are too long to fit in a single day).
% > 
% > It generates candidate schedules and then tries to apply the
% > contraints to these.  Now, the candidate schedule has to be threaded,
% > although not changed, when enforncing these constraints.  The same for
% > the constraints (not_par/3 , par/3 and not_seq/3 , seq/3), but in
% > order for this to work contexts have to be correctly treated in the
% > scope of a negation.

:- module(_, [main/0], []).

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
       permute(W, Poss),
       constrain(Poss),
       sort(Poss, Poss).

permute([], []).
permute(I, [X|R]):-
       select(X, I, I1),
       permute(I1, R).
permute(I, [S|R]):-
       (S = X+Y ; S = X*Y),
       select(X,I,I1),
       select(Y,I1,I2),
       X @> Y,
       permute(I2, R).

ws([pstd,chr,culp,dc,wlpe,cic,asp]).

constrain(Poss):-
%% Workshops which are (probably) large enough that they cannot reuse
%% another ws' room in the same day.
       not_seq(pstd,   _, Poss),
       not_seq(culp,   _, Poss),
       not_seq(chr,    _, Poss),
       not_seq(dc,     _, Poss),       % Unsure - but let's play safe
       not_seq(wlpe,   _, Poss),       % Unsure - but let's play safe
%%
%% Workshops which should not go in parallel because of
%% recommendations, preferences, etc.
%%
       par(     dc, pstd, Poss),       % pstd not a research wp
       not_par(cic, wlpe, Poss),       % WLPE is trad. interesting for ciclops
       not_par(cic, pstd, Poss),       % Many cic cont. are pstd. cont
       not_par(cic, chr, Poss),        % Probably the same for CHR
       not_par(culp, _, Poss).         % Promote CULP to a central position?

not_par(A, B, Poss):- \+ par(A, B, Poss).
not_seq(A, B, Poss):- \+ seq(A, B, Poss).

par(A, B, Poss):-
       member(A*B, Poss) ; member(B*A, Poss).
seq(A, B, Poss):-
       member(A+B, Poss) ; member(B+A, Poss).


