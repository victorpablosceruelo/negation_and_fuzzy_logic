:- module(pnf,[main/0],[]).
:- use_package(lprolog).
:- use_module(library(write)).


:- op(300, xfy, (and)).
:- op(300, xfy, (or)).
:- op(200, xfy, (imp)).
:- op(100, fy, (some)).

:- op(100, fy, (all)).

prenex(B, B) :-
	%display_ref('quant_free'), nl,
	quant_free(B), !. 
prenex((B and C), D) :-
	%display_ref('and'), nl,
	prenex(B, U), prenex(C, V), merge((U and V), D).
prenex((B or C), D) :-
	%display_ref('or'), nl,
	prenex(B, U), prenex(C, V), merge((U or V), D).
prenex((B imp C), D) :-
	%display_ref('imp'), nl,
	prenex(B, U), prenex(C, V), merge((U imp V), D).
prenex((all B), (all D)) :-
	%display_ref('all'), nl,
	(pi X \ (termp(X) => prenex(B(X), D(X)))).
prenex((some B), (some D)) :-
	%display_ref('some'), nl,
	(pi X \ (termp(X) => prenex(B(X), D(X)))).

merge(((all B) and (all C)), (all D)) :-
       (pi X\ (termp(X) => merge((B(X) and C(X)), D(X)))).
merge(((all B) and C), (all D)) :- 
       (pi X\ (termp(X) => merge((B(X) and C), D(X)))).
merge((B and (all C)), (all D)) :- 
       (pi X\ (termp(X) => merge((B and C(X)), D(X)))).

merge(((some B) and C), (some D)) :- 
       (pi X\ (termp(X) => merge((B(X) and C), D(X)))).
merge((B and (some C)), (some D)) :-
       (pi X\ (termp(X) => merge((B and C(X)), D(X)))).

merge(((all B) or C), (all D)) :-
       (pi X\ (termp(X) => merge((B(X) or C), D(X)))).
merge((B or (all C)), (all D)) :-
       (pi X\ (termp(X) => merge((B or C(X)), D(X)))).
merge(((some B) or (some C)), (some D)) :-
       (pi X\ (termp(X) => merge((B(X) or C(X)), D(X)))).
merge(((some B) or C), (some D)):-
       (pi X\ (termp(X) => merge((B(X) or C), D(X)))).
merge((B or (some C)), (some D)) :-
       (pi X\ (termp(X) => merge((B or C(X)), D(X)))).

merge(((all B) imp (some C)), (some D)) :- 
       (pi X\ (termp(X) => merge((B(X) imp C(X)), D(X)))).
merge(((all B) imp C), (some D)) :- 
       (pi X\ (termp(X) => merge((B(X) imp C), D(X)))).
merge(((some B) imp C), (all D)) :-
       (pi X\ (termp(X) => merge((B(X) imp C), D(X)))).
merge((B imp (all C)), (all D)) :-
       (pi X\ (termp(X) => merge((B imp C(X)), D(X)))).
merge((B imp (some C)), (some D)) :-
       (pi X\ (termp(X) => merge((B imp C(X)), D(X)))).

merge(B, B) :- quant_free(B).





/* recognizer for terms */
termp(a).
termp(b).
termp(c).
termp(f(X)) :- termp(X).

/* recognizer for atomic formulas */
atomc(path(X, Y)) :-
	%display('atomc path =>'), display_ref(X), display_ref(Y), nl,
	termp(X), termp(Y).
atomc(adj(X, Y)) :-
	%display('atomc adj =>'), display_ref(X), display_ref(Y), nl,
	termp(X), termp(Y).

/* recognizer for quantifier free formulas */
quant_free(perp).
quant_free(tru).
quant_free(A) :-
	%display('quant_free atom =>'), display_ref(A), nl,
	atomc(A).
quant_free(B and C) :-
	quant_free(B), quant_free(C).
quant_free(B or C) :-
	quant_free(B), quant_free(C).
quant_free(B imp C) :-
	quant_free(B), quant_free(C).





%formula 1  ((all (X \ (path a X))) imp tru).
formula(1,  ((all (X \ path(a, X))) imp tru)).
%formula 2  ((some (X \ (path a X))) imp tru).
formula(2,  ((some (X \ path(a, X))) imp tru)).
%formula 3  ((all (X \ (path a X))) and (all (Y \ (path Y a)))).
formula(3,  ((all (X \ path(a, X))) and (all (Y \ path(Y, a))))).
%formula 4  ((some (X \ (path a X))) imp ((all (Y \ (path a Y))))).
formula(4,  ((some (X \ path(a, X))) imp ((all (Y \ path(a, Y)))))).

%(test N F) :- (formula N OF), (prenex OF F).
test(F) :-
	display_ref(F), nl,
	prenex(F, PNF),
	hnorm(PNF, NF),
	display_ref(NF), nl, nl.



main :-
	test(((some (X \ path(a, X))) imp ((all (Y \ path(a, Y)))))).


%     test(1, _F1),
%     test(2, _F2),
%     test(3, _F3),
%     test(4, _F4).
