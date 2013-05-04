:- module(homeo_emb_, [homeomorphic_embedded/2], []).

:- use_package(assertions).

:- use_module(library(terms_check), [ask/2]).

:- doc(homeomorphic_embedded(Term1,Term2), "The term @var{Term1} is
     homeomorphically embedded in term @var{Term2}.").

%% Code taken from Michael Leuschel %%

/* homeomorphic_embedded(X,Y) :- var(X),!,
 (nonvar(Y) -> (print(h(X,Y)),nl) ; true). */
homeomorphic_embedded(X,Y) :- var(X),var(Y),!.
homeomorphic_embedded(_X,Y) :-
	var(Y),!,fail.
%% homeomorphic_embedded(X,Y) :-
%% 	nonvar(X),dynamic_term(X),
%% 	nonvar(Y),dynamic_term(Y),!.
homeomorphic_embedded(X,Y) :-
	strict_instance_of(X,Y),!,
	% print('$*'),debug_print('$'(X,Y)),
        fail.
homeomorphic_embedded(X,Y) :- /* coupling for unary constructors */
	nonvar(X),nonvar(Y),
	X=..[Func,XArg],
	Y=..[Func,YArg],
	!, /* do not try diving for unary matching constructors */
	homeomorphic_embedded(XArg,YArg),!.
homeomorphic_embedded(X,Y) :- /* coupling */
	nonvar(X),nonvar(Y),
	X=..[Func|XArgs],
	Y=..[Func|YArgs],
	l_homeomorphic_embedded(XArgs,YArgs),!.
homeomorphic_embedded(X,Y) :- /* diving */
	nonvar(Y),
	term_nesting_level(X,NX,SumX),
	sub_term(Y,Sub),
	term_nesting_level(Sub,NSub,SumSub),
	NSub>=NX,
	SumSub>=SumX,
	/*print(sub_term(Y,Sub)),nl,*/
	homeomorphic_embedded(X,Sub),!.

/* l_homeomorphic_embedded(X,Y) :- 
	print(l_homeomorphic_embedded(X,Y)),nl,fail. */
l_homeomorphic_embedded([],[]).
l_homeomorphic_embedded([X|TX],[Y|TY]) :-
	homeomorphic_embedded(X,Y),!,
	l_homeomorphic_embedded(TX,TY).


/* CHECK WHETHER THIS IS REALLY USEFUL */
/* term_nesting_level(_,0,0) :- !. */

term_nesting_level(X,0,0) :- var(X),!.
term_nesting_level(X,1,1) :- atomic(X),!.
term_nesting_level(X,N,S) :- nonvar(X),!,
	X=..[_F|Args],
	l_term_nesting_level(Args,NA,SA),
	N is NA + 1,
	S is SA + 1.

l_term_nesting_level([],0,0).
l_term_nesting_level([H|T],N,S) :-
	term_nesting_level(H,NH,SH),
	l_term_nesting_level(T,NT,ST),
	max(NH,NT,N),
	S is SH + ST.

sub_term(X,Sub) :-
	nonvar(X),
	X=..[_F|Args],
	member(Sub,Args).

% ciao specific

strict_instance_of(Goal1,Goal2) :-
	copy_term(Goal1,CGoal),
	ask(CGoal,Goal2),
	\+(ask(Goal2,CGoal)).

max(X,Y,X) :- X >= Y,!.
max(X,Y,Y) :- Y > X.

:- doc(version_maintenance,dir('../version')).

:- doc(version(1*0+428,2004/04/19,14:18*37+'CEST'), "Improved
   implementation by taking code from MECCE.  (German Puebla)").

:- doc(version(1*0+407,2004/04/04,15:39*35+'CEST'), "Predicates
   related to homeomorphic embedding now live in this module.  (German
   Puebla)").

