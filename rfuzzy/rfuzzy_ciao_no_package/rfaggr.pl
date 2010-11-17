:- module(rfaggr,[ inject/3, merge/4, id/2, id/3, prod/3, iprod/3, 
	min/3, luka/3, dprod/3, max/3, dluka/3, complement/3,
	'=>'/4,
	rfuzzy_conversion_in/2, rfuzzy_conversion_out/2 ],[hiord]).
:- use_module(library(write),[write/1]).
:- use_package(clpr).

% REMOVED: preinject/3,postinject/4, 

complement(X, C, Z) :-
	Temp1 .=. C - X,
	min(1, Temp1, Temp2),
	max(0, Temp2, Z).

min(X,Y,Z):- X .=<. Y, X .=. Z .
min(X,Y,Z):- X .>. Y, Y .=. Z .

prod(X,Y,M):- M .=. X * Y.
iprod(X,Y,M):- M .=. 1 - (X * Y).

luka(X,Y,M):- 
	Temp .=. X + Y  - 1, 
	max(0, Temp, M).

max(X,Y,Z):- X .>=. Y, X .=. Z .
max(X,Y,Z):- Y .>. X, Y .=. Z .

dprod(X,Y,M):- M .=. X + Y - (X * Y).

dluka(X,Y,M):- 
	Temp .=. X + Y,
	min(1, Temp, M).

rfuzzy_property_error(X) :-
	write('Error on rfuzzy property: '),
	write(X), nl, 
	!, % Backtracking is not allowed here.
	fail. % Don't continue
	
rfuzzy_error_msg(Msg1, Msg2) :-
	nl,
	write('ERROR: '),
	write(Msg1), 
	write(Msg2), 
	nl, !, % Backtracking is not allowed here.
	fail.

rfuzzy_warning_msg(Msg1, Msg2) :-
	nl,
	write('WARNING: '),
	write(Msg1), 
	write(Msg2), 
	nl.

rfuzzy_conversion_in(X, Y) :-
	nonvar(X),
	X .=. Y.
rfuzzy_conversion_in(X, _Y) :-
	\+(nonvar(X)).

rfuzzy_conversion_out(rat(X, Y), Z) :-
	Z is X/Y.
rfuzzy_conversion_out(X, X) :-
	number(X).
 
:- meta_predicate preinject(?,pred(2),?).

id(L,L).

preinject([],_,[]):-!.
preinject(L,P,T):- P(L,T).

:- meta_predicate inject(?,pred(3),?).

inject([],_,_).
inject([T],_,T).
inject([X,Y|Rest],P,T):-
	P(X,Y,T0),
	inject([T0|Rest],P,T).

:- meta_predicate postinject(?,?,pred(3),?).

id(_,V,V).
postinject([],A,_,A):-!.
postinject(L,V,P,T):- P(L,V,T).


:- meta_predicate merge(?,?,pred(3),?).

merge([],L,_,L).

merge(L,[],_,L).

merge(L1,L2,P,L):-
	list(L1),list(L2),!,
	mergeaux(L1,L2,P,L).

mergeaux([],[],_,[]).

mergeaux([X|L1],[Y|L2],P,[Z|L]):-
	P(X,Y,Z),
	mergeaux(L1,L2,P,L).

:- new_declaration(is_fuzzy/3,on).
:- is_fuzzy('=>',4,truth).

:- meta_predicate =>(pred(3),goal,goal,?).

=>(Formula,X,Y,M):- 
	functor(X,_,Ax),
	arg(Ax,X,Mx),
	functor(Y,_,Ay),
	arg(Ay,Y,My),
	call(X),
	call(Y),
	call(Formula,Mx,My,M).

