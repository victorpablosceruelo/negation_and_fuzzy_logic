:- module(rfuzzy_rt, [defined_aggregators/1, 
	inject/3, merge/4, id/2, id/3, prod/3, iprod/3, 
	min/3, luka/3, dprod/3, max/3, dluka/3, complement/3,
	mean/3, supreme/2,
	'=>'/4,
	debug_msg/2, debug_msg_list/2, debug_nl/0, 
	rfuzzy_warning_msg/3, rfuzzy_error_msg/3,
	rfuzzy_conversion_in/2, rfuzzy_conversion_out/2 ],[hiord]).

:- use_module(library(write),[write/1]).
:- use_package(clpr).
:- use_module(library(terms),[copy_args/3]).


% REMOVED: preinject/3,postinject/4, 

defined_aggregators([id, min, max, luka, dluka, prod, iprod, dprod, complement, mean]).

mean(X, Y, Z) :- Z .=. (X + Y) / 2.
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

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

supreme(List, Element) :-
	
	reorder_elements(List, [], List_Tmp_1), !,
	filter_out_repeated(List_Tmp_1, List_Tmp_2), !,
	take_an_element(List_Tmp_2, Element).

reorder_elements([], List_Out, List_Out) :- !.
reorder_elements([Element|List_In], List_Tmp_1, List_Out) :-
	get_fuzzy_args_1n2(Element, F1, F2),
	reorder_elements_aux(Element, F1, F2, List_Tmp_1, List_Tmp_2),
	reorder_elements(List_In, List_Tmp_2, List_Out).

reorder_elements_aux(Element, _F1, _F2, [], [Element]) :- !.
reorder_elements_aux(Element, F1, F2, [Current|List_In], [Current|List_Out]) :-
	get_fuzzy_args_1n2(Current, C1, C2),
	(
	    (	C2 .>. F2    )
	;
	    ( 	C2 .=. F2,	C1 .>. F1	    )
	), !,
	reorder_elements_aux(Element, F1, F2, List_In, List_Out).
reorder_elements_aux(Element, _F1, _F2, [Current|List], [Element,Current|List]) :- !.

filter_out_repeated([], []) :- !.
filter_out_repeated([Element_1|List_In], [Element_1|List_Out]) :-
	functor(Element_1, Name, Arity),
	NewArity is Arity -2,
	functor(Element_1_Tmp, Name, NewArity),
	copy_args(NewArity, Element_1, Element_1_Tmp),
	copy_term(Element_1_Tmp, Element_1_Copy),
	filter_out_repeated_aux(Element_1_Copy, List_In, List_Tmp),
	filter_out_repeated(List_Tmp, List_Out).

filter_out_repeated_aux(_Element_1_Copy, [], []) :- !.
filter_out_repeated_aux(Element_1_Copy, [Element_2|List_In], List_Tmp) :-
	functor(Element_2, Name, Arity),
	NewArity is Arity -2,
	functor(Element_2_Tmp, Name, NewArity),
	copy_args(NewArity, Element_2, Element_2_Tmp),
	copy_term(Element_2_Tmp, Element_2_Copy),
	Element_1_Copy = Element_2_Copy, !,
	filter_out_repeated_aux(Element_1_Copy, List_In, List_Tmp).
filter_out_repeated_aux(Element_1_Copy, [Element_2|List_In], [Element_2|List_Tmp]) :-
	filter_out_repeated_aux(Element_1_Copy, List_In, List_Tmp).


get_fuzzy_args_1n2(Element, F1, F2) :-
	functor(Element, _Name, Arity),
	arg(Arity, Element, F2),
	NewArity is Arity -1,
	arg(NewArity, Element, F1),
	!.

take_an_element([Element|_List], Element).
take_an_element([_FirstElement|List], Element) :-
	take_an_element(List, Element).
	

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------
 
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

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

rfuzzy_conversion_in(X, Y) :-
	nonvar(X),
	X .=. Y.
rfuzzy_conversion_in(X, _Y) :-
	\+(nonvar(X)).

rfuzzy_conversion_out(rat(X, Y), Z) :-
	Z is X/Y.
rfuzzy_conversion_out(X, X) :-
	number(X).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% This is to enable/disable debug.
% do_debug_rfuzzy('No').
do_debug_rfuzzy('Yes').

debug_msg(Msg1, Msg2) :- 
	debug_msg_aux(Msg1, '', Msg2),
	debug_nl.

debug_msg_aux(_Msg1, _Msg2, _Msg3) :- do_debug_rfuzzy('No').
debug_msg_aux( Msg1,  Msg2, Msg3) :-
	do_debug_rfuzzy('Yes'),
	write('['), write(Msg1), 
	write(Msg2),
	write(']: '),  write(Msg3),
	write('    ').

debug_msg_list(_Msg1, _Msg2) :- do_debug_rfuzzy('No').
debug_msg_list(Msg1, []) :-
	do_debug_rfuzzy('Yes'),
	debug_msg_aux(Msg1, ' (list)', ' (empty)').
debug_msg_list(Msg1, Msg2) :-
	do_debug_rfuzzy('Yes'),
	debug_msg_list_aux(Msg1, ' (list)', Msg2).

debug_msg_list_aux(Msg1, Msg2, []) :- !,
	debug_msg_aux(Msg1, Msg2, '[ ]'),
	debug_nl.
debug_msg_list_aux(Msg1, Msg2, [Msg3|Msg3_List]) :-
	debug_msg_aux(Msg1, Msg2, Msg3),
	debug_nl,
	debug_msg_list_aux(Msg1, Msg2, Msg3_List).


debug_nl :- do_debug_rfuzzy('No').
debug_nl :- do_debug_rfuzzy('Yes'), write('\n').

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

rfuzzy_property_error(X) :-
	write('Error on rfuzzy property: '),
	write(X), nl, 
	!, % Backtracking is not allowed here.
	fail. % Don't continue
	
rfuzzy_warning_msg(Function, Error, Msg) :-
	write('WARNING: in \"'),
	write(Function), write('\" '),
	write(Error), write(' '), write(Msg),
	nl.

rfuzzy_error_msg(Function, Error, Msg) :-
	write('ERROR: in \"'),
	write(Function), write('\" '),
	write(Error), write(' '), write(Msg),
	nl.
%	!, fail. % Finally fail.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------
