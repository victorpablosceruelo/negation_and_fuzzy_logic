:- module(rfuzzy_rt, [defined_aggregators/1, 
	inject/3, merge/4, prod/3, iprod/3, 
	min/3, luka/3, dprod/3, max/3, dluka/3, complement/3,
	mean/3, supreme/2,
	'=>'/4,
	print_msg/3, print_msg_nl/1, activate_rfuzzy_debug/0,
	rfuzzy_conversion_in/2, rfuzzy_conversion_out/2 ],[hiord]).

:- use_module(library(write),[write/1]).
:- use_package(clpr).
:- use_module(library(terms),[copy_args/3]).

% ---------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------

% REMOVED: preinject/3,postinject/4, id/2, id/3, id (in defined_aggregators), 

defined_aggregators([min, max, prod, iprod, dprod, luka, dluka, complement, mean]).

min(X,Y,Z):- X .=<. Y, X .=. Z .
min(X,Y,Z):- X .>. Y, Y .=. Z .

max(X,Y,Z):- X .>=. Y, X .=. Z .
max(X,Y,Z):- Y .>. X, Y .=. Z .

prod(X,Y,M):- M .=. X * Y.
iprod(X,Y,M):- M .=. 1 - (X * Y).
dprod(X,Y,M):- M .=. X + Y - (X * Y).

luka(X,Y,M):- 
	Temp .=. X + Y  - 1, 
	max(0, Temp, M).

dluka(X,Y,M):- 
	Temp .=. X + Y,
	min(1, Temp, M).

complement(X, C, Z) :-
	Temp1 .=. C - X,
	min(1, Temp1, Temp2),
	max(0, Temp2, Z).

mean(X, Y, Z) :- Z .=. (X + Y) / 2.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

supreme([], _Element) :- !, 
	print_msg('debug', 'supreme', 'list is empty. FAIL.'),
	!, fail.
supreme(List, Element) :-
	print_msg('debug', 'supreme', List),
	filter_out_repeated(List, List_Aux_1), !,
	print_msg('debug', 'supreme_without_repetitions', List_Aux_1),
	reorder_elements(List_Aux_1, [], List_Aux_2), !,
	print_msg('debug', 'supreme_reordered', List_Aux_2),
	take_an_element(List_Aux_2, Element).

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
	

% ---------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------
 
%:- meta_predicate preinject(?,pred(2),?).
%
%id(L,L).
%
%preinject([],_,[]):-!.
%preinject(L,P,T):- P(L,T).

:- meta_predicate inject(?,pred(3),?).

inject([],_,_).
inject([T],_,T).
inject([X,Y|Rest],P,T):-
	P(X,Y,T0),
	inject([T0|Rest],P,T).

%:- meta_predicate postinject(?,?,pred(3),?).
%
%id(_,V,V).
%postinject([],A,_,A):-!.
%postinject(L,V,P,T):- P(L,V,T).


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

:- data print_msg_level/1.

activate_all_rfuzzy_print_msg_level :-
	assertz_fact(print_msg_level('info')), % An intermediate level
	assertz_fact(print_msg_level('warning')), % The level printing less	
	assertz_fact(print_msg_level('error')), % The level printing less
	assertz_fact(print_msg_level('configured')). % The level printing less

% This is to enable debug. Deactivated by default.
activate_rfuzzy_debug :-	
	assertz_fact(print_msg_level('debug')). % The lowest level

% Main predicate in charge of printing.
print_msg(Level, Msg1, Msg2) :- 
	\+(print_msg_level('configured')), !,
	activate_all_rfuzzy_print_msg_level,
	print_msg(Level, Msg1, Msg2).
print_msg(Level, Msg1, Msg2) :- 
	print_msg_level('configured'),
	print_msg_level(Level), !,
	translate_level_to_pre_msg1(Level, Pre_Msg1),
	print_msg_aux(Pre_Msg1, Msg1, [], Msg2),
	print_msg_nl(Level).
print_msg(_Level, _Msg1, _Msg2) :- 
	print_msg_level('configured'), !. 

translate_level_to_pre_msg1('debug', 'DEBUG: ') :- !.
translate_level_to_pre_msg1('info', 'INFO: ') :- !.
translate_level_to_pre_msg1('warning', 'WARNING: ') :- !.
translate_level_to_pre_msg1('error', 'ERROR: ') :- !.
translate_level_to_pre_msg1('', '') :- !.

% This gets rid of lists.
print_msg_aux(Pre_Msg1, Msg1, Msg1_Info, []) :- !,
	print_msg_real(Pre_Msg1, Msg1, [ ' (list)' | Msg1_Info ], ' (empty)').
print_msg_aux(Pre_Msg1, Msg1, Msg1_Info, [ Msg2_Head | Msg2_Tail ]) :- !,
	print_msg_aux(Pre_Msg1, Msg1, [ ' (list)' | Msg1_Info ], Msg2_Head),
	print_msg_nl('error'), % Print it always.
	print_msg_aux(Pre_Msg1, Msg1, Msg1_Info, Msg2_Tail).
print_msg_aux(Pre_Msg1, Msg1, Msg1_Info, Msg2) :- !,
	print_msg_real(Pre_Msg1, Msg1, Msg1_Info, Msg2).

% Predicate that really prints.
print_msg_real(Pre_Msg1, Msg1,  Msg1_Info, Msg2) :-
	write(Pre_Msg1), 
	write(Msg1), 
	print_msg1_info(Msg1_Info),
	write(':  '),  write(Msg2),
	write('    ').

% Print msg1 Info (in reverse order to show the structure).
print_msg1_info([]) :- !.
print_msg1_info([Head | Tail]) :- !,
	print_msg1_info(Tail),
	write(' '),
	write(Head).

print_msg_nl(Level) :- print_msg_level(Level), !, nl.
print_msg_nl(_Level) :- !.

