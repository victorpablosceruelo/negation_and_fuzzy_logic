:- module(rfuzzy_rt, [
	% Rfuzzy Comparisons.
	rfuzzy_compute_defined_comparators/1,
	rfuzzy_compute_aux/6,	
	% Aggregators.
	rfuzzy_defined_connectives/1, 
	inject/3, merge/4, prod/3, iprod/3, mean/3, 
	min/3, luka/3, dprod/3, max/3, dluka/3, complement/3,
	% Modifiers.
	rfuzzy_defined_modifiers/1,
	rfuzzy_defined_negation_ops/1,
	% Auxiliar predicates.
	print_msg/3, print_msg_nl/1, activate_rfuzzy_debug/0,
	rfuzzy_conversion_in/2, rfuzzy_conversion_out/2,
	supreme/2, reorder_by_truth_value/3, 
	one_by_one_first_head/2, one_by_one_first_tail/2,
	rfuzzy_process_attribute_dump/4,
	append_local/3, memberchk_local/2, remove_list_dupplicates/3,
	sets_union/3, lists_substraction/3,
	isUserNameLocalUserNameAux/3,
	assertLocalUserNameAux/1,
	add_preffix_to_name/3,
	rfuzzy_var_truth_value/3
		     ],[hiord]).

:- use_module(library(write),[write/1]).
:- use_package(clpr).
:- use_module(library(terms),[copy_args/3]).

:- data localUserName/1.
	
% ---------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------
% ---------------------------------------------------------------------------------------------------

% REMOVED: preinject/3,postinject/4, id/2, id/3, id (in defined_connectives), 

rfuzzy_defined_connectives([min, max, prod, iprod, dprod, luka, dluka, complement, mean]).

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
supreme(List_In, Answer) :-
	functor(Answer, _Name, Arity), 
	Arguments_Arity is Arity -1,
	Truth_Value_Arity is Arity +1,
	
	print_msg('ultradebug', 'supreme :: List_In', List_In),
	supreme_aux_1(List_In, List_Aux_1), !,
	print_msg('ultradebug', 'supreme :: List_Aux_1', List_Aux_1),
	reorder_by_truth_value(List_Aux_1, [], List_Aux_2),
	print_msg('ultradebug', 'supreme :: List_Aux_2', List_Aux_2),
	one_by_one_first_head(List_Aux_2, Element),
	print_msg('debug', 'supreme :: element_taken', Element),

	copy_args(Arguments_Arity, Element, Answer),
	arg(Truth_Value_Arity, Element, Truth_Value),
	arg(Arity, Answer, Truth_Value).
	

% Head is only kept if it is the supreme.
supreme_aux_1([], []) :- !.
supreme_aux_1([Head | Tail_In], [Head | List_Out]) :- 
	print_msg_nl('ultradebug'),
	print_msg('ultradebug', 'supreme_aux_1 :: Head', Head),
	supreme_aux_2(Head, Tail_In, Tail_Out), !,
	supreme_aux_1(Tail_Out, List_Out).
supreme_aux_1([_Head | Tail_In], List_Out) :-
	supreme_aux_1(Tail_In, List_Out).

supreme_aux_2(_Head, [], []) :- !.
supreme_aux_2(Head, [Next | Tail_In], Tail_Out) :-
	split_out_fuzzy_functor_args(Head, Prio_1, TV_1, Args_1),
	split_out_fuzzy_functor_args(Next, Prio_2, TV_2, Args_2),

	Args_1 = Args_2, !, % They are for the same fuzzy values.
	print_msg('ultradebug', 'supreme_aux_2', 'equal args'),
	(
	    (	Prio_1 .>. Prio_2  )
	;
	    (   Prio_1 .=. Prio_2, TV_1 .>=. TV_2  )
	), !,
	print_msg('ultradebug', 'supreme_aux_2', 'higher Prio or TV.'),
	supreme_aux_2(Head, Tail_In, Tail_Out).
supreme_aux_2(Head, [Next | Tail_In], [Next | Tail_Out]) :-
	supreme_aux_2(Head, Tail_In, Tail_Out).

reorder_by_truth_value([], List_In, List_In) :- !.
reorder_by_truth_value([Head_1 | Tail], List_In, List_Out) :-
	reorder_by_truth_value_aux(Head_1, List_In, List_Aux), !,
	reorder_by_truth_value(Tail, List_Aux, List_Out).
	
reorder_by_truth_value_aux(Head_1, [], [Head_1]) :- !.
reorder_by_truth_value_aux(Head_1, [ Head_2 | Tail_In ], [ Head_2 | Tail_Out ]) :-
	has_less_truth_value(Head_1, Head_2), !,
	reorder_by_truth_value_aux(Head_1, Tail_In, Tail_Out).
reorder_by_truth_value_aux(Head_1, [ Head_2 | Tail_In ], [ Head_1, Head_2 | Tail_In ]) :- !.

has_less_truth_value(Head_1, Head_2) :-
	print_msg('debug', 'has_less_truth_value(Head_1, Head_2)', (Head_1, Head_2)),
	
	functor(Head_1, _Name_1, Arity_1), 
	functor(Head_2, _Name_2, Arity_2), 
	arg(Arity_1, Head_1, TV_1),
	arg(Arity_2, Head_2, TV_2),
	has_less_truth_value_aux(TV_1, TV_2).

has_less_truth_value_aux(TV_1, TV_2) :-
%	var(TV_1), var(TV_2),
	print_msg('debug', 'has_less_truth_value_aux(TV_1, TV_2)', (TV_1, TV_2)),
	TV_1 .<. TV_2, !,
	print_msg('debug', 'has_less_truth_value_aux(TV_1, TV_2)', 'yes'), !.

has_less_truth_value_aux(TV_1, TV_2) :-
%	var(TV_1), var(TV_2),
	TV_1 .>=. TV_2, !,
	print_msg('debug', 'has_less_truth_value_aux(TV_1, TV_2)', 'no'),
	!, fail.

has_less_truth_value_aux(TV_1, TV_2) :-
	print_msg('error', 'has_less_truth_value_aux(TV_1, TV_2)', (TV_1, TV_2)),
	(   (   nonvar(TV_1), print_msg('error', 'Dump_TV_1', 'nonvar'))
	;
	    (   dump_constraints(TV_1, TV_1, Dump_TV_1),
		print_msg('error', 'Dump_TV_1', (Dump_TV_1)))
	),
	(   (   nonvar(TV_2), print_msg('error', 'Dump_TV_2', 'nonvar'))
	;
	    (   dump_constraints(TV_2, TV_2, Dump_TV_2),
		print_msg('error', 'Dump_TV_2', (Dump_TV_2)))
	), !, fail.

split_out_fuzzy_functor_args(Head, Prio, TV, Other_Args) :-
%	print_msg('debug', 'split_out_fuzzy_functor_args(Head)', split_out_fuzzy_functor_args(Head)),
	copy_term(Head, Head_Copy),
	functor(Head_Copy, Name, _Arity), 
	Head_Copy=..[Name | Functor_Args],
	split_out_fuzzy_functor_args_aux(Functor_Args, Prio, TV, Other_Args), 
	print_msg('ultradebug', 'split_out_fuzzy_functor_args(Head, Prio, TV, Args)', split_out_fuzzy_functor_args(Head, Prio, TV, Other_Args)).

split_out_fuzzy_functor_args_aux([Prio, TV], Prio, TV, []) :- !.
split_out_fuzzy_functor_args_aux([Arg | Args_List_In], Prio, TV, [Arg | Args_List_Out]) :- 
	split_out_fuzzy_functor_args_aux(Args_List_In, Prio, TV, Args_List_Out).

one_by_one_first_head([Element|_List], Element).
one_by_one_first_head([_FirstElement|List], Element) :-
	one_by_one_first_head(List, Element).

one_by_one_first_tail([Element], Element) :- !.
one_by_one_first_tail([_FirstElement|List], Element) :-
	one_by_one_first_tail(List, Element).
one_by_one_first_tail([Element|_List], Element) :- !.

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

%=>(Formula,X,Y,M):- 
%	functor(X,_,Ax),
%	arg(Ax,X,Mx),
%	functor(Y,_,Ay),
%	arg(Ay,Y,My),
%	call(X),
%	call(Y),
%	call(Formula,Mx,My,M).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

rfuzzy_conversion_in(X, Y) :-
	nonvar(X),
	X .=. Y.
rfuzzy_conversion_in(X, _Y) :-
	\+(nonvar(X)).

rfuzzy_conversion_out(rat(X, Y), (X/Y)) :- !.
rfuzzy_conversion_out(X, X) :-
	number(X).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

rfuzzy_process_attribute_dump([Dump], Var, Condition, Value) :-
	functor(Dump, Condition, Arity),
	Arity = 2,
	arg(1, Dump, Var), 
	arg(2, Dump, Value), 
	!. 

rfuzzy_process_attribute_dump(Dump, _Var, _Condition, _Value) :-
	print_msg('error', 'rfuzzy_process_attribute_dump :: Dump', Dump),
	!, fail.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

rfuzzy_compute_defined_comparators([('=~=', 'rfuzzy_enum_type'), ('=', 'rfuzzy_any_type'), ('=/=', 'rfuzzy_any_type'), ('>', 'rfuzzy_number_type'), ('<', 'rfuzzy_number_type'), ('>=', 'rfuzzy_number_type'), ('=<', 'rfuzzy_number_type')]).

rfuzzy_compute_aux_args_are_numbers(Args_Type) :-
	(
	    Args_Type = 'rfuzzy_truth_value_type'
	;
	    Args_Type = 'rfuzzy_number_type'
	;
	    Args_Type = 'rfuzzy_integer_type'
	;
	    Args_Type = 'rfuzzy_float_type'
	), !.

rfuzzy_compute_aux_args_are_not_numbers(Args_Type) :-
	Args_Type \== 'rfuzzy_truth_value_type',
	Args_Type \== 'rfuzzy_number_type',
	Args_Type \== 'rfuzzy_integer_type',
	Args_Type \== 'rfuzzy_float_type',
	!.

rfuzzy_compute_aux(Operator, Args_Type, _Elt1, _Elt2, _Computed_Similarities, _Truth_Value) :-
	(   var(Operator) ; var(Args_Type)   ), !, fail.

rfuzzy_compute_aux('=', Args_Type, Elt1, Elt2, _Computed_Similarities, Truth_Value) :- !,
	(
	    (
		rfuzzy_compute_aux_args_are_numbers(Args_Type), 
		Elt1 .=. Elt2, !,	Truth_Value .=. 1    
	    )
	;
	    (   
		rfuzzy_compute_aux_args_are_not_numbers(Args_Type), 
		Elt1 = Elt2, !, Truth_Value .=. 1    
	    )
	;
	    (	Truth_Value .=. 0, !    )
	).

rfuzzy_compute_aux('=/=', Args_Type, Elt1, Elt2, _Computed_Similarities, Truth_Value) :- !,
	(
	    (	
		rfuzzy_compute_aux_args_are_numbers(Args_Type), 
		Elt1 .=. Elt2, !,	Truth_Value .=. 0    
	    )
	;
	    (	
		rfuzzy_compute_aux_args_are_not_numbers(Args_Type), 
		Elt1 = Elt2, !,	Truth_Value .=. 0    
	    )	    
	;
	    (	Truth_Value .=. 1, !    )
	).

rfuzzy_compute_aux('>', Args_Type, Elt1, Elt2, _Computed_Similarities, Truth_Value) :- !,
	rfuzzy_compute_aux_args_are_numbers(Args_Type), 
	(
	    (	Elt2 .>. Elt1, !,	Truth_Value .=. 1    )
	;
	    (	Truth_Value .=. 0, !    )
	).
rfuzzy_compute_aux('<', Args_Type, Elt1, Elt2, _Computed_Similarities, Truth_Value) :- !,
	rfuzzy_compute_aux_args_are_numbers(Args_Type), 
	(
	    (	Elt2 .<. Elt1, !,	Truth_Value .=. 1    )
	;
	    (	Truth_Value .=. 0    )
	).
rfuzzy_compute_aux('>=', Args_Type, Elt1, Elt2, _Computed_Similarities, Truth_Value) :- !,
	rfuzzy_compute_aux_args_are_numbers(Args_Type), 
	(
	    (	Elt2 .>=. Elt1, !,	Truth_Value .=. 1    )
	;
	    (	Truth_Value .=. 0, !    )
	).
rfuzzy_compute_aux('=<', Args_Type, Elt1, Elt2, _Computed_Similarities, Truth_Value) :- !,
	rfuzzy_compute_aux_args_are_numbers(Args_Type), 
	(
	    (	Elt2 .=<. Elt1, !,	Truth_Value .=. 1    )
	;
	    (	Truth_Value .=. 0, !    )
	).
rfuzzy_compute_aux('=~=', Args_Type, Elt1, Elt2, Computed_Similarities, Truth_Value) :- 
	rfuzzy_compute_aux_args_are_not_numbers(Args_Type), 
	Format = rfuzzy_computed_similarity_between(_Database, Elt1, Elt2, TV, Cred_Op, Cred),
	(
	    (
		memberchk_local(Format, Computed_Similarities), !,
		functor(Cred_Functor, Cred_Op, 3),
		arg(1, Cred_Functor, TV), 
		arg(2, Cred_Functor, Cred), 
		arg(3, Cred_Functor, Truth_Value),
		call(Cred_Functor)
	    )
	;
	    (
		Truth_Value .=. 0, !
	    )
	).

rfuzzy_compute_aux(Operator, Args_Type, _Elt1, _Elt2, _Computed_Similarities, Truth_Value) :- !,
	Truth_Value .=. -1,
	print_msg('error', 'rfuzzy_compute_aux :: Unknown operator or args type', (Operator, Args_Type)), !, fail.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% rfuzzy_defined_modifiers([]).
rfuzzy_defined_modifiers([(fairly, 2, TV_In, TV_Out, (TV_Out .=. TV_In))]).

rfuzzy_defined_negation_ops([(fnot, 2, TV_In, TV_Out, (TV_Out .=. 1 - TV_In))]).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

% This could be useful if we could execute the subbody of modifiers as if they were
% clauses' bodies, but they are not. If we have a composed subbody there is no simple 
% way to detect if we are running a condition or another subbody. We cannot accept that.
%
%clpqr_op('.=.', [A, B]) :- A .=. B.
%clpqr_op('.>=.', [A, B]) :- A .>=. B.
%clpqr_op('.=<.', [A, B]) :- A .<=. B.
%clpqr_op('.>.', [A, B]) :- A .>. B.
%clpqr_op('.<.', [A, B]) :- A .<. B.
%clpqr_op('.<>.', [A, B]) :- A .<>. B.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

sets_union(Set1, Set2, Result_Set) :-
	list(Set1), list(Set2), list(Result_Set),
	sets_union_aux(Set1, Set2, Result_Set).
sets_union(Set1, Set2, Result_Set) :-
	print_msg('error', 'sets_union :: not a list error', (Set1, Set2, Result_Set)), !, fail.

sets_union_aux([], Set2, Set2) :- !.
sets_union_aux([Member1 | Set1], Set2, Set3) :-
	memberchk_local(Member1, Set2), !,
	sets_union_aux(Set1, Set2, Set3).
sets_union_aux([Member1 | Set1], Set2, [Member1 | Set3]) :-
	sets_union_aux(Set1, Set2, Set3).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

lists_substraction([], _List_2, []) :- !.
lists_substraction([Head | Tail ], List_2, Result_List) :-
	memberchk_local(Head, List_2), !, 
	lists_substraction(Tail, List_2, Result_List).
lists_substraction([Head | Tail ], List_2, [Head | Result_List]) :-
	lists_substraction(Tail, List_2, Result_List).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

append_local([], N2, N2).
append_local([Elto|N1], N2, [Elto|Res]) :-
	append_local(N1, N2, Res).

memberchk_local(Element, [Element | _Tail]) :- !.
memberchk_local(Element, [_Head | Tail]) :- !,
	memberchk_local(Element, Tail).

% remove_list_dupplicates(List_In, List_Aux, List_Out)
remove_list_dupplicates([], List_Aux, List_Aux) :- !.
remove_list_dupplicates([Element|List_In], List_Aux, List_Out) :-
	memberchk_local(Element, List_Aux), !,
	remove_list_dupplicates(List_In, List_Aux, List_Out).
remove_list_dupplicates([Element|List_In], List_Aux, List_Out) :-
	remove_list_dupplicates(List_In, [Element|List_Aux], List_Out).

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

rfuzzy_var_truth_value(Var, Condition, Value) :-
	var(Var),					 
	print_msg('debug', 'rfuzzy_var_truth_value :: Var', Var),
	dump_constraints(Var, Var, Dump), !,
	print_msg('debug', 'rfuzzy_var_truth_value :: dump_constraints :: Dump', Dump),
	rfuzzy_process_attribute_dump(Dump, Var, Condition, Value),
	!.
rfuzzy_var_truth_value(Var, 'constant', Var) :- 
	nonvar(Var), 
	print_msg('ultradebug', 'rfuzzy_var_truth_value :: Constant', Var),
	!.
rfuzzy_var_truth_value(Var, 'error', 0) :-
	print_msg('error', 'rfuzzy_var_truth_value :: ERROR :: unknown', Var),
	!.

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

% This gets rid of lists. Be careful with variables !!!
print_msg_aux(Pre_Msg1, Msg1, Msg1_Info, Var) :- 
	var(Var), !,
	print_msg_real(Pre_Msg1, Msg1, Msg1_Info, Var).
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
	write('    '), 
	print_msg_attributes(Msg2).

print_msg_attributes(Msg2) :-
	rfuzzy_rt:varsbag(Msg2, [], [], Vars),
	print_vars_attributes(Vars).

print_vars_attributes([]) :- !.
print_vars_attributes([Var|Vars]) :-
	dump_constraints(Var, Var, Dump), !,
	write(Dump), !, 
	print_vars_attributes(Vars).
print_vars_attributes([_Var|Vars]) :-
	print_vars_attributes(Vars).

% Print msg1 Info (in reverse order to show the structure).
print_msg1_info([]) :- !.
print_msg1_info([Head | Tail]) :- !,
	print_msg1_info(Tail),
	write(' '),
	write(Head).

print_msg_nl(Level) :- print_msg_level(Level), !, nl.
print_msg_nl(_Level) :- !.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

varsbag(X, OtherBag, Bag_In, Bag_Out) :- 
        var(X), !,
	% print_msg('debug', 'varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)', varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)),
	varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out),
	% print_msg('debug', 'varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)', varsbag_local_variable(X, OtherBag, Bag_In, Bag_Out)),
	!.

varsbag(Term, OtherBag, Bag_In, Bag_Out) :-
	functor(Term, _Name, Arity),
        varsbag_go_inside(Arity, Term, OtherBag, Bag_In, Bag_Out).

varsbag_go_inside(0, _Term, _OtherBag, Bag, Bag) :- !.
varsbag_go_inside(Arity, Term, OtherBag, Bag_In, Bag_Out) :-
        NewArity is Arity-1,
        arg(Arity, Term, Argument),
        rfuzzy_rt:varsbag(Argument, OtherBag, Bag_In, Bag_Tmp), !,
        varsbag_go_inside(NewArity, Term, OtherBag, Bag_Tmp, Bag_Out).

varsbag_local_variable(X, OtherBag, Bag, Bag) :-
	var(X),
	rfuzzy_rt:memberchk(X, OtherBag), % Var is in the other bag.
	!.
varsbag_local_variable(X, _OtherBag, Bag, Bag) :- 
	var(X),
	rfuzzy_rt:memberchk(X, Bag), % Var is alredy in the bag.
	!.
varsbag_local_variable(X, _OtherBag, Bag, [X|Bag]) :- 
	var(X),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% From Ciao Prolog (idlists library).
% memberchk(X, [Y|_]) :- X == Y, !.
% memberchk(X, [_|L]) :- memberchk(X, L).

memberchk(_T1, []) :- !, fail.
memberchk(T1, [T2]) :-
	terms_are_equal(T1,T2).
memberchk(T1, [T2|_L]) :- 
	terms_are_equal(T1,T2).
memberchk(T1, [_T2|L]) :- 
        memberchk(T1, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

terms_are_equal(T1, T2) :- % Same var
        var(T1),
        var(T2),
        T1==T2,!.
terms_are_equal(T1, T2) :- % Different vars.
        (   var(T1) ;
            var(T2)),
        !, fail. % Fail

terms_are_equal(T1, T2) :- % For functors.
        functor_local(T1, Name1, Arity1, Args1),
        functor_local(T2, Name2, Arity2, Args2),
	Name1 == Name2,
	Arity1 == Arity2,
        !,
        terms_are_equal_list(Args1, Args2).

terms_are_equal(_T1, _T2) :- % Other things
%       write(terms_are_equal(T1, T2)), write(' ERROR '), nl,
        !, fail. % Fail

terms_are_equal_list([],[]) :- !. % Empty lists.
terms_are_equal_list([T1|L1],[T2|L2]) :- !, % Lists.
        terms_are_equal(T1, T2),
        terms_are_equal_list(L1,L2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
functor_local(Head, Name, Arity, Args) :-
        functor(Head, Name, Arity),
        Arity > 0, !,
        Head=..[Name|Args].

functor_local(Head, Name, Arity, []) :-
        functor(Head, Name, Arity),
        Arity == 0.

% ------------------------------------------------------
% ------------------------------------------------------
% ------------------------------------------------------

add_preffix_to_name(Input, Preffix, Output) :-
	(
	    (	nonvar(Input), nonvar(Preffix), atom(Input), atom(Preffix), !    )
	;
	    (	print_msg('error', 'add_preffix_to_name :: not atoms :: (Input, Preffix)', (Input, Preffix)), !, fail    )
	),
	atom_codes(Input, Input_Chars),
	atom_codes(Preffix, Preffix_Chars),
	string(Input_Chars),
	string(Preffix_Chars),
	(
	    (	Preffix_Chars = "", !, New_Input_Chars = Input_Chars    )
	;
	    (   append_local("_", Input_Chars, New_Input_Chars)    )
	),  
%	print_msg('debug', 'add_preffix_to_name :: Preffix_Chars', Preffix_Chars),
%	print_msg('debug', 'add_preffix_to_name :: New_Input_Chars', New_Input_Chars),
	append_local(Preffix_Chars, New_Input_Chars, Output_Chars),
%	print_msg('debug', 'add_preffix_to_name :: Output_Chars', Output_Chars),
	atom_codes(Output, Output_Chars), 
	atom(Output), !,
	print_msg('debug', 'add_preffix_to_name :: Output', Output).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assertLocalUserNameAux(UserName) :-
	assertz_fact(localUserName(UserName)).
isUserNameLocalUserNameAux(UserName, Kind, Result) :-
	localUserName(CurrentUserName),
	compareUserNames(UserName, CurrentUserName, Kind, Result).

%compareUserNames(UserName, CurrentUserName, Kind) :-

compareUserNames(X, Y, 'variables', 'ok') :- var(X), var(Y), !.
compareUserNames(X, _Y, 'var_vs_nonvar', 'error') :-
	var(X), !.
compareUserNames(_X, Y, 'nonvar_vs_var', 'error') :-
	var(Y), !.

compareUserNames(X, X, 'raw_comparison', 'ok') :- !.
compareUserNames(X, Y, 'functors', 'ok') :-
	functor(X, Name1, Arity1),
	functor(Y, Name2, Arity2),
	Name1 == Name2,
	Arity1 == Arity2, !.

compareUserNames(X, Y, 'distinct_functors_arity', 'error') :-
	functor(X, Name, _Arity1),
	functor(Y, Name, _Arity2), !.

compareUserNames(X, Y, Out, 'error') :-
	functor(X, Name1, Arity),
	functor(Y, Name2, Arity), !,
	add_preffix_to_name(Name1, Name2, Tmp),
	add_preffix_to_name(Tmp, 'distinct_functors_name', Out),
	!.

compareUserNames(X, Y, 'distinct_functors', 'error') :-
	functor(X, _Name1, _Arity1),
	functor(Y, _Name2, _Arity2), !.

compareUserNames(X, Y, 'functor_vs_ascii_list', 'error') :-
	functor(X, _Name, _Arity),
	isAsciiList(Y), !.

compareUserNames(X, _Y, 'functor_vs_unknown', 'error') :-
	functor(X, _Name, _Arity), !.

compareUserNames(X, Y, 'ascii_list_vs_functor', 'error') :-
	functor(Y, _Name, _Arity),
	isAsciiList(X), !.

compareUserNames(_X, Y, 'unknown_vs_functor', 'error') :-
	functor(Y, _Name, _Arity), !.

compareUserNames(X, Y, 'ascii_lists', 'ok') :-
	isAsciiList(X),
	isAsciiList(Y),
	compareAsciiLists(X, Y), !.

compareUserNames(X, _Y, 'ascii_list_vs_non_ascii_list', 'error') :-
	isAsciiList(X), !.

compareUserNames(_X, Y, 'non_ascii_list_vs_ascii_list', 'error') :-
	isAsciiList(Y), !.

compareUserNames(X, X, 'comparison_error', 'error') :- !.
	
isAsciiList(X) :- var(X), !, fail.
isAsciiList([]).
isAsciiList([X|Xs]) :-
	integer(X),
	isAsciiList(Xs).

compareAsciiLists([], []) :- !.
compareAsciiLists([X|Xs], [X|Ys]) :-
	compareAsciiLists(Xs, Ys).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
