:- module(build_adg_res, [argument_dependency_graph/8],
	    [resources(inferres_decl)]).

%
%  build_adg.pl			Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for building argument dependency graphs.
%
%  We assume the Prolog execution order and ground bindings
%  in current implementation.
%
:- use_module(resources(resources_basic)).
:- use_module(resources(init_res(builtin_res)),
	    [
		legal_pred_arg/1,
		second_order_predicate/1,
		second_order_predicate_pred_arg/2,
		second_order_predicate_pred_num/3
	    ]).
:- use_module(resources(init_res(symtable_res)),
	    [
		find_symbol_field/4,
		literal_property/10
	    ]).
:- use_module(resources(solution_res(relation_res)), [fail_body/1]).
:- use_module(resources(dependency_res(gvars_res)),
	    [
		close_gvars_list/1,
		insert_gvars_field/4,
		find_gvars_field/4
	    ]).
:- use_module(resources(dependency_res(position_res)), [new_pos/3]).
:- use_module(resources(dependency_res(adg_res)),
	    [
		insert_adg_entry/3,
		insert_adg_field/4
	    ]).
:- use_module(resources(top_res(utility_res)), [compound/1]).
:- use_module(resources(top_res(error_res)),   [error_message/3]).
:- use_module(program(clidtypes),              [clausebody/1]).

:- pred argument_dependency_graph/8 :: clause_t * clause_ppkey_t * atm
	* list * list * list * list * int + (not_fails, is_det) #
	"Build the argument dependency graph of a clause.".

argument_dependency_graph(rule, ClausePPKey, Key, BT, ST, Adg, Gvars, Error) :-
	clause_head_body(ClausePPKey, Head, Body),
	functor(Head, F, N),
	find_symbol_field(ST, F/N, (mode), Mode),
	adg_head_input(1, N, Head, Mode, Adg, Gvars),
	adg_body(Body, 1, BT, ST, Adg, Gvars, ClausePPKey, Key, Error1),
	( fail_body(Body) ->
	    Error2 = 1
	; adg_head_output(1, N, Head, Mode, Adg, Gvars, ClausePPKey, Error2) ),
	Error is Error1 * Error2,
	close_gvars_list(Gvars).
argument_dependency_graph(fact, ClausePPKey, _, _, ST, Adg, Gvars, Error) :-
	clause_head(ClausePPKey, Head),
	functor(Head, F, N),
	find_symbol_field(ST, F/N, (mode), Mode),
	adg_head_input(1, N, Head, Mode, Adg, Gvars),
	adg_head_output(1, N, Head, Mode, Adg, Gvars, ClausePPKey, Error),
	close_gvars_list(Gvars).

%
%  Insert the input positions of the head into the argument dependency
%  graph, and insert the variables in input positions into the ground
%  variables list.
%
:- pred adg_head_input/6 :: nnegint * nnegint * callable * list(mode_t)
	* list(adg_t) * term + (not_fails, is_det).
adg_head_input(M, N, _, _, _, _) :-
	M > N,
	!.
adg_head_input(M, N, Head, [(+)|Mode], Adg, Gvars) :-
	M =< N,
	!,
	new_pos(0, M, Pos),
	insert_adg_entry(Adg, Pos, _),
	insert_adg_field(Adg, Pos, (mode), +),
	arg(M, Head, Arg),
	insert_ground_vars(Gvars, Arg, Pos),
	M1 is M + 1,
	adg_head_input(M1, N, Head, Mode, Adg, Gvars).
adg_head_input(M, N, Head, [(-)|Mode], Adg, Gvars) :-
	M =< N,
	M1 is M + 1,
	adg_head_input(M1, N, Head, Mode, Adg, Gvars).

%
%  Insert the variables in an argument into the ground variables list.
%
insert_ground_vars(Gvars, Arg, Pos) :-
	var(Arg),
	!,
	insert_gvars_field(Gvars, Arg, def, Pos).
insert_ground_vars(_, Arg, _) :-
	atomic(Arg),
	!.
insert_ground_vars(Gvars, Arg, Pos) :-
	compound(Arg),
	functor(Arg, _, N),
	insert_ground_vars(1, N, Gvars, Arg, Pos).

:- push_prolog_flag(multi_arity_warnings, off).

insert_ground_vars(M, N, _, _, _) :-
	M > N,
	!.
insert_ground_vars(M, N, Gvars, Arg, Pos) :-
	M =< N,
	arg(M, Arg, Arg1),
	insert_ground_vars(Gvars, Arg1, Pos),
	M1 is M + 1,
	insert_ground_vars(M1, N, Gvars, Arg, Pos).

%
%  Insert the output positions of the head and the edges to their 
%  predecessors into the argument dependency graph. 
%
:- pred adg_head_output/8 :: num * num * term * list * list * list *
	clause_ppkey_t * num + (not_fails, is_det).
adg_head_output(M, N, _, _, _, _, _, 1) :-
	M > N,
	!.
adg_head_output(M, N, Head, [(+)|Mode], Adg, Gvars, ClausePPKey, Error) :-
	M =< N,
	!,
	M1 is M + 1,
	adg_head_output(M1, N, Head, Mode, Adg, Gvars, ClausePPKey, Error).
adg_head_output(M, N, Head, [(-)|Mode], Adg, Gvars, ClausePPKey, Error) :-
	M =< N,
	new_pos(0, M, Pos),
	insert_adg_entry(Adg, Pos, _),
	insert_adg_field(Adg, Pos, (mode), -),
	arg(M, Head, Arg),
	find_adg_predecessor(Adg, Gvars, Arg, Pos, ClausePPKey, Error1),
	M1 is M + 1,
	adg_head_output(M1, N, Head, Mode, Adg, Gvars, ClausePPKey, Error2),
	Error is Error1 * Error2.

%
%  Find the predecessors of an argument.
%
find_adg_predecessor(Adg, Gvars, Arg, Pos, ClausePPKey, Error) :-
	var(Arg),
	!,
	find_gvars_field(Gvars, Arg, def, PosList),
	( var(PosList) ->
	    ( error_message(bound1, (Arg, Pos), ClausePPKey),
		Error = 0 ) ;
	    ( insert_gvars_field(Gvars, Arg, use, Pos),
		insert_adg_predecessor(Adg, Pos, PosList),
		Error = 1 ) ).
find_adg_predecessor(_, _, Arg, _, _, 1) :-
	atomic(Arg),
	!.
find_adg_predecessor(Adg, Gvars, Arg, Pos, ClausePPKey, Error) :-
	compound(Arg),
	functor(Arg, _, N),
	find_adg_predecessor(1, N, Adg, Gvars, Arg, Pos, ClausePPKey, Error).

find_adg_predecessor(M, N, _, _, _, _, _, 1) :-
	M > N,
	!.
find_adg_predecessor(M, N, Adg, Gvars, Arg, Pos, ClausePPKey, Error) :-
	M =< N,
	arg(M, Arg, Arg1),
	find_adg_predecessor(Adg, Gvars, Arg1, Pos, ClausePPKey, Error1),
	M1 is M + 1,
	find_adg_predecessor(M1, N, Adg, Gvars, Arg, Pos, ClausePPKey, Error2),
	Error is Error1 * Error2.

%
%  Insert the edges to the predecessors of an argument into 
%  the argument dependency graph. 
%
insert_adg_predecessor(_, _, PosList) :-
	var(PosList),
	!.
insert_adg_predecessor(Adg, Pos, PosList) :-
	nonvar(PosList),
	PosList = [P|PList],
	insert_adg_field(Adg, Pos, pred, P),
	insert_adg_field(Adg, P,   succ, Pos),
	insert_adg_predecessor(Adg, Pos, PList).

%
%  Insert information about the body into the argument dependency
%  graph and the ground variables list.
%
:- pred adg_body/9 :: clausebody * num * list * list * list * list *
	clause_ppkey_t * atm * num + (not_fails, is_det).
adg_body((LitPPKey, Body), LitNum, BT, ST, Adg, Gvars, ClausePPKey, Key,
	    Error) :-
	!,
	lit_ppkey(LitPPKey, Lit, PPKey),
	adg_literal(Lit, PPKey, LitNum, BT, ST, Adg, Gvars, ClausePPKey, Key,
	    Error1),
	( Error1 =:= 1 ->
	    ( LitNum1 is LitNum + 1,
		adg_body(Body, LitNum1, BT, ST, Adg, Gvars, ClausePPKey, Key,
		    Error2),
		Error = Error2 ) ;
	    Error = Error1 ).
adg_body(LitPPKey, LitNum, BT, ST, Adg, Gvars, ClausePPKey, Key, Error) :-
	lit_ppkey(LitPPKey, Lit, PPKey),
	adg_literal(Lit, PPKey, LitNum, BT, ST, Adg, Gvars, ClausePPKey,
	    Key, Error).

%
%  Insert information about a literal into the argument dependency
%  graph and the ground variables list.
%
:- pred adg_literal/10 + (not_fails, is_det).
adg_literal(Lit, PPKey, LitNum, BT, ST, Adg, Gvars, ClausePPKey, Key, Error) :-
	functor(Lit, F, A),
	( second_order_predicate(F/A) ->
	    adg_literal_1(Lit, PPKey, LitNum, BT, ST, Adg, Gvars, ClausePPKey,
		Key, Error) ;
	    adg_literal_2(Lit, PPKey, LitNum, BT, ST, Adg, Gvars, ClausePPKey,
		Key, Error)
	).

:- pred adg_literal_1/10 + (not_fails, is_det).
adg_literal_1(Lit, PPKey, LitNum, BT, ST, Adg, Gvars, ClausePPKey, Key, Error)
:- functor(Lit, findall, 3),
	second_order_predicate_pred_arg(Lit, Lit1),
	( legal_pred_arg(Lit1) ->
	    ( clause_body(ClausePPKey, Body),
		second_order_predicate_pred_num(Body, LitNum, Num),
		adg_literal(Lit1, PPKey, Num, BT, ST, Adg, Gvars, ClausePPKey,
		    Key, Error),
		new_pos(LitNum, 3, Pos), % add 3rd position into Adg
		insert_adg_entry(Adg, Pos, _), % and ignore 1st and 2nd pos.
		insert_adg_field(Adg, Pos, (mode), -),
		arg(3, Lit, Arg),
		insert_ground_vars(Gvars, Arg, Pos) ) ;
	    ( Error = 0,
		error_message(second_order1, Lit1, ClausePPKey) ) ).

:- pred adg_literal_2/10 + (not_fails, is_det).
adg_literal_2(Lit, PPKey, LitNum, BT, ST, Adg, Gvars, ClausePPKey, Key,
	    Error) :-
	functor(Lit, _F, N),
	literal_property(BT, ST, Lit, ClausePPKey, Key, PPKey, LitNum, mode,
	    _Bound, Mode),
	adg_literal_input(1, N, Lit, LitNum, Mode, NewMode, Adg, Gvars,
	    ClausePPKey, Error),
	find_internal_predecessor(NewMode, 1, LitNum, Pos),
	adg_literal_output(1, N, Lit, LitNum, NewMode, Pos, Adg, Gvars).

%
%  Insert the input positions of a literal and the edges to their 
%  predecessors into the argument dependency.
%
adg_literal_input(M, N, _, _, _, [], _, _, _, 1) :-
	M > N,
	!.
adg_literal_input(M, N, Lit, LitNum, [(+)|Mode], [(+)|NewMode],
	    Adg, Gvars, ClausePPKey, Error) :-
	M =< N,
	!,
	new_pos(LitNum, M, Pos),
	insert_adg_entry(Adg, Pos, _),
	insert_adg_field(Adg, Pos, (mode), +),
	arg(M, Lit, Arg),
	find_adg_predecessor(Adg, Gvars, Arg, Pos, ClausePPKey, Error1),
	M1 is M + 1,
	adg_literal_input(M1, N, Lit, LitNum, Mode, NewMode, Adg, Gvars,
	    ClausePPKey, Error2),
	Error is Error1 * Error2.
adg_literal_input(M, N, Lit, LitNum, [(-)|Mode], [(-)|NewMode],
	    Adg, Gvars, ClausePPKey, Error) :-
	M =< N,
	!,
	M1 is M + 1,
	adg_literal_input(M1, N, Lit, LitNum, Mode, NewMode, Adg, Gvars,
	    ClausePPKey, Error).
adg_literal_input(M, N, Lit, LitNum, [(?)|Mode], [IO|NewMode], Adg,
	    Gvars, ClausePPKey, Error) :-
	M =< N,
	arg(M, Lit, Arg),
	( ground_argument(Arg, Gvars) ->
	    ( new_pos(LitNum, M, Pos),
		insert_adg_entry(Adg, Pos, _),
		insert_adg_field(Adg, Pos, (mode), +),
		find_adg_predecessor(Adg, Gvars, Arg, Pos, ClausePPKey, Error1
		),
		IO = (+) ) ;
	    ( Error1 = 1,
		IO = (-) ) ),
	M1 is M + 1,
	adg_literal_input(M1, N, Lit, LitNum, Mode, NewMode, Adg, Gvars,
	    ClausePPKey, Error2),
	Error is Error1 * Error2.

%
%  Insert the output positions of a literal into the argument dependency 
%  graph, and insert the variables in output positions into the ground
%  variables list.
%
adg_literal_output(M, N, _, _, _, _, _, _) :-
	M > N,
	!.
adg_literal_output(M, N, Lit, LitNum, [(+)|MList], PredPos, Adg, Gvars) :-
	M =< N,
	!,
	M1 is M + 1,
	adg_literal_output(M1, N, Lit, LitNum, MList, PredPos, Adg, Gvars).
adg_literal_output(M, N, Lit, LitNum, [(-)|MList], PredPos, Adg, Gvars) :-
	M =< N,
	new_pos(LitNum, M, Pos),
	insert_adg_entry(Adg, Pos, _),
	insert_adg_field(Adg, Pos, (mode), -),
	arg(M, Lit, Arg),
	insert_ground_vars(Gvars, Arg, Pos),
	insert_adg_predecessor(Adg, Pos, PredPos),
	M1 is M + 1,
	adg_literal_output(M1, N, Lit, LitNum, MList, PredPos, Adg, Gvars).

%
%  Test if the term at an argument position has been ground.
%
ground_argument(Term, Gvars) :-
	var(Term),
	!,
	find_gvars_field(Gvars, Term, def, PosList),
	nonvar(PosList).
ground_argument(Term, _) :-
	atomic(Term),
	!.
ground_argument(Term, Gvars) :-
	compound(Term),
	functor(Term, _, N),
	ground_argument(N, Term, Gvars).

ground_argument(0, _, _) :-
	!.
ground_argument(N, Term, Gvars) :-
	N > 0,
	arg(N, Term, Arg),
	ground_argument(Arg, Gvars),
	N1 is N -1,
	ground_argument(N1, Term, Gvars).

:- pop_prolog_flag(multi_arity_warnings).

%
%  Find the input positions of a literal.
%
find_internal_predecessor([],         _, _,      _).
find_internal_predecessor([(+)|Mode], N, LitNum, [Pos|PList]) :-
	!,
	new_pos(LitNum, N, Pos),
	N1 is N + 1,
	find_internal_predecessor(Mode, N1, LitNum, PList).
find_internal_predecessor([(-)|Mode], N, LitNum, Pos) :-
	N1 is N + 1,
	find_internal_predecessor(Mode, N1, LitNum, Pos).
