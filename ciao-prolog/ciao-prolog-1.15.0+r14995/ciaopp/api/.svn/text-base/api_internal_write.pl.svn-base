:- module(api_internal_write, [
		api_internal_write/1,
		api_internal_write/2
	    ], [assertions, regtypes]).


:- use_module(library(vndict)).
:- use_module(library(messages)).
:- use_module(library(format)).
:- use_module(library(assertions(assrt_write))).
:- use_module(ciaopp(api(api_internal_types))).
:- use_module(ciaopp(printer),       [check_global_props/2]).
:- use_module(library(pretty_print), [pretty_print/4]).
:- use_module(program(assrt_db),     [assertion_body/7]).
:- use_module(program(unexpand), [
		transform_clause_list/3,
		transform_assrt_body/3,
		transform_head/3,
		transform_body/3,
		transform_assrt_body/3
	    ]).
:- use_module(program(itf_db),         [curr_file/2]).
:- use_module(ciaopp(api(api_predcl)), [insert_ppi/2]).

% --- DTM: This has to be part of the API SOON!
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).

:- use_package(.(api_internal_dec)).

:- pred api_internal_write(A).

api_internal_write(A) :-
	current_output(CO),
	api_internal_write(A, CO).


api_internal_write('$goal'(G, D), S) :-
	!,
	(
	    D = [_|_]
	->
	    varnamesl2dict(D, D1)
	;
	    D1 = D
	),
	print_goal(G, D1, S).

api_internal_write('$goal'(G), S) :-
	!,
	print_goal(G, S).

% t_direc case
api_internal_write(Dir, S) :-
	Dir = direc${body => Body, dic => Dic},
	!,
	pretty_print(S, directive(Body), [], Dic),
	nl(S).


api_internal_write(Comment, S) :-
	Comment = comment${type => Type, comment => Com},
	!,
	( var(Com)
	-> format(S, "Empty Comment", [])
	; api_write_comment(Type, Com, S) ).

api_internal_write(As, S) :-
	As = as${%ref      => ID     ,
	    status => Status,
	    type => Type,
	    head => Head,
	    compat => Compat,
	    call => Call,
	    succ => Succ,
	    comp => Comp,
	    dic => Dic,
	    locator => loc${module => M},
	    fromwhere => From
	},
	!,
	(
	    (var(Head))
	->
	    format(S, "Empty Assertion", [])
	;
% --- inverse rewrite program
	    assertion_body(Head, Compat, Call, Succ, Comp, [], Body),
	    transform_head(Head, M, HeadT),
	    transform_assrt_body(Body, M, BodyT),
	    check_global_props(BodyT, BodyT2),
	    (
		Dic = no
	    ->
		create_dict((HeadT:-BodyT), _VN),
		dict2varnamesl(_VN, VN)
	    ;
		VN = Dic
	    ),
	    (
		(Type = entry ; Type = prop)
	    ->
		WriteStatus = nostatus
	    ;
		WriteStatus = status
	    ),
	    (
		From == commented
	    ->
		(
		    Type == pred
		->
		    \+ \+ write_assertion_as_double_comment(S,
			HeadT, Status, Type,
			BodyT2, VN,
			WriteStatus)
		;
		    \+ \+ write_assertion_as_comment(S, HeadT,
			Status, Type,
			BodyT2, VN,
			WriteStatus)
		)
	    ;
		Type == test ->
		true % Don't write test assertions here
	    ;
		\+ \+ write_assertion(S, HeadT, Status, Type, BodyT2,
		    VN, WriteStatus)
	    )
	).



api_internal_write('$clause'(H, B, Dic), S) :-
	remove_ppi_from_clauses([clause(H, B)], Cls1),
	(
%	    varnamesl( Dic )
	    list(Dic)
	->
	    varnamesl2dict(Dic, D)
	;
	    D = Dic
	),
	pretty_print(S, Cls1, [], [D]),
	!.

api_internal_write('$clause'(H, B), S) :-
	remove_ppi_from_clauses([clause(H, B)], Cls1),
	pretty_print(S, Cls1, [], _nodict),
	!.

api_internal_write('$comment'(T, C), S) :-
	!,
	api_write_comment(T, C, S).

api_internal_write(Cl_arg, S) :-
	Cl_arg = cls${
	    head => H1,
	    body => B1,
	    dic => Dic,
	    locator => Loc
	},
	Loc = loc${module => M},
	!,
	(
	    nonvar(H1), nonvar(B1),
	    nonvar(Dic), nonvar(M)
	->
	    ( current_pp_flag(pp_info, off) ->
		Cl = Cl_arg
	    ;
		insert_ppi(Cl_arg, Cl)
	    ),
	    Cl = cls${head => H, body => B},
	    remove_ppi_from_clauses([clause(H, B)], Cls1),
	    transform_clause_list(Cls1, M, Cls2),
	    pretty_print(S, Cls2, [], [Dic])
	;
	    format(S, "Empty Clause", [])
	).

api_internal_write(Loc, S) :-
	Loc = loc${
	    file => S,
	    line_begin => LB,
	    line_end => LE
	},
	atom(S),
	num(LB),
	num(LE),
	!,
	format(S, "~w: ~d-~d:", [S, LB, LE]).

api_write_comment(c, C, S) :-
	display(S, '/*'),
	( atom(C)
	-> atom_codes(C, CS)
	; C = CS ),
	format(S, "~s", [CS]),
	display(S, '*/\n').
api_write_comment(Type, C, S) :-
	( atom(C)
	-> atom_codes(C, CS)
	; C = CS ),
	api_write_line_comment(0, CS, Type, S),
	nl(S).



line_type(simple, '% ',  78).
line_type(double, '%% ', 77).



api_write_line_comment(0, C, Type, S) :-
	!,
	line_type(Type, Pre, Cnt),
	display(S, Pre),
	api_write_line_comment(Cnt, C, Type, S).

api_write_line_comment(_, [], _, _) :-
	!.

api_write_line_comment(_, [10|Cs], Type, S) :-
	!,
	nl(S),
	api_write_line_comment(0, Cs, Type, S).

api_write_line_comment(N, [C|Cs], Type, S) :-
	N > 0,
	format(S, "~c", [C]),
	N1 is N - 1,
	(N1 = 0 -> nl(S) ; true),
	api_write_line_comment(N1, Cs, Type, S).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                      PRINT GOAL                                        %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_prolog_flag(multi_arity_warnings, off).

:- pred print_goal(G, S) : ground * stream
# "Print goal as better as possible. It create a pretty_dictionary
 (calling @pred{create_pretty_dict/2} from vndict library.".

print_goal(G, S) :-
	create_pretty_dict(G, D),
	\+ \+ print_goal(G, D, S).

:- pred print_goal(G, D, S) : ground * varnamedict * stream
# "Print goal @var{G} as better as possible, using @var{D} as a dictionary.".

print_goal(G, D, S) :-
	\+ \+ print_goal__(G, D, S).

print_goal__(G, D, S) :-
	curr_file(_, M),
	rename(G, D),
	transform_body(G, M, G1),
	remove_module_qualificator(G1, G2),
	internal_transform_body(G2, M, G3),
	format(S, "~w", [G3]),
	!.
print_goal__(G, D, S) :-
	(rename(G, D) -> true ; true),
	format(S, "~w", [G]).




internal_transform_body(A, M, B) :-
	A =.. [F|LA],
	internal_transform_body__(LA, M, LA2),
	B =.. [F|LA2].




internal_transform_body__([], _, []).

internal_transform_body__(['$VAR'(A)|As], M, ['$VAR'(A)|Bs]) :-
	internal_transform_body__(As, M, Bs).

internal_transform_body__([A|As], M, [AT2|Bs]) :-
	transform_body(A, M, AT),
	remove_module_qualificator(AT, AT1),
	internal_transform_body(AT1, M, AT2),
	internal_transform_body__(As, M, Bs).




remove_module_qualificator(':'(_, L), L) :- !.

remove_module_qualificator(L, L).

:- set_prolog_flag(multi_arity_warnings, on).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                   REMOVE_PPI_FROM_CLAUSES                              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred remove_ppi_from_clauses(A, B)
	: (list(A, t_clause), var(B))
	=> list(B, t_clause_wo_ppi)

# "It takes a list of @pred{clause_type/1} in the argument @var{A} and
returns in @var{B} the same list of clauses but without program point info.
This is an internal predicate. Should not be exported.".

remove_ppi_from_clauses([],    []).
remove_ppi_from_clauses([A|B], [AT|BT]) :-
	remove_ppi_from_clause(A, AT),
	remove_ppi_from_clauses(B, BT).



% Remove the pp_info from literals
remove_ppi_from_clause(clause(A, B), clause(A, BT)) :-
	remove_ppi_from_literals(B, BT),
	!.
remove_ppi_from_clause(A, _) :-
	error_message(
	    "Internal Error: remove_ppi_from_clause: "||
	    "Cannot clean: ~p unexpected format~n",
	    [A]),
	fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                   REMOVE_PPI_FROM_LITERALS                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred remove_ppi_from_literals(A, B)
	: (list(A, t_literal), var(B))
	=> list(B, t_literal_wo_ppi)

# "It takes a list of @pred{clause_type/1} in the argument @var{A} and
returns in @var{B} the same list of clauses but without program point info.
This is an internal predicate. Should not be exported.".


remove_ppi_from_literals((L, Ls), (LT, LTs)) :-
	!,
	remove_ppi_from_literals(L,  LT),
	remove_ppi_from_literals(Ls, LTs).
remove_ppi_from_literals((L ; Ls), (LT ; LTs)) :-
	!,
	remove_ppi_from_literals(L,  LT),
	remove_ppi_from_literals(Ls, LTs).
remove_ppi_from_literals('->'(L, Ls), '->'(LT, LTs)) :-
	!,
	remove_ppi_from_literals(L,  LT),
	remove_ppi_from_literals(Ls, LTs).
remove_ppi_from_literals('andprolog_rt:&'(L, Ls),
	    'andprolog_rt:&'(LT, LTs)) :-
	!,
	remove_ppi_from_literals(L,  LT),
	remove_ppi_from_literals(Ls, LTs).
remove_ppi_from_literals(':'(L, _), L) :-
	!.
remove_ppi_from_literals(L, L).
