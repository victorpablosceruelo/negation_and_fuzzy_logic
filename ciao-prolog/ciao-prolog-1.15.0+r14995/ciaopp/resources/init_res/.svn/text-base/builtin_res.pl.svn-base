:- module(builtin_res,
	    [
		legal_pred_arg/1,
		second_order_predicate/1,
		second_order_predicate_pred_arg/2,
		second_order_predicate_pred_num/3,
		find_entry_trusted_field/6,
		find_point_trusted_field/10
	    ],
	    [assertions, hiord, resources(inferres_decl)]).

:- use_module(library(hiordlib)).
:- use_module(library(terms_vars)).
:- use_module(library(messages)).
:- use_module(library(aggregates)).
:- use_module(library(llists)).
:- use_module(library(sort)).
:- use_module(plai(domains)).
:- use_module(program(assrt_db)).
:- use_module(program(p_unit),           [entry_assertion/3]).
:- use_module(spec(s_simpspec),          [make_atom/2]).
:- use_module(infer(infer_db),           [inferred/3, domain/1]).
:- use_module(infer(infer_dom),          [knows_of/2]).
:- use_module(infer(gather_modes),       [vartypes_to_modes/2]).
:- use_module(infer(gather_modes_basic), [translate_to_modes/2, get_metric/2]).
:- use_module(infer(infer)).
:- use_module(resources(resources_basic)).
:- use_module(library(resdefs(rescostfunc)),   [compact_cf/3]).
:- use_module(resources(size_res(clause_res)), [number_of_literals/3]).

%  December, 1991
:- doc(author, "Nai-Wei Lin").
:- doc(author, "Edison Mera").

:- doc(module, "This file contains the procedures for obtain cost
	properties of builtin and library predicates.").

get_mode(_, 0, []) :- !.
get_mode(F, A, Mode) :-
	make_atom([F, A], PKey),
	current_fact(inferred(modes, PKey, mode(F, A, Info)), _),
	!,
	translate_to_modes(Info, Mode).
get_mode(F, A, Mode) :-
	make_atom([F, A], PKey),
	inferred(vartypes, PKey, Vartypes),
	!,
	vartypes_to_modes(Vartypes, Mode).
get_mode(_, _, _) :- !.

get_measure(Pred, Succ, Comp, Measure) :-
	copy_term([Pred, Succ, Comp], [Pred0, Succ0, Comp0]),
	get_measure_(Pred0, Succ0, Comp0, Measure).

get_measure_(Pred, SuccType, Comp, Measure) :-
	Pred =.. [_|Measure],
	match_size_metric(Comp, Pred),
	( ground(Measure) -> true
	; type2measure(Pred, SuccType, Measure) -> true
	; true
	),
	!.

:- meta_predicate find_trusted_field(?, ?, ?, ?, ?, pred(4), ?).
find_trusted_field(mode, _, _, F/A, _, ApplicableTrust, Mode) :-
	functor(Pred, F, A),
	ApplicableTrust(mode, _, Pred, Mode).
find_trusted_field(measure, _, _, F/A, _, ApplicableTrust, Measure) :-
	functor(Pred, F, A),
	applicable_trusts(ApplicableTrust, regtypes, comp,    Pred, Comp),
	applicable_trusts(ApplicableTrust, regtypes, success, Pred, Succ),
	get_measure(Pred, Succ, Comp, Measure).
find_trusted_field(det, _, _, F/A, Approx, ApplicableTrust, Det) :-
	functor(Pred, F, A),
	applicable_trusts(ApplicableTrust, regtypes, comp, Pred, Comp),
	approx_to_bound(Approx, Bound),
	compinfo_to_det(Bound, Comp, Det).
find_trusted_field(relation, _, _, F/A, _, ApplicableTrust, Rel) :-
	functor(Pred, F, A),
	applicable_trusts(ApplicableTrust, regtypes, comp, Pred, Comp),
	compinfo_to_rel(Comp, Rel).
find_trusted_field(size, _, _, F/A, Approx, ApplicableTrust, Size) :-
	functor(Mode0, F, A),
	ApplicableTrust(mode, _, Mode0, Mode),
	functor(Pred, F, A),
	applicable_trusts(ApplicableTrust, regtypes, comp,    Pred, Comp),
	applicable_trusts(ApplicableTrust, regtypes, success, Pred, Succ),
	get_measure(Pred, Succ, Comp, Measure),
	succinfo_to_size(Pred, Approx, Succ, Mode, Measure, Size).
find_trusted_field(resources(Resources), BT, ST, F/A, Approx, ApplicableTrust,
	    [Cost]) :-
	functor(Mode0, F, A),
	ApplicableTrust(mode, _, Mode0, Mode),
	functor(Pred, F, A),
	applicable_trusts(ApplicableTrust, regtypes, comp,    Pred, Comp),
	applicable_trusts(ApplicableTrust, regtypes, success, Pred, Succ),
	get_measure(Pred, Succ, Comp, Measure),
	compinfo_to_cost(BT, ST, Pred, F/A, Approx, Comp, Mode, Measure,
	    Resources, Cost).
find_trusted_field(nf, _, _, F/A, _, ApplicableTrust, NF) :-
	functor(Pred, F, A),
	applicable_trusts(ApplicableTrust, regtypes, comp, Pred, Comp),
	compinfo_to_nf(Comp, NF).
find_trusted_field(cover, _, _, F/A, _, ApplicableTrust, Cover) :-
	functor(Pred, F, A),
	applicable_trusts(ApplicableTrust, regtypes, comp, Pred, Comp),
	compinfo_to_cover(Comp, Cover).
find_trusted_field(test_type, _, _, F/A, _, ApplicableTrust, TestType) :-
	functor(Pred, F, A),
	applicable_trusts(ApplicableTrust, regtypes, comp, Pred, Comp),
	compinfo_to_test_type(Comp, TestType).

map_prop(Pred-Prop0, Pred, Prop0).

:- meta_predicate applicable_trusts(pred(4), ?, ?, ?, ?).
applicable_trusts(ApplicableTrust, TypeProp, TypeAssr, Pred, Prop) :-
	findall(Pred-Prop0, ApplicableTrust(TypeProp, TypeAssr, Pred, Prop0),
	    Prop1),
	map(Prop1, map_prop(Pred), Prop2),
	flatten(Prop2, Prop3),
	sort(Prop3, Prop).

get_assertion_prop(call, Pred, Body, Call, Call) :-
	assertion_body(Pred, _, Call, _, _, _, Body).
get_assertion_prop(comp, Pred, Body, Call, Comp) :-
	assertion_body(Pred, _, Call, _, Comp, _, Body).
get_assertion_prop(success, Pred, Body, Call, Succ) :-
	assertion_body(Pred, _, Call, Succ, _, _, Body).

usable_status(true).
usable_status(trust).

applicable_entry_trust(mode, _, Pred, Mode) :-
	functor(Pred, F, A),
	get_mode(F, A, Mode),
	!.
applicable_entry_trust(regtypes, success, Pred, Prop) :-
	get_regtypes(Pred, _, Prop).
applicable_entry_trust(PropType, TypeAssr, Pred, Prop) :-
	( knows_of(PropType, TypeAn), domain(TypeAn) -> true
	; warning_message("No ~w information available.", [PropType]) ),
	functor(Pred, F, A),
	(
	    A == 0 -> ECall = []
	;
	    entry_assertion(Pred, ECall, _) -> true
	;
	    make_atom([F, A], PKey),
	    get_info(regtypes, pred, PKey, Pred, (ECall, _))
	),
	Pred =.. [_|Vars],
	info_to_asub(TypeAn, perfect, ECall, Vars, Entry),
	get_prop_assr_value(TypeAssr, Pred, Call, Prop),
	info_to_asub(TypeAn, perfect, Call, Vars, CallASub),
	less_or_equal(TypeAn, Entry, CallASub).

get_prop_assr_value(TypeAssr, Pred, Call, Prop) :-
	usable_status(Status),
	assertion_read(Pred, _, Status, TypeAssr, Body, _, _, _, _),
	get_assertion_prop(TypeAssr, Pred, Body, Call, Prop).

get_regtypes(Pred, Call, Succ) :-
	functor(Pred, F, A),
	make_atom([F, A], PKey),
	get_info(regtypes, pred, PKey, Pred, (Call, Succ)).

get_prop_value(regtypes, success, Pred, Call, Prop) :-
	get_regtypes(Pred, Call, Prop).
get_prop_value(regtypes, TypeAssr, Pred, Call, Prop) :-
	get_prop_assr_value(TypeAssr, Pred, Call, Prop).

find_entry_trusted_field(PropName, BT, ST, PredName, Approx, Prop) :-
	find_trusted_field(PropName, BT, ST, PredName, Approx,
	    applicable_entry_trust, Prop).

% This looks like a kludge: perhaps instead of use the
% find_entry_trusted_field when PPKey=noinfo, we have to use
% the predicate call_to_entry in the head of the clause that
% implements the literal being analyzed -- EMM

nokey(nokey).
nokey(noinfo).

find_point_trusted_field(PropName, BT, ST, _, _, PPKey, _, PredName, Approx,
	    Prop) :-
	nokey(PPKey),
	!,
	find_entry_trusted_field(PropName, BT, ST, PredName, Approx, Prop).
find_point_trusted_field(PropName, BT, ST, Lit, Vars, PPKey, PPKey1, PredName,
	    Approx, Prop) :-
	find_trusted_field(PropName, BT, ST, PredName, Approx,
	    applicable_point_trust(PPKey, PPKey1, Lit, Vars), Prop).

ground_info_to_mode((+) /g, _,         _/g) :- !.
ground_info_to_mode(V/g,    PredPPKey, _/Gn) :- !,
	warn_info_to_mode(V, PredPPKey, g, Gn),
	V = (+).
ground_info_to_mode((+) /nf, _,         _/nf) :- !.
ground_info_to_mode((-) /nf, _,         _/g) :- !.
ground_info_to_mode((+) /nf, _,         _/nf) :- !.
ground_info_to_mode(V /nf,   PredPPKey, _/f) :- !,
	warn_info_to_mode(V, PredPPKey, nf, f),
	V = (+).
ground_info_to_mode((-) /f, _,         _/g) :- !.
ground_info_to_mode((-) /f, _,         _/nf) :- !.
ground_info_to_mode(V /Gn1, PredPPKey, _/Gn2) :-
	warn_info_to_mode(V, PredPPKey, Gn1, Gn2),
	V = (?).

warn_info_to_mode(_, _, _, _).
/*
warn_info_to_mode(V, PredPPKey, Gn1, Gn2) :-
	\+ \+ ( (
		ground_name(Gn1, N1),
		ground_name(Gn2, N2),
		prettyvars(t(V, PredPPKey, N1, N2)),
		warning(['In ', ''(PredPPKey), ': Variable ', ''(V), ' ', N1,
			' at entry and ', N2, ' on success.'])
	    ) ).

ground_name(g,  ground).
ground_name(f,  free).
ground_name(nf, non_free).

*/

applicable_point_trust(mode, PPKey, PPKey1, Lit, Vars, _, Pred, Mode) :-
% 	knows_of(ground, An),
% 	domain(An),
	An = shfr,
	Pred =.. [_|Mode],
	get_entry(ground, PPKey,  Lit, Vars, Pred, An, Mode, Entry),
	get_entry(ground, PPKey1, Lit, Vars, Pred, An, Mode, Entry1),
	Entry = (_, GndInput),
	Entry1 = (_, GndOutput),
	map(GndInput, ground_info_to_mode(Pred:PPKey), GndOutput),
	!.
applicable_point_trust(TypeProp, PPKey, _, Lit, Vars, TypeAssr, Pred, Prop) :-
	knows_of(TypeProp, An),
	domain(An),
	Pred =.. [_|Args],
	get_entry(TypeProp, PPKey, Lit, Vars, Pred, An, Args, Entry),
	get_prop_value(TypeProp, TypeAssr, Pred, Call, Prop),
	(
	    info_to_asub(An, perfect, Call, Args, CallASub),
	    less_or_equal(An, Entry, CallASub) -> true
	).
/*
eq_asub(ground, (_, E1), (_, E2)) :-
	E1 == E2.
eq_asub(regtypes, ASub1, ASub2) :-
	map(ASub1, eq_regtype, ASub2).
eq_regtype(V1:(_Name1, Type1), V2:(_Name2, Type2)) :-
	V1 == V2,
	Type1 == Type2.
*/
get_entry(TypeProp, PPKey, Lit, Vars, Pred, An, Args, Entry) :-
% 	ShowWarning = warn(no),
	(
	    get_absint(PPKey, An, Vars, ASub) ->
	    true
/*
	    \+ \+ ( (
		    get_info(TypeProp, point, PPKey, Vars, Types),
		    info_to_asub(An, perfect, Types, Vars, ASub0) ->
		    (
			eq_asub(TypeProp, ASub0, ASub) -> true
		    ;
			warning(['In ', Pred:PPKey,
				': get_info/info_to_asub Result\n', ''(ASub0),
				'\nIs different of get_absint Result\n',
				''(ASub)]),
			'$setarg'(1, ShowWarning, yes, true)
		    )
		;
		    true
		) )
*/
	;
	    get_info(TypeProp, point, PPKey, Vars, Types),
	    info_to_asub(An, perfect, Types, Vars, ASub)
	),
	!,
	varset(Lit, SgVars),
	project(An, SgVars, _, ASub, Proj),
	call_to_entry(An, _, Lit, Args, Pred, [], Proj, Entry, _).
% 	(arg(1, ShowWarning, yes) -> warning(['Entry: ', ''(Entry)]) ; true).

compinfo_to_rel(Comp, Rel) :-
	member('native_props:relations'(_, Rel), Comp),
	!.
compinfo_to_rel(_, _).

:- pred compinfo_to_det/3 :: bound * list * list.

compinfo_to_det(upper, Comp, Det) :-
	compinfo_to_det_upper(Comp, Det).
compinfo_to_det(lower, Comp, Det) :-
	compinfo_to_det_lower(Comp, Det).

compinfo_to_det_upper(Comp, [0]) :-
	member('native_props:fails'(_), Comp),
	!.
compinfo_to_det_upper(Comp, [1]) :-
	member('native_props:is_det'(_), Comp),
	!.
compinfo_to_det_upper(Comp, [Sol]) :- % The exact number of solutions is known
	member('native_props:num_solutions'(_, _, Sol), Comp),
	int(Sol),
	!.
compinfo_to_det_upper(_Comp, _).
%  :-
% 	member('native_props:non_det'(_), Comp),
% 	!.

compinfo_to_det_lower(Comp, [0]) :-
	member('native_props:fails'(_), Comp),
	!.
compinfo_to_det_lower(Comp, [1]) :-
	member('native_props:not_fails'(_), Comp),
	!.
compinfo_to_det_lower(Comp, [0]) :-
	member('native_props:is_det'(_), Comp),
	!.
compinfo_to_det_lower(Comp, [2]) :-
	member('native_props:non_det'(_), Comp),
	!.
compinfo_to_det_lower(Comp, [Sol]) :- % The exact number of solutions is known
	member('native_props:num_solutions'(_, Sol), Comp),
	!.
compinfo_to_det_lower(_Comp, _).
% :-
% 	member('native_props:fails'(_), Comp),
% 	!.

%

:- pred compinfo_to_cover/2 :: list * atm.

compinfo_to_cover(Comp, fail) :-
	member('native_props:not_covered'(_), Comp),
	!.
compinfo_to_cover(Comp, fail) :-
	member('native_props:fails'(_), Comp),
	!.
compinfo_to_cover(Comp, true) :-
	member('native_props:covered'(_), Comp),
	!.
compinfo_to_cover(Comp, true) :-
	member('native_props:not_fails'(_), Comp),
	!.
compinfo_to_cover(Comp, Cover) :- % The exact number of solutions is known
	member('native_props:num_solutions'(_, Sol), Comp),
	!,
	(
	    Sol > 0 ->
	    Cover = true
	;
	    Cover = fail
	).
compinfo_to_cover(_Comp, _).

:- pred compinfo_to_nf/2 :: list * atm.

compinfo_to_nf(Comp, fail) :-
	member('native_props:fails'(_), Comp),
	!.
compinfo_to_nf(Comp, true) :-
	member('native_props:not_fails'(_), Comp),
	!.
compinfo_to_nf(Comp, NF) :- % The exact number of solutions is known
	member('native_props:num_solutions'(_, Sol), Comp),
	!,
	(
	    Sol > 0 ->
	    NF = true
	;
	    NF = fail
	).
compinfo_to_nf(Comp, true) :-
	member('native_props:covered'(_), Comp),
	!.
compinfo_to_nf(_Comp, _).

compinfo_to_test_type(Comp, TestType) :-
	member('native_props:test_type'(_, TestType), Comp),
	!.
compinfo_to_test_type(_Comp, _).

succinfo_to_size(Pred, Approx, Succ, Mode, Measure, Size) :-
	copy_term([Pred, Succ], [Pred0, Succ0]),
	Pred0 =.. [_|Size],
	functor(Pred, F, A),
% 	create_size_vars(SizeVars),
% 	create_subst_list(SizeVars, Measure),
	match_input_sizes(Mode, Measure, 1, Size),
	subst_sizes(Size, Measure, Subst),
	catch(list(Succ0, match_size(Approx, Subst)),
	    error(size_expr, SizeExpr),
	    error(['In assertion for ', ~~(F/A), ', size expression ',
		    ~~(SizeExpr), ' can only depend on input data sizes.'])).
% 	approx_to_bound(Approx, Bound),
% 	translate_to_sizes(Size, Bound).

subst_sizes([],       [],          []).
subst_sizes([S|Size], [M|Measure], Subst0) :-
	(nonvar(S) -> SM =.. [M, S], Subst0 = [SM=S|Subst] ; Subst0 = Subst),
	subst_sizes(Size, Measure, Subst).

match_input_sizes([],           [],                 _,  []).
match_input_sizes([Mode|Modes], [Measure|Measures], N1, [S|Size]) :-
	match_input_size(Mode, Measure, N1, S),
	N is N1 + 1,
	match_input_sizes(Modes, Measures, N, Size).

match_input_size(+, void, _, 0) :- !.
match_input_size(+, _,    N, $(0, N)) :- !.
match_input_size(_, _,    _, _).

:- pred match_size/3 + throws([error(size_expr, _)]).
match_size(Succ, Approx, Subst) :-
	(
	    ( Succ = 'native_props:size'(Arg, SizeExpr)
	    ; Succ = 'native_props:size'(Approx, Arg, SizeExpr)
	    ) ->
	    ( subst_term(SizeExpr, Subst, Size) -> true
	    ; throw(error(size_expr, SizeExpr)), fail
	    ),
	    (
		Arg = Size ->
		true
	    ;
		error(['Input argument ', ~~(Arg), ' cannot have a size ',
			~~(Size)])
	    )
	;
	    true
	).

compinfo_to_cost(BT, ST, Pred, PredName, Approx, Comp, Mode, Measure,
	    Resources, Costs) :-
	copy_term([Pred, Comp], [Pred0, Comp0]),
	Pred0 =.. [_|Size],
	match_input_sizes(Mode, Measure, 1, Size),
	subst_sizes(Size, Measure, Subst),
	map(Resources, match_cost(Comp0, BT, ST, Pred0, PredName, Approx,
		Subst), Costs).

:- use_module(resources(res_assrt_defs(infertime_lib))).
:- use_module(resources(top_res(utility_res)),
	    [vector_multiply/3, vector_addition/2]).
:- use_module(resources(init_res(symtable_res))).

match_cost(Resource, Comp, BT, ST, Pred, PredName, Approx, Subst, Cost) :-
	member('resources_props:cost'(Pred, _, Approx, _, Resource, _, IF,
		CF), Comp)
    ->
	compact_cf(CF, IF, CFN),
	subst_term(CFN, Subst, Cost)
    ;
	compound_resource(Resource, Resources),
	map(Resources, match_cost(Comp, BT, ST, Pred, PredName, Approx, Subst),
	    Costs),
	(
	    ground(Costs) ->
	    true
	;
	    find_symbol_field(ST, PredName, resources(Resources), [Costs1]),
	    combine_results(resources(Resources), Costs, Costs1)
	),
	ground(Costs) ->
	(
	    get_platform(BT, Platform),
	    platform_constants(Platform, Resource, Approx, Constants) ->
	    vector_multiply(Constants, Costs, Cost)
	;
	    vector_addition(Costs, Cost)
	)
    ;
	true.

subst_term(Term, Dic, Subst) :-
	member(Term=Subst, Dic),
	!.
subst_term(Term, _, Term) :-
	var(Term),
	!,
	fail.
subst_term(Terms, Dic, Substs) :-
	Terms =.. [F|TermArgs],
	map(TermArgs, subst_term(Dic), SubstArgs),
	Substs =.. [F|SubstArgs].

match_size_metric([],          _).
match_size_metric([Prop|Succ], Pred) :-
	(
	    Prop = 'native_props:size_metric'(Pred, V, Metric0) ->
	    get_metric(Metric0, Metric),
	    !,
	    V = Metric
	;
	    true
	),
	match_size_metric(Succ, Pred).

second_order_predicate(findall/3).

second_order_predicate_pred_arg(findall(_, P, _), P).

second_order_predicate_pred_num(Body, LitNum, Num) :-
	number_of_literals(Body, 1, Num1),
	Num is Num1 + LitNum.

legal_pred_arg(Pred) :-
	functor(Pred, F, N),
	F / N \== ','/2, % single literal
	\+ second_order_predicate(F/N). % non-second-order predicate
