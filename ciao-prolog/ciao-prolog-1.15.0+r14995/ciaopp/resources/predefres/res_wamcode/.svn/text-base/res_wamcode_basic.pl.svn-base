:- module(_, _, [assertions, nortchecks, regtypes, fsyntax]).

:- use_module(library(aggregates)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(compiler), [make_wam/1]).
:- use_module(library(read)).

:- data clause_wamcode_db/2.
:- data lit_wamcode_db/2.

cleanup_clause_wamcode_db :-
	retractall_fact(clause_wamcode_db(_, _)).

get_clause_wamcode(Key, ClauseWamCode) :-
	clause_wamcode_db(Key, ClauseWamCode).

get_lit_wamcode(Key, LitNum, LitWamCode) :-
	atom_number(ALitNum, LitNum),
	atom_concat([Key, '/', ALitNum], LitKey),
	lit_wamcode_db(LitKey, LitWamCode).

get_wamcode(Path) :-
	make_wam(Path),
	atom_concat(Path, '.wam', PathWam),
	open(PathWam, read, S),
	read_terms(S),
	close(S).

read_terms(Stream) :-
	read(Stream, R),
	!,
	(
	    R = end_of_file ->
	    true
	;
	    (
		R = clause(Key, ClauseWamCode) ->
		assertz_fact(clause_wamcode_db(Key, ClauseWamCode))
	    ;
		true
	    ),
	    read_terms(Stream)
	).

:- data wamcode_count_db/2.

cleanup_wamcode_count_db :-
	retractall_fact(wamcode_count_db(_, _)).

inc_wamcode_count_db(WamCode, Count0) :-
	(wamcode_count_db(WamCode, Count1) -> true ; Count1 = 0),
	Count is Count0 + Count1,
	set_wamcode_count_db(WamCode, Count).

set_wamcode_count_db(WamCode, Count) :-
	retractall_fact(wamcode_count_db(WamCode, _)),
	assertz_fact(wamcode_count_db(WamCode, Count)).

wamcode_count(WamCodes, WamCodeCounters) :-
	cleanup_wamcode_count_db,
	do_wamcode_count(WamCodes),
	findall(count(WamCode, Count), wamcode_count_db(WamCode, Count),
	    WamCodeCounters),
	cleanup_wamcode_count_db.

do_wamcode_count([]).
do_wamcode_count([WamCode|WamCodes]) :-
	inc_wamcode_count_db(WamCode, 1),
	do_wamcode_count(WamCodes).

:- regtype wamcode/1 # "This predicate has been implemented using the
	pl2wam.pl documentation, and specifies the valid wam codes.".

wamcode :=
	choice|
% 	switch_on_term((Clause)*, (Clause)*, (Key-Clause)*, (Clause)*) |
% 		       %Var       %List      %Other         %Default

	true(Integer)|
	call(PredLabel, Integer)|
	execute(PredLabel)|
	proceed|
	fail|
	builtin_1(Name, Arg1)|
	builtin_2(Name, Arg1, Arg2)|
	builtin_3(Name, Arg1, Arg2, _Arg3)|
	function_1(Name, Value, Arg1, N, EffAr)|
	function_2(Name, Value, Arg1, Arg2, N, EffAr)|
	put_x_variable(Arg1, Arg2)|
	put_y_variable(Arg1, Arg2)|% not after first call
/****** put_y_first_value( Arg, Arg ) | % after first call ******/
	put_x_value(Arg1, Arg2)|
	put_y_value(Arg1, Arg2)|
	put_x_unsafe_value(Arg1, Arg2)|
	put_y_unsafe_value(Arg1, Arg2)|
	put_constant(Atomic, Arg)|
	put_structure(Functor, Arg)|
	put_nil(Arg)|
	put_list(Arg)|

	get_x_variable(Arg1, Arg2)|
	get_y_variable(Arg1, Arg2)|% not after first call
	get_y_first_value(Arg1, Arg2)|% after first call
	get_x_value(Arg1, Arg2)|
	get_y_value(Arg1, Arg2)|
	get_constant(Atomic, Arg)|
	get_structure(Functor, Arg)|
	get_nil(Arg)|
	get_list(Arg)|
	get_constant_x0(Atomic)|
	get_structure_x0(Functor)|
	get_nil_x0|
	get_list_x0|

	unify_void|
	unify_x_variable(Arg)|
	unify_y_variable(Arg)|% not after first call
	unify_y_first_value(Arg)|% after first call
	unify_x_value(Arg)|
	unify_y_value(Arg)|
	unify_x_local_value(Arg)|
	unify_y_local_value(Arg)|
	unify_constant(Atomic)|
	unify_structure(Functor)|
	unify_nil|
	unify_list|

	allocate|
	deallocate|
	init(_L)|
	neck(N)|% create/update choicepoint
	choice_x(Arg)|
	choice_y(Arg)|
	cutb|% before allocate
	cute|% after allocate before init
	cutf|% after init
	cutb_x(Arg)|% before allocate
	cute_x(Arg)|% after allocate before init
	cutf_x(Arg)|% after init
	cut_y(Arg)|

	heapmargin_call(N, EffAr).

% First experiment, grouping wamcodes in resources:

current_wamcode_resource(WamCode, Resource) :-
	wamcode(WamCode),
	wamcode_resource(WamCode, Resource).

% deforestation:

% special cases:

:- use_module(library(compiler(pl2wam_tables))).

builtin_1_name(Number, Name) :-
	name_of_builtin(Term, Number, _),
	functor(Term, Name, _).

builtin_2_name(Number, Name) :-
	name_of_builtin(Term, Number, _, _),
	functor(Term, Name, _).

builtin_3_name(Number, Name) :-
	name_of_builtin(Term, Number, _, _, _),
	functor(Term, Name, _).

function_1_name(Number, Name) :-
	name_of_function(Term, Number, _),
	functor(Term, Name, _).

function_2_name(Number, Name) :-
	name_of_function(Term, Number, _, _),
	functor(Term, Name, _).

wamcode_resource(builtin_1(Number, _), Resource) :-
	!,
	builtin_1_name(Number, Name),
	atom_concat(['builtin_', Name, '/1'], Resource).
wamcode_resource(builtin_2(Number, _, _), Resource) :-
	!,
	builtin_2_name(Number, Name),
	atom_concat(['builtin_', Name, '/2'], Resource).
wamcode_resource(builtin_3(Number, _, _, _), Resource) :-
	!,
	builtin_3_name(Number, Name),
	atom_concat(['builtin_', Name, '/2'], Resource).
wamcode_resource(function_1(Number, _, _, _, _), Resource) :-
	!,
	function_1_name(Number, Name),
	atom_concat(['function_', Name, '/1'], Resource).
wamcode_resource(function_2(Number, _, _, _, _), Resource) :-
	!,
	function_2_name(Number, Name),
	atom_concat(['function_', Name, '/1'], Resource).
wamcode_resource(WamCode, Resource) :-
	functor(WamCode, Resource, _).

wam_compound_resource(crget, Resources) :-
	findall(Resource,
	    (
		current_wamcode_resource(_, Resource),
		atom_concat(get_, _, Resource)
	    ),
	    Resources).
wam_compound_resource(crput, Resources) :-
	findall(Resource,
	    (
		current_wamcode_resource(_, Resource),
		atom_concat(put_, _, Resource)
	    ),
	    Resources).
wam_compound_resource(unify, Resources) :-
	findall(Resource,
	    (
		current_wamcode_resource(_, Resource),
		atom_concat(unify_, _, Resource)
	    ),
	    Resources).


wamcode_resource_compound(WamCode, Resource, ComposedResource) :-
	current_wamcode_resource(WamCode, Resource),
	resource_compound(Resource, ComposedResource).

% This do the grouping of resources.

basic_compound_resource := get|put|unify.

resource_compound(Resource, ComposedResource) :-
	basic_compound_resource(ComposedResource),
	atom_concat(ComposedResource, _, Resource),
	!.
resource_compound(Resource, Resource).
