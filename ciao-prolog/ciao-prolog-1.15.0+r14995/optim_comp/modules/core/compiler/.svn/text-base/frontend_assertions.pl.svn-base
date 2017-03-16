% TODO: This is an 'include' of compiler/frontend.pl because of 'errs', move
%   to a module!
% TODO: Modular-ize so that foreign interface and be external (and
%   extended the use of assertions with hooks)

:- use_module(library(lists)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(compiler(module_pli)).
:- use_module(compiler(assertions__common)).

:- use_module(compiler(foreign__gluecode)).

% ---------------------------------------------------------------------------
% Foreign interface (C language)

% TODO: Use those assertions for low-level 'props', do not use any
%   special syntax.

{
:- extends errlog_ctx.
% TODO: should this be in the 'compile' part?... see comment in todo.txt
:- meta_predicate foreign_process_assertions(module_s).
% Extract low-level properties from assertions and generate gluecode
% (after assertions are normalized)
% note: it also calls gluecode generation from assertions
foreign_process_assertions(M) :-
	M.is_open_anon_or_mixin,
	!.
foreign_process_assertions(M) :-
	% TODO: wrong name in 'pragma'?
        ProcessAssertions = ( M.pragma(treat_assertions) ? yes | no ),
        ( ProcessAssertions = yes ->
	    do_prepare_ttr(M),
	    foreign_process_assertions_(M),
	    do_clean_ttr(M)
	; true
	).
}.

:- meta_predicate do_prepare_ttr(module_s).
% TODO: this should not be necessary if I just look at ttr_def pragmas... (in the future: move to types)
do_prepare_ttr(M) :-
        LoadGluecodeTTR = ( M.pragma(load_gluecode_ttr) ? yes | no ),
        ( LoadGluecodeTTR = yes ->
	    % TODO: Wrong, do not pass 'pli' but the whole 'module_s'
	    foreign__gluecode:load_ttr_from_pli(~M.pli)
	; true
	).

:- meta_predicate do_clean_ttr(module_s).
do_clean_ttr(M) :-
        LoadGluecodeTTR = ( M.pragma(load_gluecode_ttr) ? yes | no ),
        ( LoadGluecodeTTR = yes ->
	    foreign__gluecode:clean_ttr
	; true
	).

{
:- extends errlog_ctx.
:- meta_predicate foreign_process_assertions_(module_s).
foreign_process_assertions_(M) :-
	( % (failure-driven loop)
	  M.get_assertion(Assrt),
	  Assrt = a(Status, Type, Body, Loc),
	    Errs = ~get_errs,
	    Errs.add_loc(Loc),
	    errlog:loc_dict(Loc, VarNames),
	    % treat different uses of assertions
	    maytreat_assertion__foreign(M, Type, Status, Body, VarNames),
	    Errs.del_loc,
	    fail
        ; true
        ).

:- meta_predicate maytreat_assertion__foreign(module_s, ?, ?, ?, ?).
maytreat_assertion__foreign(M, Type, Status, Body, VarNames) :-
	Type = pred,
	assertion_body(Pr, _, _, _, GP, _, Body),
	%
	functor(Pr, F, A),
	PredName = F/A,
	Pred = ~M.pred_ref(F, A),
	%
	f(ForeignName, Kind, GP1) = ~get_foreign_decl(F, GP),
	check__valid_foreign_name(PredName, ForeignName), 
	check__no_more_foreign_name(PredName, GP1),
	( Kind = foreign ->
	    Pr =.. [_|Arguments], 
	    check__foreign(Status, PredName, Arguments, Body, VarNames),
	    Desc = ~get_foreign(PredName, ForeignName, Body, Arguments, VarNames),
	    Desc = foreign(NativeName, _, _,_, _),
	    Props = [impnat=cboolforeign(NativeName, Desc)]
        ; Kind = foreign_low ->
	    NativeName = ForeignName,
	    Props = [impnat=cbool(NativeName)]
	),
	Pred.set_pred_props(Props),
	!.
maytreat_assertion__foreign(_, _, _, _, _).
}.

get_foreign_decl(DefaultName, GP) := f(DefaultName, foreign, GP1) :-
	select(foreign(_), GP, GP1), !.
get_foreign_decl(DefaultName, GP) := f(DefaultName, foreign_low, GP1) :-
	select(foreign_low(_), GP, GP1), !.
get_foreign_decl(_, GP) := f(ForeignName, foreign, GP1) :-
	select(foreign(_, ForeignName), GP, GP1), !.
get_foreign_decl(_, GP) := f(ForeignName, foreign_low, GP1) :-
	select(foreign_low(_, ForeignName), GP, GP1), !.

{
:- extends errlog_ctx.
check__no_more_foreign_name(_, GP) :-
	\+ member(foreign(_), GP), 
	\+ member(foreign(_, _), GP), 
	\+ member(foreign_low(_), GP), 
	\+ member(foreign_low(_, _), GP), !.
check__no_more_foreign_name(PredName, _) :-
	(~get_errs).compiler_error(dup_foreign(PredName)).

check__valid_foreign_name(_, Name) :- atom(Name),  !.
check__valid_foreign_name(PredName, _) :-
	(~get_errs).compiler_error(invalid_foreign_name(PredName)).
}.

{
:- extends errlog_ctx.
get_foreign(PredName, ForeignName, Body, Arguments0, VarNames) := foreign(NativeName, ForeignName, Arguments, ResVar, NeedsState) :-
	assertion_body(_, DP, CP, AP, GP, _, Body),
	PredName = PrologName/Arity,
	Arity1 is Arity - 1, 
	numbers_between(0, Arity1, Arguments0), 
	findall(X, (member(Y, GP), Y=size_of(_, A, B), X=size_of(A, B)), SizeLinks), 
	findall(X, (member(Y, GP), Y=do_not_free(_, X)), NoFreeVars),
	findall(X, (member(Y, GP), Y=ttr(_, A, B), X=ttr(A, B)), TTrs), 
	NativeName = ~atom_concat('gluecode_', PrologName), 
	Arguments = ~get_arguments(Arguments0, DP, CP, AP, TTrs, SizeLinks, NoFreeVars),
	( member(returns(_, ResVar0), GP) ->
	    ResVar = [ResVar0], returns_in_output_argument(ResVar0, Arguments, PredName, VarNames)
	; ResVar = []
	),
	NeedsState = ( member(needs_state, GP) ? yes | no ).
}.

get_arguments([X|Xs], DP, CP, AP, TTrs, SizeLinks, NoFreeVars) := [~get_argument(X, DP, CP, AP, TTrs, SizeLinks, NoFreeVars)|~get_arguments(Xs, DP, CP, AP, TTrs, SizeLinks, NoFreeVars)] :- !.
get_arguments([], _, _, _, _, _, _) := [] :- !.

get_argument(N, DP, CP, AP, TTrs, SizeLinks, NoFreeVars) := arg(N, TTr, XN, NoFree) :-
	( member(ttr(N, TTr), TTrs) ->
	    true
	; D = ~get_prop(N, DP),
	  C = ~get_prop(N, CP),
	  A = ~get_prop(N, AP),
	  TTr = ~ttr_match(D, C, A)
	),
	XN = ~sizelink(TTr, N, SizeLinks),
	NoFree = ~nofree(N, NoFreeVars).

:- redefining(get_prop/3).
get_prop(X, Ps) := P :-
	( member(PX, Ps), arg(1, PX, X), functor(PX, P, 1) ->
	    true
	; P = term
	).

sizelink(TTr, N, SizeLinks) := compound(LengthN) :- _ = ~ttr_compound(TTr), !, contains1(SizeLinks, size_of(N, LengthN)).
sizelink(_, _, _) := single :- !.

nofree(N, NoFreeNs) := yes :- contains1(NoFreeNs, N), !.
nofree(_, _) := no :- !.

{
:- extends errlog_ctx.
check__foreign(Status, PredName, Arguments, Body, VarNames) :-
	assertion_body(_, DP, CP, AP, GP, _, Body),
	check__all_arguments(DP, PredName, Arguments), 
	check__all_arguments(CP, PredName, Arguments), 
	check__all_arguments(AP, PredName, Arguments), 
	check__list_correctness(PredName, DP, GP, Arguments, VarNames),
	check__do_not_free_correctness(PredName, GP, Arguments), 
	check__status(PredName, Status), 
	check__returns(PredName, GP, Arguments).

check__all_arguments([], _, _) :- !.
check__all_arguments([X|Xs], PredName, Arguments) :-
	X =.. [_, Y], 
	( nocontainsx(Arguments, Y) ->
	    (~get_errs).compiler_error(invalid_foreign_arg(PredName))
	; true
	),
	check__all_arguments(Xs, PredName, Arguments).

check__returns(PredName, GP, Arguments) :-
	select(returns(_, Argument), GP, GP0), !, 
	valid_returns_argument(PredName, Arguments, Argument), 
	no_more_returns(PredName, GP0).
check__returns(_, _, _).

valid_returns_argument(PredName, Arguments, Argument) :-
	nocontainsx(Arguments, Argument), !,
	(~get_errs).compiler_error(invalid_foreign_ret(PredName)).
valid_returns_argument(_, _, _).

no_more_returns(_, GP) :-
	\+ member(returns(_, _), GP), 
	!.
no_more_returns(PredName, _) :-
	(~get_errs).compiler_error(dup_returns(PredName)).

returns_in_output_argument(ResN, Arguments, PredName, VarNames) :-
	member(arg(ResN, TTr, _, _), Arguments), !,
	( _ = ~ttr_ctype_res(TTr) ->
	    true
	; var_name(ResN, VarNames, VarName), 
	  (~get_errs).compiler_error(not_foreign_output(VarName, PredName))
	).		      

one_list_for_each_size_of(PredName, DP, GP, Arguments) :-
	member(size_of(_, ListVar, SizeVar), GP), 
	\+ valid_size_of_property(Arguments, ListVar, SizeVar, DP), 
	!, 
	(~get_errs).compiler_error(invalid_foreign_size_of(PredName)).
one_list_for_each_size_of(_, _, _, _).
}.

valid_size_of_property(Arguments, ListVar, SizeVar, DP) :-
	\+ nocontainsx(Arguments, ListVar), 
	\+ nocontainsx(Arguments, SizeVar),
	% TODO: encode in the ttr!
	( \+ nocontainsx(DP, byte_list(ListVar)) -> true
	; \+ nocontainsx(DP, int_list(ListVar)) -> true
	; fail
	),
	\+ nocontainsx(DP, int(SizeVar)).

{
:- extends errlog_ctx.
check__list_correctness(PredName, DP, GP, Arguments, VarNames) :-
	one_list_for_each_size_of(PredName, DP, GP, Arguments), 
	one_size_of_for_each_compound(PredName, DP, GP, VarNames).

one_size_of_for_each_compound(PredName, DP, GP, VarNames) :-
	( member(byte_list(ListVar), DP)
	; member(int_list(ListVar), DP)
	), 
	findall(Y, (member(size_of(_, Y, _), GP), Y==ListVar), S), 
	nonsingle(S), 
	!, 
	var_name(ListVar, VarNames, VarName), 
	(~get_errs).compiler_error(not_unique_foreign_size_of(VarName, PredName)).
one_size_of_for_each_compound(_, _, _, _).
}.

var_name(Var, VarNames, Name) :-
	findall(N, (member(N=X, VarNames), X==Var), [Name]).

{
:- extends errlog_ctx.
check__do_not_free_correctness(PredName, GP, Arguments) :-
	member(do_not_free(_, Var), GP), 
	nocontainsx(Arguments, Var), 
	!, 
	(~get_errs).compiler_error(invalid_do_not_free(PredName)).
check__do_not_free_correctness(_, _, _).
}.

numbers_between(A, B, []) :- A > B,  !.
numbers_between(A, B, [A|Ns]) :-
	A1 is A + 1, 
	numbers_between(A1, B, Ns).

assign_types([], _) := [] :- !.
assign_types([N|Ns], CP) := [arg_type(Type, N)|~assign_types(Ns, CP)] :-
	( member(Prop, CP), Prop =.. [Type, N] ->
	    true
	; Type = term
	).

assign_modes([], _) := [] :- !.
assign_modes([N|Ns], InN) := [A|~assign_modes(Ns, InN)] :-
	A = ( contains1(InN, N) ? in(N) | out(N) ).

{
:- extends errlog_ctx.
check__status(_, true) :- !.
check__status(_, trust) :- !.
check__status(PredName, _) :-
	(~get_errs).compiler_error(invalid_foreign_status(PredName)).
}.
