:- module(contextcore_tr, [contextual_sentence_tr/3], [assertions, regtypes, isomodes]).

% todo: unification of clauses, cuts, etc.? \texttt{p /
%     point :- !, ...} should be translated to \texttt{p(point(...), Z)
%     :- !, Z = point(...), ...}}

% todo: what is the equivalent of head unification flattening for contexts?
% E.g.
%  move/point :- inc(x)
%  move/P :- point <- P, inc(x).
%  move/P :- ^point(x,y) <- P, inc(x).

% ---------------------------------------------------------------------------
%trace(X) :- display(X).
%tracenl :- nl.
trace(_).
tracenl.
% ---------------------------------------------------------------------------

:- use_module(library(lists), [append/3, length/2]).
:- use_module(library(aggregates)).
:- use_module(library(format), [format/2, format_to_string/3]).

% :- sub_module ctxdef {
:- data '$_context_expansion'/3.

ctxdef__clean :-
	retractall_fact('$_context_expansion'(_, _, _)).
% }.

:- include(library(contextcore(contextcore_ops))).

% ---------------------------------------------------------------------------
% :- sub_module aux_name {
%   :- doc(title, "Counter for auxiliary names").

% todo: add module name?

:- data aux_name__count/1.

aux_name__init :-
	assertz_fact(aux_name__count(0)).

aux_name__clean :-
	retractall_fact(aux_name__count(_)).

aux_name__new(Name) :-
	retract_fact(aux_name__count(N)),
	N1 is N + 1,
	assertz_fact(aux_name__count(N1)),
	%
	number_codes(N, NCodes),
	atom_codes(Na, NCodes),
	atom_concat('\6\aux', Na, Name).
%}.

% ---------------------------------------------------------------------------

% mod_clause(Head, Mod, Body) - Sentences in current module
:- data mod__clause/3.
mod__clean(Mod) :-
	retractall_fact(mod__clause(_, Mod, _)).

mod__add_clause(Mod, Head, Body) :-
	assertz_fact(mod__clause(Head, Mod, Body)).

clean(Mod) :-
	mod__clean(Mod),
	aux_name__clean,
	ctxdef__clean,
	control__clean,
	submod__clean.

contextual_sentence_tr(0, [], Mod):- !,
	clean(Mod), % todo: it should not be necessary
	aux_name__init.

contextual_sentence_tr((:- def_context(Def)), Clauses, _Mod):- !,
	ctxdef__read(Def, '\6\root'),
	Clauses = [].
contextual_sentence_tr((:- control('\6\postfix_block'(Pattern, Def))), Clauses, _Mod) :- !,
	control__define(Pattern, Def, '\6\root'),
	Clauses = [].
contextual_sentence_tr((:- sub_module('\6\postfix_block'(Name, Def))), Clauses, _Mod) :- !,
	nonvar(Def),
	read_submod(Name, Def, '\6\root'),
	Clauses = [].
contextual_sentence_tr((:- _), _, _):- !, fail.
contextual_sentence_tr((?- _), _, _):- !, fail.
contextual_sentence_tr(end_of_file, Clauses, Mod):- !,
	expand_rest(Mod, Clauses, Clauses0),
	Clauses0 = [end_of_file],
	clean(Mod).
% Remember clauses (cannot be translated yet)
contextual_sentence_tr((H :- B), Cs, Mod):- !,
	mod__add_clause(Mod, H, B),
	Cs = [].
contextual_sentence_tr(H, Cs, Mod):-
	mod__add_clause(Mod, H, true),
	Cs = [].

% ----------------------------------------------------------------------

expand_rest(Mod, Clauses, Clauses0) :-
	findall(MSubmod, submod__def(MSubmod), MSubmods),
	treat_all_submods(MSubmods),
	expand_mod(Mod, Clauses, Clauses1),
	expand_all_submods(MSubmods, Clauses1, Clauses2),
	expand_all_hooks(Clauses2, Clauses0).

% Treat all the submodules
% (currently: expand extends by copying the contents of the source submodule)
% todo: maybe it can be done without an extra pass:
%   - name resolution looks into extends
%   - list of predicates to emit looks into extends
treat_all_submods([]).
treat_all_submods([MSubmod|MSubmods]) :-
	submod__treat(MSubmod),
	treat_all_submods(MSubmods).

% Expand the module clauses
expand_mod(Mod, Clauses, Clauses0) :-
	findall((H :- B), mod__clause(H, Mod, B), Cs),
	ModEnv = modenv_sub('\6\root'),
	transform_clauses(Cs, ModEnv, Clauses, Clauses0).

% Expand all the submodules
expand_all_submods([], Clauses, Clauses).
expand_all_submods([MSubmod|MSubmods], Clauses, Clauses0) :-
	submod__emit(MSubmod, Clauses, Clauses1),
	expand_all_submods(MSubmods, Clauses1, Clauses0).

expand_all_hooks(Clauses, Clauses0) :-
	findall((N,A), submod__hook_pred(N, A), Hooks),
	emit_hook_decls(Hooks, Clauses, Clauses1),
	findall((H,B), submod__hook_clause(H, B), HookClauses),
	emit_hook_clauses(HookClauses, Clauses1, Clauses0).

emit_hook_decls([], Clauses, Clauses).
emit_hook_decls([(N,A)|Hooks], Clauses, Clauses0) :-
	Clauses = [(:- discontiguous(N/A)), (:- multifile(N/A))|Clauses1],
	emit_hook_decls(Hooks, Clauses1, Clauses0).

emit_hook_clauses([], Clauses, Clauses).
emit_hook_clauses([(H,B)|HookClauses], Clauses, Clauses0) :-
	Clauses = [(H :- B)|Clauses1],
	emit_hook_clauses(HookClauses, Clauses1, Clauses0).

% ----------------------------------------------------------------------

% ModEnv: module environment (i.e. visible modules, names, etc.)

transform_clauses([], _ModEnv, Cs, Cs).
transform_clauses([(H :- B)|Xs], ModEnv, Cs, Cs0) :-
	transform_clause(H, B, ModEnv, Cs, Cs1),
	transform_clauses(Xs, ModEnv, Cs1, Cs0).

transform_clause(Head0, Body, ModEnv, Aux, Aux0):-
	occset__empty(OccSet0),
	% missing: hide variables!
 	trace('Transforming clause: '), trace((Head0 :- Body)), tracenl,
 	trace('  Using modenv: '), trace(ModEnv), tracenl,
	scan_vars(Head0, Body, ModEnv, OccSet0, OccSet),
 	trace('  Occset after scan_vars: '), tracenl,
 	display_occset(2, OccSet),
	%
	Aux = [(NewHead :- NewBody)|Aux1],
	%
	mapping_head(Head0, ModEnv, NewHead, HeadMapping0),
	( HeadMapping0 = none ->
	    none_mapping(HeadMapping)
	; HeadMapping = HeadMapping0
	),
	%
	HeadMapping = mapping(Cin, Gin, Cout, Gout),
	chunk__initial(ChunkN0),
	connect(Body, Cin, Gin, C1, _G1, Body1, ChunkN0, _ChunkN, OccSet, ModEnv, Aux1, Aux0),
% 	trace('  Connected body: '), trace(Body1), tracenl,
	commons(C1, Cout),
	preconditions(Cin, Gin, Body1, Body2),
	%
	( final_goal(Body1, FG), \+ FG=NewHead ->
	    gamma_join(Gin, Gout, Gprim),
	    gamma_diff(Gprim, Cin, C1, Gpost),
	    postconditions(C1, Gpost, Body2, NewBody)
	; NewBody = Body2
	),
 	trace('  Result of transformed clause: '), trace((NewHead :- NewBody)), tracenl.

% ---------------------------------------------------------------------------

final_goal((_, B), C):- !,
	final_goal(B, C).
final_goal((_ -> B), C):- !,
	final_goal(B, C).
final_goal(A, A).

% ---------------------------------------------------------------------------
% Dictionary of context variables

% Lookup in the context variable dictionary
memberco(X, CO, V) :- member(X0=V0, CO), X0 == X, !, V = V0.

% unify value in two context dictionaries, by name
commons([], _).
commons([X=V1|S1], S2):-
	( memberco(X, S2, V2), V1 = V2 -> true
	; true
	),
	commons(S1, S2).	

% ---------------------------------------------------------------------------

context(IO, Spec, ModEnv, T, Cout, Gout):-
	context1(Spec, IO, ModEnv, [], [], [], [], T, Cout, _, Gout-[]).

% ----------------------------------------------------------------------
% Contextual arguments

% Logical variable in context
% JF: modified to give an error
%context1(V, _, _ModEnv, C0, L0, _, _, [V], C0, L0, G-G):- 
%	var(V), !, throw(invalid_context(V)).
context1(A, _, _ModEnv, C0, L0, _, _Pfx, [V], C1, L0, G-G):-
	var(A), !, QA = A, %comb_prefix(Pfx, A, QA), 
	( memberco(QA, C0, V) -> C1 = C0 
	; C1 = [QA=V|C0] 
	).

% Empty context
context1((*), _, _ModEnv, C0, L0, _, _, [], C0, L0, G-G):- !.

% Empty list is an atom, but we want to treat it as a literal, instead
% of as a context variable
%% JF: disabled
% context1([], _, _ModEnv, C0, L0, _, _, [[]], C0, L0, G-G):- !.

% Anything - always produces a fresh variable
% JF: removed ('_' is enough)
% context1((?), _, _ModEnv, C0, L0, _, _, [_], C0, L0, G-G):- !.

% Escaping of functors
context1(^V, _, _ModEnv, C0, L0, _, _, [V], C0, L0, G-G):-
	var(V),  !.
context1(^X, IO, ModEnv, C0, L0, Expanded, Pfx, [X1], C1, L0, GA-GZ):-
	nonvar(X), !,
	X=.. [F|Args],
	context2(IO, ModEnv, C0, L0, Expanded, Pfx, Args, Args1-[], C1, GA-GZ),
	X1=.. [F|Args1].

% Name prefix
% todo: example of use? keep? remove?
context1(F^X, IO, ModEnv, C0, L0, Expanded, Pfx, T, C1, L1, GA-GZ):-
	atom(F), !,
	context1(X, IO, ModEnv, C0, L0, Expanded, [F|Pfx], T, C1, L1, GA-GZ).

% Typing
%% JF: what can I do with this?
context1('::'(X, _Y), ModEnv, IO, C0, L0, Expanded, Pfx, T, C1, L0, GA-GZ):-
	%nonvar(X), 
	context1(X, IO, ModEnv, C0, L0, Expanded, Pfx, T, C0a, L0a, GA-GB),
%	trace(add_typing_all(T,Y)), tracenl,
% JF: disabled
	%add_typing_all(T, Y, C0a, L0a, Expanded, Pfx, C1, GB-GZ),
	L0a = _,
	C1 = C0a,
	GB = GZ,
	!.

% Input only
context1(+X, IO, ModEnv, C0, L0, Expanded, Pfx, T, C1, L1, GA-GZ):- !,
	( IO = i -> context1(X, i, ModEnv, C0, L0, Expanded, Pfx, T, C1, L1, GA-GZ)
	; T=[], C1=C0, L1=L0, GA=GZ
	).

% Output only
context1(-X, IO, ModEnv, C0, L0, Expanded, Pfx, T, C1, L1, GA-GZ):- !,
	( IO = o ->
	    context1(X, o, ModEnv, C0, L0, Expanded, Pfx, T, C1, L1, GA-GZ)
	; T=[], C1=C0, L1=L0, GA=GZ
	).

% Difference notation
context1(X-Y, IO, ModEnv, C0, L0, Expanded, Pfx, T, C1, L1, GA-GZ):- !,
	( IO = i -> context1(X, i, ModEnv, C0, L0, Expanded, Pfx, T, C1, L1, GA-GZ)
	; context1(Y, o, ModEnv, C0, L0, Expanded, Pfx, T, C1, L1, GA-GZ)
	).
% JF: not necessary if difference notation is present?
%% Transformation
%context1((From -> To), IO, ModEnv, C0, L0, Expanded, Pfx, T, C1, L1, GA-GZ):-
%	nonvar(From), callable(From),
%	nonvar(To), callable(To),
%	context1((+From, -([](To))), IO, ModEnv, C0, L0, Expanded, Pfx, T, C1, L1, GA-GZ).

% Joining with comma
context1((X,Y), IO, ModEnv, C0, L0, Expanded, Pfx, T, C1, L0, GA-GZ):- !,
	context1(X, IO, ModEnv, C0, L0, Expanded, Pfx, T1, C0a, L0a, GA-GB),
	context1(Y, IO, ModEnv, C0a, L0a, Expanded, Pfx, T2, C1, _, GB-GZ),
	append(T1, T2, T).

% Contracting list
%% JF: disabled
% context1([X|Y], IO, ModEnv, C0, L0, Expanded, Pfx, T, C1, L0, GA-GZ):- !,
% 	context1(X, IO, ModEnv, C0, L0, Expanded, Pfx, T1, C0a, L0a, GA-GB),
% 	context1(Y, IO, ModEnv, C0a, L0a, Expanded, Pfx, T2, C1, _, GB-GZ),
% 	to_list(T1, T2, T).

% Extension atom for structure
% todo: disable?
context1((\X), IO, ModEnv, C0, L0, Expanded, Pfx, T, C1, L1, GA-GZ):-
	extname(X, A), !,
	context1(A, IO, ModEnv, C0, L0, Expanded, Pfx, T, C1, L1, GA-GZ).

% Local macro definition
%% JF: disabled
% context1((M=E), _, _ModEnv, C0, L0, _, _, [], C0, [N=E|L0], G-G):- 
% 	nonvar(M), 
% 	( M= \X, nonvar(X), callable(X) -> extname(X, N)
% 	; atom(M), N= M
% 	), !.

% Closed form
context1([](X), IO, ModEnv, C0, L0, Exp, Pfx, T, C1, L1, GA-GZ):-
	cl_form(X, Y), !,
	context1(Y, IO, ModEnv, C0, L0, Exp, Pfx, T, C1, L1, GA-GZ).

% Context macro expansion
context1(A, IO, ModEnv, C0, L0, Expanded, Pfx, T, C1, L0, GA-GZ):-
	nonvar(A), callable(A), 
	( member(A=Expansion, L0)
	; Module = [],
	  msexpand__resolve_name(context, A, 0, Module, ModEnv, Def),
	  Def = ctxdef(Expansion)
	), 
	!,
%	trace(exp(A, Expanded)), tracenl,
	( \+ member(A, Expanded) -> 
            context1(Expansion, IO, ModEnv, C0, L0, [A|Expanded], Pfx, T, C1, _, GA-GZ)
	; throw(invalid_context(circular_reference(A)))
	).

% Context variable
context1(A, _, _ModEnv, C0, L0, _, Pfx, [V], C1, L0, G-G):-
	atom(A), !, comb_prefix(Pfx, A, QA), 
	( memberco(QA, C0, V) -> C1 = C0 
	; C1 = [QA=V|C0] 
	).

% % Other atomic stuff taken literally
% context1(X, _, _ModEnv, C0, L0, _, _, [X], C0, L0, G-G):-
% 	atomic(X), !.

% Throw an error on other stuff
context1(X, _, _ModEnv, _, _, _, _, _, _, _, _):-
	throw(invalid_context(unknown_macro(X))).

% ......................................................................

cl_form(X, X):- var(X), !.
cl_form((X,Y), ([](X), [](Y))):- !.
cl_form((X->Y), (([](X)->Y))):- !.
cl_form([X|Y], [[](X)|[](Y)]):- !.
cl_form(^X, ^X):- var(X), !.
cl_form(^X, ^Y):- !,
	X=.. [F | A],
	cl_arg(A, CA),
	Y=.. [F | CA].
cl_form(+X, +([](X))):- !.
cl_form(-X, -([](X))):- !.
cl_form(X::Y, [](X)::Y):- !.
cl_form(M=E, M=E):- !.
cl_form(\X, \X):- !.
cl_form(X, (\X=[], X)):-
	callable(X), !.
cl_form(X, X).
	
cl_arg([], []).
cl_arg([A|L], [CA|R]):-
	cl_form(A, CA),
	cl_arg(L, R).

% ---------------------------------------------------------------------------

good_macro(X):- 
	nonvar(X), callable(X), functor(X, F, A), 
	\+ bad_macro(F, A).

bad_macro('*', 0).
bad_macro('?', 0).
bad_macro(',', 2).
bad_macro('->', 2).
bad_macro('::', 2).
bad_macro('[]', 0).
bad_macro('\\', 1).
bad_macro('.', 2).
bad_macro(',', 2).
bad_macro('^', 1).
bad_macro('+', 1).
bad_macro('-', 1).
bad_macro('=', 2).
bad_macro(':', 2).
bad_macro('-', 2).

% ---------------------------------------------------------------------------

context2(_, _ModEnv, C0, _, _, _, [], A-A, C0, G-G):- !.
context2(IO, ModEnv, C0, L0, Expanded, Pfx, [H|T], A-Z, Cout, GA-GZ):-
	context1(H, IO, ModEnv, C0, L0, Expanded, Pfx, H1, C1, L1, GA-GB),
	set_difflist(H1, A-B),
	context2(IO, ModEnv, C1, L1, Expanded, Pfx, T, B-Z, Cout, GB-GZ).

% ---------------------------------------------------------------------------

extname(X, A):-
	nonvar(X), 
	functor(X, XF, XA),
	format_to_string("$_~w/~w_ext", [XF, XA], S),
	atom_codes(A, S).

% ---------------------------------------------------------------------------

% :- export(complement/3).

complement([], C, C).
complement([A=V|B], C0, C1):-
	overridden(A=V, C0), !,
	complement(B, C0, C1).
complement([A=V|B], C0, C1):-
	complement(B, [A=V|C0], C1).

% :- export(overridden/2).

overridden(A=_, [A0=_|_]):- A == A0,
	!.
overridden(X, [_|L]):-
	overridden(X, L).


comb_prefix([], V, V).
comb_prefix([P|A], V, R):-
	comb_prefix(A, P^V, R).

set_difflist([], Z-Z).
set_difflist([X|L], [X|A]-Z):-
	set_difflist(L, A-Z).

%:- export(set_difflist/2).

comb_pair([], []).
comb_pair([X], [X]).
comb_pair([X, Y], [(X, Y)]).

to_list(X, [], [X]):- !.
to_list([], [A|_], [A]):- !.
to_list([X|L], [A|_], [R]):- 
	to_list2([X|L], A, R).

to_list2([X], A, [X|A]):- !.
to_list2([X,Y|L], A, [X|R]):-
	to_list2([Y|L], A, R).

rectail([A], A).
rectail([A,B|L], [A|R]):-
	rectail([B|L], R).

entail([X], A, [X|A]).
entail([X,Y|L], A, [X|R]):-
	entail([Y|L], A, R).

%:- export(to_list/3).

% ---------------------------------------------------------------------------

%:- export(mapping/7).
mapping_head(Goal0, ModEnv, Goal, Mapping):-
	extract_module_and_context(Goal0, Module, Context0, SubGoal0),
	msexpand__goal(SubGoal0, Module, ModEnv, SubGoal1, _Retranslate, ImplicitContext, _Kind),
%        trace(implicit_context(ImplicitContext)), tracenl,
	( Module = [] -> true
	; error(['bad module qualification ', Module, ' in head ', Goal0]) % todo: allow?
	),
%	trace(mshead(SubGoal0, Module, ModEnv, SubGoal1, Retranslate, ImplicitContext, _Kind)), tracenl,
	nonvar(SubGoal1),
	concat_context(Context0, ImplicitContext, Context),
	Goal1 = SubGoal1,
	nonvar(Goal1), callable(Goal1),
	%
	add_contextual_args(Goal1, Context, ModEnv, Goal, Mapping).
%        trace(mapping_head(Goal)), tracenl.

% ----------------------------------------------------------------------

merge([], C1, C2, [], [], L):- !,
	commons(C1, C2),
	complement(C1, C2, L).
merge([A=V0|R], C1, C2, X1, X2, [A=NV|NR]):-
	append(PC1, [A1=V1|SC1], C1), A == A1, append(PC2, [A2=V2|SC2], C2), A == A2,
	append(PC1, SC1, C1x),
	append(PC2, SC2, C2x),
	( V0==V1 ->
	    ( V0==V2 -> NV= V0, X1=X1a, X2=X2a
	    ; X1= [V1=V2|X1a], X2=X2a, NV= V2
	    )
	; ( V0==V2 -> X2=[V1=V2|X2a], X1=X1a, NV= V1
	  ; V1=V2, NV=V1, X2= X2a, X1=X1a
	  )
	),
	merge(R, C1x, C2x, X1a, X2a, NR).

conc_goals((P, Q), R, (P, U)):- !,
	conc_goals(Q, R, U).
conc_goals((P -> Q), R, (P -> U)):-
	conc_goals(Q, R, U).
conc_goals(P, (Q,R), (P, Q, R)):- !.
conc_goals(P, (Q -> R), (P, Q -> R)):- !.
conc_goals(P, Q, (P, Q)).

add_eqns([], G, G):- !.
add_eqns(L, G, G2):-
	split_eqns(L, L1, L2),
	C1=.. [context | L1],
	C2=.. [context | L2],
	conc_goals(C1=C2, G, G2).

split_eqns([], [], []).
split_eqns([V1=V2|A], [V1|B], [V2|C]):-
	split_eqns(A, B, C).


union([], C2, C2):- !.
union([A=V|B], C2, [A=V|L]):-
	( append(Prefix, [A0=V|Suffix], C2), A0 == A ->
	    append(Prefix, Suffix, C2a)
	; C2a= C2
	),
	union(B, C2a, L).

% ----------------------------------------------------------------------
% Submodules

% submod__def(MSubmod)
:- data submod__def/1.
% submod__prop(MSubmod, Prop)
:- data submod__prop/2.
% submod__submod(Submod, ParentMSubmod, MSubmod) 
%  (for hierarchical modules, Submod is a submod of MSubmod, with expanded name MSubmod)
:- data submod__submod/3.
% submod__parent(MSubmod, ParentMSubmod) 
:- data submod__parent/2.
% submod__extends(Submod, ParentMSubmod) 
%  ParentMSubmod extends Submod
:- data submod__extends/2.
% submod__pred(Name, Arity, MSubmod, ExpandedName)
:- data submod__pred/4.
% submod__pred_kind(Name, Arity, MSubmod, Kind)
:- data submod__pred_kind/4.
% submod__clause(Name, Arity, MSubmod, Head, Body)
:- data submod__clause/5.
% submod__hook_clause(MHead, MBody)
%  (for id_interface hooks)
:- data submod__hook_clause/2.
% submod__hook_pred(MN, A)
%  (for id_interface hooks)
:- data submod__hook_pred/2.

submod__all_preds(MSubmod, Names) :-
	findall((N,A), submod__pred(N, A, MSubmod, _), Names).

submod__clean :-
	retractall_fact(submod__def(_)),
	retractall_fact(submod__prop(_, _)),
	retractall_fact(submod__submod(_, _, _)),
	retractall_fact(submod__parent(_, _)),
	retractall_fact(submod__pred(_, _, _, _)),
	retractall_fact(submod__pred_kind(_, _, _, _)),
	retractall_fact(submod__clause(_, _, _, _, _)),
	retractall_fact(submod__hook_clause(_, _)),
	retractall_fact(submod__hook_pred(_, _)).

submod__set_prop(MSubmod, Prop) :-
	( current_fact(submod__prop(MSubmod, _)) ->
	    true
	; assertz_fact(submod__prop(MSubmod, Prop))
	).

submod__get_prop(MSubmod, Prop) :-
	current_fact(submod__prop(MSubmod, Prop)).

submod__add_extends(MSubmod, Submod) :-
	( current_fact(submod__extends(Submod, MSubmod)) ->
	    true
	; assertz_fact(submod__extends(Submod, MSubmod))
	).

read_submod(Name, Def, ParentMSubmod) :-
	Def = '\6\curly_block'(Clauses),
	submod__ensure_defined(Name, ParentMSubmod, MSubmod),
	read_submod__sentences(Clauses, MSubmod).

% Ensure that Submod is defined in ParentMSubmod, and fills the
% submod__submod/3 fact that relates the unqualified name Submod
% inside ParentMSubmod to the expanded MSubmod.
submod__ensure_defined(Submod, ParentMSubmod, MSubmod) :-
	qualified_name(ParentMSubmod, Submod, MSubmod),
	trace(submod__submod(Submod, ParentMSubmod, MSubmod)), tracenl,
	( current_fact(submod__def(MSubmod)) ->
	    true
	; assertz_fact(submod__submod(Submod, ParentMSubmod, MSubmod)),
	  assertz_fact(submod__parent(MSubmod, ParentMSubmod)),
	  assertz_fact(submod__def(MSubmod)) % todo: necessary?
	).

read_submod__sentences([], _).
read_submod__sentences([C|Cs], MSubmod) :-
	read_submod__sentence(C, MSubmod),
	read_submod__sentences(Cs, MSubmod).

read_submod__sentence(A, _MSubmod) :- var(A), !, throw(bug).
% --
read_submod__sentence((:- abstract), MSubmod) :- !,
	submod__set_prop(MSubmod, abstract).
% --
% id_interface
read_submod__sentence((:- id_interface), MSubmod) :- !,
	submod__set_prop(MSubmod, id_interface).
read_submod__sentence((:- id(Name)), MSubmod) :- atom(Name), !,
	submod__set_prop(MSubmod, id(Name)).
% --
read_submod__sentence((:- implicit_context(Ctx)), MSubmod) :- !,
	submod__set_prop(MSubmod, implicit_context(Ctx)).
% --
read_submod__sentence((:- virtual(Pred)), MSubmod) :- nonvar(Pred), Pred = N/A, !,
	submod__register_pred(MSubmod, N, A),
	submod__set_pred_kind(MSubmod, N, A, virtual).
% --
read_submod__sentence((:- data(Pred)), MSubmod) :- nonvar(Pred), Pred = N/A, !,
	submod__register_pred(MSubmod, N, A),
	submod__set_pred_kind(MSubmod, N, A, data).
% --
read_submod__sentence((:- extends(Submod)), MSubmod) :- !,
	submod__add_extends(MSubmod, Submod).
% --
read_submod__sentence((:- def_context(Def)), MSubmod) :- !,
	ctxdef__read(Def, MSubmod).
% --
read_submod__sentence((:- control('\6\postfix_block'(Pattern, Def))), MSubmod) :- !,
	control__define(Pattern, Def, MSubmod).
% --
read_submod__sentence((:- sub_module('\6\postfix_block'(Name, Def))), MSubmod) :- !,
	nonvar(Def),
	read_submod(Name, Def, MSubmod).
% --
read_submod__sentence((:- Decl), _MSubmod) :- !,
	error(['Unsupported declaration ', ''(Decl)]).
% --
read_submod__sentence((Head :- Body), MSubmod) :-
	submod__add_clause(MSubmod, Head, Body).
read_submod__sentence(Head, MSubmod) :-
	submod__add_clause(MSubmod, Head, true).

qualified_name('\6\root', N, MN) :- !, MN = N.
qualified_name(MSubmod, N, MN) :-
	atom_concat(MSubmod, '__', N1),
	atom_concat(N1, N, MN).

% todo: JF: doing this before contextual argument expansion
goal_name_arity(Goal0, N, A) :-
	extract_module_and_context(Goal0, _Module, _Context, Goal1),
	functor(Goal1, N, A).

submod__add_clause(MSubmod, Head, Body) :-
	goal_name_arity(Head, N, A),
	trace(submod__register_pred(MSubmod, N, A)), tracenl,
	submod__register_pred(MSubmod, N, A),
	assertz_fact(submod__clause(N, A, MSubmod, Head, Body)).

submod__register_pred(MSubmod, N, A) :-
	( current_fact(submod__pred(N, A, MSubmod, _)) ->
	    true
	; qualified_name(MSubmod, N, MN),
	  trace(submod__pred(N, A, MSubmod, MN)), tracenl,
	  assertz_fact(submod__pred(N, A, MSubmod, MN))
	).

submod__set_pred_kind(MSubmod, N, A, Kind) :-
	( current_fact(submod__pred_kind(N, A, MSubmod, OldKind)) ->
	    ( OldKind = Kind ->
	        true
	    ; error(['Predicate ', N, '/', A, ' in submodule ', MSubmod, ' already defined as ', OldKind, ', cannot be defined as ', Kind])
	    )
	; assertz_fact(submod__pred_kind(N, A, MSubmod, Kind))
	).

submod__get_pred_kind(MSubmod, N, A, Kind) :-
	( current_fact(submod__pred_kind(N, A, MSubmod, Kind0)) ->
	    Kind = Kind0
	; Kind = normal
	).

submod__treat(MSubmod) :-
	% treat 'extends'
	% todo: this is incomplete! copying may not be the right way to implement it...
	( current_fact(submod__extends(SourceSubmod, MSubmod)),
	    msexpand__submod(SourceSubmod, modenv_sub(MSubmod), MSourceSubmod),
%           trace(extended(MSubmod, MSourceSubmod)), tracenl,
	    ( submod__get_prop(MSourceSubmod, abstract) ->
	        submod__copy(MSourceSubmod, MSubmod)
	    ; submod__get_prop(MSourceSubmod, id_interface) ->
	        emit_id_interface_entries(MSourceSubmod, MSubmod)
	    ; error(['Extend not allowed for ', ''(SourceSubmod)])
	    )
	; true
	).

submod__copy(MSourceSubmod, MSubmod) :-
	submod__all_preds(MSourceSubmod, Names),
	submod__copy__preds(Names, MSourceSubmod, MSubmod).

submod__copy__preds([], _MSourceSubmod, _MSubmod).
submod__copy__preds([(N,A)|Names], MSourceSubmod, MSubmod) :-
	submod__copy__pred(N, A, MSourceSubmod, MSubmod),
	submod__copy__preds(Names, MSourceSubmod, MSubmod).

submod__copy__pred(N, A, MSourceSubmod, MSubmod) :-
	submod__get_pred_kind(MSourceSubmod, N, A, Kind),
	( Kind = normal ->
	    ( current_fact(submod__clause(N, A, MSourceSubmod, Head, Body)),
	        trace(copying_submod__add_clause(MSourceSubmod, MSubmod, Head, Body)), tracenl,
	        submod__add_clause(MSubmod, Head, Body),
	        fail
	    ; true
	    )
	; Kind = data ->
	    submod__register_pred(MSubmod, N, A),
	    submod__set_pred_kind(MSubmod, N, A, data)
	; error(['Unknown predicate kind ', ''(Kind), ' while extending (copy) ', MSourceSubmod, ' into ', MSubmod]) % todo: this should be a bug
	).

emit_id_interface_entries(MSourceSubmod, MSubmod) :-
	submod__get_prop(MSubmod, id(Id)),
	( current_fact(submod__pred(N, A, MSourceSubmod, MSourceN)),
	  current_fact(submod__pred(N, A, MSubmod, MN)),
	    A1 is A + 1,
	    functor(MSourceHead, MSourceN, A1),
	    MSourceHead =.. [_,Id|Args],
	    MHead =.. [MN|Args],
	    ( current_fact(submod__hook_pred(MSourceN, A1)) ->
	        true
	    ; assertz_fact(submod__hook_pred(MSourceN, A1))
	    ),
	    assertz_fact(submod__hook_clause(MSourceHead, MHead)),
	    trace(submod__hook_clause(MSourceHead, MHead)), tracenl,
	    fail
	; true
	).
	
submod__emit(MSubmod, Clauses, Clauses0) :-
	( submod__do_not_materialize(MSubmod) ->
	    Clauses = Clauses0
	; submod__all_preds(MSubmod, Names),
	  submod__emit__preds(Names, MSubmod, Clauses, Clauses0)
	).

submod__do_not_materialize(MSubmod) :-
	( submod__get_prop(MSubmod, abstract) ->
	    % submod cannot be materialized since it is abstract
	    true
	; submod__get_prop(MSubmod, interface) ->
	    % submod cannot be materialized since it is an interface
	    true
	).

submod__emit__preds([], _MSubmod, Clauses, Clauses).
submod__emit__preds([(N,A)|Names], MSubmod, Clauses, Clauses0) :-
	submod__emit__pred(N, A, MSubmod, Clauses, Clauses1),
	submod__emit__preds(Names, MSubmod, Clauses1, Clauses0).

submod__emit__pred(N, A, MSubmod, Clauses, Clauses0) :-
	submod__get_pred_kind(MSubmod, N, A, Kind),
	( Kind = normal ->
	    findall((Head :- Body), submod__clause(N, A, MSubmod, Head, Body), Cs),
	    transform_clauses(Cs, modenv_sub(MSubmod), Clauses, Clauses0)
	; Kind = data ->
	    current_fact(submod__pred(N, A, MSubmod, MN)),
	    Clauses = [(:- data MN/A)|Clauses0]
	; Kind = virtual ->
	    Clauses = Clauses0
	; error(['Unknown predicate kind ', ''(Kind), ' while emitting predicate ', ''(N/A), ' in ', MSubmod]) % todo: this should be a bug
	).

one_parent_msubmod(MSubmod, OneMSubmod) :-
	( OneMSubmod = MSubmod
	; current_fact(submod__parent(MSubmod, OneMSubmod0)),
	  one_parent_msubmod(OneMSubmod0, OneMSubmod)
	).

% Expand a module name N in module Module, obtaining MN (we are hierarchical)
% todo: maybe, split the process: one is name resolution and other module navigation
msexpand__submod(Submod, _ModEnv, _MSubmod) :- var(Submod), !, throw(not_implemented_var_in_submod).
msexpand__submod((A:B), ModEnv, MSubmod) :- !,
	% Look for B directly defined as part of A (module navigation)
	msexpand__submod(A, ModEnv, MA),
	msexpand__submod(B, modenv_sub_noparent(MA), MSubmod).
msexpand__submod(Submod, ModEnv, MSubmod) :-
	( ModEnv = modenv_sub(ParentMSubmod) ->
	    ( one_parent_msubmod(ParentMSubmod, OneParentMSubmod),
	      current_fact(submod__submod(Submod, OneParentMSubmod, MSubmod0)) ->
	        % Found in a submodule (from current module environment)
	        MSubmod = MSubmod0
	    ; % Not found, do nothing
	      MSubmod = Submod % todo: this is probably wrong! it should emit an error
	    )
	; ModEnv = modenv_sub_noparent(ParentMSubmod) -> % Do not try to search in parents
	    ( current_fact(submod__submod(Submod, ParentMSubmod, MSubmod0)) ->
	        % Found in a submodule (from current module environment)
	        MSubmod = MSubmod0
	    ; % Not found, do nothing
	      MSubmod = Submod % todo: this is probably wrong! it should emit an error
	    )
	).

% note: Equivalent to module resolution in mexpand.pl! Merge!
% NameKind: pred - resolve a predicate name
%           control - resolve a control pattern name
msexpand__resolve_name(NameKind, N, A, Module, ModEnv, Def) :-
	( Module = [QualSubmod] ->
%	    trace(msexpand__submod(QualSubmod, ModEnv, QualMSubmod)), tracenl,
	    ( msexpand__submod(QualSubmod, ModEnv, QualMSubmod),
	      msexpand__search_name(NameKind, N, A, QualMSubmod, Def0) ->
	        % Found in a submodule (from module qualification)
	        Def = Def0
	    ; % Not found, do nothing
	      fail
	    )
	; Module = [] ->
	    ( ModEnv = modenv_aux(AuxDic, ModEnv1) ->
	        ( dic_get(AuxDic, N, AuxDef) ->
		    MN = AuxDef,
		    Kind = normal, % todo: may be wrong
		    Def = preddef(MN, Kind)
		; msexpand__resolve_name(NameKind, N, A, Module, ModEnv1, Def)
		)
	    ; ModEnv = modenv_sub(MSubmod),
	      one_parent_msubmod(MSubmod, OneMSubmod),
	      msexpand__search_name(NameKind, N, A, OneMSubmod, Def0) ->
	        % Found in a submodule (from current module environment)
	        Def = Def0
	    ; % Not found, do nothing
	      fail
	    )
	).

msexpand__search_name(pred, N, A, MSubmod, Def) :-
	( current_fact(submod__pred(N, A, MSubmod, MN)) ->
	    submod__get_pred_kind(MSubmod, N, A, Kind),
	    Def = preddef(MN, Kind)
	; fail
	).
msexpand__search_name(control, N, A, MSubmod, Def) :-
	( functor(Loop, N, A),
	  current_fact(def_control(do(Loop, IterGoalDef), MSubmod, Def0)) ->
	    Def = ctldef(Loop, IterGoalDef, Def0)
	; fail
	).
msexpand__search_name(context, N, _A, MSubmod, Def) :- % todo: could arity be useful?
	( '$_context_expansion'(N, Expansion, MSubmod) ->
	    Def = ctxdef(Expansion)
	; fail
	).

maybe_get_module_var(Module0, Module, ModuleVar) :-
	( Module0 = [Module1], nonvar(Module1), Module1 = (ModuleVar0::Module2) ->
	    Module = [Module2],
	    ModuleVar = yes(ModuleVar0)
	; Module = Module0,
	  ModuleVar = no
	).

msexpand__goal(Goal0, Module0, ModEnv, Goal, Retranslate, ImplicitContext, Kind) :-
	% todo: use context as module var?
	maybe_get_module_var(Module0, Module, ModuleVar),
%	trace(Module::ModuleVar), tracenl,
%	trace(call_name_arity_a(Goal0, N, A)), tracenl,
	call_name_arity(Goal0, N, A),
%	trace(call_name_arity_m(Goal0, N, A)), tracenl,
	( msexpand__resolve_name(pred, N, A, Module, ModEnv, preddef(MN, Kind)) ->
	    call_args(Goal0, Args0),
	    ( ModuleVar = yes(ModuleVar0) ->
	        Args = [ModuleVar0|Args0]
	    ; Args = Args0
	    ),
	    apply_aux_def(MN, Args, Goal, Retranslate, ImplicitContext),
	    trace('    Pred '), trace(N), trace('/'), trace(A), trace(' with arguments '), trace(Args), trace(' is defined as: '), trace(MN), trace(' which results in: '), trace(Goal), trace(' with implicit context: '), trace(ImplicitContext), tracenl
	; % Not found, do nothing
	  Retranslate = no,
	  Kind = unknown,
	  ImplicitContext = '*',
	  put_module(Module, Goal0, Goal)
	).

apply_aux_def(pr(AuxName, v(Shared, CShared)), Args, Term, Retranslate, ImplicitContext) :- !,
	append(Args, Shared, Args2),
	Term =.. [AuxName|Args2],
	( CShared = [] ->
	    Retranslate = no,
	    ImplicitContext = '*'
	; list_to_conj(CShared, Ctx),
	  ImplicitContext = Ctx,
	  Retranslate = no
	).
apply_aux_def(prdef(Args, Def, ChunkN0, ModEnv), CallArgs, Term, Retranslate, ImplicitContext) :- !,
	% todo: arguments are ignored, it can only be called once
	length(Args, A),
	length(CallArgs, CA),
	( A = CA ->
	    true
	; error(['Predicate requires ', ''(A), ' arguments'])
	),
	emit_unify_args(Args, CallArgs, Term, Term0),
	Term0 = Def,
	ImplicitContext = '*',
	Retranslate = yes(ChunkN0, ModEnv).
apply_aux_def(MN, Args, Goal, Retranslate, ImplicitContext) :- atom(MN), !,
	Goal =.. [MN|Args],
	ImplicitContext = '*',
	Retranslate = no.
 
put_module(Module, Goal0, Goal) :-
	( Module = [] -> Goal = Goal0
	; Module = [M] -> Goal = M:Goal0
	).

% ----------------------------------------------------------------------
% :- sub_module control {
% :- doc(title, "Custom control structures").

% def_control(Loop, MSubmod, IterGoalDef)
:- data def_control/3.

control__clean :-
	retractall_fact(def_control(_, _, _)).

control__define(Pattern0, Def, MSubmod) :-
        % todo: allow contextual variables in patterns for_each[X,List] do X<-X*3
%	scan_pattern(Pattern0, Pattern),
%        display(pattern0(Pattern0)), nl,
%        display(pattern(Pattern)), nl,
        Pattern = Pattern0,
	nonvar(Pattern),
	( current_fact(def_control(Pattern, MSubmod, _)) ->
	    error(['Attempt to redefine control pattern ', ''(Pattern),
	           ' in module ', MSubmod]),
	    fail
	; assertz_fact(def_control(Pattern, MSubmod, Def))
	).

% todo: finish pattern scanning (for ::goal annotations)
% scan_pattern(Term, ChunkN, OccSet0, OccSet) :- var(Term), !,
% 	trace(occset__record(Term, normal, ChunkN)), tracenl,
% 	occset__record(Term, normal, ChunkN, OccSet0, OccSet).
% scan_pattern(Term, ChunkN, OccSet0, OccSet) :-
% 	functor(Term, _N, A),
% 	scan_args_vars(1, A, Term, ChunkN, OccSet0, OccSet).
% 
% scan_args_vars(I0, I, _Term, _ChunkN, OccSet0, OccSet) :- I0 > I, !,
% 	OccSet = OccSet0.
% scan_args_vars(I0, I, Term, ChunkN, OccSet0, OccSet) :-
% 	arg(I0, Term, Arg0),
% 	scan_pattern(Arg0, ChunkN, OccSet0, OccSet1),
% 	I1 is I0 + 1,
% 	scan_args_vars(I1, I, Term, ChunkN, OccSet1, OccSet).

:- use_module(library(sort), [sort/2]).
:- use_module(library(dict)).

% Algorithm:
%   Given clause H :- Body, where Body == ..., LoopHead do IterGoal, ...
%   We define:
%     - AllVars: all variables in the clause (Body)
%     - GoalVars: all variables in the iterated goal (IterGoal)
%     - LoopEscapeVars: local variables in the IterGoal

is_custom_control(do(_,_)).

lookup_control_def(Loop0, ModEnv, IterGoalDef, Def0) :-
	nonvar(Loop0),
        % todo: use more general module extractor
	( Loop0 = M:Loop ->
	    Module = [M]
	; Loop = Loop0,
	  Module = []
	),
	% find control definition
	( functor(Loop, N, A),
	  msexpand__resolve_name(control, N, A, Module, ModEnv, CtlDef),
	  CtlDef = ctldef(Loop, IterGoalDef, Def0) ->
	    true
	; throw(control_not_defined(Loop0))
	).

get_def_control_goal(Loop0, ModEnv, IterGoalArgs, CallGoal, AuxClauses) :-
        % todo: use more general module extractor
	lookup_control_def(Loop0, ModEnv, IterGoalDef, Def),
	call_args(IterGoalDef, IterGoalArgs),
	scan_control_def(Def, AuxClauses, CallGoal, _AuxHeads).

expand_custom(do(Loop, IterGoal), ChunkN0, ChunkN, OccSet, ModEnv, Aux, Aux0, NewGoal, NewModEnv) :-
	lookup_control_def(Loop, ModEnv, IterGoalDef, Def),
	%
 	trace('  Expanding loop:     '), trace(do(Loop, IterGoal)), tracenl,
 	trace('  Expanding loop def: '), trace(Def), tracenl,
        trace('            itergoal: '), trace(IterGoalDef), tracenl,
	%
	chunk__sub(ChunkN0, ChunkN, SubChunkN0),
 	trace('    ChunkN0:    '), trace(ChunkN0), tracenl,
 	trace('    ChunkN:     '), trace(ChunkN), tracenl,
 	trace('    SubChunkN0: '), trace(SubChunkN0), tracenl,
 	trace('    Occset is:  '), tracenl,
 	display_occset(3, OccSet),
	occset__get_shared(OccSet, SubChunkN0, Shared),
	%
	call_args(IterGoalDef, IterGoalArgs),
	call_name_arity(IterGoalDef, IterGoalName, _),
	scan_control_def(Def, AuxClauses, Goal, AuxHeads),
% 	trace(aux_heads(AuxHeads)), tracenl,
% 	trace(aux_clauses(AuxClauses)), tracenl,
% 	trace(goal(Goal)), tracenl,
	aux_dic(AuxHeads, Shared, AuxDic),
%	trace(itergoalname(IterGoalName)), tracenl,
%	trace(itergoal(IterGoal)), tracenl,
	% Define an auxiliary clause for iter goal, that is going to be inlined, and shares all variables
	dic_lookup(AuxDic, IterGoalName, prdef(IterGoalArgs, IterGoal, ChunkN0, ModEnv)),
 	trace('    Auxdic is:'), tracenl,
	display_auxdic(AuxDic, 3),
% 	trace(aux_clauses2(AuxClauses2)), tracenl,
% 	trace(goal2(Goal2)), tracenl,
	% todo: the following call recomputes the OccSet! (with some effort, it could be reused)
	NewGoal = Goal,
	NewModEnv = modenv_aux(AuxDic, ModEnv),
	transform_clauses(AuxClauses, NewModEnv, Aux, Aux0).
% 	trace(loop_expansion(Call)), tracenl.

% Decompose the control definition into the entry Goal, auxiliar clauses and auxiliar heads
scan_control_def('\6\curly_block'(Def), AuxClauses, Goal, AuxHeads) :-
	split_clauses_and_goals(Def, AuxClauses, [], Goals0, []),
	list_to_disj(Goals0, Goal),
	aux_clauses_heads(AuxClauses, AuxHeads0),
	sort(AuxHeads0, AuxHeads).

aux_clauses_heads([], []).
aux_clauses_heads([(H :- _)|Clauses], [Head|AuxHeads]) :-
	( call_name_arity(H, Head, _) ->
	    true
	; throw(invalid_aux_clause_head(H))
	),
	aux_clauses_heads(Clauses, AuxHeads).

% Create a renaming dictionary for auxiliar predicates
aux_dic([], _Shared, _AuxDic).
aux_dic([N|Ns], Shared, AuxDic) :-
	aux_name__new(AuxName),
	% todo: remove shared variables that are not required (transitively)
	dic_lookup(AuxDic, N, pr(AuxName, Shared)),
	aux_dic(Ns, Shared, AuxDic).

emit_unify_args([], [], Term, Term).
emit_unify_args([X|Xs], [Y|Ys], Term, Term0) :-
	( X == Y ->
	    Term = Term1 % unification was not necessary
	; Term = (X = Y, Term1) % emit unification
	),
	emit_unify_args(Xs, Ys, Term1, Term0).

split_clauses_and_goals([], Clauses, Clauses0, Goals, Goals0) :- !,
	Clauses = Clauses0,
	Goals = Goals0.
split_clauses_and_goals([X0|Xs], Clauses, Clauses0, Goals, Goals0) :-
	nonvar(X0),
	( X0 = (Head :- Body) ->
	    X = X0
	; X = (X0 :- true)
	),
	( nonvar(Head), Head = '' ->
	    Clauses = Clauses1,
	    Goals = [Body|Goals1]
	; Clauses = [X|Clauses1],
	  Goals = Goals1
	),
	split_clauses_and_goals(Xs, Clauses1, Clauses0, Goals1, Goals0).

% From a head and body, scan normal and contextual variable
% occurrences, annotating in which chunk each variable appears.
% If a variable appears in more than one chunk, it must be shared.
%
% todo: limited support for contexts todo: reuse recursion pattern!
% (define control patterns for goals)
scan_vars(Head, Body, ModEnv, OccSet0, OccSet) :-
	chunk__initial(ChunkN0),
	trace('Scanning head'(Head, ChunkN0)), tracenl,
	scan_goal_vars(Head, ModEnv, ChunkN0, ChunkN1, OccSet0, OccSet1),
	trace('Scanning body'(ChunkN1)), tracenl,
	scan_goal_vars(Body, ModEnv, ChunkN1, _ChunkN, OccSet1, OccSet).

scan_goal_vars(A, _ModEnv, ChunkN0, ChunkN, OccSet0, OccSet) :- var(A), !,
	% normal var
	ChunkN = ChunkN0,
	occset__record(A, normal, ChunkN, OccSet0, OccSet).
scan_goal_vars((A,B), ModEnv, ChunkN0, ChunkN, OccSet0, OccSet) :- !,
	scan_goal_vars(A, ModEnv, ChunkN0, ChunkN1, OccSet0, OccSet1),
	scan_goal_vars(B, ModEnv, ChunkN1, ChunkN, OccSet1, OccSet).
scan_goal_vars((A;B), ModEnv, ChunkN0, ChunkN, OccSet0, OccSet) :- !,
	scan_goal_vars(A, ModEnv, ChunkN0, ChunkN1, OccSet0, OccSet1),
	scan_goal_vars(B, ModEnv, ChunkN1, ChunkN, OccSet1, OccSet).
scan_goal_vars((A->B), ModEnv, ChunkN0, ChunkN, OccSet0, OccSet) :- !,
	scan_goal_vars(A, ModEnv, ChunkN0, ChunkN1, OccSet0, OccSet1),
	scan_goal_vars(B, ModEnv, ChunkN1, ChunkN, OccSet1, OccSet).
scan_goal_vars(G, ModEnv, ChunkN0, ChunkN, OccSet0, OccSet) :- is_custom_control(G), !,
	G = do(Loop, IterGoal),
	get_def_control_goal(Loop, ModEnv, IterGoalArgs, CallGoal, AuxClauses),
	trace('TODO rename IterGoalArgs: '), trace(IterGoalArgs), tracenl,
%	trace('CallGoal: '), trace(CallGoal), tracenl,
	% todo: kludge! spurious hiord vars
	scan_goal_vars(CallGoal, ModEnv, ChunkN0, ChunkN1, OccSet0, OccSet1),
	OccSet2 = OccSet1, % remove
%	scan_term_vars(Shared, ChunkN, OccSet1, OccSet2),
	chunk__sub(ChunkN1, ChunkN, SubChunkN0),
	trace('  Scanning goal vars for: '), trace(IterGoal), tracenl,
	trace('             In subchunk: '), trace(SubChunkN0), tracenl,
	scan_goal_vars(IterGoal, ModEnv, SubChunkN0, _SubChunkN, OccSet2, OccSet3),
%	AuxClauses = _,
%	OccSet = OccSet3.
	trace('  missing scanning of auxiliary clauses: '), trace(AuxClauses), tracenl,
	scan_term_vars(AuxClauses, SubChunkN0, OccSet3, OccSet).
scan_goal_vars(M:('\6\postfix_block'(G, C0)), ModEnv, ChunkN0, ChunkN, OccSet0, OccSet) :- list_to_ctx(C0,C), !,
	scan_term_vars(M, ChunkN0, OccSet0, OccSet1),
	scan_goal_vars(G, ModEnv, ChunkN0, ChunkN, OccSet1, OccSet2),
	scan_ctx_vars(C, ChunkN, OccSet2, OccSet).
scan_goal_vars('\6\postfix_block'(G, C0), ModEnv, ChunkN0, ChunkN, OccSet0, OccSet) :- list_to_ctx(C0,C), !,
	scan_goal_vars(G, ModEnv, ChunkN0, ChunkN, OccSet0, OccSet1),
	scan_ctx_vars(C, ChunkN, OccSet1, OccSet).
scan_goal_vars((C<-G), ModEnv, ChunkN0, ChunkN, OccSet0, OccSet) :- !,
	scan_goal_vars(G, ModEnv, ChunkN0, ChunkN, OccSet0, OccSet1),
	scan_ctx_vars(C, ChunkN, OccSet1, OccSet).
scan_goal_vars(G, ModEnv, ChunkN0, ChunkN, OccSet0, OccSet) :-
	call_name_arity(G, N, A),
	call_args(G, Args),
	% scan goal vars over resolved goal (to introduce implicit variables present in ModEnv)
	( Module = [], % todo: ad-hoc, fix
	  msexpand__resolve_name(pred, N, A, Module, ModEnv, preddef(MN, _Kind)),
	  apply_aux_def(MN, Args, G2, Retranslate, ImplicitContext),
	  trace('  Scanning aux def: '), tracenl,
	  trace('    retranslate: '), trace(Retranslate), tracenl,
	  trace('    goal: '), trace(G), tracenl,
	  trace('    mn: '), trace(MN), tracenl,
	  trace('    implicit_context: '), trace(ImplicitContext), tracenl,
	  trace('    goal2: '), trace(G2), tracenl ->
	    ( Retranslate = yes(RChunkN, RModEnv) ->
	        trace('    (in retranslation) scanning goal vars'), tracenl,
		scan_goal_vars(G2, RModEnv, RChunkN, _ChunkN, OccSet0, OccSet),
		ChunkN = ChunkN0
	    ; scan_goal_vars(G2, ModEnv, ChunkN0, ChunkN1, OccSet0, OccSet1),
	      scan_ctx_vars(ImplicitContext, ChunkN1, OccSet1, OccSet), % todo: ChunkN1??
	      ChunkN = ChunkN0
	    )
	; ChunkN = ChunkN0,
	  scan_term_vars(G, ChunkN, OccSet0, OccSet)
	).

scan_term_vars(Term, ChunkN, OccSet0, OccSet) :- var(Term), !,
	trace(occset__record(Term, normal, ChunkN)), tracenl,
	occset__record(Term, normal, ChunkN, OccSet0, OccSet).
scan_term_vars(Term, ChunkN, OccSet0, OccSet) :-
	functor(Term, _N, A),
	scan_args_vars(1, A, Term, ChunkN, OccSet0, OccSet).

scan_args_vars(I0, I, _Term, _ChunkN, OccSet0, OccSet) :- I0 > I, !,
	OccSet = OccSet0.
scan_args_vars(I0, I, Term, ChunkN, OccSet0, OccSet) :-
	arg(I0, Term, Arg0),
	scan_term_vars(Arg0, ChunkN, OccSet0, OccSet1),
	I1 is I0 + 1,
	scan_args_vars(I1, I, Term, ChunkN, OccSet1, OccSet).

% scan vars in a context
scan_ctx_vars(A, ChunkN, OccSet0, OccSet) :- var(A), !,
	occset__record(A, context, ChunkN, OccSet0, OccSet).
scan_ctx_vars(+A, ChunkN, OccSet0, OccSet) :- var(A), !,
	occset__record(A, context, ChunkN, OccSet0, OccSet).
scan_ctx_vars(-A, ChunkN, OccSet0, OccSet) :- var(A), !,
	occset__record(A, context, ChunkN, OccSet0, OccSet).
scan_ctx_vars((*), _ChunkN, OccSet0, OccSet) :- !, OccSet = OccSet0.
scan_ctx_vars((A,B), ChunkN, OccSet0, OccSet) :- 
	scan_ctx_vars(A, ChunkN, OccSet0, OccSet1),
	scan_ctx_vars(B, ChunkN, OccSet1, OccSet).
scan_ctx_vars(A, _ChunkN, OccSet0, OccSet) :- !,
	display('warning: not scanned context '), display(A), nl,
	OccSet = OccSet0.

%}.

% ----------------------------------------------------------------------
% :- sub_module chunk {
%   :- doc(title, "Chunk paths").

% A chunk path is a list of indexes that indicates the current
% location in a predicate body. Suppose a goal:
%
%   Parent = ..., Control do Code, ...
%
% Given a chunk path [N|Ns], the latest index N represents the
% relative count of other control structures in the Code and the rest
% of indexes Ns the previous chunk path (the chunk path for the Parent
% code).

% The initial chunk path
chunk__initial([0]).

% Increment the last chunk counter and obtain an new inner chunk path.
chunk__sub(ChunkN0, ChunkN, SubChunkN) :-
	ChunkN0 = [N0|Rest],
	N is N0 + 1,
	ChunkN = [N|Rest],
	SubChunkN = [0|ChunkN].
% }.

% ----------------------------------------------------------------------
% :- sub_module occset {
%   :- doc(title, "Occurrence sets").

% Empty occset
occset__empty(occset(_, [])).

% Record the occurrence of a variable.in a occset
occset__record(A, VarKind, ChunkN, occset(OccDic0, Vs0), occset(OccDic, Vs)) :-
	ChunkN = [_|VarChunk], % skip the chunk counter
	( dic_get(OccDic0, A, Occ0) ->
	    update_var_kind(Occ0, VarChunk, VarKind, Occ),
	    dic_replace(OccDic0, A, Occ, OccDic),
	    Vs = Vs0
	; dic_replace(OccDic0, A, occ(VarKind, [VarChunk]), OccDic),
	  Vs = [A|Vs0]
	).

% Update the var kind in a given occ
update_var_kind(Occ0, ChunkN, VarKind, Occ) :-
	Occ0 = occ(PrevKind, ChunkSet),
	( PrevKind = VarKind ->
	    NewVarKind = VarKind
	; Kind = normal ->
	    NewVarKind = PrevKind
	; PrevKind = normal ->
	    NewVarKind = VarKind
	; Kind = context ->
	    NewVarKind = context
	),
	( member(ChunkN, ChunkSet) ->
	    NewChunkSet = ChunkSet
	; NewChunkSet = [ChunkN|ChunkSet]
	),
	Occ = occ(NewVarKind, NewChunkSet).

occset__get_shared(OccSet, ChunkN, Shared) :-
	ChunkN = [_|VarChunk], % skip the chunk counter
	OccSet = occset(OccDic, Vs),
	trace('    Looking for shared var for chunk '), trace(VarChunk), tracenl,
	occset__filter_shared(Vs, VarChunk, OccDic, SharedVars, SharedCVars),
	Shared = v(SharedVars, SharedCVars),
 	trace('    Shared variables for the subchunk SubChunkN0 are: '), trace(Shared), tracenl.

occset__filter_shared([], _VarChunk, _OccDic, [], []).
occset__filter_shared([X|Xs], VarChunk, OccDic, Vs, CVs) :-
	( var_is_shared(X, VarChunk, OccDic, Kind) ->
	    ( Kind = context ->
	        Vs = Vs0, CVs = [X|CVs0]
	    ; Vs = [X|Vs0], CVs = CVs0
	    )
	; Vs = Vs0,
	  CVs = CVs0
	),
	occset__filter_shared(Xs, VarChunk, OccDic, Vs0, CVs0).

var_is_shared(Var, VarChunk, OccDic, Kind) :-
	( dic_get(OccDic, Var, Occ),
	  trace('      The occurrences registered for '), trace(Var), trace(' are: '), trace(Occ), tracenl,
	  occ__is_shared(VarChunk, Occ, Kind) ->
	    trace('      (shared)'), tracenl
	; trace('      (no shared)'), tracenl,
	  fail
	).

% todo: it does not seem very efficient
occ__is_shared(VarChunk, occ(Kind0, ChunkSet), Kind) :-
	Kind = Kind0,
	member(VarChunk1, ChunkSet), append(_, VarChunk, VarChunk1), !,
	member(VarChunk0, ChunkSet), \+ append(_, VarChunk, VarChunk0), !.
%	fail.

%/*
display_occset(I, occset(OccDic, Vars)) :-
 	display_occdic(OccDic, I),
 	display_tab(I), trace('All variables in the occset are: '), trace(Vars), tracenl.
 
display_occdic(D, _I) :- var(D), !.
display_occdic(dic(Key,Val,L,R), I) :-
 	display_occdic(L, I),
 	display_tab(I), trace('Key: '), trace(Key), trace(' Val: '), trace(Val), tracenl,
 	display_occdic(R, I).

display_auxdic(D, _I) :- var(D), !.
display_auxdic(dic(Key,Val,L,R), I) :-
 	display_auxdic(L, I),
 	display_tab(I), trace('Key: '), trace(Key), trace(' Val: '), trace(Val), tracenl,
 	display_auxdic(R, I).

display_tab(0) :- !.
display_tab(I) :- I > 0, trace('  '), I1 is I - 1, display_tab(I1).
%*/

% }.

% ----------------------------------------------------------------------

list_to_ctx([], A) :- !, A = '*'.
list_to_ctx([A], A) :- !.
list_to_ctx([A|As], (A,Bs)) :-
	list_to_ctx(As, Bs).

% ----------------------------------------------------------------------

list_to_conj([], A) :- !, A = true.
list_to_conj([A], A) :- !.
list_to_conj([A|As], (A,Bs)) :-
	list_to_conj(As, Bs).

% ----------------------------------------------------------------------

list_to_disj([], A) :- !, A = fail.
list_to_disj([A], A) :- !.
list_to_disj([A|As], (A ; Bs)) :-
	list_to_disj(As, Bs).

% ----------------------------------------------------------------------

%connect(Goal, Cin, _Gin, _Cout, _Gout, _ExpGoal, _ChunkN0, _ChunkN, _OccSet, _ModEnv, _Aux, _Aux0):-
%	display(cccin(Goal, Cin)), nl, fail.
connect(Goal, Cin, Gin, Cout, Gout, ExpGoal, ChunkN0, ChunkN, OccSet, ModEnv, Aux, Aux0):-
	transform_goal(Goal, GoalPrim),
	!,
	connect(GoalPrim, Cin, Gin, Cout, Gout, ExpGoal, ChunkN0, ChunkN, OccSet, ModEnv, Aux, Aux0).
%% JF: extension
connect(G, Cin, Gin, Cout, Gout, XG1, ChunkN0, ChunkN, OccSet, ModEnv, Aux, Aux0):- is_custom_control(G), !,
	expand_custom(G, ChunkN0, ChunkN1, OccSet, ModEnv, Aux, Aux1, NewGoal, NewModEnv),
%	trace(expand_custom(G, ChunkN0, ChunkN1, OccSet, ModEnv, Aux, Aux1, NewGoal, NewModEnv)), tracenl,
	connect(NewGoal, Cin, Gin, Cout, Gout, XG1, ChunkN1, ChunkN, OccSet, NewModEnv, Aux1, Aux0).
connect((G1 , G2), Cin, Gin, Cout, Gout, ExpGoal, ChunkN0, ChunkN, OccSet, ModEnv, Aux, Aux0):- !,
	connect(G1, Cin, Gin, Cprim, Gprim, XG1, ChunkN0, ChunkN1, OccSet, ModEnv, Aux, Aux1),
	connect(G2, Cprim, Gprim, Cout, Gout, XG2, ChunkN1, ChunkN, OccSet, ModEnv, Aux1, Aux0),
	conc_goals(XG1, XG2, ExpGoal).
connect((G1 -> G2), Cin, Gin, Cout, Gout, (XG1 -> XG2), ChunkN0, ChunkN, OccSet, ModEnv, Aux, Aux0):- !,
	connect(G1, Cin, Gin, Cprim, Gprim, XG1, ChunkN0, ChunkN1, OccSet, ModEnv, Aux, Aux1),
	connect(G2, Cprim, Gprim, Cout, Gout, XG2, ChunkN1, ChunkN, OccSet, ModEnv, Aux1, Aux0).
connect((G1 ; G2), Cin, Gin, Cout, Gout, (XEG1; XEG2), ChunkN0, ChunkN, OccSet, ModEnv, Aux, Aux0):- !,
%	display(cccin_a(Cin)), nl,
	connect(G1, Cin, Gin, C10, G10, EG1, ChunkN0, ChunkN1, OccSet, ModEnv, Aux, Aux1),
%	display(cccin_b(Cin)), nl,
	connect(G2, Cin, Gin, C20, G20, EG2, ChunkN1, ChunkN, OccSet, ModEnv, Aux1, Aux0),
%	display(cccin_c(Cin, C10, C20)), nl,
	merge(Cin, C10, C20, X1, X2, Cout),
%	display(cccin_d(Cin, C10, C20)), nl,
	add_eqns(X1, EG1, XEG1),
%	display(cccin_e(Cin)), nl,
	add_eqns(X2, EG2, XEG2),
%	display(cccin_f(Cin)), nl,
	gamma_isect(G10, G20, Gout).
%	display(cccin_g(Cin)), nl.
connect(\+ G0, Cin, Gin, Cout, Gout, G, ChunkN0, ChunkN, OccSet, ModEnv, Aux, Aux0) :- !,
	connect((G0 -> fail ; true), Cin, Gin, Cout, Gout, G, ChunkN0, ChunkN, OccSet, ModEnv, Aux, Aux0).
connect(Goal0, Cin, Gin, Cout, Gout, Goal, ChunkN, ChunkN, OccSet, ModEnv, Aux, Aux0):-
	expand_context_vars(Goal0, Cin, Goal1),
	mapping_body(Goal1, ModEnv, Goal2, Mapping, Retranslate),
	( Retranslate = yes -> % todo: this is wrong if Goal had contexts...
	    trace('  Retranslating: '), trace(''(Goal2, ChunkN)), tracenl,
	    display_occset(2, OccSet),
%	    chunk__initial(RetranslateChunkN),
	    connect(Goal2, Cin, Gin, Cout, Gout, Goal, ChunkN, _ChunkN, OccSet, ModEnv, Aux, Aux0),
	    trace('  Retranslate: '), trace((Retranslate, Goal0, Goal)), tracenl
	; Retranslate = yes(RChunkN, RModEnv) -> % todo: this is wrong if Goal had contexts...
%	    display(beforeretranslating(ModEnv)), nl,
%	    display(in(Cin, Gin)), nl,
	    trace('  Retranslating (using rchunk): '), trace(''(Goal2, ChunkN, RChunkN)), tracenl,
	    display_occset(2, OccSet),
%	    chunk__initial(RetranslateChunkN),
	    connect(Goal2, Cin, Gin, Cout, Gout, Goal, RChunkN, _, OccSet, RModEnv, Aux, Aux0),
%	    display(after_in(Cin, Gin)), nl, %AKI EL CTX DE AUX0 PARA UNIFCTX_BUG SE UNIFICA!
%	    display(afterretranslating(ModEnv)), nl,
	    trace('  Retranslated (using rchunk): '), trace(''(Retranslate, Goal0, Goal)), tracenl
	; Aux = Aux0,
	  Goal = Goal2,
	  join_mapping(Goal1, Mapping, Cin, Gin, Cout, Gout)
	).

% todo: goal appears only to show the error 
join_mapping(Goal, Mapping, Cin, Gin, Cout, Gout) :-
	  ( Mapping = mapping(C0, _G0, C1, G1) ->
	      commons(Cin, C0),
	      theta_diff(C0, Cin, Cuninit),
	      warn_uninit(Cuninit, Goal),
	      theta_diff(Cin, C1, Cprim),
	      append(Cprim, C1, Cout),
	      gamma_join(Gin, G1, Gout)
	  ; Mapping = none ->
	      Cout = Cin,
	      Gout = Gin
	  ).

expand_fact(Fact, ModEnv, Fact3) :-
	extract_module_and_context(Fact, ModuleFact, ContextFact, Fact2),
	empty_context(ContextFact),
	msexpand__goal(Fact2, ModuleFact, ModEnv, Fact3, RetranslateFact, ImplicitContext, FactKind),
	FactKind = data,
	empty_context(ImplicitContext), % todo: it may not be correct!
	RetranslateFact = no.

add_contextual_args(Goal1, Context, ModEnv, Call, Mapping) :-
	( nonempty_context(Context) ->
	    Mapping = mapping(C0, G0, C1, G1),
	    context(i, Context, ModEnv, T0, C0, G0),
	    context(o, Context, ModEnv, T1, C1, G1),
	    Goal1 =.. [F|L],
	    append(L, T0, L1),
	    append(L1, T1, L2),
	    Call =.. [F|L2]
	; Call = Goal1,
	  Mapping = none
	).

none_mapping(mapping([], [], [], [])).

fact_method(assertz(Fact), assertz_fact, Fact).
fact_method(asserta(Fact), assertz_fact, Fact).
fact_method(retract(Fact), retract_fact, Fact).
fact_method(retractall(Fact), retractall_fact, Fact).

empty_context(X) :- nonvar(X), X = '*'.
nonempty_context(X) :- \+ empty_context(X).

mapping_body(Call0, ModEnv, Call, Mapping, Retranslate):-
	extract_module_and_context(Call0, Module, Context, SubGoal), 
%	trace(extract_module_and_context(Call0, Module, Context, SubGoal)), tracenl, 
	( SubGoal = get(X), Module = [], nonempty_context(Context) ->
	    mapping_body('\6\postfix_block'(=(X), [+Context]), ModEnv, Call, Mapping, Retranslate)
	; SubGoal = set(X), Module = [], nonempty_context(Context) ->
	    mapping_body('\6\postfix_block'(=(X), [-Context]), ModEnv, Call, Mapping, Retranslate)
	; fact_method(SubGoal, Method, Fact), nonvar(Fact), Module = [], empty_context(Context) ->
	    Mapping = none,
	    ( expand_fact(Fact, ModEnv, Fact3) ->
	        Call =.. [Method,Fact3],
		Retranslate = no
	    ; trace(no), tracenl, Call = SubGoal,
	      % todo: this should be an error! (after integration with mexpand)
	      Retranslate = no
	    )
	; msexpand__goal(SubGoal, Module, ModEnv, Goal1, Retranslate, ImplicitContext, _Kind),
	  nonvar(Goal1), callable(Goal1),
	  %
	  concat_context(Context, ImplicitContext, Context2),
	  add_contextual_args(Goal1, Context2, ModEnv, Call, Mapping),
	  trace('    Adding context: '), trace(Context2), trace(' to goal: '), trace(Goal1), trace(' gave: '), trace(Call), tracenl
	).

% ----------------------------------------------------------------------

concat_context(A, B, C) :- empty_context(A), !, C = B.
concat_context(A, B, C) :- empty_context(B), !, C = A.
concat_context(A, B, (A,B)).

% ----------------------------------------------------------------------
% bug: context is not traversed

expand_context_vars(Goal0, _, Goal) :- var(Goal0), !, Goal = Goal0.
expand_context_vars(M:Goal0, Cin, Goal) :- nonvar(Goal0), Goal0 = '\6\postfix_block'(Goal1, C), !,
	Goal = M:('\6\postfix_block'(Goal2, C)),
	expand_context_vars(Goal1, Cin, Goal2).
expand_context_vars('\6\postfix_block'(Goal0, C), Cin, Goal) :- !,
	Goal = '\6\postfix_block'(Goal1, C),
	expand_context_vars(Goal0, Cin, Goal1).
expand_context_vars(Goal0, Cin, Goal) :-
	expand_context_vars__term(Goal0, Cin, Goal).

expand_context_vars__term(Term00, Cin, Term) :-
	( var(Term00) -> Term0 = Term00
	; Term00 = '@'(Term0)
	),
	!,
	( memberco(Term0, Cin, Var) -> Term = Var ; Term = Term00 ).
expand_context_vars__term(Term0, Cin, Term) :-
	functor(Term0, N, A),
	functor(Term, N, A),
	expand_context_vars__args(1, A, Term0, Cin, Term).

expand_context_vars__args(I0, I, _Term0, _Cin, _Term) :- I0 > I, !.
expand_context_vars__args(I0, I, Term0, Cin, Term) :-
	arg(I0, Term0, Arg0),
	arg(I0, Term, Arg),
	expand_context_vars__term(Arg0, Cin, Arg),
	I1 is I0 + 1,
	expand_context_vars__args(I1, I, Term0, Cin, Term).

% ----------------------------------------------------------------------

transform_goal(Goal, call(Goal)):-
	var(Goal), !.
% todo: this hides DCG support!
transform_goal('.'(Context, V), '\6\postfix_block'(V, [Context])):- !.
%transform_goal(V / Context, call(V) / Context):- 
%        var(V), !.
% JF: syntactic extension
transform_goal((Context <- V), '\6\postfix_block'(set(V), [Context])) :- !.
% JF: disabled to take any context as output
%	( context_var_name(Name) ->
%	    true
%	; throw(invalid_context_name(Name))
%	).

% context_var_name(Name) :- atom(Name).
% context_var_name(Name) :- var(Name).

% ----------------------------------------------------------------------

theta_diff([], _, []).
theta_diff([X=_|A], B, C):-
	memberco(X, B, _), !,
	theta_diff(A, B, C).
theta_diff([E|A], B, [E|C]):-
	theta_diff(A, B, C).

gamma_join([], L, L).
gamma_join([X/P|A], L, B):-
	member(X/R, L),
	P==R, !,
	gamma_join(A, L, B).
gamma_join([E|A], L, [E|B]):-
	gamma_join(A, L, B).

gamma_diff([], _, _, []).
gamma_diff([X/_|A], C0, C1, B):-
	memberco(X, C0, V0),
	memberco(X, C1, V1),
	V0==V1, !,
	gamma_diff(A, C0, C1, B).
gamma_diff([E|A], C0, C1, [E|B]):-
	gamma_diff(A, C0, C1, B).

gamma_isect([], _, []).
gamma_isect([X/P|A], B, [X/P|C]):-
	member(X/R, B),
	R==P, !,
	gamma_isect(A, B, C).
gamma_isect([_|A], B, C):-
	gamma_isect(A, B, C).

gamma_new([], _, []).
gamma_new([X/_|A], C, B):-
	memberco(X, C, _), !,
	gamma_new(A, C, B).
gamma_new([E|A], C, [E|B]):-
	gamma_new(A, C, B).

add_tail([], T, T).
add_tail([A|B], Y, [A|C]):-
	add_tail(B, Y, C).

norm_seq((A,B), D):- !,
	norm_seq(A, C),
	norm_seq(B, E),
	norm_seq2(C, E, D).
norm_seq(D, D).

norm_seq2((A,B), C, D):- !,
	norm_seq2(A, (B,C), D).
norm_seq2(A, C, (A,C)).

elim_dup_guards([], []).
elim_dup_guards([X/Guard|A], B):-
	\+(\+((member(X/Duplicate, A), Duplicate==Guard ) )), !,
	elim_dup_guards(A, B).
elim_dup_guards([P|A], [P|B]):-
	elim_dup_guards(A, B).

filter_guards([], []).
filter_guards([_/Q|A], B):- nonvar(Q), Q=term, !,
	filter_guards(A, B).
filter_guards([G|A], [G|B]):-
	filter_guards(A, B).

%:- export(build_checker/2).

%:- export(guards_to_seq/3).

guards_to_seq(C, [X], Y) :- guard_to_goal(C, X, Y).
guards_to_seq(C, [X,Y|R], (U, W)):-
	guard_to_goal(C, X, U),
	guards_to_seq(C, [Y|R], W).

guard_to_goal(C, X/P, call(P, V)):- 
	var(P), !,
	memberco(X, C, V).
guard_to_goal(C, X/P, Goal):-
	callable(P),
	memberco(X, C, V),
	P =.. [ PF | PA ],
	Goal =.. [ PF, V | PA ]. 

preconditions(C, G, Body, BodyWithPreconds):-
	filter_guards(G, G1),
	elim_dup_guards(G1, G2),
	( guards_to_seq(C, G2, S) -> conc_goals(S, Body, BodyWithPreconds)
	; BodyWithPreconds = Body
	).

postconditions(C, G, Body, BodyWithPostconds):-
	filter_guards(G, G1),
	elim_dup_guards(G1, G2),
	( guards_to_seq(C, G2, S) -> conc_goals(Body, S, BodyWithPostconds)
	; BodyWithPostconds = Body
	).

% ---------------------------------------------------------------------------

warn_uninit([], _).
warn_uninit([A|B], Goal):-
	extract_cvars([A|B], L),
	warning(['Uninitialized context variables ', L, ' in call to ', ''(Goal)]).

extract_cvars([], []).
extract_cvars([X=_|L], [X|R]):-
	extract_cvars(L, R).

% ---------------------------------------------------------------------------

ctxdef__read(Def, MSubmod) :-
	nonvar(Def),
	def_lr(Def, Macro, Expansion),
	( '$_context_expansion'(Macro, _, _) ->
	    error(['Attempt to redefine context ', ''(Macro),
	           ' in module ', MSubmod]),
	    fail
	; assertz_fact('$_context_expansion'(Macro, Expansion, MSubmod))
	).

def_lr(Left=Right0, Left, Right):-
%	trace(deflr(Left, Right0)), tracenl,
	( nonvar(Right0), Right0 = (_ ; _) -> % disjunction of types (not context definitions) % todo: generate type for that
	    def_lr1(Left, Right)
	; Right = Right0
	),
	good_macro(Left).

def_lr1(Derived, ^FullExtension):- !,
	Derived =.. [DF | DA],
	append(DA, ['*'], DAX),
	FullExtension=.. [ DF, \Derived | DAX].

% ---------------------------------------------------------------------------	

% Expansion of loops
%
% todo: use analysis info to infer maybe_empty and transform the code
%   accordingly if the first condition is true, then the condition
%   predabs is only called once and so it can be expanded after the
%   end of the code (that transforms a while into a do_while)
%
% Loops are expanded following this scheme:
%
%   loop :- init, cond.
%   cond :- ( c -> ok ; true ).
%   ok :- code, cond.

/*
mexpand__for_each(Ctx, Iter, Goal) := AR :-
	( nonvar(Iter), Iter = iter(Init, Cond, Next, MaybeEmpty) ->
	    true
	; mexpand__error(internal(itererror__unknown_iter(Iter)))
	),
	AR = ~mexpand__loop(Ctx, MaybeEmpty, Init, Cond, Goal, Next).

% Generate 'while' loop
mexpand__while(Ctx, Cond, Goal) := AR :-
	AR = ~mexpand__loop(Ctx, yes, true, Cond, Goal, true).

% Generate 'do-while' loop
% note: do-while is necessary since it may more optimal in very low level code (i.e. absmach definition)
mexpand__do_while(Ctx, Goal, Cond) := AR :-
	AR = ~mexpand__loop(Ctx, no, true, Cond, Goal, true).

mexpand__loop(Ctx, MaybeEmpty, Init, Cond, Goal, Next) := AR :-
	% Inside unifies Vi with the value of context var vi, Outside sets the context var vi to Vi (for all vi in context)
	mexpand__enter_cont_ctx(Ctx, Inside, Outside),
	%
	CondR = ( Cond -> '$predabs_static_call$'(Ctx, LoopDef, [])
                ; % finish loop
                  Inside
                ),
	LoopR = (Goal, Next, '$predabs_static_call$'(Ctx, CondDef, [])),
	PD = ('$predabs_static$'(Ctx, [], CondR, CondDef),
	      '$predabs_static$'(Ctx, [], LoopR, LoopDef)),
	% todo: this was a todo in ptoc__impcomp: obtain MaybeEmpty automatically from the previous scheme: the first cond is true and inline after next the second cond: use builtins for iterators and properties for iterators! do not expand them during this analysis
	( MaybeEmpty = yes -> FirstDef = CondDef % start by the condition check
	; MaybeEmpty = no -> FirstDef = LoopDef % enter the loop, the initial status of condition is always true
	),
	%
	AR = (PD, Init, '$predabs_static_call$'(Ctx, FirstDef, []), Outside).

mexpand__for_each_list_elem(Ctx, X, Xs, Goal) := AR :-
	% Inside unifies Vi with the value of context var vi, Outside sets the context var vi to Vi (for all vi in context)
	mexpand__enter_cont_ctx(Ctx, Inside, Outside),
	%
	LoopR = ( Arg = [], Inside 
                ; Arg = [X|Arg0], Goal, '$predabs_static_call$'(Ctx, LoopDef, [Arg0])),
	AR = ('$predabs_static$'(Ctx, [Arg], LoopR, LoopDef), '$predabs_static_call$'(Ctx, LoopDef, [Xs]), Outside).

mexpand__maplistn(Ctx, Lists, Elems, Goal) := AR :-
	% Inside unifies Vi with the value of context var vi, Outside sets the context var vi to Vi (for all vi in context)
	mexpand__enter_cont_ctx(Ctx, Inside, Outside),
	%
	( maplistn__unifs(Lists, Elems, Args, Args0, UNil, UCons) ->
	    true
	; mexpand__error(internal(bad_spec_in_maplistn(Lists, Elems))),
	  fail
	),
	LoopR = ( UNil, Inside
		; UCons, Goal, '$predabs_static_call$'(Ctx, LoopDef, Args0)),
	AR = ('$predabs_static$'(Ctx, Args, LoopR, LoopDef), '$predabs_static_call$'(Ctx, LoopDef, Lists), Outside).
:- '$end_context'.

maplistn__unifs([], [], [], [], true, true).
maplistn__unifs([_|Lists], [X|Elems], [Arg|Args], [Arg0|Args0], (Arg = [], UNil), (Arg = [X|Arg0], UCons)) :-
	var(X),
	maplistn__unifs(Lists, Elems, Args, Args0, UNil, UCons).

mexpand__accum_list(Xs, _, _) :- var(Xs), !, fail.
mexpand__accum_list([], Xs, Xs).
mexpand__accum_list([X|Xs], [X|Ys], Zs) :- mexpand__accum_list(Xs, Ys, Zs).
*/

% ---------------------------------------------------------------------------
% Goals

extract_module_and_context(Call0, Module, Context, SubGoal) :-
	extract_module(Call0, Module, Goal0), 
	extract_context(Goal0, SubGoal, Context).

% Extract the Context from a goal
extract_context(Call0, SubGoal, Context) :-
	nonvar(Call0), 
	Call0 = '\6\postfix_block'(SubGoal, Context0), 
	nonvar(SubGoal),
	!,
	list_to_ctx(Context0, Context).
extract_context(Call0, Call0, '*').

extract_module(Call, Module, Call0) :- var(Call), !, Module = [], Call0 = Call.
extract_module(M:Call, Module, Call0) :- nonvar(M), !, Module = [M], Call0 = Call.
extract_module(Call, Module, Call0) :- Module = [], Call0 = Call.

list_spine(C0) :-
	nonvar(C0),
	( C0 = []
	; C0 = [_|Rest], list_spine(Rest)
	).

call_name_arity(X0, X, A) :- var(X0), !, X = X0, A = 0.
call_name_arity('\6\postfix_block'(X0, C0), X, A) :- list_spine(C0), !, call_name_arity(X0, X, A).
call_name_arity(X0, X, A) :- atom(X0), !, X = X0, A = 0.
call_name_arity(X0, X, A) :- functor(X0, call, A0), !,
	arg(1, X0, X), A is A0 - 1. % , trace(callname(X0, X)), tracenl.
call_name_arity(X0, X, A) :- functor(X0, N, A0), !,
	X = N, A = A0.

call_args(A0, A) :- var(A0), !, A = [].
call_args('\6\postfix_block'(A0, C0), A) :- list_spine(C0), !, call_args(A0, A).
call_args(A0, A) :- atom(A0), !, A = [].
call_args(F, A) :- F =.. [call,_|Args], !, A = Args.
call_args(F, A) :- F =.. [_|Args], !, A = Args.



