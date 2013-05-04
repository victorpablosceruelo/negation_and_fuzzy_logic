:- module(_, [], [compiler(complang)]).

:- doc(title, "Low level instructions").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module defines the low level instructions and
   its properties.").

:- use_module(compiler(errlog)).

%:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(compiler(list_common), [repeat_n/3]).

:- use_module(.(module_exp)).
:- use_module(.(module_ipexp)).
:- use_module(.(ptoc__props)).

:- use_module(.(sht_analyzer)).
:- use_module(compiler(memoize)). % because of module_ipexp

:- include(.(absint__interface)).

{
:- fluid exp :: module_exp.

% ---------------------------------------------------------------------------

:- public goal_prop/3.
goal_prop(Prop, G) := Value :-
	trust(G instance_of strgoal),
	GId = ~G.predid,
	Value = ~GId.get_prop(Prop).

:- public check_test_str_new/3.
% builtin that tests if A is a structure of kind B
check_test_str_new(A, B) :=
	~strgoal.new_n(~predicate_x.reg_new('term_basic:$check_test_str'/2), [A, B]).

:- public bind_new/3.
% bind A with B
bind_new(A, B) :=
	~strgoal.new_n(~predicate_x.reg_new('term_basic:$bind'/2), [A, B]).

:- public equal_new/3.
% builtin that tests if the one-cell terms A and B are equal
equal_new(A, B) :=
	~strgoal.new_n(~predicate_x.reg_new('term_basic:$equal'/2), [A, B]).

:- public cut_new/2.
% builtin that cuts
cut_new(A) :=
	~strgoal.new_n(~predicate_x.reg_new('basiccontrol:$cut'/1), [A]).

:- public choice_new/2.
choice_new(A) :=
	~strgoal.new_n(~predicate_x.reg_new('basiccontrol:$choice'/1), [A]).

:- public caller_choice_new/2.
caller_choice_new(A) :=
	~strgoal.new_n(~predicate_x.reg_new('basiccontrol:$caller_choice'/1), [A]).

:- public trail_if_conditional_new/2.
trail_if_conditional_new(A) :=
	~strgoal.new_n(~predicate_x.reg_new('term_basic:$trail_if_conditional'/1), [A]).

:- public trust_domain/2.
% X is a trust mark of domain AbsInt
% TODO: bad name... this is not exactly like that
trust_domain(X) := AbsInt :-
	trust(X instance_of strgoal),
	~X.name = N/A,
	AbsInt = ~trust_domain__2(N, A).

:- '$ctxprj'(trust_domain__2/3, []).
trust_domain__2('term_basic:$varmem', 2) := mem.
trust_domain__2('term_basic:$trust_type', 2) := sht_analyzer.
trust_domain__2('term_basic:$trust_imptype', 2) := impt.

:- public is_instance/1.
:- pred is_instance(+X) :: aterm # "@var{X} is an @emph{instance/2} goal.".
is_instance(X) :-
	trust(X instance_of strgoal),
	~X.name = 'term_basic:$instance'/2, !.

:- public is_unify/1.
:- pred is_unify(+X) :: aterm # "@var{X} is an @emph{unify/2} goal.".
is_unify(X) :-
	trust(X instance_of strgoal),
	~X.name = 'term_basic:$unify'/2, !.

:- public unify/3.
unify(A, B) := ~strgoal.new_n(~predicate_x.reg_new('term_basic:$unify'/2), [A, B]).

:- public instance/3.
instance(A, B) := ~strgoal.new_n(~predicate_x.reg_new('term_basic:$instance'/2), [A, B]).

% ---------------------------------------------------------------------------

:- public fail_new/1.
:- pred fail_new(-T) :: aterm # "@var{T} is a @texttt{fail/0} goal.".
fail_new := ~strgoal.new_n(~predicate_x.reg_new('basiccontrol:fail'/0), []).

% ---------------------------------------------------------------------------

% Static properties which does not depend on when the 
% predicate is called, but only on the goal implementation
% (can use annotations).

:- public ins_uses_successcont/1.
ins_uses_successcont(call(X)) :-
	true = ~goal_prop(uses_successcont, X).

:- public ins/1.
:- regtype ins(X) # "@var{X} is a low level instruction".
:- '$ctxprj'(ins/1, []).
ins(_).

:- public ins_args/2.
:- '$ctxprj'(ins_args/2, []).
ins_args(call(G)) := As :- !,
	trust(G instance_of strgoal),
	~G.args = As.
ins_args(neck(A, Xs)) := [A|Xs] :- !.
ins_args(switchrw(I)) := ~ins_args(I) :- !.
ins_args(I) := Xs :- I =.. [_|Xs].

:- public ins_set_args/3.
:- '$ctxprj'(ins_set_args/3, []).
ins_set_args(call(G), Xs) := call(~G.set_args(Xs)) :- !,
	trust(G instance_of strgoal).
ins_set_args(neck(_, _), [A|Xs]) := neck(A, Xs) :- !.
ins_set_args(switchrw(I), Xs) := switchrw(~ins_set_args(I, Xs)) :- !.
ins_set_args(I, Xs) := I2 :- functor(I, N, _), I2 =.. [N|Xs].

% ---------------------------------------------------------------------------
% Goal properties (particular for each call)

:- public ins_frame_live_size/2.
:- '$ctxprj'(ins_frame_live_size/2, []).
ins_frame_live_size(call(G)) := ~G.getp(frame_live_size) :-
	trust(G instance_of strgoal).

:- public ins_live_set/2.
:- '$ctxprj'(ins_live_set/2, []).
ins_live_set(call(G)) := ~G.getp(live_set) :-
	trust(G instance_of strgoal).

:- public ins_heap/2.
% amount of extra-heap that has been required until the execution of this instruction
:- '$ctxprj'(ins_heap/2, []).
ins_heap(call(G)) := ~G.getp(heap) :-
	trust(G instance_of strgoal).

:- public ins_mark_last/2.
:- '$ctxprj'(ins_mark_last/2, []).
ins_mark_last(call(G)) := call(~G.addp(last, true)) :- !,
	trust(G instance_of strgoal).
ins_mark_last(I) := _ :-
	errlog:bug(['ins_mark_last failed for ', I]).

:- public ins_last/1.
:- '$ctxprj'(ins_last/1, []).
ins_last(call(G)) :-
	trust(G instance_of strgoal),
	~G.getp(last) = true, !.

% ---------------------------------------------------------------------------
% Predicate properties (global for all calls)

:- public ins_argmodes/2.
ins_argmodes(I) := Occs :- Occs = ~ins_argmodes_2(I), !.
ins_argmodes(I) := _ :- errlog:bug(['ins_argmodes failed for ', I]).

ins_argmodes_2(switchrw(isnonvar(_))) := [in] :- !.
ins_argmodes_2(switch_on_index(_, _, _)) := [in, param, param] :- !.
ins_argmodes_2(init(_)) := [out] :- !.
ins_argmodes_2(mvardecl(_)) := [out] :- !.
ins_argmodes_2(move(_, _)) := [in, out] :- !.
ins_argmodes_2(globalize(_, _, _)) := [param, in, out] :- !.
ins_argmodes_2(deref(_, _)) := [in, out] :- !.
ins_argmodes_2(unify(_, _)) := [in, in] :- !.
ins_argmodes_2(read(_, _, _)) := [in, param, param] :- !.
ins_argmodes_2(load(_, _, _)) := [out, param, param] :- !.
ins_argmodes_2(neck(_, As)) := [param| ~repeat_n(~length(As), in)] :- !.
ins_argmodes_2(trim_frame(_)) := [param] :- !.
ins_argmodes_2(cframe(_)) := [param] :- !.
ins_argmodes_2(alloc) := [] :- !.
ins_argmodes_2(validate_local_top) := [] :- !.
ins_argmodes_2(invalidate_local_top) := [] :- !.
ins_argmodes_2(dealloc) := [] :- !.
ins_argmodes_2(dummy_cut) := [] :- !.
ins_argmodes_2(call(G)) := ~goal_prop(argmodes, G) :- !.

:- public ins_argderefs/2.
ins_argderefs(I) := Occs :- Occs = ~ins_argderefs_2(I), !.
ins_argderefs(I) := _ :- errlog:bug(['ins_argderefs failed for ', I]).

ins_argderefs_2(switchrw(isnonvar(_))) := [true] :- !.
ins_argderefs_2(switch_on_index(_, _, _)) := [true, false, false] :- !.
ins_argderefs_2(init(_)) := [true] :- !.
ins_argderefs_2(mvardecl(_)) := [false] :- !.
ins_argderefs_2(move(_, _)) := [false, false] :- !.
ins_argderefs_2(globalize(_, _, _)) := [false, false, false] :- !.
ins_argderefs_2(deref(_, _)) := [false, true] :- !.
ins_argderefs_2(unify(_, _)) := [true, true] :- !.
ins_argderefs_2(read(_, _, _)) := [true, false, false] :- !.
ins_argderefs_2(load(_, _, _)) := [true, false, false] :- !.
ins_argderefs_2(neck(_, As)) := [false| ~repeat_n(~length(As), false)] :- !.
ins_argderefs_2(trim_frame(_)) := [false] :- !.
ins_argderefs_2(cframe(_)) := [false] :- !.
ins_argderefs_2(alloc) := [] :- !.
ins_argderefs_2(validate_local_top) := [] :- !.
ins_argderefs_2(invalidate_local_top) := [] :- !.
ins_argderefs_2(dealloc) := [] :- !.
ins_argderefs_2(dummy_cut) := [] :- !.
ins_argderefs_2(call(G)) := ArgDerefs :- !,
	ArgDerefs = ~goal_prop(argderefs, G).

:- public ins_argunboxs/2.
ins_argunboxs(I) := Occs :- Occs = ~ins_argunboxs_2(I), !.
ins_argunboxs(I) := _ :- errlog:bug(['ins_argunboxs failed for ', I]).

ins_argunboxs_2(switchrw(isnonvar(_))) := [false] :- !.
ins_argunboxs_2(switch_on_index(_, _, _)) := [false, false, false] :- !.
ins_argunboxs_2(init(_)) := [false] :- !.
ins_argunboxs_2(mvardecl(_)) := [false] :- !.
ins_argunboxs_2(move(_, _)) := [false, false] :- !.
ins_argunboxs_2(globalize(_, _, _)) := [false, false, false] :- !.
ins_argunboxs_2(unbox(_, _)) := [false, false] :- !.
ins_argunboxs_2(unify(_, _)) := [false, false] :- !.
ins_argunboxs_2(read(_, _, _)) := [false, false, false] :- !.
ins_argunboxs_2(load(_, _, _)) := [false, false, false] :- !.
ins_argunboxs_2(neck(_, As)) := [false| ~repeat_n(~length(As), false)] :- !.
ins_argunboxs_2(trim_frame(_)) := [false] :- !.
ins_argunboxs_2(cframe(_)) := [false] :- !.
ins_argunboxs_2(alloc) := [] :- !.
ins_argunboxs_2(validate_local_top) := [] :- !.
ins_argunboxs_2(invalidate_local_top) := [] :- !.
ins_argunboxs_2(dealloc) := [] :- !.
ins_argunboxs_2(dummy_cut) := [] :- !.
ins_argunboxs_2(call(G)) := ArgDerefs :- !,
	ArgDerefs = ~goal_prop(argunboxs, G).

:- public ins_bottom_shtdef/1.
ins_bottom_shtdef(call(G)) :-
	trust(G instance_of strgoal),
	GId = ~G.predid,
	call((
          intr :: absint <- sht_analyzer,
	  ShtDef = ~intr.get_usermemo(GId),
	  trust(ShtDef instance_of shtdef)
	)),
	ShtDef.is_bottom.

:- public ins_exittypes/2.
% TODO: instructions should not contain this information... but an external analysis table; too loose types!
ins_exittypes(I) := Occs :- Occs = ~ins_exittypes_2(I), !.
ins_exittypes(I) := _ :- errlog:bug(['ins_exittypes failed for ', I]).

ins_exittypes_2(switchrw(isnonvar(_))) := ~map_type_norm([any]) :- !.
ins_exittypes_2(switch_on_index(_, _, _)) := ~map_type_norm([any, any, any]) :- !.
ins_exittypes_2(init(_)) := ~map_type_norm([var]) :- !.
ins_exittypes_2(mvardecl(_)) := ~map_type_norm([any]) :- !. % TODO: what is the type of a unk?
ins_exittypes_2(move(_, _)) := ~map_type_norm([any, any]) :- !.
ins_exittypes_2(globalize(_, _, _)) := ~map_type_norm([any, any, any]) :- !.
ins_exittypes_2(deref(_, _)) := ~map_type_norm([any, any]) :- !.
ins_exittypes_2(unify(_, _)) := ~map_type_norm([any, any]) :- !.
ins_exittypes_2(read(_, _, _)) := ~map_type_norm([any, any, any]) :- !.
ins_exittypes_2(load(_, _, Type)) := ~map_type_norm([fnc(F), any, any]) :- !,
	'$absmach'(Absmach),
	trust(Type instance_of termunk),
	Absmach.functorcons__functor(~Type.value, F).
ins_exittypes_2(neck(_, As)) := ~map_type_norm([any| ~repeat_n(~length(As), any)]) :- !.
ins_exittypes_2(trim_frame(_)) := ~map_type_norm([any]) :- !.
ins_exittypes_2(cframe(_)) := ~map_type_norm([any]) :- !.
ins_exittypes_2(alloc) := [] :- !.
ins_exittypes_2(validate_local_top) := [] :- !.
ins_exittypes_2(invalidate_local_top) := [] :- !.
ins_exittypes_2(dealloc) := [] :- !.
ins_exittypes_2(dummy_cut) := [] :- !.
ins_exittypes_2(call(G)) := ExitTypes :- !,
	trust(G instance_of strgoal),
	GId = ~G.predid,
	call((
          intr :: absint <- sht_analyzer,
	  ShtDef = ~intr.get_usermemo(GId),
	  trust(ShtDef instance_of shtdef)
	)),
	ExitTypes = ~ShtDef.get_exit_types.

% TODO: recode using ins_argmodes
:- public ins_used/2.
:- pred ins_used(+I, -Us) :: ins * list(aterm)
   # "@var{Us} is the list of used variables in the instruction @var{I}".
ins_used(I) := Us :-
	Us = ~ins_used_2(~ins_args(I), ~ins_argmodes(I)).

:- '$ctxprj'(ins_used_2/3, []).
ins_used_2([], []) := [] :- !.
ins_used_2([X|Xs], [in|Ms]) := [X|~ins_used_2(Xs, Ms)] :- X instance_of termvar, !.
ins_used_2([_|Xs], [_|Ms]) := ~ins_used_2(Xs, Ms) :- !.

% TODO: recode using ins_argmodes
:- public ins_defined/2.
:- pred ins_defined(+I, -Ds) :: ins * list(aterm) 
   # "@var{Ds} is the list of variables defined in the instruction @var{I}.".
ins_defined(I) := Ds :-
	Ds = ~ins_defined_2(~ins_args(I), ~ins_argmodes(I)).

:- '$ctxprj'(ins_defined_2/3, []).
ins_defined_2([], []) := [] :- !.
ins_defined_2([X|Xs], [out|Ms]) := [X|~ins_defined_2(Xs, Ms)] :- X instance_of termvar, !.
ins_defined_2([_|Xs], [_|Ms]) := ~ins_defined_2(Xs, Ms) :- !.

:- public ins_nosideeffdet/1.
% the instruction only modify the output (argmode) registers (i.e. p(In,Out), Foo, where Out is a fresh variable, is equivalent to Foo if Out is not used in Foo)
% TODO: is 'cannot fail' necessary for the optimizations where this is used??
%ins_nosideeffdet(switchrw(isnonvar(_))) :- !. % TODO: why???
ins_nosideeffdet(move(_, _)) :- !.
ins_nosideeffdet(load(_, _, B)) :-
	% TODO: necessary the functor _/0 part...?
	trust(B instance_of termunk),
	functorcons__is_constant(~B.value), !. 
ins_nosideeffdet(deref(_, _)) :- !.
ins_nosideeffdet(init(_)) :- !.
ins_nosideeffdet(mvardecl(_)) :- !.
ins_nosideeffdet(call(G)) :- !,
	true = ~goal_prop(nosideeffdet, G).

:- public ins_noderefmod/1.
% a instruction which does not modify the dereferentiation state of other variables
% TODO: i.e. does not unify variables...? does it implies that it does not use the trail?
ins_noderefmod(load(_, _, _)).
ins_noderefmod(switchrw(isnonvar(_))).
ins_noderefmod(mvardecl(_)). % TODO: hmmm 
ins_noderefmod(call(G)) :-
	true = ~goal_prop(noderefmod, G).

:- public ins_clear_regs/1.
ins_clear_regs(call(G)) :-
	true = ~goal_prop(clear_regs, G).

:- public ins_should_trim_frame/1.
ins_should_trim_frame(call(G)) :-
	true = ~goal_prop(should_trim_frame, G).

:- public ins_needs_liveness_info/1.
ins_needs_liveness_info(call(G)) :-
	true = ~goal_prop(needs_liveness_info, G).

:- public ins_heap_usage/2.
ins_heap_usage(init(U)) := max(S) :-
	trust(U instance_of termvar),
	~U.getp(mem) = x(_), !,
	'$absmach'(Absmach),
	Absmach.tagged_size(S).
ins_heap_usage(globalize(Globalize, _, _)) := max(S) :-
	trust(Globalize instance_of termunk),
	~Globalize.value = if_unsafe,
	!,
	'$absmach'(Absmach),
	Absmach.tagged_size(S).
ins_heap_usage(load(_, _, B)) := max(Heap) :- !,
	'$absmach'(Absmach),
	trust(B instance_of termunk),
	Absmach.functorcons__heap_usage(~B.value, Heap).
ins_heap_usage(call(G)) := HeapUsage :- !,
	HeapUsage0 = ~goal_prop(heap_usage, G),
	% TODO: user provided heap usage may be wrong!! do something about it (e.g. infer or check from low level code)
	% TODO: user provides heap usage in tagged words, use more units (e.g. F functors+T taggeds+...)
	( HeapUsage0 = max(HeapUsage1) ->
	    % from heap usage in user units to bytes (for the current abstract machine)
	    '$absmach'(Absmach),
	    Absmach.tagged_size(S),
	    HeapUsage2 is HeapUsage1 * S,
	    HeapUsage = max(HeapUsage2)
	; HeapUsage = HeapUsage0
	).
ins_heap_usage(_) := max(0).

:- public ins_frame_usage/2.
ins_frame_usage(call(G)) := FrameUsage :- !,
	FrameUsage = ~goal_prop(frame_usage, G).
ins_frame_usage(_) := max(0).

:- public ins_uses_trail/1.
% TODO: rename to forces failure to do a restore_mem?
ins_uses_trail(bind(_, _)) :- !.
ins_uses_trail(unify(_, _)) :- !.
ins_uses_trail(call(G)) :- !,
	true = ~goal_prop(uses_trail, G).

:- public ins_uses_failcont/1.
ins_uses_failcont(call(G)) :-
	true = ~goal_prop(uses_failcont, G).

:- public ins_modifies_choice_on_success/1.
% the instruction creates or deletes choice points on success (so, semidet preds do not hold this property)
ins_modifies_choice_on_success(call(G)) :-
	true = ~goal_prop(modifies_choice_on_success, G).

:- public ins_name/2.
ins_name(I) := Ds :- Ds = ~ins_name_2(I), !.
ins_name(I) := _ :- errlog:bug(['functor failed for ', I]).

ins_name_2(deref(_, _)) := internal__deref/2 :- !.
ins_name_2(move(_, _)) := internal__move/2 :- !.
ins_name_2(globalize(_, _, _)) := internal__globalize/3 :- !.
ins_name_2(unify(_, _)) := internal__unify/2 :- !.
ins_name_2(init(_)) := internal__init/1 :- !.
ins_name_2(mvardecl(_)) := internal__mvardecl/1 :- !.
ins_name_2(read(_, _, _)) := internal__read/3 :- !.
ins_name_2(load(_, _, _)) := internal__load/3 :- !.
ins_name_2(switchrw(isnonvar(_))) := internal__isnonvar/1 :- !.
ins_name_2(switch_on_index(_, _, _)) := internal__switch_on_index/3 :- !.
ins_name_2(neck(_, _)) := internal__neck/2 :- !.
ins_name_2(trim_frame(_)) := internal__trim_frame/1 :- !.
ins_name_2(cframe(_)) := internal__cframe/1 :- !.
ins_name_2(alloc) := internal__alloc/0 :- !.
ins_name_2(validate_local_top) := internal__validate_local_top/0 :- !.
ins_name_2(invalidate_local_top) := internal__invalidate_local_top/0 :- !.
ins_name_2(dealloc) := internal__dealloc/0 :- !.
ins_name_2(dummy_cut) := internal__dummy_cut/0 :- !.
ins_name_2(call(G)) := NA :- !,
	trust(G instance_of strgoal),
	~G.name = NA.

:- public ins_needs_neck/1.
ins_needs_neck(call(G)) :-
	true = ~goal_prop(needs_neck, G).

:- public ins_needs_cframe/1.
ins_needs_cframe(call(G)) :-
	true = ~goal_prop(needs_cframe, G).

}.
