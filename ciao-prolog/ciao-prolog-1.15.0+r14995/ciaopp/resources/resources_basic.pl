:- module(resources_basic, [bound/1, bottom/1, clause_t/1, resource/1,
		approx_to_bound/2, get_litinfo/9, slitinfo/1, clause_ppkey_t/1,
		clause_key_t/1, litinfo_get_key/2, adg_t/1, litinfo_get_litnum/2,
		litinfo_get_bt/2, bound_bottom/2,
		valid_size_measure/1, mark_used_metrics/4, vector_is_infinite/1,
		approx_bottom/2, compatible_measure/2, combine_results/3,
		measure_t/1],
	    [assertions, nativeprops,
% regtypes, % (regtypes already in inferres_decl)
		resources(inferres_decl), library(resdefs(resources_decl)),
		hiord]).

:- use_module(library(hiordlib)).
:- use_module(library(messages)).
:- use_module(program(clidtypes)).
:- use_module(resources(dependency_res(position_res)), [new_pos/3]).
:- use_module(program(clause_db),                      [source_clause/3]).
:- reexport(program(clidlist_basic), [lit_ppkey/3]).

:- doc(author, "Edison Mera").


:- prop resource(Res) + regtype # "Unifies @var{Res} with a defined
   resource.  Note that it only includes declared resources, but not
   components of compound resources.".

resource(Res) :-
	source_clause(_, directive(resource(Res)), _).

/*
resource(Res) :-
	source_clause(_, directive(compound_resource(CR, Comps)), _),
	member(Res, [CR|Comps]).
*/

:- regtype bound/1.

bound(upper).
bound(lower).

:- regtype bottom/1.

bottom(inf).
bottom(0).

:- pred approx_to_bound/2 :: approx * bound #
	"Specifies the relationship between an approximation and a bound.".

approx_to_bound(ub,  upper).
approx_to_bound(lb,  lower).
approx_to_bound(oub, upper).
approx_to_bound(olb, lower).
approx_to_bound(o,   upper).
approx_to_bound(me,  upper). % This analysis is not implemented --EMM
approx_to_bound(ome, upper). % This analysis is not implemented --EMM

:- pred bound_bottom(A, B) : bound(A) => bottom(B).

bound_bottom(upper, inf). % Assuming inf for ub
bound_bottom(lower, 0). % Assuming 0 for lb

approx_bottom(Ap, Bot) :-
	approx_to_bound(Ap, Bound),
	bound_bottom(Bound, Bot).

:- pred get_litinfo/9 :: % rtcheck -- EMM
	term * nnegint * atm * list(bottom_entry) * list(symbol_entry) *
	clause_ppkey_t * atm * approx * litinfo #
"Construct a litinfo structure using the first arguments, which are
the clause key, a literal, the literal number and a list of modes.".

get_litinfo(Lit, LitNum, Key, BT, ST, ClausePPKey, PPKey, Approx, LitInfo) :-
	get_litinfo(Lit, Approx,
	    stat_litinfo${
		litnum => LitNum,
		key => Key,
		bt => BT,
		st => ST,
		clauseppkey => ClausePPKey,
		ppkey => PPKey}, LitInfo).

:- pred litinfo_get_key/2 :: slitinfo * atm.
litinfo_get_key(litinfo${extra => stat_litinfo${key => Key}}, Key).

:- pred litinfo_get_litnum/2 :: slitinfo * nnegint.
litinfo_get_litnum(litinfo${extra => stat_litinfo${litnum => LitNum}}, LitNum).

:- use_module(library(resdefs(res_litinfo))).
:- reexport(library(resdefs(res_litinfo)), [litinfo_get_lit/2]).

:- pred litinfo_get_bt/2 :: slitinfo * list(bottom_entry).
litinfo_get_bt(litinfo${extra => stat_litinfo${bt => BT}}, BT).

:- regtype slitinfo/1.

slitinfo(litinfo${literal => Literal, approx => Approx,
		extra => stat_litinfo${
		    litnum => LitNum,
		    key => Key,
		    bt => BT,
		    st => ST,
		    clauseppkey => ClausePPKey,
		    ppkey => PPKey}}) :-
	term(Literal),
	nnegint(LitNum),
	atm(Key),
	list(BT, bottom_entry),
	list(ST, symbol_entry),
	clause_ppkey_t(ClausePPKey),
	atm(PPKey),
	approx(Approx).

mark_used_metrics(Metrics, Resources, UsedMetrics, ValueIndexes) :-
	mark_used_metrics_(Metrics, Resources, 1, 1, UsedMetrics,
	    ValueIndexes).

mark_used_metrics_([_Metric|Metrics], Resources, N, I,
	    [UsedMetric|UsedMetrics], ValueIndexes) :-
	new_pos(0, N, Pos),
	( have_term(Pos, Resources) ->
	    UsedMetric = I,
	    ValueIndexes = [N|ValueIndexes2],
	    I1 is I + 1
	;
	    UsedMetric = 0,
	    ValueIndexes = ValueIndexes2,
	    I1 = I
	),
	N1 is N + 1,
	mark_used_metrics_(Metrics, Resources, N1, I1, UsedMetrics,
	    ValueIndexes2).
mark_used_metrics_([], _Resources, _, _, [], []).

have_term(A, B) :-
	A == B -> true
    ;
	( have_term_list(A, B) -> true
	;
	    ( \+ atom(B) ->
		B =.. [_|Args],
		have_term_list(A, Args)
	    )
	).

have_term_list(A, [X|Xs]) :-
	have_term(A, X) -> true
    ;
	have_term_list(A, Xs).

vector_is_infinite(Vector) :-
	member(inf, Vector),
	!.

:- regtype measure_t/1.

measure_t(void).
measure_t(int).
measure_t(length).
measure_t(size).
measure_t(depth([_|_])).

valid_size_measure(F/A) :-
	measure_t(P),
	functor(P, F, A0),
	A is A0 + 1.

compatible_measure(_,  M2) :- var(M2), !.
compatible_measure(M1, M2) :- M1 == M2, !. %%% ??
compatible_measure(M1, M2) :-
	error_message("~q and ~q are not compatible metrics\n", [M1, M2]), !,
	fail.

combine_results(resources(_), [Time1], [Time]) :-
	!,
	map(Time1, combine_result, Time).
combine_results(_, Prop1, Prop) :-
	combine_result(Prop1, Prop).

combine_result(Prop, Prop) :- !.
combine_result(_,    _).

:- regtype clause_ppkey_t/1.

clause_ppkey_t((A :- B)) :-
	callable(A),
	clausebody(B).
clause_ppkey_t(A) :-
	A \= (_ :- _),
	callable(A).

:- regtype clause_t/1.

clause_t(decl).
clause_t(fact).
clause_t(rule).

:- regtype clause_key_t/1.

clause_key_t(Clause:Key) :-
	clause_ppkey_t(Clause),
	atm(Key).

:- export(bottom_t/1).
:- regtype bottom_t/1.
bottom_t(default).
bottom_t(bottom).

:- export(clause_head/2).
:- pred clause_head/2 # "Get the head of a clause.".
clause_head((H :- _), H).

:- export(clause_body/2).
:- pred clause_body/2.
clause_body((_ :- B), B).

:- export(clause_head_body/3).
:- pred clause_head_body/3.
clause_head_body((H :- B), H, B).

:- export(clause_key/3).
:- pred clause_key/3.
clause_key(Clause:Key, Clause, Key).


:- export(bottom_entry/1).
:- regtype bottom_entry/1.
bottom_entry(bp(Platform)) :-
	atm(Platform).
bottom_entry(bt(PredName, Approx, Type, BTE)) :-
	predname(PredName),
	approx(Approx),
	symbol_field(Type),
	list(BTE, bottom_element).

:- export(bottom_element/1).
:- regtype bottom_element/1.
bottom_element(bs(_,     _,        _)).
bottom_element(be(_Name, _PreType, _Value)).

:- export(symbol_field/1).
:- regtype symbol_field/1.
symbol_field(clause).
symbol_field(mode).
symbol_field(measure).
symbol_field(mutex).
symbol_field(det).
symbol_field(size).
symbol_field(relation).
% symbol_field(time).
symbol_field(resources(Resources)) :-
	list(Resources, resource).
symbol_field(domain).
symbol_field(resource(Resource)) :-
	resource(Resource).

:- export(mode_t/1).
:- regtype mode_t/1.
mode_t('+').
mode_t('-').

:- export(symbol_entry/1).
:- regtype symbol_entry/1.
symbol_entry(st(PredName, Clauses, Modes, Measures, Mut,
		_Det, _Sizes, _Sol, _Ress, _Dom)) :-
	predname(PredName),
	list(Clauses, clause_key_t),
	list(Modes,   mode_t),
	list(Measures),
	list(Mut).
%	list(Det),
%	list(Sizes),
%	list(Sol),
%	list(Ress),
%	list(Dom).

:- export(gran_entry/1).
:- regtype gran_entry/1.
gran_entry(st(PredName, ClauseSeq, ClausePar, ClauseAnn, _Type, _Sizes,
		DictList)) :-
	predname(PredName),
	list(ClauseSeq, clause_ppkey_t),
	list(ClausePar, clause_ppkey_t),
	list(ClauseAnn, clause_ppkey_t),
	list(DictList).


:- export(comp_t/1).
:- regtype comp_t/1.
comp_t(comp(PredName, _)) :- predname(PredName).

:- export(test_t/1).
:- regtype test_t/1.
test_t(test(Lit, _)) :- callable(Lit).

:- export(adg_t/1).
:- regtype adg_t/1.
adg_t(adg(Node, Pred, Succ, Mode)) :-
	node_t(Node),
	list(Pred, node_t),
	list(Succ, node_t),
	mode_t(Mode).

:- export(node_t/1).
:- regtype node_t/1.
node_t($(N)) :-
	nnegint(N).
node_t($(N1, N2)) :-
	nnegint(N1), nnegint(N2).

:- export(insert_element/2).
insert_element(E, L) :-
	member(E, L),
	!.
