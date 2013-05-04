:- module(_, [], [compiler(complang)]).

% This module defines the transformation of predicates containing
% complex control structures (disjunctions, negations by failure,
% if-then-else, predicate abstractions, etc.) into a set of predicates
% defined only by definite clauses (Horn clauses with one exactly one
% positive literal -- i.e. predicates whose clauses contain only
% conjunctions of literals).
%
% Non-logic literals such as meta-cut and meta-choice may be
% introduced to support cut semantics.
%
% The output of the translation is represented as 'aterm' data
% structures.
%
% Authors:
% CLIP group, modified by Jose F. Morales

:- use_module(library(dict)).
:- use_module(library(lists)).

:- use_module(compiler(module_exp)).
:- use_module(compiler(errlog)).
:- use_module(compiler(memoize)).
:- use_module(compiler(ptoc__props)).
:- use_module(compiler(ptoc__ins)).

%:- include(.(aux_profile_ctx)).
:- include(.(aux_profile_ctx__disabled)).

:- public mixin ctx_module {
    :- fluid exp :: module_exp.
}.

{
:- extends ctx_module.
:- public norm_preds/2.
% Normalize the predicate list: input is module-expanded, output is
% 'acode' predicate definitions.
norm_preds(Preds0, Preds) :-
	preds :: accum(Preds), 
	maplist(([preds] -> ''(PredId) :- norm_pred(PredId)), Preds0).
}.

{
:- extends ctx_module.
:- fluid preds :: accum.
norm_pred(PredId) :-
	trust(PredId instance_of predicate_x),
	( Fun = ~PredId.get_prop(defswitchcfun) ->
	    PredId.set_code(pswitchcfun(Fun)),
	    preds.add(PredId)
  	; _ = ~PredId.get_prop(impmacro) ->
	    PredId.set_code(imacro),
	    preds.add(PredId)
	; Foreign = ~PredId.get_prop(impforeign) ->
	    % TODO: This test and the following one are not
	    %   disjoint. Is it right? Base the next one in other
	    %   property?
	    PredId.set_code(iforeign(Foreign)),
	    preds.add(PredId)
	; _ = ~PredId.get_prop(impcode) ->
	    PredId.set_code(icd),
	    preds.add(PredId)
	; true = ~PredId.get_prop(impdyn) ->
	    Code = ~PredId.code,
	    PredId.set_code(int(Code)),
	    preds.add(PredId)
	; Name = ~PredId.name,
	  Name = MF/A,
	  functor(Head, MF, A), Head =.. [_|Args],
	  Code = ~PredId.code,
	  norm_clauses(Args, Code, Cs0),
	  optim_clauses(Cs0, Cs1),
	  PredId.set_code(normcode(Args, Cs1)),
	  preds.add(PredId),
	  call((
            sh :: shareddic,
	    occurrences_clause_fixpo(OccDic, Cs1, Args),
	    call((
              occdic :: u_dic <- OccDic,
	      occpath :: any <- start,
	      straight_clauses(PredId)
	    ))
          ))
        ).
}.

% Normalize clauses: conjunctions are translated into lists,
% disjunctions and if-then-else's to special '$or$' blocks
norm_clauses(Args, Cs, Cs2) :-
	maplist((''(C, C2) :-
	  C = c(H, B00, _),
	  H =.. [_|As0],
	  call((
	    goals :: accum(C2),
	    goals.add('basiccontrol:$caller_choice'(Choice)),
	    head_unifs(As0, Args),
	    call((
              choice :: any <- Choice,
	      subchunk_n :: m_int <- 1,
	      norm_goal(B00)
	    ))
	  ))
        ), Cs, Cs2).

% Flat the head of the clause (no structures or repeated variables in the head)
% TODO: Define a 'listset' class with insert/1 and contains/1 operations
{
:- fluid goals :: accum.
head_unifs(As0, As) :-
	seen :: m_any <- [],
	maplist(([goals, u(seen)] -> ''(A0, A) :-
	  ( var(A0), \+ (member(A00, ~seen), A00 == A0) ->
	      A = A0,
	      seen <- [A0|(~seen)]
	  ; % emit a unification if the argument is not a variable, or it is an already seen variable
            A = _,
	    goals.add('term_basic:='(A0, A))
	  )
	), As0, As).
}.

% Expand X (var), \+/1, if/3 and (A->B) without alternatives to (A->B;fail))
% Also expand !/0 and ->/2
% TODO: document: $caller_choice is the most recent choice point
{
:- fluid choice :: any.
:- fluid subchunk_n :: m_int.
:- fluid goals :: accum.
norm_goal(Var) :- var(Var), !,
	goals.add('hiord_rt:call'(Var)).
norm_goal('hiord_rt:$meta_call'(P0)) :- !,
	goals.add('hiord_rt:call'(P0)).
% norm_goal('aggregates:^'(V, P0)) := 'aggregates:^'(V, P)) :- !,
%	P = ~norm_goal(P0, outer).
norm_goal('basiccontrol:false') :- !,
	goals.add('basiccontrol:fail').
norm_goal('basiccontrol:otherwise') :- !.
norm_goal('basiccontrol:true') :- !.
norm_goal('basiccontrol:!') :- !,
	goals.add('basiccontrol:$cut'(~choice)).
norm_goal(G) :- G = 'basiccontrol:;'(_, _), !,
	Hs = ~norm_goal__or(G),
	ChunkN = ~subchunk_n,
	subchunk_n.inc(1),
	goals.add('$or$'(ChunkN, Hs)).
norm_goal(G) :- G = '$subpr$'(PredAbs, Args, G0), !,
	% TODO: indeed, just one case...
	Hs = ~norm_goal__or(G0),
	ChunkN = ~subchunk_n,
	subchunk_n.inc(1),
	goals.add('$subpr$'(ChunkN, PredAbs, Args, Hs)).
norm_goal(G) :- G = '$subpr_static$'(PredAbs, Args, G0), !,
	% TODO: indeed, just one case...
	Hs = ~norm_goal__or(G0),
	ChunkN = ~subchunk_n,
	subchunk_n.inc(1),
	goals.add('$subpr_static$'(ChunkN, PredAbs, Args, Hs)).
norm_goal(G) :- G = 'basiccontrol:->'(_, _), !,
	norm_goal('basiccontrol:;'(G, 'basiccontrol:fail')).
norm_goal('basiccontrol:\\+'(P0)) :- !,
	% TODO: optimize later?
	( P1 = ~negated(P0) ->
	    norm_goal(P1)
	; norm_goal('basiccontrol:;'(
                      'basiccontrol:->'(P0, 'basiccontrol:fail'), 
                      'basiccontrol:true'))
	).
norm_goal('basiccontrol:,'(P, Q)) :- !,
	norm_goal(P),
	norm_goal(Q).
% TODO: document: like (P->Q;R) but does backtracking on P? (i.e. executes Q for all P solutions; if P did not have any solution then execute R)
norm_goal('basiccontrol:if'(P, Q, R)) :- !,
	% TODO: I am not sure about this... where do I cut inside if/3?
        norm_goal('basiccontrol:,'(
                    'term_basic:='(Flag, [no]),
                    'basiccontrol:;'(
                      'basiccontrol:,'(
                        P, 
                        'basiccontrol:,'(
                          'basiccontrol:IF BUILTIN'(Flag), 
                          Q
                        )
                      ),
                      'basiccontrol:,'(
                        'term_basic:='(Flag, [no]),
                        R
                      )
                    )
                  )).
norm_goal(P) :-
	goals.add(P).
}.

{
:- fluid choice :: any.
norm_goal__or(G) := Hs :-
	call(( cases :: accum(Hs0), norm_goal__or__2(G) )),
	optim_clauses(Hs0, Hs).

{
:- fluid cases :: accum.
norm_goal__or__2(G) :-
	nonvar(G), G = 'basiccontrol:;'(P0, Q0), !,
	call((
          subchunk_n :: m_int <- 1,
	  goals :: accum(P),
	  norm_goal__case(P0)
        )),
	cases.add(P),
	norm_goal__or__2(Q0).
norm_goal__or__2(Q0) :-
	call((
          subchunk_n :: m_int <- 1,
	  goals :: accum(P),
	  norm_goal__case(Q0)
        )),
	( P = ['basiccontrol:fail'] ->
	    true
	; cases.add(P)
	).
}.

{
:- fluid goals :: accum.
:- fluid subchunk_n :: m_int.
norm_goal__case(G) :-
	nonvar(G), G = 'basiccontrol:->'(P0, Q0), !,
	goals.add('basiccontrol:$caller_choice'(Choice2)),
	norm_goal(P0),
	goals.add('basiccontrol:$cut'(Choice2)),
	norm_goal(Q0).
norm_goal__case(G) :-
	norm_goal(G).
}.

}.

negated('term_typing:var'(X)) := 'term_typing:nonvar'(X) :- !.
negated('term_typing:nonvar'(X)) := 'term_typing:var'(X) :- !.
negated('term_compare:=='(X, Y)) := 'term_compare:\\=='(X, Y) :- !.
negated('term_compare:\\=='(X, Y)) := 'term_compare:=='(X, Y) :- !.
negated('term_compare:@<'(X, Y)) := 'term_compare:@>='(X, Y) :- !.
negated('term_compare:@>='(X, Y)) := 'term_compare:@<'(X, Y) :- !.
negated('term_compare:@>'(X, Y)) := 'term_compare:@=<'(X, Y) :- !.
negated('term_compare:@=<'(X, Y)) := 'term_compare:@>'(X, Y) :- !.

% ---------------------------------------------------------------------------
% Avoid some temporal predicates

% TODO: this avoids temporal predicates, write a better version (should I move this to norm_pred? does that simplify treatment of $caller_choice?)
% TODO: there are other possible optimizations... delay $or$ to the compilation/transformation phase??
% TODO: do some more intelligent analysis with caller_choice (maybe in ptoc__lowcomp:default_cut/3, to detect what does a cut cut)
optim_clauses(Cs0, Cs) :-
	% a predicate with only one clause composed of disjunctions
	append(PreCs0, [[G, '$or$'(_ChunkN, Cs1)]], Cs0),
	G = 'basiccontrol:$caller_choice'(Choice),
	!,
	append(PreCs0, Cs2, Cs),
	% unify the choice of all caller_choice in Cs1 with Choice, add missing caller_choice
	maplist((''(C, C2) :-
	  ( C = ['basiccontrol:$caller_choice'(Choice0)|_] ->
	      Choice = Choice0,
	      C2 = C
	  ; % add caller choice
            % TODO: make caller choice optional in rest of compilation steps so that this is not necessary
            C2 = ['basiccontrol:$caller_choice'(Choice)|C]
	  )
        ), Cs1, Cs2).
optim_clauses(Cs, Cs).

% ---------------------------------------------------------------------------
% Calculate shared variables for subpredicates

% TODO: this algorithm is quadratic w.r.t the disjunction depth, fix -> however profiling does not indicate that most predicates are in the worse case
%   A better algorithm would be computing an occurence list for all the clause
{
:- extends ctx_module.
:- fluid sh :: shareddic.
% TODO: calculated the shared variables for recursive calls of subpr using a fixpoint (it could be improved!)
occurrences_clause_fixpo(OccDic, Cs1, Args) :-
%	errlog:trace([occclauses_iter]),
	call((
          occdic :: u_dic <- OccDic0,
	  occpath :: any <- start,
	  changed :: any <- Changed,
	  occurrences_clauses__2(Cs1, Args)
        )),
	( nonvar(Changed) ->
	    occurrences_clause_fixpo(OccDic, Cs1, Args)
	; OccDic = OccDic0
	).

{
:- fluid changed :: any.
:- fluid occdic :: u_dic.
:- fluid occpath :: any.
occurrences_clauses__2(Cs, Args0) :-
	% TODO: Share code with record_occurrences_clauses__2
	clause_no :: m_int <- 1,
	maplist(([sh, u(clause_no)] -> ''(Body0) :-
	  NewPath0 = (~clause_no, ~occpath),
	  call((
	    occpath :: any <- NewPath0,
	    NewPath = ~occpath,
            call((
              in_occpath :: any <- NewPath,
	      in_subchunk_n :: any <- 0,
	      seen_dic :: u_dic,
	      vars :: accum(_AllVars),
	      record_occurrences_term(Args0),
	      record_occurrences_body(Body0)
            )),
            occurrences_body(Body0),
	    clause_no.inc(1)
	  ))
        ), Cs).
}.

}.

% Given a clause, break off separate predicates for special goals such
% as disjunctions, negations, implications, and subpredicates
% (i.e. '$subpr$', '$subpr_static$').
%
% note: It also expands unifications and output arguments.
%
% Description of the algorithm:
% - first pass:
%   + each goal going to a separate goal is assigned a chunk number.
%   + each variable is annotated with the chunk numbers it appears in.
% - second pass:
%   + calculate the arguments for each chunk (linking vars), by filtering
%      out variables that belong to just the current chunk number.

% Get the lifetime of each variable (chunk numbers where each variable appears (0 for body, or a number for a '$or$' or '$subpr$'))
{
:- fluid occdic :: u_dic.
:- fluid sh :: shareddic.
:- fluid occpath :: any.
:- fluid in_occpath :: any.
:- fluid in_subchunk_n :: any.
:- fluid vars :: accum.
:- fluid seen_dic :: u_dic.
record_occurrences_body(Gs) :-
	maplist(([sh, vars] -> ''(G) :- ( 
          G = '$or$'(SubChunkN, Hs) ->
	    OccPath = c(SubChunkN, ~occpath),
	    record_occurrences_subpr(OccPath, [], Hs)
	; G = '$subpr$'(SubChunkN, PredAbs, Args, Hs) ->
	    record_occurrences_term(PredAbs),
	    OccPath = c(SubChunkN, ~occpath),
	    record_occurrences_subpr(OccPath, Args, Hs)
	; G = '$subpr_static$'(SubChunkN, PredAbs, Args, Hs) ->
	    % ignore PredAbs for subpr_static (not a normal variable)
	    OccPath = c(SubChunkN, ~occpath),
	    % bind predabs with the subpr identifier (OccPath)
	    ( var(PredAbs) -> PredAbs = OccPath ; true ),
	    record_occurrences_subpr(OccPath, Args, Hs)
	; G = '$subpr_static_call$'(PredAbs, Args) ->
	    ( var(PredAbs) ->
	        true % do nothing, the predabs is unknown
%	        errlog:trace(['recocc: unbound PredAbs in subpr_static_call'])
	    ; OccPath = PredAbs,
%	      errlog:trace(['recocc: bound PredAbs in subpr_static_call ', OccPath]),
	      ( TempShared = ~straight_shared_vars(OccPath), nonvar(TempShared) ->
%		  errlog:trace(['recocc: tempshared ', TempShared]),
		  record_occurrences_term(TempShared)
	      ; true %errlog:trace(['recocc: notempshared yet'])
	      )
	    ),
	    % ignore PredAbs for subpr_static_call (not a normal variable)
	    record_occurrences_term(Args)
	; record_occurrences_goal(G)
	)), Gs).

% TODO: rewrite record_occurrences_* predicate so that it does not do multiple passes
record_occurrences_subpr(OccPath, Args, Hs) :-
	'$predabs_info$'(_Gen, Args, Hs, _SubPredId) = ~straight_predabs_info(OccPath),
	straight_set_vars(OccPath, AllVars),
	%
	OccPath = c(SubChunkN, PrevOccPath),
	InOccPath = ~in_occpath,
	InSubChunkN = ( InOccPath == PrevOccPath ? SubChunkN
		      | ~in_subchunk_n
		      ),
	%
	call((
	  seen_dic :: u_dic,
          in_subchunk_n :: any <- InSubChunkN,
	  vars :: accum(AllVars),
	  record_occurrences_term(Args),
	  call((
            occpath :: any <- OccPath,
	    record_occurrences_clauses__2(Hs)
          ))
        )),
	call((
          in_subchunk_n :: any <- InSubChunkN,
	  record_vars_list(AllVars)
        )).

record_occurrences_clauses__2(Cs) :-
	% TODO: Share code with occurrences_clauses__2
        clause_no :: m_int <- 1,
	maplist(([sh, vars, u(clause_no)] -> ''(Body0) :-
	  NewPath = (~clause_no, ~occpath),
          call((
            occpath :: any,
	    ~occpath = NewPath,
	    record_occurrences_body(Body0)
          )),
	  clause_no.inc(1)
	), Cs).

record_occurrences_goal(Var) :-
	record_occurrences_term(Var).

record_occurrences_term(Var) :-
	var(Var), !,
	occ_lookup_path(Var, Occs),
	occ_insert(Occs, ~in_subchunk_n),
	record_var(Var). % TODO: optimize
record_occurrences_term(X) :-
	term :: any <- X,
	record_occurrences_args(1).
{
:- fluid term :: any.
record_occurrences_args(I) :-
	arg(I, ~term, X), !,
	record_occurrences_term(X),
	I1 is I + 1,
	record_occurrences_args(I1).
record_occurrences_args(_).
}.
}.

% Emit subpredicates using the variable lifetime
{
:- extends ctx_module.
:- fluid changed :: any.
:- fluid occdic :: u_dic.
:- fluid sh :: shareddic.
:- fluid occpath :: any.
occurrences_body(Gs) :-
	maplist(([sh] -> ''(G) :- (
	  G = '$subpr$'(SubChunkN, _PredAbs, Args, Hs) ->
	    OccPath = c(SubChunkN, ~occpath),
	    occurrences_subpr(OccPath, Args, Hs)
	; G = '$subpr_static$'(SubChunkN, _PredAbs, Args, Hs) ->
	    OccPath = c(SubChunkN, ~occpath),
	    occurrences_subpr(OccPath, Args, Hs)
	; G = '$or$'(SubChunkN, Hs) ->
	    OccPath = c(SubChunkN, ~occpath),
	    occurrences_subpr(OccPath, [], Hs)
	; true
	)), Gs).

:- '$ctxprj'(occurrences_subpr/3, [exp, changed, occdic, sh]).
occurrences_subpr(OccPath, Args, Hs) :-
	sh.get_path(OccPath, subpr_args(Shared00, PredAbsInfo, AllVars)),
	( var(Shared00) -> Shared0 = []
	; Shared0 = Shared00
	),
	% TODO: this tries to obtain a better ordering, to enable indexing, etc. but it is still not very efficient...
	OrList = AllVars,
	OccPath = c(SubChunkN, PrevOccPath),
	% TODO: complexity??? when I put an unnecessary test the performance does not go down very much...
	call((
          occpath :: any <- PrevOccPath,
	  in_subchunk_n :: any <- SubChunkN,
	  shared :: accum(Shared),
	  linking_vars(OrList)
        )),
	( Shared == Shared0 ->
	    true
	; ~changed = true
	),
	SharedWA = ~append(Args, Shared),
%	errlog:trace([oldsharedwas(OccPath, Shared0)]),
%	errlog:trace([newsharedis(OccPath, Shared)]),
	sh.replace_path(OccPath, subpr_args(Shared, PredAbsInfo, AllVars)),
	call((
          occpath :: any <- OccPath,
	  occurrences_clauses__2(Hs, SharedWA)
        )).

:- '$ctxprj'(straight_shared_vars/2, [occdic, u(sh)]).
straight_shared_vars(OccPath) := Shared :-
	sh.get_path(OccPath, subpr_args(Shared, _, _)).

:- '$ctxprj'(straight_predabs_info/2, [occdic, u(sh)]).
straight_predabs_info(OccPath) := PredAbsInfo :-
	sh.lookup_path(OccPath, subpr_args(_, PredAbsInfo, _)).

:- '$ctxprj'(straight_set_vars/2, [occdic, sh]).
straight_set_vars(OccPath, Vars) :-
	sh.get_path(OccPath, subpr_args(Shared, PredAbsInfo, _)),
	sh.replace_path(OccPath, subpr_args(Shared, PredAbsInfo, Vars)).
}.

% pair(vars): list of all the variables in a complex goal
{
:- fluid vars :: accum.
:- fluid seen_dic :: u_dic.
:- fluid in_subchunk_n :: any.
:- fluid in_occpath :: any.
record_vars_list(Gs) :-
	maplist(([vars] -> ''(G) :- record_var(G)), Gs).
}.

{
:- fluid vars :: accum.
:- fluid seen_dic :: u_dic.
record_var(Var) :- % var(Var),
	seen_dic.lookup(Var, _, New),
	( New = new -> vars.add(Var) ; true ).
}.

% List of variables which appears in more than one literal.
{
:- fluid shared :: accum.
:- fluid occdic :: u_dic.
:- fluid occpath :: any.
:- fluid in_subchunk_n :: any.
linking_vars(Vs) :-
	maplist(([shared] -> ''(V) :-
	  ( occ_get_path(V, Occs),
	    occ_nonsingle(Occs), % the variable appears in more than one chunk
	    occ_in(Occs, ~in_subchunk_n) -> % the variable appears in this chunk
	      shared.add(V)
	  ; true
	  )
        ), Vs).
}.

:- class shareddic {
    :- '$statemodel'(pair).
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    :- constant lookup_path/2.
    lookup_path(OccPath, Shared) :-
            dic_lookup(~self, OccPath, Shared).

    replace_path(OccPath, Shared) :-
            self <- ~dic_replace(~self, OccPath, Shared).

    :- constant get_path/2.
    get_path(OccPath, Shared) :-
            dic_get(~self, OccPath, Shared).
}.

{
:- fluid occdic :: u_dic.
:- fluid in_occpath :: any.
occ_lookup_path(Var, Occs) :-
	occdic.lookup(~in_occpath, Dic),
	dic_lookup(Dic, Var, Occs).
}.

{
:- fluid occdic :: u_dic.
:- fluid occpath :: any.
occ_get_path(Var, Occs) :-
	occdic.get(~occpath, Dic),
	dic_get(Dic, Var, Occs).
}.

occ_insert([X|_], X) :- !.
occ_insert([_|Xs], X) :- occ_insert(Xs, X).

occ_nonsingle([_]) :- !, fail.
occ_nonsingle(_).

occ_in([], _) :- !, fail.
occ_in([X|_], X).
occ_in([_|Xs], X) :- occ_in(Xs, X).

% ---------------------------------------------------------------------------
% Emit subpredicates, unfold builtins and change term representation

% TODO: use a different accumulator for goals in straight_body (one that is always closed)
{
:- extends ctx_module.
:- fluid occdic :: u_dic.
:- fluid sh :: shareddic.
:- fluid occpath :: any.
:- fluid preds :: accum.
straight_clauses(PredId) :-
	trust(PredId instance_of predicate_x),
	normcode(Args0, Clauses0) = ~PredId.code,
	% TODO: share code structure with record_occurrences_clauses__2
        call((
	  from_id :: predicate_x <- PredId,
	  clause_no :: m_int <- 1,
	  maplist(([sh, preds, u(clause_no)] -> ''(Body0, Body) :-
            NewPath = (~clause_no, ~occpath),
            call((
              occpath :: any <- NewPath,
	      goals :: accum(Body),
	      straight_body(Body0)
            )),
	    clause_no.inc(1)
          ), Clauses0, Clauses)
        )),
	Args = ~aterm__terms(Args0),
	PredId.set_code(icode(a, Args, or(Clauses))).
}.

{
:- extends ctx_module.
:- fluid occdic :: u_dic.
:- fluid sh :: shareddic.
:- fluid preds :: accum.
:- fluid goals :: accum.
:- fluid from_id :: predicate_x.

{
:- fluid occpath :: any.
straight_body(Gs) :-
	maplist(([preds, goals, sh] -> ''(G) :-
	  G = '$or$'(SubChunkN, Hs) ->
	    OccPath = c(SubChunkN, ~occpath),
	    SubPredId = ~straight_new_subpred(OccPath, [], Hs),
	    call((
              occpath :: any <- OccPath,
	      straight_clauses(SubPredId)
            )),
	    straight_call_pa(OccPath, SubPredId, [])
	; G = '$subpr$'(SubChunkN, PredAbs, Args, Hs) ->
	    OccPath = c(SubChunkN, ~occpath),
	    SubPredId = ~straight_new_subpred(OccPath, Args, Hs),
	    call((
	      occpath :: any <- OccPath,
	      straight_clauses(SubPredId)
            )),
	    straight_unif_pa(OccPath, SubPredId, PredAbs)
	; G = '$subpr_static$'(_SubChunkN, _PredAbs, _Args, _Hs) ->
	    true % ignore, call straight_new_subpred on call
	; G = '$subpr_static_call$'(PredAbs, Args) ->
	    ( var(PredAbs) ->
	        errlog:bug(['unbound PredAbs in subpr_static_call']),
		fail
	    ; true
	    ),
	    OccPath = PredAbs,
	    '$predabs_info$'(Gen, PredArgs, Hs, SubPredId) = ~straight_predabs_info(OccPath),
	    ( var(Gen) ->
	        Gen = yes,
		% note: it is necessary to split predid creation and straight_clauses, since we bind here SubPredId to an actual value
	        SubPredId = ~straight_new_subpred(OccPath, PredArgs, Hs),
%	        errlog:trace([donenewpred(OccPath, Hs)]),
	        call((
	          occpath :: any <- OccPath,
	          straight_clauses(SubPredId)
                ))
%	        errlog:trace([donestraightclauses(OccPath)])
	    ; true % do nothing, already generated
	    ),
	    straight_call_pa(OccPath, SubPredId, Args)
%	    errlog:trace([doneocccall(OccPath)])
	; unfold_builtin(G)
	), Gs).
}.

straight_new_subpred(OccPath, Args, Hs) := SubPredId :-
	Shared = ~straight_shared_vars(OccPath),
	SharedWA = ~append(Args, Shared),
	length(SharedWA, SubArity),
	SubPredId = ~from_id.new_sub(SubArity),
	SubPredId.set_code(normcode(SharedWA, Hs)),
	preds.add(SubPredId).
}.

{
:- extends ctx_module.
:- fluid goals :: accum.
:- fluid occdic :: u_dic.
:- fluid sh :: shareddic.
% Unify Var with the predicate abstraction for OccPath-SubPredId
straight_unif_pa(OccPath, SubPredId, Var) :-
	trust(SubPredId instance_of predicate_x),
	Shared = ~straight_shared_vars(OccPath),
	SubName = ~SubPredId.name,
	SubName = N/A,
	% register hiord pred so that it is not lost in analysis % TODO: add as a property
	% TODO: do this only if this code is reachable!
	Exp = ~exp,
	( Exp.uses_hiord_pred(N, A) -> true ; Exp.add(uses_hiord_pred(N, A)) ),
	PredAbs =.. [N|Shared],
	% note: omit arguments
	% TODO: allow missing arguments in any argument place? or use a fixed scheme to make it very fast? or reuse the subterm as a kind of environment?
	expand_unif(Var, '$PA$'(PredAbs)).
% Do a call to the predicate abstraction for OccPath-SubPredId
straight_call_pa(OccPath, SubPredId, Args) :-
	trust(SubPredId instance_of predicate_x),
	Shared = ~straight_shared_vars(OccPath),
	SubName = ~SubPredId.name,
	SharedWA = ~append(Args, Shared),
	SubCall = ~aterm__goal(SubName, SharedWA),
	goals.add(SubCall).
}.

% ---------------------------------------------------------------------------

% Unfold (expanded) definition of some built-in predicates
% TODO: generalize to include user defined expansions? - arithmetic expansions are not trivial
{
:- extends ctx_module.
:- fluid goals :: accum.
unfold_builtin('term_basic:C'(X, Y, Z)) :- !,
	unfold_builtin('term_basic:='(X, [Y|Z])).
unfold_builtin('term_compare:compare'(Value, X, Y)) :- !,
	unfold_builtin('term_compare:$compare'(X, Y, Value)).
unfold_builtin('term_basic:='(X, Y)) :- !,
	expand_unif(X, Y).
%unfold_builtin('data_facts:current_fact'(X)) := X :- nonvar(X), !. % TODO: check if this is faster or not...
unfold_builtin('arithmetic:is'(Value, Expr)) :- !,
	unfold_builtin__is(Value, Expr).
unfold_builtin(S0) :-
	functor(S0, N, A), eval_builtin(N, A), !,
	S0 =.. [_|Args],
	unfold_builtin__eval(N/A, Args).
unfold_builtin(S) :-
	S =.. [_|Args],
	functor(S, N, A),
	expand_out_args(N/A, Args).

unfold_builtin__eval(Name, [X, Y]) :-
	unfold_builtin__expr(Y, Y1),
	unfold_builtin__expr(X, X1),
	goals.add(~aterm__goal(Name, [X1, Y1])).

% TODO: communicate empty expansion in other way?
unfold_builtin__is(Value, Expr0) :-
	( call((
	    % TODO: define a concat_accum(CtxVar, G) binder?
            goals :: accum <- S0,
	    unfold_builtin__expr(Expr0, Expr),
	    ~goals = S
          )), 
          S0 \== S -> % note that fails if the expansion is empty!
	    S0 = ~goals, goals <- S
	; unfold_builtin__expr('+'(Expr0), Expr)
	),
	expand_unif(Expr, Value).

% postcondition: output arguments are fresh variables in all emitted functions 
unfold_builtin__expr(Expr, Expr) :- var(Expr), !.
unfold_builtin__expr([Expr0], Expr) :- !,
	unfold_builtin__expr(Expr0, Expr).
unfold_builtin__expr(E, Value) :-
	E2 = ( E = '-'(X, Y), integer(Y), Y = 1 ? '--'(X)  %% shorthand for 'SUB1 FUNCTION'
	     | E = '+'(X, Y), integer(Y), Y = 1 ? '++'(X)  %% shorthand for 'ADD1 FUNCTION'
	     | E
	     ),
	functor(E2, E2Atom, E2Arity), function_pred(E2Atom, E2Arity, Name), !,
	E2 =.. [_|Args],
	( Args = [X] ->
	    unfold_builtin__expr(X, X1),
	    Args2 = [X1, Value]
	; Args = [X, Y] ->
	    unfold_builtin__expr(Y, Y1),
	    unfold_builtin__expr(X, X1),
	    Args2 = [X1, Y1, Value]
	; errlog:bug(['invalid function ', E]), fail
	),
	goals.add(~aterm__goal(Name, Args2)).
unfold_builtin__expr(E, E).

% TODO: use compiler declarations!
:- '$ctxprj'(function_pred/3, []).
function_pred('-', 1, 'arithmetic:$-'/2).
function_pred('+', 1, 'arithmetic:$+'/2).
function_pred('--', 1, 'arithmetic:$--'/2).      %% shorthand for 'SUB1 FUNCTION'
function_pred('++', 1, 'arithmetic:$++'/2).      %% shorthand for 'ADD1 FUNCTION'
function_pred('integer', 1, 'arithmetic:$integer'/2).
function_pred('truncate', 1, 'arithmetic:$truncate'/2).
function_pred('float', 1, 'arithmetic:$float'/2).
function_pred('\\', 1, 'arithmetic:$\\'/2).
function_pred('abs', 1, 'arithmetic:$abs'/2).
function_pred('sign', 1, 'arithmetic:$sign'/2).
function_pred('float_integer_part', 1, 'arithmetic:$float_integer_part'/2).
function_pred('float_fractional_part', 1, 'arithmetic:$float_fractional_part'/2).
function_pred('floor', 1, 'arithmetic:$floor'/2).
function_pred('round', 1, 'arithmetic:$round'/2).
function_pred('ceiling', 1, 'arithmetic:$ceiling'/2).
function_pred('+', 2, 'arithmetic:$+'/3).
function_pred('-', 2, 'arithmetic:$-'/3).
function_pred('*', 2, 'arithmetic:$*'/3).
function_pred('/', 2, 'arithmetic:$/'/3).
function_pred('//', 2, 'arithmetic:$//'/3).
function_pred('rem', 2, 'arithmetic:$rem'/3).
function_pred('#', 2, 'arithmetic:$#'/3).
function_pred('/\\', 2, 'arithmetic:$/\\'/3).
function_pred('\\/', 2, 'arithmetic:$\\/'/3).
function_pred('<<', 2, 'arithmetic:$<<'/3).
function_pred('>>', 2, 'arithmetic:$>>'/3).
function_pred('mod', 2, 'arithmetic:$mod'/3).
function_pred('**', 2, 'arithmetic:$**'/3).
function_pred('gcd', 2, 'arithmetic:$gcd'/3).
function_pred('exp', 1, 'arithmetic:$exp'/2).
function_pred('log', 1, 'arithmetic:$log'/2).
function_pred('sqrt', 1, 'arithmetic:$sqrt'/2).
function_pred('sin', 1, 'arithmetic:$sin'/2).
function_pred('cos', 1, 'arithmetic:$cos'/2).
function_pred('atan', 1, 'arithmetic:$atan'/2).

:- '$ctxprj'(eval_builtin/2, []).
eval_builtin('arithmetic:=:=', 2).
eval_builtin('arithmetic:=\\=', 2).
eval_builtin('arithmetic:<', 2).
eval_builtin('arithmetic:>=', 2).
eval_builtin('arithmetic:>', 2).
eval_builtin('arithmetic:=<', 2).

% Normalize unifications. Replace out args by fresh variables (good for analysis and precondition to the compiler)
expand_unif(X, Y) :- var(X), var(Y), !,
	goals.add(~aterm__goal('term_basic:$unify'/2, [X, Y])).
expand_unif(X, Y) :- var(X), !,
	goals.add(~aterm__goal('term_basic:$instance'/2, [X, Y])).
expand_unif(X, Y) :- var(Y), !,
	goals.add(~aterm__goal('term_basic:$instance'/2, [Y, X])).
expand_unif(X, Y) :- functor(X, N, A), functor(Y, N, A), !,
	expand_unif_3(1, A, X, Y).
expand_unif(_, _) :-
	goals.add(~aterm__goal('basiccontrol:fail'/0, [])).

expand_unif_3(I, A, _, _) :- I > A, !.
expand_unif_3(I, A, X, Y) :-
	arg(I, X, Xi),
	arg(I, Y, Yi),
	I1 is I + 1,
	expand_unif(Xi, Yi),
	expand_unif_3(I1, A, X, Y).

expand_out_args('basiccontrol:$caller_choice'/1, [Ch]) :- !,
	% TODO: temporal unifications are not corretly handled by DefaultCut
	goals.add(~aterm__goal('basiccontrol:$caller_choice'/1, [Ch])).
expand_out_args(Name, Args0) :-
	GId = ~predicate_x.reg_new(Name),
	Modes = ~GId.get_prop(argmodes),
	% put free vars in output arguments 
	OArgs0 = ~filter_mode(Args0, Modes, out),
	OArgs = ~out_args(OArgs0),
	Args = ~replace_mode(Args0, Modes, out, OArgs),
	G = ~strgoal.new_n(GId, ~aterm__args(Args, Modes)),
	goals.add(G),
	% move unifications later
	maplist(([goals] -> ''(OArg0, OArg) :-
          expand_unif(OArg0, OArg)
	), OArgs0, OArgs).
}.

out_args(As) := ~maplist((''(_,_) :- true), As).

% Change the representation to a domain where terms and variables can be annotated with pairs name-value
% TODO: think about cheaper representations
{
:- extends ctx_module.
aterm__goal(Name, Xs) := X2 :-
	PredId = ~predicate_x.reg_new(Name),
	ArgModes = ~PredId.get_prop(argmodes),
	X2 = ~strgoal.new_n(PredId, ~aterm__args(Xs, ArgModes)).
}.

aterm__args([], []) := [].
aterm__args([X|Xs], [M|Ms]) := [~aterm__arg(X, M) | ~aterm__args(Xs, Ms)].

aterm__arg(V, param) := ~termunk.new(V) :- !.
aterm__arg(V, _) := ~aterm__term(V) :- !.

aterm__term(V) := ~termvar.new_n(V) :- var(V), !.
aterm__term(X) := ~termstr.new_n(N/A, ~aterm__terms(Xs)) :-
	functor(X, N, A),
	X =.. [_|Xs].

aterm__terms([]) := [].
aterm__terms([X|Xs]) := [~aterm__term(X) | ~aterm__terms(Xs)].


