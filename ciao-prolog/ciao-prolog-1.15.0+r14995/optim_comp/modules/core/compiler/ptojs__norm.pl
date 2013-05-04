% Version of compiler__expand.pl for JS-backend [see documentation in compiler__expand.pl]
%
% TODO:
%  - This module mixes 'mexpand' (with functional expansion) and
%    'compiler__expand'.  It should be separated and merged.
%
%  - The transformations in compiler__expand are done before
%    module expansions, fix the order.
%    (mexpnorm_goals should happen before norm_preds)
%
%  - 'clause' translations are not executed at this moment
%
%  - aterm__* invoke 'resolve_term'; this is not correct,
%    do it in a separate pass; avoid aterm__* representation;
%    give special meaning to qualified atoms to solve this

:- use_module(compiler(meta_syntax), [get_qualifier/3, apply_qual/3]).
:- use_module(library(string_type(string_type_rt))).
:- use_module(library(dict)).

% TODO: ctx_module is similar to 'cscope' (and is this just an environment?)

{
%:- extends ctx_module.
:- public norm_preds/2.
% Normalize the predicate list: input is module-expanded, output is 'acode' predicate definitions
% TODO: not yet 'acode' definitions
norm_preds(Preds0, Preds) :-
	preds :: accum(Preds),
        maplist(([preds] -> ''(PredId) :- norm_pred(PredId)), Preds0).
}.

{
% :- extends ctx_module.
:- fluid preds :: accum.
norm_pred(PredId) :-
	trust(PredId instance_of predicate_s),
	PredId.get_prop(virtual), !,
	preds.add(PredId).
norm_pred(PredId) :-
	trust(PredId instance_of predicate_s),
 	icode(_, _, _) = ~PredId.code, !, % already in normalized form
	preds.add(PredId).
norm_pred(PredId) :-
	trust(PredId instance_of predicate_s),
	Code = ~PredId.all_clauses,
	( Code = [] ->
	    % a functor with no associated code
	    set_nodef(PredId),
	    preds.add(PredId)
	; PredId.clause_defines_js_native(Code, Args, Code2),
	  ModuleR0 = ~PredId.owner_module,
	  ModuleR0.get_prop_in_scope(allow_js_lang) ->
	    set_native_def(PredId, Args, Code2),
	    preds.add(PredId)
	; % Note: 'Args' is instantiated in norm_clauses
	  ( PredId.needs_self ->
	      % Add extra argument for Self (if we are defining a method)
	      Args2 = [_Self|Args]
	  ; Args2 = Args
	  ),
	  Scope = ~get_scope(PredId, Args2),
	  call((
            cscope :: scope <- Scope,
	    Cs0 = ~norm_clauses(Args, Code),
	    optim_clauses(Cs0, Cs1),
	    Def = normcode(Args2, Cs1),
	    preds.add(PredId),
	    % TODO: complete
	    call((
              sh :: shareddic,
	      occurrences_clause_fixpo(OccDic, Cs1, Args2),
	      call((
                occdic :: u_dic <- OccDic,
	    	occpath :: any <- start,
	    	straight_clauses(PredId, Def)
	      ))
            ))
            % TODO: merge mexpnorm_clauses here
	  ))
	).
}.

{
:- fluid cscope :: scope.
norm_clauses(Args, Cs) := Cs2 :-
	Cs2 = ~maplist((''(C, C2) :-
	  C = tc(H, B00),
	  H =.. [_|As0],
	  call((
            goals :: accum(C1),
            head_unifs(As0, Args),
	    call((
              use_choice :: any <- UseChoice,
              choice :: any <- Choice,
	      subchunk_n :: m_int <- 1,
	      norm_goal(B00)
            ))
          )),
	  % TODO: only necessary in JS-backend
	  ( nonvar(UseChoice) ->
	      C2 = ['$caller_choice'(Choice)|C1]
	  ; C2 = C1
	  )
        ), Cs).
}.

{
:- fluid goals :: accum.
head_unifs(As0) := As :-
	seen :: m_any <- [],
	As = ~maplist(([goals, u(seen)] -> ''(A0, A) :-
	  ( var(A0), \+ (member(A00, ~seen), A00 == A0) ->
	      A = A0,
	      seen <- [A0|~seen]
	  ; % emit a unification if the argument is not a variable, or it is an already seen variable
            A = _,
	    goals.add(A0 = A) % TODO: missing mod
	  )
	), As0).
}.

% Expand X (var), \+/1, if/3 and (A->B) without alternatives to (A->B;fail))
% Also expand !/0 and ->/2
% TODO: document: $caller_choice is the most recent choice point
{
:- fluid cscope :: scope.
:- fluid choice :: any.
:- fluid use_choice :: any.
:- fluid subchunk_n :: m_int.
:- fluid goals :: accum.
norm_goal(Var) :- var(Var), !, % TODO: missing mod
	goals.add('call'(Var)).
%% norm_goal('hiord_rt:$meta_call'(P0)) :- !,
%% 	goals.add('hiord_rt:call'(P0)).
% norm_goal('aggregates:^'(V, P0)) := 'aggregates:^'(V, P)) :- !,
%	P = ~norm_goal(P0, outer).
norm_goal('false') :- !, % TODO: missing mod
	goals.add('fail').
norm_goal('otherwise') :- !. % TODO: missing mod
% norm_goal('true') :- !. % TODO: NOT WORKING!
norm_goal('!') :- !,
	~use_choice = yes, % mark that we use the choice
	goals.add('$cut'(~choice)). % TODO: rename $set_choie by $cut
% (Optimize call/1 with nonvar)
% TODO: semantics of 'cut' may be different, fix
% TODO: resolve module of call/1?
norm_goal(call(X)) :- nonvar(X), !, % TODO: missing mod
	norm_goal(X).
norm_goal(G) :- G = ';'(_, _), !, % TODO: missing mod
	Hs = ~norm_goal__or(G),
	ChunkN = ~subchunk_n,
	subchunk_n.inc(1),
	goals.add('$or$'(ChunkN, Hs)).
norm_goal(G) :- G = '->'(_, _), !, % TODO: missing mod
	norm_goal(';'(G, 'fail')).
norm_goal('\\+'(P0)) :- !, % TODO: missing mod
	% TODO: optimize later?
	( P1 = ~negated(P0) ->
	    norm_goal(P1)
	; norm_goal(';'(
                      '->'(P0, 'fail'), 
                      'true'))
	).
norm_goal(','(P, Q)) :- !, % TODO: missing mod
	norm_goal(P),
	norm_goal(Q).
% TODO: document: like (P->Q;R) but does backtracking on P? (i.e. executes Q for all P solutions; if P did not have any solution then execute R)
norm_goal('if'(P, Q, R)) :- !,
	% TODO: I am not sure about this... where do I cut inside if/3?
        norm_goal(','(
                    '='(Flag, [no]),
                    ';'(
                      ','(
                        P, 
                        ','(
                          'IF BUILTIN'(Flag), 
                          Q
                        )
                      ),
                      ','(
                        '='(Flag, [no]),
                        R
                      )
                    )
                  )).
norm_goal(A) :- % TODO: This is not the place to put this
	% This translation does not know about modules of each goal yet
        mexpand__do_goal_trans(A, NB), !,
	% Call expansion again (it may loop if goal translation rules
	% are wrong)
        norm_goal(NB).
norm_goal(P) :-
	goals.add(P).
}.

{
:- fluid cscope :: scope.
:- fluid choice :: any.
:- fluid use_choice :: any.
norm_goal__or(G) := Hs :-
	call(( cases :: accum(Hs0), norm_goal__or__2(G) )),
	optim_clauses(Hs0, Hs).

{
:- fluid cases :: accum.
norm_goal__or__2(G) :-
	nonvar(G), G = ';'(P0, Q0), !, % TODO: missing mod
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
	( P = ['fail'] ->
	    true
	; cases.add(P)
	).
}.

{
:- fluid goals :: accum.
:- fluid subchunk_n :: m_int.
norm_goal__case(G) :-
	nonvar(G), G = '->'(P0, Q0), !, % TODO: missing mod
	goals.add('$caller_choice'(Choice2)), % TODO: missing mod
	norm_goal(P0),
	goals.add('$cut'(Choice2)), % TODO: missing mod
	norm_goal(Q0).
norm_goal__case(G) :-
	norm_goal(G).
}.

}.

% TODO: missing mod
negated('var'(X)) := 'nonvar'(X) :- !.
negated('nonvar'(X)) := 'var'(X) :- !.
negated('=='(X, Y)) := '\\=='(X, Y) :- !.
negated('\\=='(X, Y)) := '=='(X, Y) :- !.
negated('@<'(X, Y)) := '@>='(X, Y) :- !.
negated('@>='(X, Y)) := '@<'(X, Y) :- !.
negated('@>'(X, Y)) := '@=<'(X, Y) :- !.
negated('@=<'(X, Y)) := '@>'(X, Y) :- !.

% ---------------------------------------------------------------------------
% Avoid some temporal predicates

% TODO: this avoids temporal predicates, write a better version (should I move this to norm_pred? does that simplify treatment of $caller_choice?)
% TODO: there are other possible optimizations... delay $or$ to the compilation/transformation phase??
% TODO: do some more intelligent analysis with caller_choice (maybe in ptoc__lowcomp:default_cut/3, to detect what does a cut cut)
optim_clauses(Cs0, Cs) :-
	% a predicate with only one (last) clause composed of disjunctions
	append(PreCs0, [[G, '$or$'(_ChunkN, Cs1)]], Cs0),
	G = '$caller_choice'(Choice), % TODO: missing mod
	!,
	append(PreCs0, Cs2, Cs),
	% unify the choice of all caller_choice in Cs1 with Choice, add missing caller_choice
	maplist((''(C, C2) :-
	  ( C = ['$caller_choice'(Choice0)|_] -> % TODO: missing mod
	      Choice = Choice0,
	      C2 = C
	  ; % add caller choice
            % TODO: make caller choice optional in rest of compilation steps so that this is not necessary
            C2 = ['$caller_choice'(Choice)|C] % TODO: missing mod
	  )
        ), Cs1, Cs2).
optim_clauses(Cs0, Cs) :-
	% TODO: (version where caller_choice is not used; not required in compiler__expand)
	append(PreCs0, [['$or$'(_ChunkN, Cs1)]], Cs0),
	!,
	append(PreCs0, Cs1, Cs).
optim_clauses(Cs, Cs).

% ---------------------------------------------------------------------------
% Calculate shared variables for subpredicates

% TODO: this algorithm is quadratic w.r.t the disjunction depth, fix -> however profiling does not indicate that most predicates are in the worse case
%   A better algorithm would be computing an occurence list for all the clause
{
%:- extends ctx_module.
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
	  call((
            vars :: accum(AllVars),
	    record_occurrences_term(Args),
	    call((
              occpath :: any <- OccPath,
	      record_occurrences_clauses__2(Hs)
            ))
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
%:- extends ctx_module.
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

%:- '$ctxprj'(occurrences_subpr/3, [exp, changed, occdic, sh]).
:- '$ctxprj'(occurrences_subpr/3, [changed, occdic, sh]).
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
	maplist(([vars] -> ''(G) :-
          record_var(G)
        ), Gs).
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
%:- extends ctx_module.
:- fluid cscope :: scope.
:- fluid occdic :: u_dic.
:- fluid sh :: shareddic.
:- fluid occpath :: any.
:- fluid preds :: accum.
straight_clauses(PredId, Def) :-
	trust(PredId instance_of predicate_s),
	Def = normcode(Args0, Clauses0),
	% TODO: share code structure with record_occurrences_clauses__2
        call((
	  from_id :: predicate_s <- PredId,
	  clause_no :: m_int <- 1,
	  Clauses = ~maplist(([sh, preds, u(clause_no)] -> ''(Body0, Body) :-
            NewPath = (~clause_no, ~occpath),
            call((
              occpath :: any <- NewPath,
	      goals :: accum(Body),
	      straight_body(Body0)
            )),
	    clause_no.inc(1)
          ), Clauses0)
        )),
	PredId.set_code(icode(a, Args0, or(Clauses))).
}.

{
%:- extends ctx_module.
:- fluid cscope :: scope.
:- fluid occdic :: u_dic.
:- fluid sh :: shareddic.
:- fluid preds :: accum.
:- fluid goals :: accum.
:- fluid from_id :: predicate_s.

{
:- fluid occpath :: any.
straight_body(Gs) :-
	maplist(([preds, goals, sh] -> ''(G) :- ( 
	  G = '$or$'(SubChunkN, Hs) ->
	    OccPath = c(SubChunkN, ~occpath),
	    (SubPredId, SubPredDef) = ~straight_new_subpred(OccPath, [], Hs),
	    call((
              occpath :: any <- OccPath,
	      straight_clauses(SubPredId, SubPredDef)
            )),
	    straight_call_pa(OccPath, SubPredId, [])
	; G = '$subpr$'(SubChunkN, PredAbs, Args, Hs) ->
	    OccPath = c(SubChunkN, ~occpath),
	    (SubPredId, SubPredDef) = ~straight_new_subpred(OccPath, Args, Hs),
	    call((
	      occpath :: any <- OccPath,
	      straight_clauses(SubPredId, SubPredDef)
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
	        (SubPredId, SubPredDef) = ~straight_new_subpred(OccPath, PredArgs, Hs),
%	        errlog:trace([donenewpred(OccPath, Hs)]),
	        call((
	          occpath :: any <- OccPath,
	          straight_clauses(SubPredId, SubPredDef)
                ))
%	        errlog:trace([donestraightclauses(OccPath)])
	    ; true % do nothing, already generated
	    ),
	    straight_call_pa(OccPath, SubPredId, Args)
%	    errlog:trace([doneocccall(OccPath)])
	; unfold_builtin(G)
	)), Gs).
}.

straight_new_subpred(OccPath, Args, Hs) := (SubPredId, SubPredDef) :-
	Shared = ~straight_shared_vars(OccPath),
	SharedWA = ~append(Args, Shared),
	length(SharedWA, SubArity),
	SubPredId = ~from_id.new_sub(SubArity),
	( SubPredId.needs_self ->
	    % Add extra argument for Self (if we are defining a method)
	    cscope.set_selfvar(Self),
	    SharedWA2 = [Self|SharedWA]
	; SharedWA2 = SharedWA
	),
	SubPredDef = normcode(SharedWA2, Hs), % TODO: Delayed because variable sharing is necessary in this algorithm, and 'set_code' make copies (this does not happen with predicate_x structures)
	preds.add(SubPredId).
}.

{
%:- extends ctx_module.
:- fluid goals :: accum.
:- fluid occdic :: u_dic.
:- fluid sh :: shareddic.
% Unify Var with the predicate abstraction for OccPath-SubPredId
straight_unif_pa(OccPath, SubPredId, Var) :-
	trust(SubPredId instance_of predicate_s),
	Shared = ~straight_shared_vars(OccPath),
	SubName = ~SubPredId.name,
	SubName = N/A,
	% register hiord pred so that it is not lost in analysis % TODO: add as a property
	% TODO: do this only if this code is reachable!
	% Exp = ~exp,
	% ( Exp.uses_hiord_pred(N, A) -> true ; Exp.add(uses_hiord_pred(N, A)) ),
	display(user_error, uses_hiord_pred(N, A)), nl(user_error),
	PredAbs =.. [N|Shared],
	% note: omit arguments
	% TODO: allow missing arguments in any argument place? or use a fixed scheme to make it very fast? or reuse the subterm as a kind of environment?
	expand_unif(Var, '$PA$'(PredAbs)).
% Do a call to the predicate abstraction for OccPath-SubPredId
straight_call_pa(OccPath, SubPredId, Args) :-
	trust(SubPredId instance_of predicate_s),
	Shared = ~straight_shared_vars(OccPath),
	SubName/_ = ~SubPredId.name,
	SharedWA = ~append(Args, Shared),
	SubCall =.. [SubName|SharedWA],
	goals.add(SubCall).
}.

% ---------------------------------------------------------------------------

% Unfold (expanded) definition of some built-in predicates
% TODO: generalize to include user defined expansions? - arithmetic expansions are not trivial
{
%:- extends ctx_module.
:- fluid goals :: accum.
unfold_builtin(X) :- goals.add(X).
}.

% Normalize unifications. Replace out args by fresh variables (good for analysis and precondition to the compiler)
{
%:- extends ctx_module.
:- fluid goals :: accum.
expand_unif(X,Y) :- goals.add(X = Y).
}.

% ---------------------------------------------------------------------------
% Goal expansions
% TODO: merge with compiler/frontend.pl, apply before norm_clause

% Apply a goal translation (the first that match) on goal G.
% This can be done several times until a fixpoint is reached (or it may
% not end if goal translation rules are wrong).
{
:- fluid cscope :: scope.
mexpand__do_goal_trans(G,  NG) :-
	\+ mexpand__forbidden_syntax(G),
        mexpand__goal_trans(T),
          arg(1, T, G),
          arg(2, T, NG),
          '$meta_call'(T), !.

mexpand__goal_trans(T) :-
	cscope.currmodule.get_translation_hook(goal, KV), % (nondet)
	pqueue_values([KV], [T]).

% TODO: duplicated
% Obtain all the values of the priority queue
pqueue_values([], []).
pqueue_values([_-V|Xs], [V|Vs]) :-
	pqueue_values(Xs, Vs).
}.

% Syntactic control structures that the goal_trans will not treat.
% TODO: configure using a declaration? 
mexpand__forbidden_syntax(','(_,_)).
mexpand__forbidden_syntax(';'(_,_)).
mexpand__forbidden_syntax('->'(_,_)).
mexpand__forbidden_syntax('\\+'(_)).
mexpand__forbidden_syntax('if'(_,_,_)).

% ---------------------------------------------------------------------------
% TODO: does it belong here?

% Define a predicate with no associated code
set_nodef(PredId) :-
	trust(PredId instance_of predicate_s),
	PredId.get_name(_, A),
	functor(Head, dummy, A), Head =.. [_|As],
	Def = functor_def,
	PredId.set_prop(specialdef(As, Def)),
	C = ['\6\predid_apply'(~predicate_s.from_id('$nodef/0'), [])],
	PredId.set_code(icode(a, As, or([C]))).

% ---------------------------------------------------------------------------
% TODO: does it belong here?

% Define the predicate as native JS code
set_native_def(PredId, Args, Code) :- !,
	trust(PredId instance_of predicate_s),
        prepare_js_native_args(Args, CArgs), % TODO: temporal, use cmem
        PredId.set_insns_def(CArgs, [js_stats(Code)]).

% Prepare arguments for js_native
{
:- static prepare_js_native_args/2.
% TODO: This is a temporal solution, use cmem instead
prepare_js_native_args(Args, Exprs) :-
        ( Args = [Arg|_], var(Arg) ->
	    % arguments are variables, instantiate them
	    N = ~length(Args),
	    Args = Exprs,
	    Exprs = ~cargs_mem(N)
	; Exprs = ~plain_mem(Args)
	).
}.

% ===========================================================================
% (functional expansion, module resolution, and translation to aterms)
% TODO: (merge with mexpand and the previous code)

{
:- fluid cscope :: scope.
mexpnorm_clauses([], []).
mexpnorm_clauses([C|Cs], [C2|Cs2]) :-
	call(( goals :: accum(C2), mexpnorm_goals(C) )),
	mexpnorm_clauses(Cs, Cs2).
}.

{
:- fluid cscope :: scope.
:- fluid goals :: accum.
mexpnorm_goals([]) :- !.
mexpnorm_goals([X|Xs]) :- mexpand__goal0(X), mexpnorm_goals(Xs).
}.

{
:- fluid goals :: accum.
:- fluid cscope :: scope.
%mexpand__goal0(A) :- trace(ng(A)), fail.
% Some built-ins
mexpand__goal0('$metargs'(MetArgs)) :- !, % TODO: special case, one arg that is a list of aterm
	FunctorR = ~predicate_s.from_id('$metargs/1'),
	G0 = ~strgoal.new_psym(FunctorR, [~aterm__args(MetArgs)]), % (special case)
	goals.add(G0).
mexpand__goal0('\6\predid_apply'(FunctorR, As)) :- !,
	% used to emit wrapper code for special predicates
	G0 = ~aterm__goal(FunctorR, ~defunc_args(As)),
	goals.add(G0).
mexpand__goal0('$kind_of'(Var, Class)) :- !,
	type_to_qmod(Class, QMod), % TODO: this should not be necessary
	mexpand__goal0(':'(QMod, '$check'(Var))).
mexpand__goal0('$suspend') :- !, % TODO: make it optional (bypasses the module system)
	FunctorR = ~predicate_s.from_id('$suspend/0'),
	G0 = ~aterm__goal(FunctorR, []),
	goals.add(G0).
mexpand__goal0('$trail_set_attr'(X, M, Attr)) :- !, % TODO: make it optional (bypasses the module system)
	FunctorR = ~predicate_s.from_id('$trail_set_attr/3'),
	G0 = ~aterm__goal(FunctorR, ~defunc_args([X, M, Attr])),
	goals.add(G0).
mexpand__goal0('$nodef') :- !, % TODO: make it optional (bypasses the module system)
	FunctorR = ~predicate_s.from_id('$nodef/0'),
	G0 = ~aterm__goal(FunctorR, []),
	goals.add(G0).
mexpand__goal0('$module'(X)) :- !, % TODO: make it optional (bypasses the module system)
	% (unify X with the current module)
	% TODO: call should not be necessary, modules should be atoms and this should be a unification
	M = ~cscope.currmodule.get_name,
	G =.. [M, X],
	mexpand__goal0(G).
%
mexpand__goal0('$boot') :- !,
	%% It was: ':'('\6\root', static_noself_new__), ':'('\6\root', '__call_main__').
	RootR = ~module_s.lookup_module(root),
	% TODO: Add impl_defined, etc. we should not need a builtin for 'boot'
	P = ~RootR.pred_ref('static_noself_new__', 0), % (even if it does not exist)
	Q = ~RootR.pred_ref('__call_main__', 0), % (even if it does not exist)
	G0 = ~aterm__goal(P, []),
	goals.add(G0),
	G1 = ~aterm__goal(Q, []),
	goals.add(G1). % TODO: missing arguments in main...
% Mutable assignment
mexpand__goal0((A0 <- B)) :- !,
	A = ~ensure_obj(A0),
	mexpand__goal0(~mcall(A, '$mutset'(B))).
% Get element of a field (Var[Index])
mexpand__goal0('\6\get_field'(Obj, Field, T)) :- !,
        mexpand__goal0(~mcall(Obj, '$get_field'(Field, T))).
% Other calls
mexpand__goal0(X) :-
	term_r(FunctorR, MaybeObj, Args1) = ~cscope.resolve_term(X, error),
	mexpand__goal00(FunctorR, ~add_maybe_obj(MaybeObj, Args1)).

mexpand__goal00(FunctorR, Args1) :-
	trust(FunctorR instance_of predicate_s),
	( ~FunctorR.get_id = 'term_basic:C/3',
	  Args1 = [X, Y, Z] ->
	    mexpand__goal00(~lookup_mod_pred('term_basic', '=', 2), [X, [Y|Z]])
	; % TODO: functional expansion must be done BEFORE c_terms (not during)
	  ( is_arith_is(FunctorR),
	    Args1 = [Value, Expr] ->
	      unfold_builtin__is(Value, Expr)
	  ; is_eval_builtin(FunctorR) ->
	      unfold_builtin__eval(FunctorR, Args1)
	  ; G0 = ~aterm__goal(FunctorR, ~defunc_args(Args1)),
	    goals.add(G0)
	  )
	).
}.

is_unify(FunctorR) :-
	trust(FunctorR instance_of predicate_s),
	~FunctorR.get_id == 'term_basic:=/2'.

is_arith_is(FunctorR) :-
	trust(FunctorR instance_of predicate_s),
	~FunctorR.get_id == 'arithmetic:is/2'.

is_eval_builtin(FunctorR) :-
	trust(FunctorR instance_of predicate_s),
	eval_builtin(~FunctorR.get_id).

% TODO: merge with compiler__expand:eval_builtin/2
eval_builtin('arithmetic:=:=/2').
eval_builtin('arithmetic:=\\=/2').
eval_builtin('arithmetic:</2').
eval_builtin('arithmetic:>=/2').
eval_builtin('arithmetic:>/2').
eval_builtin('arithmetic:=</2').

{
:- fluid goals :: accum.
:- fluid cscope :: scope.

unfold_builtin__eval(FunctorR, [X, Y]) :-
	unfold_builtin__expr(Y, Y1),
	unfold_builtin__expr(X, X1),
	G0 = ~aterm__goal(FunctorR, ~defunc_args([X1, Y1])),
	goals.add(G0).

% TODO: communicate empty expansion in other way?
unfold_builtin__is(Value, Expr0) :-
	( call((
            goals :: accum <- S0,
	    unfold_builtin__expr(Expr0, Expr),
	    ~goals = S
          )), 
          S0 \== S -> % note that fails if the expansion is empty!
	    S0 = ~goals, goals <- S
	; unfold_builtin__expr('+'(Expr0), Expr)
	),
%	mexpand__goal00(~lookup_mod_pred('term_basic', '=', 2), [Expr, Value]). % TODO: goal not necessary at this moment (fix)
	Expr = Value. % TODO: goal not necessary at this moment (fix)
%%	goals.add((Expr = Value)).

% postcondition: output arguments are fresh variables in all emitted functions 
unfold_builtin__expr(Expr, Expr) :- var(Expr), !.
unfold_builtin__expr([Expr0], Expr) :- !,
	unfold_builtin__expr(Expr0, Expr).
unfold_builtin__expr(E, Value) :-
	E2 = ( E = '-'(X, Y), integer(Y), Y = 1 ? '--'(X)  %% shorthand for 'SUB1 FUNCTION'
	     | E = '+'(X, Y), integer(Y), Y = 1 ? '++'(X)  %% shorthand for 'ADD1 FUNCTION'
	     | E
	     ),
	functor(E2, E2Atom, E2Arity), function_pred(E2Atom, E2Arity, NameM, NameF, NameA), !,
	ArithModR = ~module_s.lookup_module(NameM), % TODO: check?
	FunctorR = ~ArithModR.pred_ref(NameF, NameA), % (even if it does not exist)
	E2 =.. [_|Args],
	( Args = [X] ->
	    unfold_builtin__expr(X, X1),
	    Args2 = [X1, Value]
	; Args = [X, Y] ->
	    unfold_builtin__expr(Y, Y1),
	    unfold_builtin__expr(X, X1),
	    Args2 = [X1, Y1, Value]
	; bug(['invalid function ', E]), fail
	),
	G0 = ~aterm__goal(FunctorR, ~defunc_args(Args2)),
	goals.add(G0).
unfold_builtin__expr(E, E).
}.

:- meta_predicate lookup_mod_pred(?, ?, ?, out(predicate_s)).
lookup_mod_pred(NameM, NameF, NameA) := FunctorR :-
	ModR = ~module_s.lookup_module(NameM), % TODO: check?
	FunctorR = ~ModR.pred_ref(NameF, NameA). % (even if it does not exist)

% TODO: use compiler declarations!
function_pred('-', 1, 'arithmetic', '$-', 2).
function_pred('+', 1, 'arithmetic', '$+', 2).
function_pred('--', 1, 'arithmetic', '$--', 2).      %% shorthand for 'SUB1 FUNCTION'
function_pred('++', 1, 'arithmetic', '$++', 2).      %% shorthand for 'ADD1 FUNCTION'
function_pred('integer', 1, 'arithmetic', '$integer', 2).
function_pred('truncate', 1, 'arithmetic', '$truncate', 2).
function_pred('float', 1, 'arithmetic', '$float', 2).
function_pred('\\', 1, 'arithmetic', '$\\', 2).
function_pred('abs', 1, 'arithmetic', '$abs', 2).
function_pred('sign', 1, 'arithmetic', '$sign', 2).
function_pred('float_integer_part', 1, 'arithmetic', '$float_integer_part', 2).
function_pred('float_fractional_part', 1, 'arithmetic', '$float_fractional_part', 2).
function_pred('floor', 1, 'arithmetic', '$floor', 2).
function_pred('round', 1, 'arithmetic', '$round', 2).
function_pred('ceiling', 1, 'arithmetic', '$ceiling', 2).
function_pred('+', 2, 'arithmetic', '$+', 3).
function_pred('-', 2, 'arithmetic', '$-', 3).
function_pred('*', 2, 'arithmetic', '$*', 3).
function_pred('/', 2, 'arithmetic', '$/', 3).
function_pred('//', 2, 'arithmetic', '$//', 3).
function_pred('rem', 2, 'arithmetic', '$rem', 3).
function_pred('#', 2, 'arithmetic', '$#', 3).
function_pred('/\\', 2, 'arithmetic', '$/\\', 3).
function_pred('\\/', 2, 'arithmetic', '$\\/', 3).
function_pred('<<', 2, 'arithmetic', '$<<', 3).
function_pred('>>', 2, 'arithmetic', '$>>', 3).
function_pred('mod', 2, 'arithmetic', '$mod', 3).
function_pred('**', 2, 'arithmetic', '$**', 3).
function_pred('gcd', 2, 'arithmetic', '$gcd', 3).
function_pred('exp', 1, 'arithmetic', '$exp', 2).
function_pred('log', 1, 'arithmetic', '$log', 2).
function_pred('sqrt', 1, 'arithmetic', '$sqrt', 2).
function_pred('sin', 1, 'arithmetic', '$sin', 2).
function_pred('cos', 1, 'arithmetic', '$cos', 2).
function_pred('atan', 1, 'arithmetic', '$atan', 2).

{
:- fluid cscope :: scope.
% IMPORTANT NOTE: fsyntax AND js-backend
%
%   In the JS-backend I made the 'functional expansion' flag a
%   property of the symbol, which can be exported. Thus, it is
%   slightly more complex than a syntactic transformation.
%
%   In an atom-based module system, it make sense treat symbols which
%   belong to different modules differently. That is, doing symbol
%   resolution while applying the functional translation.
%
%   However, that has some deep interaction with the semantics. For
%   example, one could no longer know that 'M:foo' is an atom or a
%   predicate, and that complicates things (maybe
%   unnecessarily). Probably, I should include a pure 'syntactic' flag
%   and study the repercussions of the 'semantic' flag alone.

needs_funexp(X, _) :- var(X), !, fail.
needs_funexp(X, _) :- X = ~mcall(_,_), !, fail.
% Var[Index]
% TODO: Make the V[I] notation optional (it can be handy for some programs, anyway)
needs_funexp(X, Y) :- X = '\6\get_field'(_, _), !, Y = X.
needs_funexp(X, Y) :- square_index(X, V, I), !, Y = '\6\get_field'(V, I).
needs_funexp(X, Y) :- X = ~funcall(X0), square_index(X0, V, I), !, Y = '\6\get_field'(V, I). % TODO: needed?
%
needs_funexp(X, Y) :- X = ~funcall(X0), !, Y = X0.
needs_funexp(X, X) :- needs_funexp0(X).

needs_funexp0(X0) :-
	% Not a local functor
	\+ (
	  term_r(FunctorR2, _, _) = ~cscope.resolve_term(X0, fail),
	  trust(FunctorR2 instance_of predicate_s),
	  FunctorR2.get_prop(local_functor) % Locally defined % TODO: recode
        ),
	% And the extended functor is marked with 'use_functional'
	get_qualifier(X0, Qual, X1),
	funexp_goal(X1, _T, X2),
	X3 = ~apply_qual(Qual, X2),
	%
	term_r(FunctorR1, _, _) = ~cscope.resolve_term(X3, fail),
	trust(FunctorR1 instance_of predicate_s),
	FunctorR1.get_prop(use_functional).
}.

% TODO: do funexp in a separate pass
{
:- fluid goals :: accum.
:- fluid cscope :: scope.

% TODO: see mexpand:defunc_args/4
defunc_args([], []).
defunc_args([X0|Xs0], [X|Xs]) :-
        defunc_exp(X0, X),
        defunc_args(Xs0, Xs).

defunc_exp(X) := X :- var(X), !.
defunc_exp(X) := X :- number(X), !.
defunc_exp(X) := X :- string_codes(X, _), !.
defunc_exp(X) := R :- needs_funexp(X, X2), !,
	defunc_fun(X2, R).
defunc_exp(X) := R :-
	X =.. [N|Xs],
	Ys = ~defunc_args(Xs),
	R =.. [N|Ys].

% Expansion of interpreted terms:
%  - goals in functional notation
%  - partial application of terms with arguments
%  - local object terms
% TODO: this should be just a goal
defunc_fun(self, R) :- ~cscope.get_selfvar = Self, !,
	R = Self.
% execution of ~Goal
defunc_fun(X0, R) :-
	R = T,
	get_qualifier(X0, Qual, X1),
	funexp_goal(X1, T, X2),
	X3 = ~apply_qual(Qual, X2),
	mexpand__goal0(X3).
}.

% Symbol for the functional expansion of other symbol
% (typically, just add '1' to the arity')
funexp_sym(Sym, Sym1) :-
	Sym = sympred(N, GoalA0),
	GoalA is GoalA0 + 1,
	Sym1 = sympred(N, GoalA).

% Goal for the functional expansion (Goal is Goal0 with an additional
% argument T at the end)
funexp_goal(Goal0, T, Goal) :-
        Goal0 =.. [N|As],
	As2 = ~append(As, [T]),
        Goal =.. [N|As2].

:- public ensure_obj/2.
% Syntactic aid used to add ~ automatically in positions where
% unevaluated terms are meaningless.
%   e.g. a.b <- foo ====> (~a).b <- foo
% TODO: I am not sure that this would be required...
ensure_obj(Obj0) := Obj :-
	( var(Obj0) -> Obj = Obj0
	; Obj0 = ~funcall(_) -> Obj = Obj0
	; Obj = ~funcall(Obj0)
	).

% ---------------------------------------------------------------------------

% Change the representation to a domain where terms and variables can
% be annotated with pairs name-value.
{
:- fluid goals :: accum. % for expansion of 'self' in term_r
:- fluid cscope :: scope.

aterm__goal(FunctorR, As) := X2 :-
	X2 = ~strgoal.new_psym(FunctorR, ~aterm__args(As)).

aterm__args([]) := [].
aterm__args([X|Xs]) := [~aterm__arg(X) | ~aterm__args(Xs)].

aterm__arg(V) := ~aterm__term(V) :- !.

aterm__term(V) := ~termvar.new_n(V) :- var(V), !.
aterm__term(X) := ~termstr.new_prim(X) :- use_opt(native_string), is_string(X), !.
aterm__term(X) := ~termstr.new_prim(X) :- number(X), !.
aterm__term(X) := R :- !,
	term_r(FunctorR, MaybeObj, Args) = ~cscope.resolve_term(X, unqual_user),
	prepare_maybe_obj(MaybeObj, MaybeObj2),
        aterm__terms(~add_maybe_obj(MaybeObj2, Args), As),
	R = ~termstr.new_psym(FunctorR, As).

aterm__terms([]) := [].
aterm__terms([X|Xs]) := [~aterm__term(X) | ~aterm__terms(Xs)].
}.

% Add the 'Obj' argument, if necessary
% It may require the inclusion of some goals (like in functional expansion)
add_maybe_obj(no, Args, Args).
add_maybe_obj(yes(Obj), Args0, Args) :-
	Args = [Obj|Args0].

{
:- fluid cscope :: scope.
:- fluid goals :: accum. % for expansion of 'self'
prepare_maybe_obj(yes(Obj0), yes(Obj)) :-
	% TODO: avoid functional notation in MaybeObj so that call to defunc_exp is not needed
	Obj = ~defunc_exp(Obj0).
prepare_maybe_obj(no, no).
}.

