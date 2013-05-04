:- module(_, [], [compiler(complang)]).

:- doc(title, "Fixpoint based analyzer").
:- doc(author, "Jose F. Morales").

% This module implements a very simple fixpoint based analyzer based
% on CiaoPP. Its purpuse is to help in integrating analysis and
% compilation phases, by reusing similar data structures to store
% programs.

% TODO: add a property to raise a compilation error/warning when no entry is found in memo table for a particular lambda during emit__preds
% TODO: encode 'nonfresh' info in the type? (as a subdivision of var type)

% TODO: control versions before emit predicates
% TODO: create a version of indexing that does not specialize each branch
% TODO: halts with loops: copy a naive fixpoint algorithm and implement widening in domains
% TODO: limit the info in Program passed to the domains, because memo and preds are not needed! only domain specific info like type definitions or some abstract definition of builtins (native info)

:- use_module(library(lists)).
:- use_module(library(dict)).
:- use_module(compiler(errlog)).
:- use_module(compiler(ptoc__tbl)).
:- use_module(compiler(module_exp)).
:- use_module(compiler(ptoc__ins)).
:- use_module(compiler(frontend), [error/1]). % TODO: a kludge?

:- include(.(absint__interface)).

:- include(.(aux_profile_ctx__disabled)).
%:- include(.(aux_profile_ctx)).

% ---------------------------------------------------------------------------

{
:- fluid exp :: module_exp.
:- public analyze/5.
% note: do not call any other analysis on the same predicates until
% this one finishes (the memodic of each predicate can only be used by
% one analysis at the same time)
analyze(AbsInt, Treat, Entries, Anot) := Preds3 :-
	s :: fixpo_s <- ~fixpo_s.new,
	intr :: absint <- AbsInt,
	treat :: any <- Treat,
	s.compute(Entries),
%	errlog:trace(['analysis of ', Module, ' using ', ~intr, ' found a fixpoint']),
	Preds3 = ~emit(Entries, Anot),
	% finish analysis: ensure that all predicates are registed and clean the memodic
	exp.preddic__register_preds(Preds3),
	pred_list__clean_memodic(Preds3).
%	errlog:trace(['analysis using ', ~intr, ' finished in ', ~s.iteration, ' steps']).
}.

% ---------------------------------------------------------------------------
% Fixpoint algorithm

:- class fixpo_s {
    :- attr iteration :: m_int.
    :- attr maybe_fixpoint :: m_any.

    :- constructor new_/0.
    new_ :-
        ~iteration = 0,
        ~maybe_fixpoint = true.

    next_iteration :-
        iteration.inc(1),
        maybe_fixpoint <- true.

    mark_no_fixpoint :-
	( error = ~maybe_fixpoint ->
	    true
	; maybe_fixpoint <- false
	).

    :- constant no_fixpoint/0.
    no_fixpoint :- ~maybe_fixpoint = false.
    :- constant had_errors/0.
    had_errors :- ~maybe_fixpoint = error.

    error(Message) :-
        % TODO: add program lines and a nicer error message, use errs?
        frontend:error(Message),
	maybe_fixpoint <- error.

    {
    :- fluid intr :: absint.
    :- fluid exp :: module_exp.
    :- fluid treat :: any.
    compute(Entries) :-
        ( compute__1(Entries) ->
	    true
	; errlog:bug(['fixpoint using ', ~intr, ' failed']),
	  fail
	),
	% TODO: it should not fail
	\+ had_errors.

    compute__1(Entries) :-
    	compute__2(Entries),
    	( no_fixpoint ->
    	    next_iteration,
    	    compute__1(Entries)
    	; true
    	).

    compute__2([]).
    compute__2([E|Es]) :-
	E = ent(PredId, Lambda0),
	s :: fixpo_s <- ~self,
	AbsCall = ~abspredcall.from_lambda(PredId, Lambda0),
	analyze__code(AbsCall),
        self <- ~s, % TODO: strange
	compute__2(Es).
    }.
}.

% ---------------------------------------------------------------------------
% Abstract predicate call closure

% This class represents an abstraction of the semantics of a predicate
% call (i.e., for a given abstract input), for a particular
% specialization key.

% TODO: it contains a method to obtain entry_d from lambda0...
%       Could it be done the other way? (obtain lambda0 from entry_d)
% TODO: Is it a predicate call, a predicate closure, or just a predicate? 
%       I would like to replace the absdomain with a concrete domain
%       and still have a working interpreter. But this abspredcall
%       does not have any argument once constructed. So it
%       seems to be a closure more than a predicate.
% TODO: Recode like 'predicate' using muts; memodic must link abspredcall
:- class abspredcall {
    :- '$statemodel'(single).
    :- '$raw_state'.

%    :- attr p :: predicate_x # "Concrete predicate".
%    :- attr key :: any # "Specialization key".
%    :- attr def :: absdef # "Abstract semantics of the predicate".

    % TODO: missing instance_of__/1

    {
    :- fluid s :: fixpo_s + u.
    :- constructor new_/3.
    new_(P, Key, Def) :-
	Status = pending,
	Iter = ~s.iteration,
	~self = ~'$mut__init'('$ap'(P, Key, Def, Status, Iter)).
    }.

    :- public p/1.
    :- meta_predicate p(out(predicate_x)).
    p := P :-
	'$ap'(P, _, _, _, _) = ~'$mut__value'(~self).

    :- public key/1.
    key := Key :-
	'$ap'(_, Key, _, _, _) = ~'$mut__value'(~self).

    :- public get_def/1.
    :- meta_predicate get_def(out(absdef)).
    get_def := Def :-
	'$ap'(_, _, Def, _, _) = ~'$mut__value'(~self).
    :- public set_def/1.
    set_def(Def) :-
	'$ap'(P, Key, _, Status, Iter) = ~'$mut__value'(~self),
	'$mut__assign'(~self, '$ap'(P, Key, Def, Status, Iter)).

    :- public get_status/1.
    get_status := Status :-
	'$ap'(_, _, _, Status, _) = ~'$mut__value'(~self).
    :- public set_status/1.
    set_status(Status) :-
	'$ap'(P, Key, Def, _, Iter) = ~'$mut__value'(~self),
	'$mut__assign'(~self, '$ap'(P, Key, Def, Status, Iter)).

    :- public get_iter/1.
    get_iter := Iter :-
	'$ap'(_, _, _, _, Iter) = ~'$mut__value'(~self).
    :- public set_iter/1.
    set_iter(Iter) :-
	'$ap'(P, Key, Def, Status, _) = ~'$mut__value'(~self),
	'$mut__assign'(~self, '$ap'(P, Key, Def, Status, Iter)).

    {
    :- fluid exp :: module_exp.
    :- fluid intr :: absint.
    :- fluid s :: fixpo_s + u.
    :- static meta_predicate from_lambda(?, ?, out(abspredcall)).
    from_lambda(P, Lambda) := AbsCall :-
        Key = ~get_abskey(P, Lambda),
	( AbsCall = ~query_abs(P, Key) ->
            AbsCall.expand_precond(Lambda)
	; % First time
	  OldAbsDef = ~intr.bottom_absdef(Lambda),
	  AbsCall = ~new(P, Key, OldAbsDef),
	  register_abs(P, Key, AbsCall)
	).

    % AbsCall existed, but 
    expand_precond(Lambda) :-
        OldAbsDef0 = ~get_def,
        % TODO: avoid iteration, keep call dependencies and abspredcall queue instead
        ( ~get_iter = ~s.iteration,
          trust(OldAbsDef0 instance_of absdef),
          OldAbsDef0.precond_consequence(Lambda) ->
            % Already seen, return computed (complete or incomplete) absdef
	    true
        ; % Reanalyze with a more general call pattern
          % TODO: collapsing all versions after fixpoint is more precise (but probably requires more memory)
          call((
            ad :: absdef <- OldAbsDef0,
            ad.precond_lub(Lambda),
	    set_def(~ad),
	    set_status(pending),
	    set_iter(~s.iteration)
	  ))
        ).
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid intr :: absint.
    :- static register_abs/3.
    register_abs(P, Key, AbsCall) :-
        trust(P instance_of predicate_x),
	MemoDic0 = ~P.memodic,
	P.set_memodic(~memodic_replace(MemoDic0, Key, AbsCall)).

    :- static meta_predicate query_abs(?, ?, out(abspredcall)).
    query_abs(P, Key) := AbsCall :-
        trust(P instance_of predicate_x),
	MemoDic = ~P.memodic,
	AbsCall = ~memodic_get(MemoDic, Key).
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid intr :: absint.
    :- fluid s :: fixpo_s.
    % Tell this abspredcall that a new def has been calculated, mark
    % in the 's' state that more analysis passes are required if
    % necessary.  TODO: to avoid 'iterations', include new elements in
    % the analysis queue here? (i.e. the predicates affected by this
    % one)
    update_def(AbsDef) :-
	trust(AbsDef instance_of absdef),
	OldAbsDef = ~get_def,
	( \+ OldAbsDef.unchanged(AbsDef) ->
	    s.mark_no_fixpoint
	; true
	),
	% TODO: def could be updated only when not unchanged... but it may be less optimal (it could extend the lifetime of some objects stored in the old absdef, although they are equivalent)
	set_def(AbsDef).
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid intr :: absint.
    :- constant meta_predicate materialize(out(predicate_x)).
    % Obtain the concrete predicate for this abspredcall
    materialize := PredId2 :-
	    PredId = ~p,
	    PredId2 = ( has_default_key ? PredId % reuse the predicate name (for the default entry)
		      | ~PredId.clone([])
		      ),
	    trust(PredId2 instance_of predicate_x),
	    AbsDef = ~get_def,
	    intr.set_usermemo(PredId2, AbsDef).

    :- constant has_default_key/0.
    has_default_key :-
        P = ~p,
        AbsKey2 = ~get_abskey(P, ~P.get_prop(default_entry)),
	abskey_equal(~key, AbsKey2).
    }.
}.

{
:- fluid exp :: module_exp.
% TODO: ciaopp does widening during the lub but I could widen lambda here too (it is a different thing)
get_abskey(PredId, Lambda) := AbsKey :-
	trust(PredId instance_of predicate_x),
	AbsKey = ( true = ~PredId.get_prop(genvers) ?
		     ~abskey_from_lambda(Lambda)
		 | null
		 ).

:- '$ctxprj'(abskey_from_lambda/2, []).
% TODO: optimize?
abskey_from_lambda(Lambda) := Lambda.
}.

{
:- fluid exp :: module_exp.
:- fluid intr :: absint.

memodic_get([AbsKey-Value0|_], AbsKey0) := Value :- abskey_equal(AbsKey, AbsKey0), !,
	Value = Value0.
memodic_get([_|Xs], AbsKey0) := Value :-
	Value = ~memodic_get(Xs, AbsKey0).

memodic_replace([], AbsKey0, Value) := [AbsKey0-Value].
memodic_replace([X|Xs], AbsKey0, Value) := Ys :-
	( X = AbsKey-_, abskey_equal(AbsKey, AbsKey0) ->
	    Ys = [AbsKey-Value|Xs]
	; Ys = [X|Ys0],
	  Ys0 = ~memodic_replace(Xs, AbsKey0, Value)
	).
}.

{
:- fluid exp :: module_exp.
:- fluid intr :: absint.
abskey_equal(null, null) :- !.
abskey_equal(AbsKey1, AbsKey2) :-
	% TODO: optimize?
	intr.lambda_equal(AbsKey1, AbsKey2).
}.

% ---------------------------------------------------------------------------

:- use_module(library(aggregates)).
:- use_module(.(ptoc__props)).

{
:- fluid intr :: absint.
:- fluid exp :: module_exp.
:- public get_entries/2.
% TODO: may not use exp...
get_entries([], []).
get_entries([PredId|Xs], Es) :-
	trust(PredId instance_of predicate_x),
	Name = ~PredId.name,
	findall(Lambda, user_entry(Name, Lambda), UEs),
	( UEs = [_,_|_] -> % more than one element
	    errlog:bug(['multiple user entries not yet supported, predicate ', Name, ', entries ', UEs])
	; UEs = [Lambda] ->
	    PredId.set_prop(default_entry, Lambda),
	    Es = [ent(PredId, Lambda)|Es0]
	; Lambda = ~default_entry(PredId) ->
	    PredId.set_prop(default_entry, Lambda),
	    Es = [ent(PredId, Lambda)|Es0]
	; Es = Es0
	),
	get_entries(Xs, Es0).

% TODO: only uses default entries for nonptoc predicates (incorrect but necessary to preserve benchmark results)... fix
% the predicate do not require a default entry in analysis (trust that analyzer will find all call patterns) % TODO: temporal!?
default_entry(PredId) := Lambda :-
	trust(PredId instance_of predicate_x),
	Def = ~PredId.get_prop(def),
	( Def = ptoc_macro -> fail % do not add entry for this predicate
	; Def = ptoc_builtin -> fail % do not add entry for this predicate
	; true
	),
	%
	( requires_analysis(PredId) ->
	    ( called_from_anywhere(PredId) ->
	        true
	    ; fail
	    )
	; true % TODO: fix this kludge: we are not analyzing everthing: define a default entry for every predicate so that no predicate is lost
	),
	Name = ~PredId.name,
	Name = MF/A,
	\+ exp.trust_entry(MF, A, ~intr, _),
	Lambda = ~intr.deflambda(PredId).

user_entry(Name, Lambda) :-
	Name = MF/A,
	exp.trust_entry(MF, A, ~intr, Lambda).

% TODO: analyze everything! - but make sure that multifile and dynamic predicates are correctly treated
% TODO: (urgent) change that so that we have two
% meanings: one is 'requires_analysis' and the other 'can_be_called_from_any_place'
% The first is used to do skip or not. The second is used to define the default entries!!
% And this last one should also check for 'uses_hiord' and 'uses_hiord_pred'.

called_from_anywhere(PredId) :-
	trust(PredId instance_of predicate_x),
	( exp.uses_hiord ->
	    true
	; Name = ~PredId.name,
	  Name = N/A,
	  exp.uses_hiord_pred(N, A) ->
	    true
	; vs_public = ~PredId.get_prop(visibility) ->
	    true
	; vs_multifile = ~PredId.get_prop(visibility) ->
	    true
	; fail
	).

requires_analysis(PredId) :-
	trust(PredId instance_of predicate_x),
	( true = ~PredId.get_prop(req_sht) ->
	    true
	; true = ~PredId.get_prop(impdyn) ->
	    fail % dynamic predicate cannot be analyzed
	; exp.pragma(analyze_all) ->
	    true
	; exp.pragma(analyze_idet), ~intr = idet_analyzer ->
	    true
	),
	Code0 = ~PredId.code,
	Code0 = icode(a, _, _).
}.

% ---------------------------------------------------------------------------

% TODO: may not end or give bad results with recursive preds!
% TODO: document? Lambda0 is the input lambda, Lambda the suggested lambda (may change with collapsing and widening)
%       (needed widening in domains + fixpoint -> mark memo entries as complete, incomplete, etc.)
{
:- fluid intr :: absint.
:- fluid exp :: module_exp.
:- fluid treat :: any.
:- fluid s :: fixpo_s.

% TODO: do not anot the pred, store the lambdas, at least during analysis
% TODO: treatment of non ptoc predicates is not correct!!!, but something has to be done to reduce the complexity... for those predicates just do this thing: follow the code and add default entries for everything that is found; also, handle hiord calls correctly

% TODO: Unify as generic code with forward_analyze in ptoc__lowcomp!

% TODO: reuse to implement transformations?
analyze__code(AbsCall) :-
	trust(AbsCall instance_of abspredcall),
	OldVersion = ~AbsCall.get_status,
	\+ OldVersion = pending,
	!. % Do not update ~AbsCall.def
analyze__code(AbsCall) :-
	trust(AbsCall instance_of abspredcall),
	PredId = ~AbsCall.p,
	requires_analysis(PredId),
	!,
	% TODO: Move this preprocessing to other phase?
	( ~treat = yes(AbsIntTreat) ->
	    % TODO: intr.index may create new predicates for each fixpoint iteration!!! that is wrong!!!
	    call((
              intr :: absint <- AbsIntTreat,
	      intr.index(~AbsCall.p)
            ))
	; true
	),
	%
	AbsCall.set_status(working),
	Code0 = ~PredId.code,
	Code0 = icode(a, Args, Code),
	mode :: any <- all,
	args :: any <- Args,
	entry_d :: abs_d <- ~intr.entry_dom(PredId, Args, ~AbsCall.get_def),
	exit_d :: abs_d <- ~intr.bottom,
	( Code = index(_, _) ->
	    analyze__index(Code, Code2)
	; Code = or(Cs) ->
	    analyze__alts(Cs, Cs2),
	    Code2 = or(Cs2)
	),
	% update memo table
	% TODO: Keep a call stack to short-circuit analysis of less general (non-specializing) call patterns
	OldVersion = ~AbsCall.get_status,
	( OldVersion = working ->
	    % TODO: improve 'delta of domains' (for all d.get_absdef...)
	    % TODO: use something more abstract than 'Args' (like 'abstarget'?)
	    AbsDef = ~exit_d.get_absdef(PredId, Args, ~entry_d),
	    AbsCall.update_def(AbsDef),
	    % TODO: status is updated to remember $anot, can this be avoided (e.g. if anot is not active?) ?
	    AbsCall.set_status(anotpred(icode(a, Args, Code2)))
	; % do nothing, a more general entry has been written
	  true
	).
analyze__code(AbsCall) :-
	% Analysis is not required for this predicate
	% TODO: at least, traverse code!! (dead code transformation must not lose predicates)
	trust(AbsCall instance_of abspredcall),
	OldAbsDef = ~AbsCall.get_def,
	AbsCall.set_def(~OldAbsDef.unknown_post),
	AbsCall.set_status(skippred).

{
:- fluid entry_d :: abs_d + u.
:- fluid exit_d :: abs_d.
:- fluid args :: any.

analyze__index(G0, G) :-
	G0 = index(Cases0, DefGoal0),
	G = index(Cases, DefGoal),
	mode :: any <- all,
	call(( cases :: accum(Cases), analyze__cases(Cases0) )),
	analyze__alts([DefGoal0], [DefGoal]).

{
% exit_d: accumulates changes on an initial 'd'
% The use of an accumulator allows recursive predicates with tail call optimization.
:- fluid cases :: accum.
analyze__cases([]).
analyze__cases([C|Xs0]) :-
	mode :: any <- all,
	d :: abs_d <- ~entry_d,
	%
	C = case(KeyType, X0),
	~args = [IndexArg|_], % TODO: put this in the index(...) structure to support indexing in other args... or replace keytype by builins in the index generation code...
	d.trust_type(IndexArg, KeyType),
	( d.is_bottom ->
	    true % remove from the index, since this case will always fail
	; entry_to_exit(X0, X),
	  cases.add(case(KeyType, X))
	),
	%
	exit_d.proceed(~args, ~d),
	analyze__cases(Xs0).
}.

{
:- fluid mode :: any.
analyze__alts([], []).
analyze__alts([C|Cs], [C2|Cs2]) :-
	d :: abs_d <- ~entry_d,
	( Cs = [] -> true
	; d.push_choice
	),
	entry_to_exit(C, C2),
	exit_d.proceed(~args, ~d),
        analyze__alts(Cs, Cs2).
}.

}.
}.

% ---------------------------------------------------------------------------

% TODO: this predicate creates a conjuction of the lambda abstract
% substitions; a bottom lambda means that the tail of the list can be
% replaced with a fail without altering the semantics

{
:- fluid intr :: absint.
:- fluid mode :: any.
:- fluid exp :: module_exp.
:- fluid treat :: any.
:- fluid s :: fixpo_s.
:- fluid d :: abs_d.
entry_to_exit(Xs, Xs3) :-
	call(( code :: accum(Xs2), entry_to_exit__2(Xs) )),
	( d.is_bottom ->
	    % TODO: it works in forward analysis, but not in backward
	    % analysis (example: if the abstract domain stores
	    % computation properties like if choice points are needed or
	    % not, the analysis has to be recomputed after removing
	    % nosideeff)
	    Xs3 = ~conj__remove_nosideeff(Xs2)
	; Xs3 = Xs2
	).

% Analyze a list of goals (conjunction)
{
:- fluid code :: accum.
entry_to_exit__2(_) :-
	% stop if state after latest goal was failure
	d.is_bottom,
	!.
entry_to_exit__2([]) :- !.
entry_to_exit__2([X|Xs]) :-
        % ignore trust marks that belong to other domains
	AbsInt2 = ~trust_domain(X),
	\+ ~intr = AbsInt2,
	!,
	code.add(X),
	entry_to_exit__2(Xs).
entry_to_exit__2(['$anot'(X,_)|Xs]) :- !,
	entry_to_exit__2([X|Xs]).
entry_to_exit__2([X|_]) :-
	intr.stop(~mode, X),
	!. % side effects! stop guard analysis here
entry_to_exit__2([X|Xs]) :- !,
	trust(X instance_of strgoal),
	PredId = ~X.predid,
	( Code = ~PredId.code,
	  \+ Code = no_code,
	  \+ Code = imacro ->
	    Lambda0 = ~d.get_entry(X),
	    AbsCall = ~abspredcall.from_lambda(PredId, Lambda0),
	    analyze__code(AbsCall),
	    Args = ~X.args,
	    try_goalpost(Args, ~AbsCall.get_def),
	    X2 = '$anot'(X, Lambda0),
	    code.add(X2)
	; entry_to_exit__external(X)
	),
	entry_to_exit__2(Xs).

% TODO: do not fill the memo table
:- '$ctxprj'(entry_to_exit__external/1, [intr, exp, s, d, code]).
entry_to_exit__external(X) :-
	d.is_builtin(X),
	!,
	Lambda0 = ~d.get_entry(X),
	X1 = '$anot'(X, Lambda0),
%	errlog:trace([b0(X, ~d)]),
	code.add(X1),
	d.builtin(X).
%	errlog:trace([b1(X, ~d)]).
entry_to_exit__external(X) :-
	trust(X instance_of strgoal),
	GId = ~X.predid,
	Lambda0 = ~d.get_entry(X),
	Args = ~X.args,
	% TODO: Try to obtain a abspredcall before
	intr.spec(Lambda0, GId, Args, AbsDef, VersionId, Args2),
	trust(VersionId instance_of predicate_x),
	% TODO: this is not very clean...
	( ~VersionId.name = 'basiccontrol:true'/0 ->
	    true
	; % TODO: merge this test with intr.spec!!
	  trust(AbsDef instance_of absdef),
          ( AbsDef.precond_consequence(Lambda0) ->
	      true
	  ; s.error(invalid_call(~GId.name, AbsDef, Lambda0))
	  ),
	  X2b = ~strgoal.new_n(VersionId, Args2),
	  Lambda2 = ~d.get_entry(X2b),
	  try_goalpost(Args2, AbsDef),
	  X2 = '$anot'(X2b, Lambda2),
	  code.add(X2)
	).
}.

% Remove nosideeff goals when the last goal in the list ends with failure state
% TODO: this is not correct if we use attributed variables, since the unification may have sideeffs
:- '$ctxprj'(conj__remove_nosideeff/2, [intr, exp, u(d)]).
conj__remove_nosideeff(Xs) := ~reverse(~conj__remove_nosideeff__2(~reverse(Xs))).

% (handle with reversed goal list)
:- '$ctxprj'(conj__remove_nosideeff__2/2, [intr, exp, u(d)]).
conj__remove_nosideeff__2(Xs) := Xs :-
        % last goal (the one that fails) and has sideeff, leave untouched
	Xs = [X0|_],
	( X0 = '$anot'(X,_) -> true ; X0 = X ),
	\+ true = ~goal_prop(nosideeff, X), !.
conj__remove_nosideeff__2(Xs) := Xs2 :-
	% replace last nosideeff goals by a call to fail/0
	Xs1 = ~conj__remove_nosideeff__3(Xs),
	X1 = ~fail_new,
	Lambda0 = ~d.get_entry(X1),
	Xs2 = ['$anot'(X1, Lambda0)|Xs1].

% remove nosideeff goals
:- '$ctxprj'(conj__remove_nosideeff__3/2, [exp]).
conj__remove_nosideeff__3([X0|Xs]) := ~conj__remove_nosideeff__3(Xs) :-
	( X0 = '$anot'(X,_) -> true ; X0 = X ),
	true = ~goal_prop(nosideeff, X), !.
conj__remove_nosideeff__3(Xs) := Xs.
}.

{
:- fluid d :: abs_d.
:- fluid exp :: module_exp.
try_goalpost(Args, AbsDef) :-
        trust(AbsDef instance_of absdef),
        ( AbsDef.is_bottom ->
            d.make_bottom
        ; d.is_bottom ->
	    true
	; d.goalpost(Args, AbsDef)
        ).
}.

% ---------------------------------------------------------------------------
% Analyze the clause and extract its keytype (used for indexing)

% TODO: this documentation is outdated

% TODO: the key of "p(X) :- var(X), X=5" is var, but currently X is inferred.  
%       The problem is that instead of selecting the input type of X, the output
%       type is used (?). 
%       Alternative method:
%          p(X, ...) :- l1, l2, ... , !, ...
%       
%       without 'var' type: Obtain the deepest instantiation of X without executing predicates
%                           with side effects -> current, and incorrect.
%       with 'var' type: ?????
%
% The guard is made with all the literals until a side effect (not included).
% P has side effects if (P, fail) produces the same changes in state that (fail).
% The cut is not inside the guard. Counterexample:
%   p(X) :- !, X = 5. p(6). 
% With the cut in the guard, the code is:
%   if (x==5) 
%     C1
%   else if (x==6)
%     C2
%   else (C1;C2)
% (SUCCESS WITH 6)
%
% Without the cut in the guard, the code is:
%   if (x==6) 
%     (C1;C2)
%   else (C1;C2)
% (DOES NOT SUCCESS WITH 6 (correct))


% (I have also put this text commented in the indexing code module)
% 
% The index guard cannot contain the cut.
% Let p/1 be:
% 
% C1: p(X) :- !, X=a.
% C2: p(X) :- X=b.
% 
% WITH CUT IN GUARD 
% 
% Step 1 (select guard)
% 
% C1: !, X=a
% C2: X=b
% 
% Step 2 (calculate guard input type)
% 
% C1: type(X, (atom(a);var))
% C2: type(X, (atom(b);var))
% 
% Step 3 (generate indexing code)
% 
% if type(X, var)
%   C1 ; C2
% else if type(X, atom(a))
%   C1
% else if type(X, atom(b))
%   C2
% else 
%   fail
% 
% For p(b) the predicate success, which is incorrect.
% 
% WITHOUT CUT IN GUARD 
% 
% Step 1 (select guard)
% 
% C1: true
% C2: X=b
% 
% Step 2 (calculate guard input type)
% 
% C1: type(X, any)
% C2: type(X, (atom(b);var))
% 
% Step 3 (generate indexing code)
% 
% if type(X, var)
%   C1 ; C2
% else if type(X, atom(b))
%   C1 ; C2
% else 
%   C1
%    
% For p(b) the predicate executes C1, forgets about the C2 clause,
% unifies X with 'a' and fails, which is the correct behaviour.

{
:- fluid intr :: absint.
:- fluid exp :: module_exp.
:- public analyze_guard/4.
% TODO: alternative: put special indexing fake instructions to obtain the program-point information
% It obtains the @emph{Key} of the clause Xs0
% Precondition: the predicate arity is greater than 0
analyze_guard(PredId, OldAbsDef, Xs) := Key :-
	trust(PredId instance_of predicate_x),
	Code = ~PredId.code,
	Code = icode(a, Args, _),
	call((
	  treat :: any <- no,
	  mode :: any <- guard,
	  args :: any <- Args,
          s :: fixpo_s <- ~fixpo_s.new,
	  entry_d :: abs_d <- ~intr.entry_dom(PredId, ~args, OldAbsDef),
	  exit_d :: abs_d <- ~intr.bottom,
	  %
	  analyze__alts([Xs], _),
	  %
	  intr.get_key(~entry_d, ~exit_d, Args, Key)
	)).
}.

/* DEACTIVATED
mark_always_cuts(C) := C @+ (clcode = Body) @+ (always_cuts = AlwaysCuts) :-
	Body0 = C@.clcode,
	( always_cuts(Body0) ->
	    AlwaysCuts = yes,
	    Body = ~remove_unnecessary_cut(Body0)
	; AlwaysCuts = no,
	  Body = Body0
	).

always_cuts([X|Xs]) :- X@.i_cannot_fail = true, !, always_cuts(Xs).
always_cuts([X|_]) :-
	trust(X instance_of strgoal),
	~X.name = 'basiccontrol:$cut'/1,
	!.

remove_unnecessary_cut([X|Xs]) := [X|~remove_unnecessary_cut(Xs, Eq)] :- X@.i_cannot_fail = true, !.
remove_unnecessary_cut([X|Xs]) := Xs :-
	trust(X instance_of strgoal),
	~X.name = 'basiccontrol:$cut'/1,
	~X.args = [A],
	X@.eqSetEq(Eq, ~eqId(A), argument(-1)), !.
remove_unnecessary_cut(Xs) := Xs.
*/

% ---------------------------------------------------------------------------
% Emit predicates
% TODO: collapse versions (how? here or in analysis? how is program point info build again?)

{
:- fluid intr :: absint.
:- fluid exp :: module_exp.
:- fluid s :: fixpo_s.
:- '$ctxprj'(emit/3, [intr, exp, u(s)]).
emit(Es, Anot) := Preds :-
	anot :: any <- Anot,
	preds :: accum <- Preds,
	emit__2(Es),
	~preds = [].
{
:- fluid anot :: any.
:- fluid preds :: accum.
:- '$ctxprj'(emit__2/1, [intr, exp, u(s), anot, preds]).
emit__2([]).
emit__2([E|Es]) :-
	E = ent(PredId, Lambda0),
	AbsCall = ~abspredcall.from_lambda(PredId, Lambda0),
	( emit__pred(AbsCall, _Res) ->
	    true
	; errlog:bug(['emit using ', ~intr, ' for entry ', E, ' failed']),
	  fail
	),
	% continue with the rest of entries
	emit__2(Es).

% TODO: this rewrites the memo table to remember what preds were emited... good or bad idea?
:- '$ctxprj'(emit__pred/2, [intr, exp, u(s), anot, preds]).
emit__pred(AbsCall, Res) :-
	trust(AbsCall instance_of abspredcall),
	Version = ~AbsCall.get_status,
	( Version = anotpred(icode(a, VersionArgs, Body0)) ->
	    PredId = ~AbsCall.materialize,
	    AbsCall.set_status(ver(PredId)),
	    PredId.set_code(icode(a, VersionArgs, Body)),
	    preds.add(PredId),
	    % put domain properties in the version (and emit other predicates)
	    emit__code(Body0, Body),
            % TODO: props must be copied when creating a version
	    Res = ver(PredId)
	; Version = skippred ->
	    % TODO: this case is temporal and INCORRECT! (merge with the previous one, but use a simpler analysis that supposes top for everything and do not annotates anything... use cheap encoding where nothing means anything
	    PredId = ~AbsCall.p,
	    AbsCall.set_status(ver(PredId)),
	    preds.add(PredId),
	    Res = ver(PredId)
	; % do nothing, a version for that pred was emited
	  Version = ver(PredId2) ->
	    Res = ver(PredId2)
	; Version = pending -> % not found in the memo
	    % TODO: This is strange...
	    Res = fail
	).

:- '$ctxprj'(emit__code/2, [intr, exp, u(s), anot, preds]).
emit__code(Code, Code2) :-
	Code = index(_, _), !,
	emit__index(Code, Code2).
emit__code(or(Cs), or(Cs2)) :-
	emit__alts(Cs, Cs2).

:- '$ctxprj'(emit__index/2, [intr, exp, u(s), anot, preds]).
emit__index(G0, G) :-
	G0 = index(Cases0, DefGoal0),
	emit__cases(Cases0, Cases),
	emit__conj(DefGoal0, DefGoal),
	G = index(Cases, DefGoal).

:- '$ctxprj'(emit__cases/2, [intr, exp, u(s), anot, preds]).
emit__cases([], []).
emit__cases([case(KeyType, X0)|Xs0], [case(KeyType, X)|Xs]) :-
	emit__conj(X0, X),
	emit__cases(Xs0, Xs).

% Analyze alternatives
:- '$ctxprj'(emit__alts/2, [intr, exp, u(s), anot, preds]).
emit__alts([], []).
emit__alts([C|Cs], [C2|Cs2]) :-
	emit__conj(C, C2),
	emit__alts(Cs, Cs2).

% emit code for the body
:- '$ctxprj'(emit__conj/2, [intr, exp, u(s), anot, preds]).
emit__conj([], []) :- !.
emit__conj([X|Xs], [X|Xs2]) :-
        % do nothing with trust marks
	_ = ~trust_domain(X),
	!,
	emit__conj(Xs, Xs2).
emit__conj(['$anot'(X1,Lambda0)|Xs], [X3|Xs2]) :-
	trust(X1 instance_of strgoal),
	PredId = ~X1.predid,
	( Code = ~PredId.code,
	  \+ Code = no_code,
	  \+ Code = imacro ->
	    AbsCall = ~abspredcall.from_lambda(PredId, Lambda0),
	    emit__pred(AbsCall, Res),
	    X2 = ( Res = ver(PredId2) ? ~X1.set_predid(PredId2)
		 | Res = fail ? ~fail_new
		 )
	; X2 = X1
	),
	X3 = ( ~anot = yes ? ~intr.anot_lambda(X2, Lambda0)
	     | X2
	     ),
	emit__conj(Xs, Xs2).
}.
}.

% ---------------------------------------------------------------------------
% Dump analysis results

{
:- fluid intr :: absint.
:- fluid exp :: module_exp.
:- public dump/2.
dump(OutputStream, Preds) :-
	current_output(OldOutput),
	set_output(OutputStream),
	Ok = ( dump__1(Preds) ? yes | no ),
	set_output(OldOutput),
	Ok = yes.

dump__1(Preds) :-
	intr.get_dumper(Dumper),
	dumper :: abs_dumper <- Dumper,
	display('Analysis info for domain '), display(~intr), nl,
	dump__2(Preds).
{
:- fluid dumper :: abs_dumper.
dump__2([]).
dump__2([PredId|Xs]) :-
	trust(PredId instance_of predicate_x),
	display('Predicate: '),	display(~PredId.name), nl,
	dumper.pred(PredId),
	dump__2(Xs).
}.
}.


