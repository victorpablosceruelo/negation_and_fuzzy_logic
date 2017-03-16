:- module(_, [], [compiler(complang)]).

% Domain for determinism analysis (from the point of view of the
% choice-point stack)
%
% Author: Jose F. Morales

:- use_module(library(lists)).
:- use_module(library(dict)).

:- use_module(compiler(errlog)).
:- use_module(.(module_exp)).
:- use_module(.(ptoc__props)).
:- use_module(.(ptoc__ins)).

:- use_module(.(sht_analyzer)). % TODO: it uses info from sht analysis

% ---------------------------------------------------------------------------

:- include(.(absint__interface)).
:- extends absint.

% ---------------------------------------------------------------------------

% This abstract domain represents the state of the choice-point stack
% w.r.t. the initial state on predicate call, and how the choice-point
% stack may be accessed. 
%
% The domain is a lattice obtained from the product of (n,u,c) so
% that:
%
%   n: the top choice point is the default choice point (that points to the next clause)
%   u: on exit some choice points may have been removed
%   c: on exit some choice points may have been inserted
%
% The domain of each component is the following lattice:
%
%            00 inconsistent
%           /  \ 
%  always 10    01 never
%           \  /
%            11 sometimes
%
% That is both a join-semilattice (using bitwise-or) and
% meet-semilattice (using bitwise-and).

:- class idet_d {
    :- extends abs_d.
    :- attr bits :: m_any.

    % transitions
    % TODO: simplify, generate automatically (define single bit fields)
    n__true :- n__enable_true, n__disable_false.
    n__false :- n__disable_true, n__enable_false.
    n__unknown :- n__enable_true, n__enable_false.
    n__can_be_true   :- 0 =\= ~bits /\ 2'100000.
    n__enable_true   :- N is  ~bits \/ 2'100000, bits <- N.
    n__disable_true  :- N is  ~bits /\ 2'011111, bits <- N.
    n__can_be_false  :- 0 =\= ~bits /\ 2'010000.
    n__enable_false  :- N is  ~bits \/ 2'010000, bits <- N.
    n__disable_false :- N is  ~bits /\ 2'101111, bits <- N.
    n__is_true :- n__can_be_true, \+ n__can_be_false.

    u__true :- u__enable_true, u__disable_false.
    u__false :- u__disable_true, u__enable_false.
    u__unknown :- u__enable_true, u__enable_false.
    u__can_be_true   :- 0 =\= ~bits /\ 2'001000.
    u__enable_true   :- N is  ~bits \/ 2'001000, bits <- N.
    u__disable_true  :- N is  ~bits /\ 2'110111, bits <- N.
    u__can_be_false  :- 0 =\= ~bits /\ 2'000100.
    u__enable_false  :- N is  ~bits \/ 2'000100, bits <- N.
    u__disable_false :- N is  ~bits /\ 2'111011, bits <- N.

    c__true :- c__enable_true, c__disable_false.
    c__false :- c__disable_true, c__enable_false.
    c__unknown :- c__enable_true, c__enable_false.
    c__can_be_true   :- 0 =\= ~bits /\ 2'000010.
    c__enable_true   :- N is  ~bits \/ 2'000010, bits <- N.
    c__disable_true  :- N is  ~bits /\ 2'111101, bits <- N.
    c__can_be_false  :- 0 =\= ~bits /\ 2'000001.
    c__enable_false  :- N is  ~bits \/ 2'000001, bits <- N.
    c__disable_false :- N is  ~bits /\ 2'111110, bits <- N.

    :- constructor bottom_/0.
    bottom_ :-
	~bits = 0. % bottom state

    make_bottom :- bits <- 0. % bottom state

    :- constant is_bottom/0.
    is_bottom :- ~bits = 0. % bottom state

    % Project domain info into the goal
    % TODO: In this domain, precond is always the same
    :- constant get_entry/1.
    get_entry(_G) := '$lambda_det'.

    push_choice :-
            ( c__can_be_true ->
                % it is possible that there exist another choice point at this point
                errlog:bug(['d_push_choice: there cannot exist a choice point here']), fail
            ; c__unknown, % TODO: why not c__true?
              n__true
            ).

    {
    :- fluid exp :: module_exp.
    goalpost(_Args, AbsDef) :-
            trust(AbsDef instance_of idetdef),
	    LambdaPrime = ~AbsDef.postcond,
	    apply_lambda(LambdaPrime).
    }.

    may_fail :-
            u__unknown.

    neck_cut :-
            ( n__is_true -> % TODO: check if: n__can_be_true? \+ n__can_be_false?
                % The top choice point is the default choice point (points to the next clause)
                % there exist another choice point at this point
                % set 'u' to false (if we reach this point, we have not failed)
                u__false,
                n__false,
                c__false
            ; true
              % TODO: it should do the following:
              % n__false,
              % c__false
              % TODO: ... but it does not, because we keep 'nondet' compilation for nondet preds (it is necessary for ptoc, but it shouldn't fix!!)
              % TODO: ... that is why "p :- (a;b),!." should be semidet but it is annotated as nondet
            ).

    may_add_choice_or_fail :-
            n__false,
            c__unknown,
            u__unknown.

    apply_lambda(Lambda) :-
            ( Lambda = '$lambda_det' -> true
            ; Lambda = '$lambda_semidet' -> may_fail
            ; Lambda = '$lambda_nondet' -> may_add_choice_or_fail
            ).

    % TODO: it could be refined... make implementation det for ptoc and real det different things
    to_lambda := ( c__can_be_true ? '$lambda_nondet' % can create choice points
		 % | u__is_true -> '$lambda_failure' % use choice points (fail)
		 | u__can_be_true ? '$lambda_semidet' % can use choice points (fail)
		 | '$lambda_det' % does not use or create choice points (it is deterministic)
		 ).

    % Obtain absdef
    {
    :- fluid exp :: module_exp.
    :- constant get_absdef/4.
    get_absdef(PredId, _Args, _D0) := AbsDef :-
            trust(PredId instance_of predicate_x),
            ( vs_multifile = ~PredId.get_prop(visibility) ->
                AbsDef = ~idetdef.new_nondet
            ; def__bytecode(~PredId.get_prop(def)) ->
                % TODO: distinguish 'effective_imp' from 'imp'!
                AbsDef = ~idetdef.new_nondet
            ; is_bottom ->
                AbsDef = ~idetdef.new_bottom
            ; to_lambda(LambdaPrime),
              AbsDef = ~idetdef.new(LambdaPrime)
            ).
    }.

    {
    :- fluid exp :: module_exp.
    :- constant is_builtin/1.
    is_builtin(G) :- is_unify(G), !.
    is_builtin(G) :- is_instance(G), !.
    is_builtin(G) :-
        trust(G instance_of strgoal),
	NA = ~G.name,
	( NA = 'term_basic:$trust_type'/2 -> true
	; NA = 'basiccontrol:fail'/0 -> true
	; NA = 'basiccontrol:$caller_choice'/1 -> true
	; NA = 'basiccontrol:$cut'/1
	).
    }.

    % TODO: do not generate different builtins for unify and instance, only unify
    % TODO: this is call_to_success_builtin
    {
    :- fluid exp :: module_exp.
    builtin(G) :- is_unify(G), !,
            trust(G instance_of strgoal),
            ~G.args = [A, B],
	    trust(A instance_of termvar),
	    trust(B instance_of termvar),
            ( typemode_is_var(~A.getp(type)) -> true
            ; typemode_is_var(~B.getp(type)) -> true
            ; % TODO: improve!! check that the type of A can unify with type of B
              may_fail
            ).
    builtin(G) :- is_instance(G), !,
            trust(G instance_of strgoal),
            ~G.args = [A, _B],
	    trust(A instance_of termvar),
            ( typemode_is_var(~A.getp(type)) -> true
            ; % TODO: improve!! check that the type of A can unify with type of B
              may_fail
            ).
    builtin(G) :-
            % TODO: replace trust_type by a trust goal, that abstractly executes the code, but is ignored in compilation!!
            trust(G instance_of strgoal),
            ~G.name = 'term_basic:$trust_type'/2,
            !.
    builtin(G) :-
            trust(G instance_of strgoal),
	    ~G.name = 'basiccontrol:$caller_choice'/1,
            !.
    builtin(G) :-
            trust(G instance_of strgoal),
	    ~G.name = 'basiccontrol:$cut'/1,
            !,
            neck_cut.
    builtin(G) :-
            trust(G instance_of strgoal),
            ~G.name = 'basiccontrol:fail'/0,
            !,
    % TODO: disabled because internal backtracking is not supported in non multidet predicates (at this moment)
    %        bottom.
            may_fail.
    }.

    :- constructor entry_dom_/3.
    entry_dom_(_PredId, _Args, _AbsDef) :-
	    ~bits = 2'010101. % n:{false}, u:{false}, c:{false}
    % TODO: Useful?
    %   nondet: idet_d(2'011111). % n:{false}, u:{true,false}, c:{true,false}

    % TODO: add d_fail? that may be useful to analyze halting predicates (e.g. rec :- rec.)

    proceed(_Args, Db) :-
            trust(Db instance_of idet_d),
            Nc is ~bits \/ ~Db.bits,
            bits <- Nc.

    trust_type(_X, _Type).
}.

{
:- fluid exp :: module_exp.

deflambda(_) := '$lambda_det'.

:- '$ctxprj'(labmdatop/1, []).
lambdatop(_) := Lambda :- Lambda = '$lambda_nondet'.

:- '$ctxprj'(lambdabottom/1, []).
lambdabottom('$bottom').

:- '$ctxprj'(bottom/1, []).
bottom := ~idet_d.bottom.

anot_lambda(G, _Lambda) := G.

:- '$ctxprj'(entry_dom/4, []).
entry_dom(PredId, Args, AbsDef) := ~idet_d.entry_dom(PredId, Args, AbsDef).

set_usermemo(PredId, AbsDef) :-
	trust(PredId instance_of predicate_x),
        PredId.set_prop(idet_usermemo, AbsDef).

get_usermemo(PredId) := AbsDef :-
	trust(PredId instance_of predicate_x),
        ( AbsDef0 = ~PredId.get_prop(idet_usermemo) ->
            AbsDef = AbsDef0
        ; Impdet = ~PredId.get_prop(imp) ->
            LambdaPrime0 = ( Impdet = det ? '$lambda_det'
			   | Impdet = detcut ? '$lambda_det'
			   | Impdet = semidet ? '$lambda_semidet'
			   | Impdet = semidet_re ? '$lambda_semidet'
			   | '$lambda_nondet'
			   ),
            AbsDef = ~idetdef.new(LambdaPrime0)
        ; AbsDef = ~idetdef.new_nondet
        ).

% Specialize (select other predicate based on lambda)
spec(_Lambda0, PredId0, Xs0, AbsDef, PredId, Xs) :-
        PredId = PredId0,
        Xs = Xs0,
        AbsDef = ~get_usermemo(PredId).

lambda_equal(A, B) :-
        A = B.

:- '$ctxprj'(lambda_lub/3, []).
lambda_lub(LambdaA, LambdaB) :=
        ( LambdaA = LambdaB ? LambdaA
        %
        | LambdaA = '$bottom' ? LambdaB
        | LambdaB = '$bottom' ? LambdaA
        %
        | LambdaA = '$lambda_nondet' ? '$lambda_nondet'
        | LambdaB = '$lambda_nondet' ? '$lambda_nondet'
        %
        | LambdaA = '$lambda_multidet', LambdaB = '$lambda_semidet' ? '$lambda_nondet'
        | LambdaA = '$lambda_semidet', LambdaB = '$lambda_multidet' ? '$lambda_nondet'
        %
        | LambdaA = '$lambda_multidet', LambdaB = '$lambda_det' ? '$lambda_multidet'
        | LambdaA = '$lambda_det', LambdaB = '$lambda_multidet' ? '$lambda_multidet'
        %
        | LambdaA = '$lambda_semidet', LambdaB = '$lambda_det' ? '$lambda_semidet'
        | LambdaA = '$lambda_det', LambdaB = '$lambda_semidet' ? '$lambda_semidet'
        %
        | LambdaA = '$lambda_det', LambdaB = '$lambda_det' ? '$lambda_det'
        ).

:- '$ctxprj'(bottom_absdef/2, []).
bottom_absdef(_Lambda) := ~idetdef.new_bottom.

% LambdaA is a consequence of LambdaB (=<)
:- '$ctxprj'(lambda_consequence/2, []).
lambda_consequence(LambdaA, LambdaB) :-
        ( lambdabottom(LambdaA) ->
            true
        ; lambdabottom(LambdaB) ->
            fail
        ; LambdaB = '$lambda_nondet' ->
            true
        ; LambdaA = '$lambda_nondet' ->
            fail
        ; LambdaB = '$lambda_semidet' ->
            true
        ; LambdaA = '$lambda_semidet' ->
            fail
        ; LambdaA = LambdaB % ('$lambda_det')
        ).

% Guard key for indexing
% TODO: do not include as a generic domain operation?!?! (we need the entry and exit)
get_key(_EntryD, _GuardD, _Args, none).

% Stop guard analysis
stop(_Mode, _G) :- fail.

:- '$ctxprj'(index/1, []).
index(_) :- fail.

:- public idet__absdef_to_imp/2.
:- '$ctxprj'(idet__absdef_to_imp/2, []).
idet__absdef_to_imp(AbsDef, Imp) :-
	trust(AbsDef instance_of idetdef),
        IDetLambdaPrime = ~AbsDef.postcond,
        Imp = ( IDetLambdaPrime = '$lambda_nondet' ? nondet
	      | IDetLambdaPrime = '$lambda_semidet' ? semidet
	      | IDetLambdaPrime = '$lambda_det' ? det
	      % TODO: introduced for (rec :- rec) predicate (define lambda_faildet, lambda_errordet, lambda_exceptiondet?)
	      | IDetLambdaPrime = '$bottom' ? nondet
	      ).

% Dump analysis results
:- '$ctxprj'(get_dumper/1, []).
get_dumper(Dumper) :-
	Dumper = ~idet_dumper.new.
}.

% ---------------------------------------------------------------------------
% idetdef: abstract semantics of a predicate

% TODO: A big refactoring is required in this module!  lambda_*,
%       etc. should be replaced by the 'computational effects'. In
%       this analysis, the input state does not matter (is always the
%       same).

:- public class idetdef {
    :- extends absdef.

    % note: lambda (precond) is '$lambda_det' because get_entry is
    %   always lambda_det

    % lambda prime
    :- attr postcond :: m_any.

    :- constructor new_/1.
    new_(LambdaPrime) :-
        ~postcond = LambdaPrime.

    :- constructor new_bottom_/0.
    new_bottom_ :-
        lambdabottom(LambdaPrime),
        new_(LambdaPrime).

    :- public constant is_bottom/0.
    is_bottom :-
        lambdabottom(~postcond).

    :- constructor new_nondet_/0.
    new_nondet_ :-
        new_('$lambda_nondet').

    {
    :- fluid exp :: module_exp.
    :- public constant unchanged/1.
    unchanged(Other) :-
	trust(Other instance_of idetdef),
        lambda_equal(~Other.postcond, ~postcond).
    }.

    :- public constant unknown_post/1.
    unknown_post := AbsDef :-
        % TODO: wrong!! unify LambdaPrime with lambdatop
        AbsDef = ~idetdef.new_nondet.

    % Increase precondition (lambda_lub)
    :- public precond_lub/1.
    precond_lub(_). % TODO: It does nothing, precondition is fixed

    :- public constant precond_consequence/1.
    precond_consequence(_). % TODO: Always true
}.

% ---------------------------------------------------------------------------

:- class idet_dumper {
    :- extends abs_dumper.
    :- attr dummy :: m_any. % TODO: Not used, but required for interfaces

    :- constructor new_/0.
    new_ :-
	~dummy = 0.

    {
    :- fluid exp :: module_exp.
    pred(PredId) :-
	trust(PredId instance_of predicate_x),
        Imp = ~PredId.get_prop(imp),
        display('  imp: '), display(Imp), nl.
    }.
}.

