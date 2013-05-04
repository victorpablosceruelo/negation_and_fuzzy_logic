:- module(_, [], [compiler(complang)]).

:- use_module(library(lists)).
:- use_module(library(dict)).

:- use_module(compiler(errlog)).
:- use_module(.(module_exp)).
:- use_module(.(ptoc__props)).
:- use_module(.(ptoc__ins)).

% ---------------------------------------------------------------------------

:- include(.(absint__interface)).
:- extends absint.

% ---------------------------------------------------------------------------

:- class triv_d {
    :- extends abs_d.
    :- attr state :: m_any.

    :- constructor new_/0.
    new_ :- ~state = 1.

    :- constructor bottom_/0.
    bottom_ :- ~state = 0.

    :- constant is_bottom/0.
    is_bottom :- ~state = 0.

    make_bottom :-
	    state <- 0.

    % Project domain info into the goal
    :- constant get_entry/2.
    get_entry(_G) := '$lambdatop'.

    % Obtain absdef
    :- constant get_absdef/4.
    get_absdef(_PredId, _Args, _D0) :=
            ( is_bottom ?
                ~trivdef.new_bottom
            | ~trivdef.new('$lambdatop')
            ).

    % Merge states
    proceed(_Args, D) :-
            trust(D instance_of triv_d),
	    ( D.is_bottom ->
	        true
	    ; S is ~state \/ ~D.state,
	      state <- S
	    ).

    builtin(_G) :- fail.

    :- constant is_builtin/1.
    is_builtin(_G) :- fail.

    push_choice.

    goalpost(_Args, _AbsDef).

    trust_type(_X, _Type).
}.

% ---------------------------------------------------------------------------
% trivdef: abstract semantics of a predicate

:- public class trivdef {
    :- extends absdef.

    % lambda is always the same
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

    {
    :- fluid exp :: module_exp.
    :- public constant unchanged/1.
    unchanged(Other) :-
	trust(Other instance_of trivdef),
        lambda_equal(~Other.postcond, ~postcond).
    }.

    :- public constant unknown_post/1.
    unknown_post := AbsDef :-
        % TODO: wrong!! unify LambdaPrime with lambdatop
        AbsDef = ~trivdef.new('$lambdatop').

    % Increase precondition (lambda_lub)
    :- public precond_lub/1.
    precond_lub(_). % TODO: It does nothing, precondition is fixed

    :- public constant precond_consequence/1.
    % TODO: Lambda = '$lambdatop'. ?
    precond_consequence(_). % TODO: Always true
}.

% ---------------------------------------------------------------------------

deflambda(_) := '$lambdatop'.

% ---------------------------------------------------------------------------

lambda_equal(Lambda1, Lambda2) :- Lambda1 == Lambda2.

% ---------------------------------------------------------------------------

lambdatop(_) := '$lambdatop'.

% ---------------------------------------------------------------------------

lambdabottom('$bottom').

% ---------------------------------------------------------------------------

bottom := ~triv_d.bottom.

% ---------------------------------------------------------------------------

anot_lambda(G, _Lambda) := G.

% ---------------------------------------------------------------------------

entry_dom(_PredId, _Args, _AbsDef) := ~triv_d.new.

% ---------------------------------------------------------------------------

{
:- fluid exp :: module_exp.

set_usermemo(PredId, AbsDef) :-
	trust(PredId instance_of predicate_x),
        PredId.set_prop(triv_usermemo, AbsDef).

get_usermemo(PredId) := AbsDef :-
	trust(PredId instance_of predicate_x),
        AbsDef = ( AbsDef0 = ~PredId.get_prop(triv_usermemo) ?
		     AbsDef0
		 | ~trivdef.new('$lambdatop')
		 ).

% specialize (select other predicate based on lambda)
spec(_Lambda0, PredId0, Xs0, AbsDef, PredId, Xs) :-
        PredId = PredId0,
        Xs = Xs0,
        AbsDef = ~get_usermemo(PredId).

lambda_lub(LambdaA, LambdaB) :=
        ( lambdabottom(LambdaA) ? LambdaB
        | lambdabottom(LambdaB) ? LambdaA
        | '$lambdatop'
        ).

:- '$ctxprj'(bottom_absdef/2, []).
bottom_absdef(_Lambda) := ~trivdef.new_bottom.

% Guard key for indexing
% TODO: do not include as a generic domain operation?!?! (we need the entry and exit)
get_key(_EntryD, _ExitD, _Args, none).

:- '$ctxprj'(index/1, []).
index(_) :- fail.

% Stop guard analysis
stop(_Mode, _G) :- fail.

% Dump analysis results
:- '$ctxprj'(get_dumper/1, []).
get_dumper(Dumper) :-
	Dumper = ~triv_dumper.new.
}.

:- class triv_dumper {
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


