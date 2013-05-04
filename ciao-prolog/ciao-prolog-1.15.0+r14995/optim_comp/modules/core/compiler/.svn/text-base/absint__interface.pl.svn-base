% Interface for abstract domain
% TODO: use ':- pred' assertions in interfaces
:- interface abs_d {
    :- '$statemodel'(pair).
    {
    :- fluid exp :: module_exp.
 
    % is_bottom: is the abstract domain bottom?
    :- '$ctxprj'(is_bottom/0, []).
    :- constant multifile is_bottom/0.

    % make_bottom: make the abstract domain bottom
    :- multifile make_bottom/0.

    % get_entry(G): given a goal call get the entry info 
    :- constant multifile get_entry/2.

    % proceed(Args, Db): make self the least upper bound (lub) of self and Db (w.r.t. Args)
    :- multifile proceed/2.

    % get_absdef(PredId, Args, D0) := Absdef: obtain the absdef (given a goal call get the exit info)
    :- constant multifile get_absdef/4.

    % builtin(G): analyze a builtin
    :- multifile builtin/1.

    % is_builtin(G): is a builtin
    :- constant multifile is_builtin/1.

    % push_choice: analyze a push choice
    :- multifile push_choice/0.

    % Precondition: \+ AbsDef.is_bottom
    % goalpost(Args, AbsDef): apply goal postcondition
    :- multifile goalpost/2.

    % trust_type(X, Type): 
    % TODO: see sht_analyzer todo
    :- multifile trust_type/2.
    }.
}.

% Interface for abstract predicate semantics
:- interface absdef {
    :- '$statemodel'(pair).
    {
    :- fluid exp :: module_exp.

    :- constant multifile is_bottom/0.

    :- constant multifile unchanged/1.

    :- constant multifile unknown_post/1.

    % Increase precondition with a new lambda (lambda_lub)
    :- multifile precond_lub/1.

    % Lambda is in the precondition
    :- constant multifile precond_consequence/1.
    }.
}.

% Interface for abstract interpreter
:- interface absint {
    :- '$statemodel'(single).
    {
    :- fluid exp :: module_exp.

    % bottom := D: return the bottom abstract domain
    :- '$ctxprj'(bottom/1, []).
    :- multifile bottom/1.

    % entry_dom(PredId, Args, AbsDef) := D: get the entry for a predicate
    :- multifile entry_dom/4.

    % anot_lambda(G, Lambda): include program-point info in the goal (using lambda)
    :- multifile anot_lambda/3.

    % spec(Lambda0, PredId0, Xs0, AbsDef, PredId, Xs): get specialized version of the pred
    :- multifile spec/6.

    % set_usermemo(PredId, AbsDef): set the memo entry for the predicate
    :- multifile set_usermemo/2.

    % get_usermemo(PredId, AbsDef): get the memo entry for the predicate
    :- multifile get_usermemo/2.

    % lambdatop(PredId, Lambda): get lambda top for a predicate
    :- multifile lambdatop/2.

    % deflambda(PredId, Lambda): get lambda top for a predicate
    :- multifile deflambda/2.

    % lambda_equal(Lambda1, Lambda2): two lambda are equal
    :- multifile lambda_equal/2.

    % bottom_absdef(Lambda, AbsDef): ??
    % TODO: document
    :- multifile bottom_absdef/2.

    % get_key(EntryD, GuardD, Args, KeyType): ??
    % TODO: document
    :- multifile get_key/4.

    % stop(Mode, G): ??
    % TODO: document
    :- multifile stop/2.

    % index(PredId): ??
    % TODO: document
    :- multifile index/1.

    % get_dumper(Dumper): obtain a Dumper to dump analysis info
    :- '$ctxprj'(get_dumper/1, []).
    :- multifile get_dumper/1.
    }.
}.

% Interface for dumper of absint results
:- interface abs_dumper {
    :- '$statemodel'(pair).
    {
    :- fluid exp :: module_exp.

    :- multifile pred/1.
    }.
}.

