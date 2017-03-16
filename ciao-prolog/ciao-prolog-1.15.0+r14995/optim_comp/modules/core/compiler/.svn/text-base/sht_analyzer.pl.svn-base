:- module(_, [], [compiler(complang)]).

% Analysis of moded-types
%
% Author: Jose F. Morales

:- use_module(library(lists)).
:- use_module(library(dict)).

:- use_module(.(errlog)).
:- use_module(.(module_exp)).
:- use_module(.(module_ipexp)).
:- use_module(.(ptoc__props)).
:- use_module(.(ptoc__ins)).
:- use_module(compiler(memoize)). % because of module_ipexp

% Type operations
:- public type/1.
:- regtype type(X) # "@var{X} is a type.".
type(_).

:- public type_exp/1.
:- regtype type_exp(X) # "@var{X} is a type expression.".
type_exp(_).

:- include(.(aux_profile_ctx__disabled)).
%:- include(.(aux_profile_ctx)).

% TODO: see the entry list generation for more information (results
% may be different if the module uses runtime expansions or not, etc.)

:- include(.(absint__interface)).
:- extends absint.

% ---------------------------------------------------------------------------
% sht_d: Abstract domain combining sht_share, sht_equal, sht_typedic,
% and sht_reldic.

% TODO: How to represent the bottom state? (look below)
%   a) Two classes (like cases classes) that inherit from a 
%     common case
%   b) A 'inconsistent' field.
%
%   Currently 'b' is implmented. 
%
%   Using 'a' could be nicer, but how is the state change implemented?
%   Code like (self <- sht_bottom_d) changes the class of
%   'self'. Problems:
%
%     - after virtually every method call, the self class could change

:- class sht_d {
    :- extends abs_d.
    :- attr inconsistent :: m_any.
    :- attr share :: sht_share. 
    :- attr equal :: sht_equal.
    :- attr typedic :: sht_typedic.
    :- attr reldic :: sht_reldic.

    % TODO: can constructors have '$ctxprj'?
    :- constructor top_/0.
    top_ :-
            inconsistent <- false, 
            share <- ~sht_share.empty,
            equal <- ~sht_equal.empty,
            typedic <- ~sht_typedic.empty,
            reldic <- ~sht_reldic.empty.

    % TODO: allow two cases or define another class?
    :- constructor bottom_/0.
    bottom_ :-
	    make_bottom.

    make_bottom :-
	    inconsistent <- true.

    {
    :- fluid exp :: module_exp.

    :- '$ctxprj'(is_bottom/0, []).
    :- constant is_bottom/0.
    %is_bottom :- ~self = sht_bottom_d.
    is_bottom :- ~inconsistent = true.

    % Project domain info into the goal
    :- constant get_entry/2.
    get_entry(G) := Lambda :-
            trust(G instance_of strgoal),
            Lambda = ~get_types(~G.args).

    :- constructor entry_dom_/3.
    % TODO: add sharing and equality to shtdef
    entry_dom_(PredId, Args, AbsDef) :-
	    trust(PredId instance_of predicate_x),
            trust(AbsDef instance_of shtdef),
            top_, % TODO: calling another constructor, right?
            Modes = ~PredId.get_prop(argmodes),
            In = ~filter_mode(Args, Modes, in),
            CallTypes = ~AbsDef.call_types,
            InTypes = ~filter_mode(CallTypes, Modes, in),
            % note: pass In-Types because out args do not share
            Xts = ~terms_to_types(In),
            ( unif__xs(Xts, InTypes), set_share(Xts) ->
                true
            ; make_bottom % TODO: does this make sense?
            ).

    % Merge the current abstract state with exit state D
    % TODO: change it to work with entry and exit and not with domains??
    % TODO: indeed, this predicate does a project to Args and then a lub 
    % TODO: 'Args' are the common vars in Da and Db... do we need them?
    % TODO: extend to sharing and equality (not only types)
    proceed(_, D) :-
            trust(D instance_of sht_d),
            D.is_bottom, !.
    proceed(Args, D) :-
            trust(D instance_of sht_d), 
            ( is_bottom ->
	        TypesC = ~D.get_types(Args)
	    ; TypesA = ~get_types(Args),
	      TypesB = ~D.get_types(Args),
	      TypesC = ~typedic.map_or(TypesA, TypesB)
	    ),
            inconsistent <- false, 
            share <- ~sht_share.empty,
            equal <- ~sht_equal.empty,
            typedic <- ~sht_typedic.empty,
            reldic <- ~sht_reldic.empty,
            Xts = ~terms_to_types(Args),
            ( unif__xs(Xts, TypesC), set_share(Xts) ->
                true
            ; make_bottom % TODO: does this make sense?
            ).

    % Obtain absdef
    :- constant get_absdef/3.
    get_absdef(_PredId, Args, D0) := AbsDef :-
            trust(D0 instance_of sht_d),
            Lambda = ~D0.get_types(Args),
            ( is_bottom ->
                AbsDef = ~shtdef.new_bottom(Lambda)
            ; LambdaPrime = ~get_types(Args),
              AbsDef = ~shtdef.new(Lambda,LambdaPrime)
            ).

    % share the vars
    set_share(ArgTs) :-
            \+ is_bottom, % TODO: this is a precondition, the test is not necessary
            ArgTs2 = ~typedic.filter_nonatomic(ArgTs),
            share.setxs(ArgTs2).

    :- constant get_types/2.
    get_types([]) := [].
    get_types([X|Xs]) := [~get_type(X)| ~get_types(Xs)].

    :- constant get_type/2.
    %:- pred get_type :: term * normtype.
    get_type(X) := MT :- X instance_of termunk, !, MT = ~typedic.eval__any. % TODO: this is not really correct...
    get_type(X) := Type :- X instance_of termvar, !,
            Type = ~typedic.postwiden_or_limit(~type_plain(~term_to_type(X))).
    % TODO: This is expensive: big terms are re-build from the arguments!
    get_type(X) := MT :-
            trust(X instance_of termstr),
	    MT = ~fnctype.new(~X.name, ~get_types(~X.args)).

    % TODO: make depth configurable?? depth makes this analysis very similar to depth-k
    % type recursive eval (removes all references to T)
    % TODO: This is expensive: big terms are re-build from scratch
    :- constant type_plain/2.
    type_plain(X) := X2 :-
            xname :: any <- none,
	    depth :: m_int <- 10,
	    X2 = ~type_plain_0(X).
    {
        :- fluid xname :: any.
        :- fluid depth :: m_int.
        :- '$ctxprj'(type_plain_0/2, [exp, xname, u(depth)]).
        :- constant type_plain_0/2. 
        type_plain_0(X) := Z :-
                X2 = ~typedic.expmt(X),
                ( X2.decomp(XVar, XNva), XNva = '$t_reclist'(X3) ->
                    % TODO: check...
                    depth.dec(1),
                    call(( xname :: any <- none, X4 = ~type_plain_0(X3) )),
                    Z = ~modedtype.new(XVar, '$t_reclist'(X4))
                ; ( ( Depth = ~depth, Depth /\ 1 =:= 0 ) ->
                      XName = ( X instance_of vtype ? ~X.name | none )
                  ; XName = ~xname
                  ),
                  call(( xname :: any <- XName, Z = ~type_plain_1(X2) ))
                ).

        :- '$ctxprj'(type_plain_1/2, [exp, xname, u(depth)]).
        :- constant type_plain_1/2.
        type_plain_1(_) := X :- Depth = ~depth, Depth = 0, !, X = ~typedic.eval__any.
        type_plain_1(X1) := X2 :-
            trust(X1 instance_of modedtype),
            X1.decomp(XVar, XNva),
	    ( XNva = '$t_any' ->
	        X2 = X1
	    ; depth.dec(1),
	      X2 = ~modedtype.new(XVar, ~t__plain(XNva))
	    ).

        :- '$ctxprj'(t__plain/2, [exp, xname, u(depth)]).
        :- constant t__plain/2.
        t__plain([]) := [].
        t__plain(['$f'(Kx, Tx)|Xs]) := ['$f'(Kx, Tx2)| ~t__plain(Xs)] :-
                ( XName = ~xname, var(XName), Kx = (_/_), Nts = ~reldic.get(XName-Kx) ->
                    % use reldic to get the type of the arguments
                    Tx2 = ~t__plain_args2(Nts)
                ; Tx2 = ~t__plain_args(Tx)
                ).

        :- '$ctxprj'(t__plain_args/2, [exp, xname, u(depth)]).
        :- constant t__plain_args/2.
        t__plain_args([]) := [].
        t__plain_args([X|Xs]) := [~type_plain_0(X)| ~t__plain_args(Xs)].

        :- '$ctxprj'(t__plain_args2/2, [exp, xname, u(depth)]).
        :- constant t__plain_args2/2.
        t__plain_args2([]) := [].
        t__plain_args2([Nt|Nts]) := [~type_plain_0(Nt)| ~t__plain_args2(Nts)].
    }.

    goalpost(Args0, AbsDef) :-
            % TODO: this is call_to_entry
            trust(AbsDef instance_of shtdef),
            ExitTypes = ~AbsDef.exit_types,
            % generic call
            ( % move to types
              args(Args0, Args),
              % unify with types on exit
              unif__xs(Args, ExitTypes),
              set_share(Args) ->
                true
            ; make_bottom
            ).

    % TODO: THIS is a costly operation for benchmarks crypt and deriv!
    args([], []).
    args([X|Xs], [Arg|Args]) :-
            Xt = ~term_to_type(X),
            ( X instance_of termvar ->
                Arg = Xt
            ; % unify the structure with a new variable
              Arg = ~new_type_var,
              unif__2(Arg, Xt)
            ),
            args(Xs, Args).

    :- constant is_builtin/1.
    is_builtin(G) :- is_unify(G), !.
    is_builtin(G) :- is_instance(G), !.
    is_builtin(G) :-
        trust(G instance_of strgoal),
	NA = ~G.name,
	( NA = 'term_basic:$trust_type'/2 -> true
	; NA = 'basiccontrol:fail'/0
	).

    % TODO: do not generate different builtins for unify and instance, only unify
    % TODO: this is call_to_success_builtin
    builtin(G) :- is_unify(G), !,
            trust(G instance_of strgoal),
            ~G.args = [X, Y],
            unif(~term_to_type(X), ~term_to_type(Y)).
    builtin(G) :- is_instance(G), !,
            trust(G instance_of strgoal),
            ~G.args = [X, Y],
            unif(~term_to_type(X), ~term_to_type(Y)).
    builtin(G) :-
            % TODO: replace trust_type by a trust goal, that abstractly executes the code, but is ignored in compilation!!
            trust(G instance_of strgoal),
            ~G.name = 'term_basic:$trust_type'/2,
            !,
            ~G.args = [X, Type0],
	    trust(Type0 instance_of termunk),
	    Type = ~typedic.normtype(~Type0.value),
            % a trusted type intersection (for external trust when this analysis is too conservative)
            trust_type(X, Type).
    builtin(G) :-
            trust(G instance_of strgoal),
            ~G.name = 'basiccontrol:fail'/0,
            !,
            make_bottom.

    unif(X, Y) :-
            ( unif__2(X, Y) ->
                true
            ; make_bottom
            ).

    % TODO: OPTIMIZE! this is a bottleneck in analysis
    % TODO: check that it works with cyclic terms
    % note: fails if abstract unification fails
    % TODO: more intelligent sharing: If X is f(A,B,C) and Y is f(D,E,F), set the sharing of X and Y is set the sharing of A and D, B and E, and C and F
    unif__2(Xt, _) :-
            Xt instance_of modedtype, Xt.is_unktype, % TODO: for is_unk... a better solution?
            !.
    unif__2(Xt, Yt) :-
            Xt instance_of vtype,
            Yt instance_of fnctype,
            !,
            XMayBeVar = ( typedic.may_be_var(Xt) ? true | false ),
	    YFunctor = ~Yt.functor,
            ( Ats = ~typedic.nonvar_args(Xt, YFunctor) ->
                Nts1 = ( XName = ~Xt.name,
		         Nts0 = ~reldic.get(XName-YFunctor) ? Nts0
		       | no
		       ),
                Ats2 = ( XMayBeVar = true ? ~orvars(Ats) % note: the var part do not share here
		       | Ats
		       )
            ; Ats2 = no
            ),
            ( XMayBeVar = true, Ats2 == no ->
                % may be a 'var' but not the required functor
                YFunctor = _/YArity,
                Nts = ~new_type_vars(YArity),
                XYt = Yt,
                YFunctor = _/YArity,
                Ats3 = ~varvars(YArity),
                typedic.listset(Nts, Ats3),
                bind(Xt, XYt),
                % TODO: insert and extract that info from the type! using typeof, not the reldic
                share.n_to_1(Nts, Xt)
            ; Ats2 \== no,
              ( Nts1 == no ->
                  % arguments of Xt for YFunctor has no links with known variables (except with Xt)
                  % build the reldic attributes
                  YFunctor = _/YArity,
                  Nts = ~new_type_vars(YArity),
                  typedic.listset(Nts, Ats2)
              ; Nts = Nts1
    %            typedic.listset(Nts, Ats2) % TODO: this should not be necessary if all unifications takes into account the reldic
              ),
              bind(Xt, ~fnctype.new(YFunctor, Nts)),
              % X type included that functor
              % TODO: if X can be a var, arguments must include the 'var' type
              ( Nts1 == no ->
                  % arguments of Xt for YFunctor has no links with known variables (except with Xt)
                  % build the reldic attributes
                  % TODO: insert and extract that info from the type! using typeof, not the reldic
                  share.n_to_1(Nts, Xt)
              ; true
              )
            ),
	    XName = ~Xt.name,
	    reldic.set(XName-YFunctor, Nts),
            % instance arguments (that will handle sharing and types of arguments)
            unif__xs(Nts, ~Yt.args).
    unif__2(Xt, Yt) :-
            Xt instance_of vtype,
            Yt instance_of vtype,
            !,
            % TODO: do not share variables without links... etc. (e.g. two ground variables)
            ( equal.equiv(Xt, Yt) ->
                true % do nothing, exactly the same variable
            ; typedic.mode_is_var(Xt), typedic.mode_is_var(Yt) ->
                % both types are variables, do nothing (shared variables do not get more instantiated)
                % TODO: it should work without setting share...
                equal.set2(Xt, Yt), % X and Y are equal
                share.set2(Xt, Yt) % X and Y share
            ; equal.set2(Xt, Yt), % X and Y are equal
              share.set2(Xt, Yt), % X and Y share
              % unify type of X and shared variables
              % TODO: do something so that if X=[A,B],Y=[C,D],X=Y then A=C and B=D
              XYt = ~typedic.unify(Xt, Yt),
              typedic.not_bottom(XYt), % fail if Xt and Yt were not compatible
              bind(Xt, XYt)
            ).
    unif__2(Xt, Yt) :-
            Xt instance_of vtype,
            Yt instance_of modedtype, % Yt has no links with known variables
            !,
            ( typedic.mode_is_var(Xt), typedic.mode_is_var(Yt) ->
                true
            ; XYt = ~typedic.unify(Xt, Yt),
              typedic.not_bottom(XYt), % fail if Xt and Yt were not compatible
              bind(Xt, XYt)
            ).
    unif__2(Xt, Yt) :- !,
            errlog:bug(['unif__2 cannot handle ', Xt, ' ', Yt]),
            fail.

    % set the type of Xt to Yt, affecting equivalent variables and shared variables
    % TODO: is 'bind' a good name?
    bind(Xt, Yt) :-
    %        errlog:trace([e1(Xt, Yt, T0)]),
            typedic_equiv_set(Xt, Yt),
    %        errlog:trace([e2(Xt, Yt, T1)]),
            typedic.map_forget(~share.equiv_of(Xt)).
    %        errlog:trace([e3(Xt, Yt, T)]).

    % set type of all variables equivalent (for E or A) to Xt
    typedic_equiv_set(Xt, Yt) :-
            typedic.listset1(~equal.equiv_of(Xt), Yt).

    :- '$ctxprj'(new_type_vars/2, []).
    :- static new_type_vars/2.
    new_type_vars(0, Nts) :- !,
            Nts = [].
    new_type_vars(I, Nts) :-
            Nt = ~new_type_var,
            Nts = [Nt|Nts0],
            I1 is I - 1,
            new_type_vars(I1, Nts0).

    :- '$ctxprj'(varvars/2, []).
%    :- static varvars/2.
    varvars(0, Nts) :- !,
            Nts = [].
    varvars(I, Nts) :-
            Nts = [~typedic.eval__var|Nts0],
            I1 is I - 1,
            varvars(I1, Nts0).

%    :- '$ctxprj'(orvars/2, []).
%    :- static orvars/2.
    orvars([]) := [].
    orvars([T|Ts]) := [~typedic.or(T, ~typedic.eval__var)|~orvars(Ts)].

    unif__xs([], []).
    unif__xs([Nt|Nts], [Yt|Yts]) :-
            unif__2(Nt, Yt),
            unif__xs(Nts, Yts).

    :- '$ctxprj'(new_type_var/1, []).
    :- static new_type_var/1.
    % TODO: this representation must be the same than the used in ptoc__type and must be able to represent variables (without losing information?)
    new_type_var := ~vtype.new.

%    :- '$ctxprj'(term_to_type/2, []).
%    :- static term_to_type/2.
    :- constant term_to_type/2.
    term_to_type(X) := ~vtype.new_n(~X.name) :- X instance_of termvar, !.
    term_to_type(X) := ~typedic.normtype(unk) :- X instance_of termunk, !. % TODO: strange...
    term_to_type(X) := MT :-
            trust(X instance_of termstr),
	    MT = ~fnctype.new(~X.name, ~terms_to_types(~X.args)).

%    :- '$ctxprj'(terms_to_types/2, []).
%    :- static terms_to_types/2.
    :- constant terms_to_types/2.
    terms_to_types([]) := [].
    terms_to_types([X|Xs]) := [~term_to_type(X)| ~terms_to_types(Xs)].

    % a trusted type intersection (for external trust when this analysis is too conservative)
    % (intersect with types on call)
    trust_type(X, Type) :-
            Xt = ~term_to_type(X),
            ( \+ is_bottom,
              intersect_t(Xt, Type) ->
                true
            ; make_bottom
            ).

    intersect_t(Xt, _) :-
            Xt instance_of modedtype, Xt.is_unktype, % TODO: for is_unk... a better solution?
            !.
    intersect_t(Xt, Yt) :-
            % TODO: this is a precondition
            trust(Xt instance_of vtype),
            XYt = ~typedic.and(Xt, Yt),
            typedic.not_bottom(XYt),
            % set the type of X and Y
            typedic_equiv_set(Xt, XYt).

    push_choice.
    }.
}. % sht_d

% ---------------------------------------------------------------------------
% shtdef: abstract semantics of a predicate

:- public class shtdef {
    :- extends absdef.

    :- attr call_types :: m_any # "Lambda".
    :- attr exit_types :: m_any # "Lambda prime".

    :- constructor new_/2.
    new_(CallTypes, ExitTypes) :-
        ~call_types = CallTypes, ~exit_types = ExitTypes.

    :- constructor new_bottom_/1.
    new_bottom_(Lambda) :-
        lambdabottom(LambdaPrime),
        new_(Lambda, LambdaPrime).

    :- public constant is_bottom/0.
    is_bottom :-
        lambdabottom(~exit_types).

    {
    :- fluid exp :: module_exp.
    :- constructor new_norm_/2.
    new_norm_(CallTypes0, ExitTypes0) :-
        CallTypes = ~norm_lambda(CallTypes0),
        ExitTypes = ~norm_lambda(ExitTypes0),
	new_(CallTypes, ExitTypes).

    :- static norm_lambda/2.
    norm_lambda(X, X) :- lambdabottom(X), !.
    norm_lambda(X, Y) :- Y = ~map_type_norm(X).
    }.

    {
    :- fluid exp :: module_exp.
    :- constructor new_top_/1.
    new_top_(Modes) :-
          missing_typedic(CallTypes = ~typedic.top_call(Modes)),
          missing_typedic(ExitTypes = ~typedic.top_exit(Modes)),
          new_(CallTypes, ExitTypes).
    }.

    % TODO: export getters/setters and remove
    :- public constant get_call_types/1.
    get_call_types := ~call_types.
    :- public constant get_exit_types/1.
    get_exit_types := ~exit_types.

    {
    :- fluid exp :: module_exp.
    :- public constant unchanged/1.
    % TODO: Removing this produces a silent 'clause expansion' error, fix
    unchanged(Other) :-
	trust(Other instance_of shtdef),
        lambda_equal(~Other.call_types, ~call_types),
        lambda_equal(~Other.exit_types, ~exit_types).

    :- public constant unknown_post/1.
    unknown_post := AbsDef :-
	Lambda = ~call_types,
        % TODO: wrong!! unify LambdaPrime with lambdatop
        missing_typedic(LambdaPrime = ~typedic.any_types(Lambda)),
        AbsDef = ~shtdef.new(Lambda,LambdaPrime).

    % Increase precondition (lambda_lub)
    :- public precond_lub/1.
    precond_lub(Lambda) :-
        call_types <- ~lambda_lub(~call_types, Lambda).

    :- public constant precond_consequence/1.
    precond_consequence(Lambda) :-
        Lambda2 = ~call_types,
        missing_typedic(typedic.lambda_consequence(Lambda, Lambda2)).
    }.
}.

% ---------------------------------------------------------------------------
% sht_typedic: abstract domain for moded types

% TODO: this context constructor is used when typedic is (incorrectly) missing
:- '$def_binder'(missing_typedic, (typedic :: sht_typedic <- [])).

:- public class sht_typedic {
    :- '$statemodel'(pair). % note: this module manages self directly
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    :- constructor empty_/0.
    empty_ :- self <- ~keyval_empty.

    {
    :- fluid exp :: module_exp.

    % Set type of X
    :- '$ctxprj'(set/2, []).
    set(Xt, Value) :-
        Xt instance_of vtype,
	self <- ~set_value(~Xt.name, Value, ~self).

    % Get type of X
%    :- '$ctxprj'(get/2, []).
    :- constant get/2.
    get(Xt) := Value :-
        Xt instance_of vtype,
        Value = ~get_value(~Xt.name, ~self), !.
    get(_) := ~eval__var.

    % Set one value for all elements in the list
    :- '$ctxprj'(listset1/2, []).
    listset1([], _).
    listset1([Xt|Xts], Value) :- set(Xt, Value), listset1(Xts, Value).

    % Set the values of several elements
    :- '$ctxprj'(listset/2, []).
    listset([], []).
    listset([Nt|Nts], [At|Ats]) :- set(Nt, At), listset(Nts, Ats).

    :- constant any_types/2.
    any_types([]) := [].
    any_types([At|Ats]) := [Ct|Cts] :-
            Ct = ~unify_any(At),
            Cts = ~any_types(Ats).

    % types are equivalent
    :- constant equal/2.
    equal(A0, B0) :-
            A = ~expmt(A0),
            B = ~expmt(B0),
            A.decomp(AVar, ANva), B.decomp(BVar, BNva),
	    t__equal0(AVar, BVar),
	    t__equal0(ANva, BNva).

    :- constant t__equal/2.
    t__equal0('$t_any', '$t_any') :- !.
    t__equal0('$t_reclist'(X), '$t_reclist'(Y)) :- !,
            equal(X, Y).
    t__equal0(X, Y) :- !,
            X2 = ~t__expand1rec(X),
            Y2 = ~t__expand1rec(Y),
	    t__equal(X2, Y2).

    :- constant t__equal/2.
    t__equal([], []) :- !.
    t__equal(['$f'(Kx, Tx)|Xs], ['$f'(Ky, Ty)|Ys]) :- Kx == Ky, !,
            map_equal(Tx, Ty),
            t__equal(Xs, Ys).

    :- constant map_equal/2.
    map_equal([], []).
    map_equal([X|Xs], [Y|Ys]) :- equal(X, Y), map_equal(Xs, Ys).

    :- constant not_bottom/1.
    not_bottom(X) :- \+ equal(X, ~eval__none).

    % get variables that can share
    % precondition: Xt instance_of vtype | Xt = any
    :- constant filter_nonatomic/2.
    filter_nonatomic([Xt|Xts]) := Xts1 :-
            ( Xt instance_of modedtype, Xt.is_unktype ->
                Xts1 = Xts2 % TODO: not really correct
            ; consequence(Xt, ~normtype(atomic)) ->
                % TODO: put native types here
                % TODO: not necessary if the type do not have links...
                % the variable cannot have links
                Xts1 = Xts2
            ; Xts1 = [Xt|Xts2]
            ),
            Xts2 = ~filter_nonatomic(Xts).
    filter_nonatomic([]) := [].

    map_forget([]).
    map_forget([Xt|Xts]) :- forget(Xt), map_forget(Xts).

    % Forget the type of X (equivalent to unification with a variable
    % of unknown type, to preserve monotonicity)
    forget(Xt) :-
            % TODO: I could be more precise (''$t_any'' is too big)
            Xt2 = ~unify_any(Xt),
            set(Xt, Xt2).

    % Unification of moded types:
    % -----------------------------------------------------------------------
    %
    % Definition[compatibility types]:
    %
    %   Let t/1 be a pure predicate that defines a set of terms T, a
    %   type constraint 'compat(X, t)' means that all the possible values
    %   of variable X are in T (i.e. are solutions of t/1).
    %
    % TODO: if T is not instantiated, should it put a constraint?
    %
    % Definition[moded type]:
    % TODO: There is a problem with arguments of moded types
    %
    %   A moded type T is defined as a pair (Tv,Tn), where Tv and Tn
    %   are the set of values in the case of uninstantiated variables
    %   and instantiated variables, respectively.
    %
    %   The possible values of each set are:
    %   - {top, bottom}.
    %   - a richer lattice
    %
    %   Let the moded types A=(Av,An), B=(Bv,Bn), the unification of A
    %   and B is defined as:
    %
    %     unify(A,B) = union { unify((Av,b),(Bv,b)),
    %                          unify((b,An),(Bv,b)),
    %                          unify((Av,b),(b,Bn)),
    %                          unify((b,An),(b,Bn)) }
    %   where:
    %
    %   unify((Av,b),(Bv,b)) = (intersect(Av,Bv),b)
    %   unify((b,An),(Bv,b)) = (b,intersect(An,Bv))
    %   unify((Av,b),(b,Bn)) = (b,intersect(Av,Bn))
    %   unify((b,An),(b,Bn)) = (b,intersect(An,Bn))
    %
    %   The union of all the components gives the type Z=(Zv,Zn):
    %
    %     Zv = intersect(Av,Bv)
    %     Zn = union { intersect(An,Bv),
    %                  intersect(Av,Bn),
    %                  intersect(An,Bn) }
    %
    %   Since Av and Bv can only be in {top,bottom}, we can simplify
    %   Zn as:
    %
    %     Av = b, Bv = b: Zn = intersect(An,Bn)
    %     Av = b, Bv = t: Zn = An
    %     Av = t, Bv = b: Zn = Bn
    %     Av = t, Bv = t: Zn = union(An,Bn)
    %

    % type resulting from unifying two types
    :- constant unify/3.
    unify(X0, Y0) := ~unify_or(X0, [], Y0, []).

    :- constant unify_or/5.
    unify_or(A0, XVar, B0, YVar) := Z :-
            A = ~expmt(A0),
            B = ~expmt(B0),
            A.decomp(AVar, ANva), B.decomp(BVar, BNva),
            ZVar = ( AVar = [], BVar = [] ? []
		   | AVar = '$t_any', BVar = [] ? XVar
		   | AVar = [], BVar = '$t_any' ? YVar
		   | AVar = '$t_any', BVar = '$t_any' ? '$t_any'
		   ),
	    %
            ( AVar = '$t_any', ANva = [] ->
		ZNva = BNva
            ; BVar = '$t_any', BNva = [] ->
		ZNva = ANva
            ; ANva = '$t_any' ->
	        ZNva = ( BVar = [] ?
		           ( BNva = '$t_any' ? '$t_any'
			   | BNva = '$t_reclist'(X) ?
			       ( XVar = '$t_any' ? '$t_any'
			       | '$t_reclist'(~unify_any(X))
			       )
			   | ( XVar = '$t_any' ? '$t_any'
			     | ~t__unify_any(BNva, AVar)
			     )
			   )
		       | '$t_any'
		       )
            ; BNva = '$t_any' ->
	        ZNva = ( AVar = [] ?
		           ( ANva = '$t_any' ? '$t_any'
			   | ANva = '$t_reclist'(X) ?
			       ( YVar = '$t_any' ? '$t_any'
			       | '$t_reclist'(~unify_any(X))
			       )
			   | ( YVar = '$t_any' ? '$t_any'
			     | ~t__unify_any(ANva, BVar)
			     )
			   )
		       | '$t_any'
		       )
            ; AVar = [], ANva = '$t_reclist'(X), BVar = [], BNva = '$t_reclist'(Y) ->
		Ze = ~unify_or(X, XVar, Y, YVar),
                ZNva = ( is_none(Ze) ? [] | '$t_reclist'(Ze) )
            ; Xs = ~t__expand1rec(ANva),
              Ys = ~t__expand1rec(BNva),
              XVar2 = ~t__or0(XVar, BVar),
              YVar2 = ~t__or0(YVar, AVar),
              ZNva = ~t__unify_2ff(Xs, XVar2, Ys, YVar2)
            ),
	    %
	    Z = ( ZVar == BVar, ZNva == BNva ? B
		| ZVar == AVar, ZNva == ANva ? A
		| ~postwiden(~modedtype.new(ZVar, ZNva))
		).

    :- constant t__unify_2ff/5.
    t__unify_2ff([], _XVar, _, _YVar) := [] :- !.
    t__unify_2ff(_, _XVar, [], _YVar) := [] :- !.
    t__unify_2ff([X|Xs], XVar, [Y|Ys], YVar) := Zs :- !,
            X = '$f'(Kx, Tx),
            Y = '$f'(Ky, Ty),
            ( Kx @< Ky, XVar = '$t_any' ->
                Zs = [X|Zs1], Xs1 = Xs, Ys1 = [Y|Ys]
            ; Kx @< Ky ->
                Zs = Zs1, Xs1 = Xs, Ys1 = [Y|Ys]
            ; Kx @> Ky, YVar = '$t_any' ->
                Zs = [Y|Zs1], Xs1 = [X|Xs], Ys1 = Ys
            ; Kx @> Ky ->
                Zs = Zs1, Xs1 = [X|Xs], Ys1 = Ys
            ; ( Tz = ~unify_args(Tx, XVar, Ty, YVar) -> Zs = ['$f'(Kx, Tz)|Zs1] ; Zs = Zs1 ),
              Xs1 = Xs, Ys1 = Ys
            ),
            Zs1 = ~t__unify_2ff(Xs1, XVar, Ys1, YVar).

    :- constant unify_args/5.
    unify_args([], _XVar, [], _YVar) := [].
    unify_args([X|Xs], XVar, [Y|Ys], YVar) := [Z|~unify_args(Xs, XVar, Ys, YVar)] :-
            Z = ~unify_or(X, XVar, Y, YVar),
            \+ is_none(Z).

    :- constant unify_any/2.
    unify_any(A0) := Z :-
            A = ~expmt(A0),
	    BVar = '$t_any',
	    YVar = [],
            A.decomp(AVar, ANva),
            ( AVar = [] ->
                % A nonvariable, keep the functors of the first level and
                % unify with 'any' all arguments
	        ZVar = [], 
                ZNva = ( ANva = '$t_any' ? ANva
		       | ANva = '$t_reclist'(X) ? '$t_reclist'(~unify_any(X))
		       | ~t__unify_any(ANva, BVar)
		       )
            ; % Moded type A may be a variable, the unification of a
              % variable and a top type gives the top type, and the
              % top type is bigger than any other type (and bigger
              % than the intersection of the nonvar part of the
              % types): the resulting type is top.
	      ZVar = BVar,
              ZNva = '$t_any'
            ),
	    %
	    ( YVar = '$t_any' ->
	        ZVar2 = ~t__or0(ZVar, BVar),
		ZNva2 = '$t_any'
	    ; ZVar2 = ZVar, ZNva2 = ZNva
	    ),
	    Z = ( ZVar2 == AVar, ZNva2 == ANva ? A
		| ~postwiden(~modedtype.new(ZVar2, ZNva2))
		).

    :- constant t__unify_any/3.
    t__unify_any([], _) := [] :- !.
    t__unify_any([X|Xs], '$t_any') := Zs :- !,
            X = '$f'(Kx, Tx),
            Tz = ~unify_any_args(Tx),
            Zs = ['$f'(Kx, Tz)|Zs1],
            Zs1 = ~t__unify_any(Xs, '$t_any').
    t__unify_any([X|Xs], []) := Zs :- !,
            X = '$f'(Kx, Tx),
            ( Tz = ~unify_any_args(Tx) ->
                Zs = ['$f'(Kx, Tz)|Zs1]
            ; Zs = Zs1
            ),
            Zs1 = ~t__unify_any(Xs, []).

    :- constant unify_any_args/2.
    unify_any_args([]) := [].
    unify_any_args([X|Xs]) := [Y|~unify_any_args(Xs)] :-
            Y = ~unify_any(X).

    :- constant map_or/3.
    map_or([], []) := [].
    map_or([At|Ats], [Bt|Bts]) := [Ct|Cts] :-
            Ct = ~or(At, Bt),
            Cts = ~map_or(Ats, Bts).

    % LambdaA is a consequence of LambdaB (=<)
    :- constant lambda_consequence/2.
    lambda_consequence([], []) :- !.
    lambda_consequence([Ta|Tas], [Tb|Tbs]) :-
            consequence(Ta, Tb),
            lambda_consequence(Tas, Tbs).

    % the top during call entry for an output argument is 'var'
    :- '$ctxprj'(top_call/2, []).
    :- constant top_call/2.
    top_call([]) := [] :- !.
    top_call([Mode|Modes]) := [X|~top_call(Modes)] :-
            X = ( Mode = out ? ~eval__var | ~eval__any ).

    :- '$ctxprj'(top_exit/2, []).
    :- constant top_exit/2.
    top_exit([]) := [] :- !.
    top_exit([_|Modes]) := [MT|~top_exit(Modes)] :- MT = ~eval__any.

    % TODO: this is a hack...?
    :- constant postwiden/2.
    postwiden(X0) := X1 :-
	    trust(X0 instance_of modedtype),
            ( X0.decomp(XVar, XNva),
	      XVar = [],
              t__is_list01r(XNva, Et, Rest0),
              Rest = ~expmt(Rest0), % TODO: necessary?
              Rest.decomp(RestVar, RestNva),
	      RestVar = [],
	      RestNva = '$t_reclist'(Et2) ->
                X1 = ~modedtype.new([], '$t_reclist'(~or(Et, Et2)))
            ; X1 = X0
            ).

    :- constant postwiden_or_limit/2.
    postwiden_or_limit(X0) := X1 :-
	    trust(X0 instance_of modedtype),
            ( X0.decomp(XVar, XNva), XVar = [],
              t__is_list01r(XNva, Et, Rest0),
              Rest = ~expmt(Rest0), % TODO: necessary?
              Rest.decomp(RestVar, RestNva),
	      RestVar = [],
	      RestNva = '$t_reclist'(Et2) ->
                X1 = ~modedtype.new([], '$t_reclist'(~or(Et, Et2)))
            ; X1 = ~prune(X0, 20) % TODO: make threshold configurable
            ).

    % TODO: kludge with termination problems
    %type_size_threshold(200).
    %
    %term_size(X, N0, N) :- var(X), !, N is N0 + 1.
    %term_size(X, N0, N) :- X =.. [_|Xs],
    %        N1 is N0 + 1,
    %        term_size__2(Xs, N1, N).
    %
    %term_size__2([], N, N) :- !.
    %term_size__2([X|Xs], N0, N) :-
    %        term_size(X, N0, N1),
    %        term_size__2(Xs, N1, N).

    % limit the type depth
    :- constant prune/3.
    prune(X0, I) := Z :-
            X = ~expmt(X0),
            Z = ~prune_1(X, I).

    :- constant prune_1/3.
    prune_1(_, 0) := Z :- !, Z = ~eval__any.
    prune_1(A, I) := Z :-
	    trust(A instance_of modedtype),
            A.decomp(AVar, ANva),
            ( ANva = '$t_any' ->
                Z = A
            ; ANva = '$t_reclist'(X) ->
                I1 is I - 1,
                Z0 = ~prune(X, I1),
                Z = ~modedtype.new(AVar, '$t_reclist'(Z0))
            ; % TODO: define width on the type part (more general than this)
              Width = ~length(ANva),
              ( Width = 0 -> I1 = I ; I1 is I // Width ),
              Z = ~modedtype.new(AVar, ~t__prune(ANva, I1))
            ).

    :- constant t__prune/3.
    t__prune([], _) := [] :- !.
    t__prune([X|Xs], I) := Zs :- !,
            X = '$f'(Kx, Tx),
            I1 is I - 1,
            Tz = ~prune_args(Tx, I1),
            Zs = ['$f'(Kx, Tz)|Zs1],
            Zs1 = ~t__prune(Xs, I).

    :- constant prune_args/3.
    prune_args([], _) := [].
    prune_args([X|Xs], I) := [Z|~prune_args(Xs, I)] :-
            Z = ~prune(X, I).

    % Evaluate the first level of the type (except if it is a reclist)
    % TODO: may halt... add mechanisms to detect loops??
    % already evaluated
    :- constant expmt/2.
    :- meta_predicate expmt(?, out(modedtype)).
    expmt(X) := X :- X instance_of modedtype, !.
    % get type from type dic
    % TODO: may loop forever!!
    expmt(Xt) := X2 :- Xt instance_of vtype, !,
        X2 = ~expmt(~normtype(~get(Xt))).
    expmt(Xt) := ~eval__fnc2(NA, As) :- Xt instance_of fnctype, !,
        NA = ~Xt.functor,
	As = ~Xt.args.
    expmt(X) := _ :-
        errlog:bug(cannot_expmt(X)).

    %:- constant normtypes0/3.
    %normtypes0([], _) := [].
    %normtypes0([X|Xs], L) := [~normtype0(X, L) | ~normtypes0(Xs, L)].
    %:- constant normtype0/3.
    %normtype0(X, _) := X :- is_normtype(X), !.
    %normtype0(X, L) := Y :- errlog:trace(normtype(X, L)), Y = ~normtype(X).

    :- constant normtypes/2.
    normtypes([]) := [].
    normtypes([X|Xs]) := [~normtype(X) | ~normtypes(Xs)].

    % Normalize a symbolic type specification to a modedtype, vtype, or fnctype
    :- constant normtype/2.
    normtype(X) := X :- is_normtype(X), !.
    % basic opts
    % TODO: use or/2 and and/2 when possible (to avoid normalizing arguments)
    normtype((A ; B)) := ~or(~normtype(A), ~normtype(B)) :- !.
    normtype((A, B)) := ~and(~normtype(A), ~normtype(B)) :- !.
    % basic types
    normtype(any) := ~eval__any :- !.
    normtype(none) := ~eval__none :- !.
    normtype(fnc(NA)) := ~eval__fnc(NA) :- !.
    normtype(fnc(NA, As)) := ~eval__fnc2(NA, ~normtypes(As)) :- !.
    normtype(fnc_var(N/A)) := ~eval__fnc2(N/A, ~var_args(A)) :- !.
    % TODO: make these types private... the user can only use fnc?
    normtype(atomic(X)) := ~eval__atomic(X) :- !.
    % TODO: document that 'list' cannot be removed because we need the 'str' domain
    normtype(list(As)) := ~eval__list1(~normtypes(As)) :- !.
    normtype(list) := ~eval__list :- !.
    normtype(str(NA, As)) := ~eval__str2(NA, ~normtypes(As)) :- !.
    normtype(str(NA)) := ~eval__str1(NA) :- !. % TODO: deprecate, use always a functor => functor will only be comparable with functor (not with any or []...)
    normtype(str) := ~eval__str0 :- !.
    normtype(var) := ~eval__var :- !.
    normtype(nonvar) := ~eval__nonvar :- !.
    normtype(float(X)) := ~eval__float1(X) :- !.
    normtype(float) := ~eval__float :- !.
    normtype(bignum(X)) := ~eval__bignum1(X) :- !.
    normtype(bignum) := ~eval__bignum :- !.
    normtype(smallint(X)) := ~eval__smallint1(X) :- !.
    normtype(smallint) := ~eval__smallint :- !.
    normtype(atom(X)) := ~eval__atom1(X) :- !.
    normtype(atom) := ~eval__atom :- !.
    normtype(reclist(Type)) := ~eval__reclist(Type) :- !.
    %        condtrace(['enorec ', reclist(Type)]).
    % user types
    % TODO: Type normalization should not expand those types... do it at expmt?
    normtype(unk) := ~modedtype.new_nva_f1(unk, ~modedtype.new_nva('$t_any')) :- !.
    normtype(Type) := ~modedtype.new_nva_f1(native, ~modedtype.new_nva(['$f'(Type, [])])) :-
            exp.ptoc_typeprop(Type, native, true),
            !.
    normtype(X) := ~expmt(~normtype(T)) :- !,
            T = ~exp.get_typedef(X).

    :- '$ctxprj'(eval__var/1, []).
    :- constant eval__var/1.
    eval__var := ~modedtype.new_var.
    :- '$ctxprj'(eval__nonvar/1, []).
    :- constant eval__nonvar/1.
    eval__nonvar := ~modedtype.new_nva('$t_any').
    :- '$ctxprj'(eval__float1/2, []).
    :- constant eval__float1/2.
    eval__float1(X) := ~modedtype.new_nva_f1(float, ~modedtype.new_nva(['$f'(X/0, [])])).
    :- '$ctxprj'(eval__float/1, []).
    :- constant eval__float/1.
    eval__float := ~modedtype.new_nva_f1(float, ~modedtype.new_nva('$t_any')).
    :- '$ctxprj'(eval__bignum1/2, []).
    :- constant eval__bignum1/2.
    eval__bignum1(X) := ~modedtype.new_nva_f1(bignum, ~modedtype.new_nva(['$f'(X/0, [])])).
    :- '$ctxprj'(eval__bignum/1, []).
    :- constant eval__bignum/1.
    eval__bignum := ~modedtype.new_nva_f1(bignum, ~modedtype.new_nva('$t_any')).
    :- '$ctxprj'(eval__smallint1/2, []).
    :- constant eval__smallint1/2.
    eval__smallint1(X) := ~modedtype.new_nva_f1(smallint, ~modedtype.new_nva(['$f'(X/0, [])])). 
    :- '$ctxprj'(eval__smallint/1, []).
    :- constant eval__smallint/1.
    eval__smallint := ~modedtype.new_nva_f1(smallint, ~modedtype.new_nva('$t_any')).
    :- '$ctxprj'(eval__atom1/2, []).
    :- constant eval__atom1/2.
    eval__atom1(X) := ~modedtype.new_nva_f1(atom, ~modedtype.new_nva(['$f'(X/0, [])])).
    :- '$ctxprj'(eval__atom/1, []).
    :- constant eval__atom/1.
    eval__atom := ~modedtype.new_nva_f1(atom, ~modedtype.new_nva('$t_any')).
%    :- '$ctxprj'(eval__reclist/2, []).
    :- constant eval__reclist/2.
    eval__reclist(Type) := ~modedtype.new_nva('$t_reclist'(~normtype(Type))).

    :- constant eval__fnc/2.
    eval__fnc(NA) := Z :-
            NA = N/A,
            Z = ( A = 0 ? ~eval__atomic(N)
		| N = '.', A = 2 ? ~eval__list
		| ~eval__str1(NA)
		).

    :- constant eval__fnc2/3.
    eval__fnc2(NA, As) := Z :-
            NA = N/A,
            Z = ( A = 0, As = [] ? ~eval__atomic(N)
		| N = '.', A = 2 ? ~eval__list1(As)
		| ~eval__str2(NA, As)
		).

    :- constant eval__atomic/2.
    eval__atomic(X) := ~expmt(~normtype(float(X))) :- is_float(X), !. 
    eval__atomic(X) := ~expmt(~normtype(bignum(X))) :-
            '$absmach'(Absmach),
            Absmach.is_bignum(X),
            !. 
    eval__atomic(X) := ~expmt(~normtype(smallint(X))) :- integer(X), !. 
    eval__atomic(X) := ~expmt(~normtype(atom(X))) :- atom(X), !. 

    :- constant eval__str0/1.
    eval__str0 := ~modedtype.new_nva_f1(str, ~modedtype.new_nva('$t_any')). 

    :- constant eval__str1/2.
    eval__str1(NA) := ~eval__str2(NA, ~any_args(A)) :- !, NA = _/A. % TODO: deprecate, use always a functor => functor will only be comparable with functor (not with any or []...)

    :- constant eval__str2/3.
    % TODO: postwiden_or_limit may be called too much times
    eval__str2(NA, As) := ~postwiden_or_limit(~modedtype.new_nva_f1(str, ~modedtype.new_nva(['$f'(NA, As)]))) :- !. % TODO: if '$t_any' of As is none, the type is none

    :- constant eval__list1/2.
    eval__list1(As) := ~postwiden_or_limit(~modedtype.new_nva_f1(list, ~modedtype.new_nva(['$f'('.'/2, As)]))) :- !.

    :- constant eval__list/1.
    eval__list := ~eval__list1([~eval__any,~eval__any]) :- !.

    % Meaning of representation
    % any              : top
    % ['$f'(K, A, As), ...] : or list
    % []               : bottom
    % <anything else>  : <user defined or builtin type>

    % expand one level of a reclist
    :- '$ctxprj'(t__expand1rec/2, []).
    :- static t__expand1rec/2.
    t__expand1rec(X1) := X2 :-
            ( X1 = '$t_reclist'(Type) ->
                XNil = ~modedtype.new_nva(['$f'([]/0,[])]),
                XCons = ~modedtype.new_nva(['$f'('.'/2, [Type, RecType])]),
                X2 = ['$f'(atom,[XNil]), '$f'(list, [XCons])],
                RecType = ~modedtype.new_nva('$t_reclist'(Type))
            ; X2 = X1
            ).

    :- '$ctxprj'(eval__any/1, []).
    :- static eval__any/1.
    eval__any := ~modedtype.new_any('$t_any').
    :- '$ctxprj'(eval__none/1, []).
    :- static eval__none/1.
    eval__none := ~modedtype.new_nva([]).

    :- '$ctxprj'(is_none/1, []).
    :- static is_none/1.
    is_none(X) :-
        trust(X instance_of modedtype),
        X.decomp(XVar, XNva), XVar = [], XNva = [].
    :- '$ctxprj'(is_any/1, []).
    :- static is_any/1.
    is_any(X) :-
	trust(X instance_of modedtype),
        X.decomp(XVar, XNva), XVar = '$_any', XNva = '$t_any'.

    :- '$ctxprj'(fnc_k1/2, []).
    :- static fnc_k1/2.
    fnc_k1(NA, K1) :-
            NA = N/A,
            K1 = ( A = 0 ? ~atomic_k1(N)
		 | N = '.', A = 2 ? list
		 | str
		 ).

    :- '$ctxprj'(atomic_k1/2, []).
    :- static atomic_k1/2.
    atomic_k1(X) := float :- is_float(X), !. 
    atomic_k1(X) := bignum :- '$absmach'(Absmach), Absmach.is_bignum(X), !. 
    atomic_k1(X) := smallint :- integer(X), !. 
    atomic_k1(X) := atom :- atom(X), !. 

    :- '$ctxprj'(any_args/2, []).
    :- static any_args/2.
    any_args(0) := [] :- !.
    any_args(N) := [~eval__any| ~any_args(N1)] :- N1 is N - 1.

    :- '$ctxprj'(var_args/2, []).
    :- static var_args/2.
    var_args(0) := [] :- !.
    var_args(N) := [~eval__var| ~var_args(N1)] :- N1 is N - 1.

    % type intersection
    % TODO: is postwiden needed after or and and??
    :- constant and/3.
    and(A, B) := A :- A == B, !. % make trivial case fast
    and(A0, B0) := Z :-
            A = ~expmt(A0),
            B = ~expmt(B0),
            A.decomp(AVar, ANva), B.decomp(BVar, BNva),
            ZVar = ~t__and0(AVar, BVar),
	    ZNva = ~t__and0(ANva, BNva),
	    Z = ( ZVar == AVar, ZNva == ANva ? A
		| ZVar == BVar, ZNva == BNva ? B
		| ~postwiden(~modedtype.new(ZVar, ZNva))
		).

    :- constant t__and0/3.
    t__and0('$t_any', A) := A :- !.
    t__and0(A, '$t_any') := A :- !.
    t__and0('$t_reclist'(X), '$t_reclist'(Y)) := Z :- !,
        Z0 = ~and(X, Y),
	Z = ( is_none(Z0) ? [] | '$t_reclist'(Z0) ).
    t__and0(A, B) := Z :-
        A2 = ~t__expand1rec(A),
	B2 = ~t__expand1rec(B),
	Z = ~t__and(A2, B2).

    :- constant t__and/3.
    t__and([], _) := [] :- !.
    t__and(_, []) := [] :- !.
    t__and([X|Xs], [Y|Ys]) := Zs :- !,
            X = '$f'(Kx, Tx),
            Y = '$f'(Ky, Ty),
            ( Kx @< Ky ->
                Zs = Zs1, Xs1 = Xs, Ys1 = [Y|Ys]
            ; Kx @> Ky ->
                Zs = Zs1, Xs1 = [X|Xs], Ys1 = Ys
            ; ( Tz = ~and_args(Tx, Ty) ->
                  Zs = ['$f'(Kx, Tz)|Zs1]
              ; Zs = Zs1
              ),
              Xs1 = Xs, Ys1 = Ys
            ),
            Zs1 = ~t__and(Xs1, Ys1).

    % note: fails if any resulting type is bottom
    :- constant and_args/3.
    and_args([], []) := [].
    and_args([X|Xs], [Y|Ys]) := [Z|~and_args(Xs, Ys)] :-
            Z = ~and(X, Y),
            \+ is_none(Z).

    %and(or($m_nonvar([$f(smallint,[$m_nonvar($t_any)])]),var),
    %         $m_nonvar([$f(smallint,[$m_nonvar($t_any)])]))

    % union of types
    % TODO: is postwiden needed after or and and??
    :- constant or/3.
    or(A, B) := A :- A == B, !. % make trivial case fast
    or(A0, B0) := Z :-
        A = ~expmt(A0),
        B = ~expmt(B0),
        A.decomp(AVar, ANva), B.decomp(BVar, BNva),
	ZVar = ~t__or0(AVar, BVar),
	ZNva = ~t__or0(ANva, BNva),
	Z = ( ZVar == BVar, ZNva == BNva ? B
	    | ZVar == AVar, ZNva == ANva ? A
	    | ~postwiden(~modedtype.new(ZVar, ZNva))
	    ).

    :- constant t__or0/3.
    t__or0('$t_any', _) := '$t_any' :- !.
    t__or0(_, '$t_any') := '$t_any' :- !.
    t__or0([], A) := A :- !.
    t__or0(A, []) := A :- !.
    t__or0(ANva, BNva) := ZNva :-
        ( ( ANva = '$t_reclist'(_) -> true
	  ; BNva = '$t_reclist'(_)
	  ),
	  ( ANva = '$t_reclist'(X) -> true
	  ; t__is_list1(ANva, X) -> true
	  ),
	  ( BNva = '$t_reclist'(Y) -> true
	  ; t__is_list1(BNva, Y) -> true
	  ) ->
	    ZNva = '$t_reclist'(~or(X, Y))
	; ANva = '$t_reclist'(_), t__is_list0(BNva) ->
	    ZNva = ANva
	; t__is_list0(ANva), BNva = '$t_reclist'(_) ->
	    ZNva = BNva
	; t__is_list0(ANva), t__is_list1(BNva, J) ->
	    ZNva = '$t_reclist'(J)
	; t__is_list1(ANva, J), t__is_list0(BNva) ->
	    ZNva = '$t_reclist'(J)
	%
	; ANva2 = ~t__expand1rec(ANva),
	  BNva2 = ~t__expand1rec(BNva),
	  ZNva = ~t__or(ANva2, BNva2)
	).

    :- constant t__or/3.
    t__or([], Xs) := Xs :- !.
    t__or(Xs, []) := Xs :- !.
    t__or([X|Xs], [Y|Ys]) := Zs :- !,
            X = '$f'(Kx, Tx),
            Y = '$f'(Ky, Ty),
            ( Kx @< Ky ->
                Zs = [X|Zs1], Xs1 = Xs, Ys1 = [Y|Ys]
            ; Kx @> Ky ->
                Zs = [Y|Zs1], Xs1 = [X|Xs], Ys1 = Ys
            ; Tz = ~or_args(Tx, Ty),
              % TODO: optim: if all are '$t_any', put an '$t_any'? (or similar)
              Zs = ['$f'(Kx, Tz)|Zs1], Xs1 = Xs, Ys1 = Ys
            ),
            Zs1 = ~t__or(Xs1, Ys1).

    :- constant or_args/3.
    or_args([], []) := [].
    or_args([X|Xs], [Y|Ys]) := [Z|~or_args(Xs, Ys)] :-
            Z = ~or(X, Y).

    % T is a type_t for lists of length 0 or 1 (with first element of type TElem and rest of type TRest)
    :- constant t__is_list01r/3.
    t__is_list01r(TX, Et, Rest) :-
            TX = ['$f'(atom,[XNil]), '$f'(list, [XCons])],
            trust(XNil instance_of modedtype),
            trust(XCons instance_of modedtype),
            XNil.decomp([], ['$f'([]/0,[])]),
            XCons.decomp([], ['$f'('.'/2, [Et, Rest])]).
    % T is a type_t for lists of length 1 (first element of type TElem)
    :- constant t__is_list1/2.
    t__is_list1(TX, TElem) :-
            TX = ['$f'(list, [TCons])],
            trust(TCons instance_of modedtype),
            trust(TNil instance_of modedtype),
            TCons.decomp([], ['$f'('.'/2, [TElem, TNil])]),
            TNil.decomp([], TTNil),
            t__is_list0(TTNil).
    % T is a type_t for lists of length 0 ([])
    :- constant t__is_list0/1.
    t__is_list0(TTNil) :-
            TTNil = ['$f'(atom, [TNil0])],
            trust(TNil0 instance_of modedtype),
            TNil0.decomp([], ['$f'([]/0, [])]).

    % :- public mode_is_var/1.
    :- constant mode_is_var/1.
    mode_is_var(A) :-
            A1 = ~expmt(A),
            A1.decomp('$t_any', []).

    % :- public mode_is_nonvar/1.
    :- constant mode_is_nonvar/1.
    mode_is_nonvar(A) :-
            A1 = ~expmt(A),
            A1.decomp([], _).

    % :- public may_be_var/1.
    :- constant may_be_var/1.
    may_be_var(A) :-
            A1 = ~expmt(A),
            A1.decomp('$t_any', _).

    % A is consequence of B <=> A = A/\B % (A is inside B)
    % :- public consequence/2.
    :- constant consequence/2.
    consequence(A0, B0) :-
            A = ~expmt(A0),
            B = ~expmt(B0),
            A.decomp(AVar, ANva), B.decomp(BVar, BNva),
	    t__consequence0(AVar, BVar),
	    t__consequence0(ANva, BNva).

    :- constant t__consequence0/2.
    t__consequence0(_, '$t_any') :- !.
    t__consequence0([], _) :- !.
    t__consequence0('$t_any', _) :- !, fail.
    t__consequence0(_, []) :- !, fail.
    t__consequence0('$t_reclist'(X), '$t_reclist'(Y)) :- !,
            consequence(X, Y).
    t__consequence0(A, B) :-
            A2 = ~t__expand1rec(A),
            B2 = ~t__expand1rec(B),
            t__consequence(A2, B2).

    :- constant t__consequence/2.
    t__consequence([], _) :- !.
    t__consequence(['$f'(Kx, _)|_], ['$f'(Ky, _)|_]) :- Kx @< Ky, !, fail.
    % Ky is not in X, ignore it and continue.
    t__consequence(['$f'(Kx, Tx)|Xs], ['$f'(Ky, _)|Ys]) :- Kx @> Ky, !,
            t__consequence(['$f'(Kx, Tx)|Xs], Ys).
    t__consequence(['$f'(Kx, Tx)|Xs], ['$f'(Ky, Ty)|Ys]) :- Kx == Ky, !,
            consequence_args(Tx, Ty),
            t__consequence(Xs, Ys).

    :- constant consequence_args/2.
    consequence_args([], []).
    consequence_args([X|Xs], [Y|Ys]) :-
            consequence(X, Y),
            consequence_args(Xs, Ys).

    % A is contradictory with B <=> A = A/\¬B
    % (A is outside B)
    % (every element x in A is not in B)
    % :- public contradictory/2.
    :- constant contradictory/2.
    contradictory(A0, B0) :-
            A = ~expmt(A0),
            B = ~expmt(B0),
            A.decomp(AVar, ANva), B.decomp(BVar, BNva),
	    t__contradictory0(AVar, BVar),
	    t__contradictory0(ANva, BNva).

    :- constant t__contradictory0/2.
    t__contradictory0(_, []) :- !.
    t__contradictory0([], _) :- !.
    t__contradictory0('$t_any', B) :- !, B = [].
    t__contradictory0(A, '$t_any') :- !, A = [].
    t__contradictory0('$t_reclist'(X), '$t_reclist'(Y)) :- !,
            contradictory(X, Y).
    t__contradictory0(A, B) :- !,
            A2 = ~t__expand1rec(A),
            B2 = ~t__expand1rec(B),
            t__contradictory(A2, B2).

    :- constant t__contradictory/2.
    t__contradictory(_, []) :- !.
    t__contradictory([], _) :- !.
    t__contradictory(['$f'(Kx, _)|Xs], ['$f'(Ky, Ty)|Ys]) :- Kx @< Ky, !,
            t__contradictory(Xs, ['$f'(Ky, Ty)|Ys]).
    % Ky is not in X, ignore it and continue.
    t__contradictory(['$f'(Kx, Tx)|Xs], ['$f'(Ky, _)|Ys]) :- Kx @> Ky, !,
            t__contradictory(['$f'(Kx, Tx)|Xs], Ys).
    t__contradictory(['$f'(Kx, Tx)|Xs], ['$f'(Ky, Ty)|Ys]) :- Kx == Ky, !,
            contradictory_args(Tx, Ty),
            t__contradictory(Xs, Ys).

    % just one element (i.e. dimension) will make the struct be contradictory
    :- constant contradictory_args/2.
    contradictory_args([X|_], [Y|_]) :-
            contradictory(X, Y), !.
    contradictory_args([_|Xs], [_|Ys]) :-
            contradictory_args(Xs, Ys).

    % types of arguments of the nonvar part of a type, for a particular functor (any or nva mode)
    % TODO: can be implemented more efficiently
    % :- public nonvar_args/3.
    :- constant nonvar_args/3.
    nonvar_args(X, NA) := As :-
            X2 = ~expmt(X),
            F1 = ~fnc_k1(NA),
            F2 = NA,
            X2.decomp(_, XNva),
	    ( XNva = '$t_any' -> % any k1 is possible
	        NA = _/A, As = ~any_args(A)
	    ; XNva2 = ~t__expand1rec(XNva),
	      nonvar_args_1(XNva2, F1, F2, As)
            ).

    :- static nonvar_args_1/4.
    nonvar_args_1([X|Ts], F1, F2, As) :-
            ( X = '$f'(F1, [X2]) ->
	        trust(X2 instance_of modedtype),
		X2.decomp(_, X2Nva),
                ( X2Nva = '$t_any' ->
                    % any functor is possible for that k1
                    F2 = _/A, As = ~any_args(A)
                ; As = ~nonvar_args_2(X2Nva, F2)
                )
            ; nonvar_args_1(Ts, F1, F2, As)
            ).

    :- static nonvar_args_2/3.
    nonvar_args_2([X|Ts2], F2) :=
            ( X = '$f'(F2, As0) ? As0
            | ~nonvar_args_2(Ts2, F2)
            ).

    % ---------------------------------------------------------------------------

    % list of basic types of @var{T} that can be used as indexing keys
    % :- public keytypes/2.
    :- constant keytypes/2.
    % :- fun keytypes :: term -> list(normtype).
    keytypes(Xs) := BasicTypes :-
            seen :: u_dic,
	    basictypes :: accum(BasicTypes),
	    keytypes__collect(Xs).
    {
    :- fluid seen :: u_dic.
    :- fluid basictypes :: accum.
    :- constant keytypes__collect/1.
    keytypes__collect([]).
    keytypes__collect([X|Xs]) :-
            keytypes__collect__2(X),
            keytypes__collect(Xs).

    :- constant keytypes__collect__2/1.
    keytypes__collect__2(X) :-
            call(( types :: accum(Ys), keytypes__split(X) )),
            keytypes__collect__3(Ys).

    :- constant keytypes__collect__3/1.
    keytypes__collect__3([]).
    keytypes__collect__3([X|Xs]) :-
            keytypes__collect__4(X),
            keytypes__collect__3(Xs).

    :- use_module(library(dict)).
    :- constant keytypes__collect__4/1.
    keytypes__collect__4(X) :-
            seen.lookup(~inverse(X), SeenType),
            ( var(SeenType) ->
                SeenType = yes,
                basictypes.add(X)
            ; true
            ).
    }.

    {
    :- fluid types :: accum.
    :- constant keytypes__split/1.
    keytypes__split(A0) :-
            A = ~expmt(A0),
            A.decomp(AVar, ANva),
            ( AVar = '$t_any', ANva = '$t_any' ->
	        true % TODO: nothing is known, it does not worth indexing...
            ; ( AVar = '$t_any' ->
	          types.add(~modedtype.new('$t_any', []))
	      ; true
	      ),
	      ( ANva = '$t_any' ->
		  % TODO: what to do with it?
		errlog:bug(['found \'$m_nonvar\'(\'$t_any\') in keytypes__split']), fail
	      ; true
	      ),
	      ANva2 = ~t__expand1rec(ANva),
	      t__keytypes_split(ANva2)
            ).

    :- constant t__keytypes_split/1.
    t__keytypes_split([]) :- !.
    t__keytypes_split(['$f'(K, [Y])|Xs]) :-
	    ( keytypes__indexable(K),
	      trust(Y instance_of modedtype),
	      Y.decomp(_YVar, YNva),
	      \+ YNva = '$t_any' ->
	        t__keytypes_split__2(YNva, K)
	    ; true
	    ),
            t__keytypes_split(Xs).

    % note: split_nonvar should never be feed with a empty list
    :- constant t__keytypes_split__2/2.
    t__keytypes_split__2([], _) :- !.
    t__keytypes_split__2([T|Ts], K) :- !,
            % this forgets about argument types if necessary
            Type = ( K = str, T = '$f'(N/A, _) ?
		       ~normtype(str(N/A))
		   | K = list, T = '$f'('.'/2, _) ?
		       ~normtype(list)
		   | ~modedtype.new_nva_f1(K, ~modedtype.new_nva([T]))
		   ),
            types.add(Type),
            t__keytypes_split__2(Ts, K).
    }.

    :- '$ctxprj'(keytypes__indexable/1, []).
    :- static keytypes__indexable/1.
    keytypes__indexable(float) :- !, fail. % floats are not indexable
    keytypes__indexable(bignum) :- !, fail. % bignums are not indexable
    keytypes__indexable(unboxed) :- !, fail. % unboxed types are not indexable
    keytypes__indexable(_).

    % decoded type
    % note: table is not used yet
    % note: not all possible A are covered
    % :- public inverse/2.
    :- constant inverse/2.
    inverse(A) := B :-
            A1 = ~expmt(A),
            A1.decomp(AVar, ANva),
            ( AVar = '$t_any', ANva = [] -> B = var
            ; AVar = [], ANva = ['$f'(N, [TI])],
	      trust(TI instance_of modedtype),
              TI.decomp(IVar, INva),
	      IVar = [],
              B = ( N = str, INva = ['$f'(SN/SA, _)] ? str(SN/SA)
		  | N = list, INva = ['$f'('.'/2, _)] ? list
		  | N = smallint, INva = ['$f'(F/0, _)] ? atomic(F)
		  | N = atom, INva = ['$f'(F/0, _)] ? atomic(F)
		  | N = float, INva = ['$f'(F/0, _)] ? atomic(F)
		  | N = bignum, INva = ['$f'(F/0, _)] ? atomic(F)
		  | N = native, INva = ['$f'(Type, _)] ? Type
		  | N = unk, INva = '$t_any' ? unk
		  )
            ).

    % :- public prop/3.
    :- constant prop/3.
    prop(Name, Type0) := Prop :-
            Type = ~inversenative(Type0),
            Exp = ~exp,
            Exp.ptoc_typeprop(Type, Name, Prop).

    % TODO: this version is used for unboxing
    % TODO: this is incomplete
    :- constant inversenative/2.
    inversenative(A) := B :-
            A1 = ~expmt(A),
            A1.decomp(AVar, ANva),
            ( AVar = '$t_any', ANva = [] -> B = var
            ; AVar = [], ANva = ['$f'(N, [TI])] ->
                ( N = smallint -> B = smallint
                ; N = atom -> B = atom
                ; N = float -> B = float
                ; N = bignum -> B = bignum
                ; N = unk -> B = unk
                ; N = native,
		  trust(TI instance_of modedtype),
		  TI.decomp(IVar, INva),
		  IVar = [], INva = ['$f'(Type, _)] ->
		    B = Type
                )
            ).

    }.
}. % end of sht_typedic

% Export wrappers for some typedic methods
% TODO: a default typedic is used (thus, typeof types cannot appear)
{
:- fluid exp :: module_exp.
:- public type_equal/2.
type_equal(A, B) :- missing_typedic(typedic.equal(A, B)).
:- public typemode_is_var/1.
typemode_is_var(A) :- missing_typedic(typedic.mode_is_var(A)).
:- public typemode_is_nonvar/1.
typemode_is_nonvar(A) :- missing_typedic(typedic.mode_is_nonvar(A)).
:- public typemode_may_be_var/1.
typemode_may_be_var(A) :- missing_typedic(typedic.may_be_var(A)).
:- public type_consequence/2.
type_consequence(A, B) :- missing_typedic(typedic.consequence(A, B)).
:- public type_contradictory/2.
type_contradictory(A, B) :- missing_typedic(typedic.contradictory(A, B)).
:- public type_nonvar_args/3.
type_nonvar_args(X, NA) := X2 :-
	missing_typedic(X2 = ~typedic.nonvar_args(X, NA)).
:- public type_inverse/2.
type_inverse(A) := A2 :-
	missing_typedic(A2 = ~typedic.inverse(A)).
:- public type_prop/3.
type_prop(Name, Type0) := Val :-
	missing_typedic(Val = ~typedic.prop(Name, Type0)).
:- public type_norm/2.
type_norm(A) := A2 :-
	missing_typedic(A2 = ~typedic.normtype(A)).
:- public map_type_norm/2.
map_type_norm([]) := [].
map_type_norm([X|Xs]) := [~type_norm(X) | ~map_type_norm(Xs)].

:- public aterm_type/2.
aterm_type(X) := ~X.getp(type) :- X instance_of termvar, !.
aterm_type(X) := ~type_norm(any) :- X instance_of termunk, !. % TODO: this is not really correct...
aterm_type(X) := T :-
	trust(X instance_of termstr),
	missing_typedic(T = ~typedic.eval__fnc2(~X.name, ~aterm_type_as(~X.args))).

aterm_type_as([]) := [].
aterm_type_as([X|Xs]) := [~aterm_type(X)| ~aterm_type_as(Xs)].
}.

% ---------------------------------------------------------------------------
% sht_equal: abstract domain for equality
% i.e. X=Y => X,Y are equal

:- class sht_equal {
    :- '$statemodel'(pair). % note: this module manages self directly
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    :- constructor empty_/0.
    empty_ :- self <- ~equiv_empty.

    set2(X, Y) :-
            self <- ~equiv_set2(X, Y, ~self).
    equiv_of(X) := ~equiv_of(X, ~self).

    :- constant equiv/2.
    equiv(Xt, Yt) :-
            equiv_in(Xt, Yt, ~self).
}.

% ---------------------------------------------------------------------------
% sht_share: Abstract domain for sharing 
% (X=f(Y) => X,Y shares; X=Y => X,Y shares)

:- class sht_share {
    :- '$statemodel'(pair). % note: this module manages self directly
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    :- constructor empty_/0.
    empty_ :- self <- ~equiv_empty.

    % TODO: replace setxs/1 by set/1, but fix ambiguity in mexpand
    setxs(Xs) :-
            self <- ~equiv_set(Xs, ~self).
    set2(X, Y) :-
            self <- ~equiv_set2(X, Y, ~self).
    equiv_of(X) := ~equiv_of(X, ~self).

    % Make each element in Nts share with Xt
    n_to_1([], _).
    n_to_1([Nt|Nts], Xt) :-
            set2(Nt, Xt),
            n_to_1(Nts, Xt).
}.

% ---------------------------------------------------------------------------
% sht_reldic: Abstract domain to store the relation between a
% structure and the possible arguments for each functor

:- class sht_reldic {
    :- '$statemodel'(pair). % note: this module manages self directly
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    :- constructor empty_/0.
    empty_ :- self <- ~keyval_empty.

    % Set type of X
    set(X, Value) :- self <- ~set_value(X, Value, ~self).

    % Get type of X
    :- constant get/2.
    get(X) := Value :- Value = ~get_value(X, ~self), !.
}.

% ---------------------------------------------------------------------------

{
:- fluid exp :: module_exp.

deflambda(PredId) := ~lambdatop(PredId).

lambdatop(PredId) := Lambda :-
	trust(PredId instance_of predicate_x),
        Modes = ~PredId.get_prop(argmodes),
        missing_typedic(CallTypes = ~typedic.top_call(Modes)),
        Lambda = CallTypes.

:- '$ctxprj'(lambdabottom/1, []).
lambdabottom('$bottom').

:- '$ctxprj'(bottom/1, []).
bottom := ~sht_d.bottom.

anot_lambda(G, Lambda) := ~G.set_args(~aterm_anot_types(~G.args, Lambda)) :-
	trust(G instance_of strgoal).

aterm_anot_types([], []) := [].
aterm_anot_types([X|Xs], [T|Ts]) := [~aterm_anot_type(X, T)| ~aterm_anot_types(Xs, Ts)].

%aterm_anot_type(X, Type) := X :- X instance_of termunk, !, typedic.is_any(Type). % TODO: this is not really correct...
aterm_anot_type(X, _Type) := X :- X instance_of termunk, !. % TODO: this is not really correct...
aterm_anot_type(X, Type) := Y :- X instance_of termvar, !,
        Y = ~X.addp(type, ~type_norm(Type)).
aterm_anot_type(X, Type) := Y :-
	trust(X instance_of termstr),
        NA = ~X.name,
        Type instance_of fnctype, ~Type.functor = NA,
        !,
        Y = ~X.set_args(~aterm_anot_types(~X.args, ~Type.args)).
aterm_anot_type(X, Type) := _ :-
        errlog:bug(['aterm ', X, ' cannot be annotated with type ', Type]),
        fail.

entry_dom(PredId, Args, AbsDef) := D :-
	D = ~sht_d.entry_dom(PredId, Args, AbsDef).

:- '$ctxprj'(bottom_absdef/2, []).
bottom_absdef(Lambda) := ~shtdef.new_bottom(Lambda).

set_usermemo(PredId, AbsDef) :-
	trust(PredId instance_of predicate_x),
        PredId.set_prop(sht_usermemo, AbsDef).

get_usermemo(PredId) := AbsDef :-
	trust(PredId instance_of predicate_x),
        ( AbsDef0 = ~PredId.get_prop(sht_usermemo) ->
            AbsDef = AbsDef0
        ; Modes = ~PredId.get_prop(argmodes),
	  AbsDef = ~shtdef.new_top(Modes)
        ).

% specialize (select other predicate based on lambda)
spec(Lambda, PredId, Xs, AbsDef, PredId2, Xs2) :-
        % TODO: improve this method to have multiple usermemo per predicate
        ( spec__1(Lambda, PredId, Xs, PredId2, Xs2, PreLambda) ->
            % TODO: select right LambdaPrime?
            AbsDef0 = ~get_usermemo(PredId2),
	    trust(AbsDef0 instance_of shtdef),
            % TODO: check Lambda0 precondition?
            AbsDef = ~shtdef.new(PreLambda, ~AbsDef0.exit_types)
        ; PredId2 = PredId,
          Xs2 = Xs,
          AbsDef = ~get_usermemo(PredId2)
        ).

spec__1(Lambda, PredId, Xs, PredId2, Xs2, PreLambda) :-
	trust(PredId instance_of predicate_x),
        SpecList = ~PredId.get_prop(specialize),
        spec__2(SpecList, Lambda, PredId, Xs, PredId2, Xs2, PreLambda).

spec__2([on(Pattern0, What)|_], Lambda, PredId, Xs, PredId2, Xs2, PreLambda) :-
	% TODO: Preprocess spec to normalize patterns
	trust(PredId instance_of predicate_x),
	missing_typedic(Pattern = ~typedic.normtypes(Pattern0)),
        missing_typedic(typedic.lambda_consequence(Lambda, Pattern)), !,
        % found a matching pattern, replace and spec again
        ( What = true ->
            PredId2 = ~predicate_x.reg_new('basiccontrol:true'/0),
            Xs2 = [],
            PreLambda = []
        ; What = rename(Name1) ->
            PredId1 = ~predicate_x.reg_new(Name1),
	    trust(PredId1 instance_of predicate_x),
            Name = ~PredId.name,
            Name1 = ~PredId1.name,
            Name = _/Arity,
            Name1 = _/Arity1,
            ( Arity1 == Arity ->
                true
            ; errlog:bug(['arity mismatch in spec rename from ',
                        ~~(Name), ' to ', ~~(Name1)])
            ),
            ( spec__1(Lambda, PredId1, Xs, PredId2, Xs2, PreLambda) ->
                true
            ; PredId2 = PredId1,
              Xs2 = Xs,
              PreLambda = Pattern
            )
        ; % TODO: should check that in compiler/frontend?
          errlog:temperror(['specified spec ', What, ' for predicate ', ~PredId.name, ' is unknown']),
          fail
        ).
spec__2([_|SpecList], Lambda, PredId, Xs, PredId2, Xs2, PreLambda) :-
        spec__2(SpecList, Lambda, PredId, Xs, PredId2, Xs2, PreLambda).

lambda_equal(A, B) :-
        ( lambdabottom(A) -> lambdabottom(B)
        ; lambdabottom(B) -> fail % A is not a lambdabottom
        ; missing_typedic(lambda_equal__2(A, B))
        ).

lambda_equal__2([], []) :- !.
lambda_equal__2([Ta|Tas], [T|Ts]) :-
        missing_typedic(typedic.equal(Ta, T)), !,
        lambda_equal__2(Tas, Ts).

lambda_lub(LambdaA, LambdaB, Lambda) :-
        ( lambdabottom(LambdaA) ->
            Lambda = LambdaB
        ; lambdabottom(LambdaB) ->
            Lambda = LambdaA
        ; missing_typedic(Lambda = ~typedic.map_or(LambdaA, LambdaB))
        ).

/*
% TODO: use?
lambda_consequence_(LambdaA, LambdaB) :-
        ( lambdabottom(LambdaA) ->
            true
        ; lambdabottom(LambdaB) ->
            fail
        ; lambda_consequence(LambdaA, LambdaB, [])
        ).
*/

% Guard key for indexing: the type that input needs so that the guard may succeed (i.e. if the input type is not the KeyType, then the guard always fail)
% TODO: do not include as a generic domain operation?!?! (we need the entry and exit)
get_key(EntryD, GuardD, Args, KeyType) :-
	trust(EntryD instance_of sht_d),
	trust(GuardD instance_of sht_d),
        % TODO: in the future use the first Arg to get the type in Lambda/LambdaPrime
        ( GuardD.is_bottom ->
	    KeyType = ~sht_typedic.eval__none
        ; % TODO: if the guard contains nonvar(X), then var can be removed from the keytype under some circumstances (look at the use of Auto and Trivial variables in bytecode__compiler)
	  Args = [Arg0|_],
	  CallArgType = ~EntryD.get_type(Arg0),
	  ExitArgType = ~GuardD.get_type(Arg0),
          call((
            d :: sht_d <- ~sht_d.top,
	    KeyType0 = ~d.typedic.and(~d.typedic.or(ExitArgType, ~d.typedic.eval__var), CallArgType),
	    KeyType = ~d.type_plain(KeyType0)
          ))
        ).

% Stop guard analysis
stop(guard, G) :-
        \+ is_guard_goal(G).

% TODO: do not allow more complex goals in guard analysis...
% TODO: do indexation looking at the bytecode...
%is_guard_goal(G) :-
%	trust(G instance_of strgoal),
%       ~G.predid = GId,
%       true = ~GId.get_prop(nosideeff),
%       !.
is_guard_goal(G) :- is_unify(G), !.
is_guard_goal(G) :- is_instance(G), !.
is_guard_goal(G) :-
	trust(G instance_of strgoal),
	~G.name = N/A, guard_goal__2(N, A), !.
%is_guard_goal(G) :- errlog:trace([notgg(G)]), fail.

% TODO: merge with definitions in bytecode__compiler
:- '$ctxprj'(guard_goal__2/2, []).
guard_goal__2('term_basic:$trust_type', 2).
guard_goal__2('basiccontrol:$caller_choice', 1).
guard_goal__2('term_typing:var', 1).
guard_goal__2('attributes:get_attribute', 2).
guard_goal__2('term_typing:nonvar', 1).
guard_goal__2('term_typing:atom', 1).
guard_goal__2('term_typing:atomic', 1).
guard_goal__2('term_typing:number', 1).
%
guard_goal__2('basiccontrol:fail', 0).
guard_goal__2('basiccontrol:true', 0).

:- use_module(compiler(ptoc__analyze), [analyze_guard/4]).

% Index predicate
% TODO: move prepro to other module
% TODO: use common representation for bytecode compiler and ptoc compiler of predicates, at least at the first functor, do not use aterm, use a tuple and store the name and the code+args 
% TODO: bytecode predicates are special, they support incremental definition!!! ptoc preds do not... try to add inc def in ptoc and closed def in bytecode

% TODO: this is very costly: specializes each clause list for each index key!
index(PredId) :-
	trust(PredId instance_of predicate_x),
        % TODO: put first the 'then' branch
        ( CompMode = ~PredId.get_prop(compmode),
          CompMode = lowcomp,
          Indexed = ~PredId.get_prop(indexed),
          Indexed = true,
	  Code = ~PredId.code,
	  Code = icode(a, Args, or(Cs)),
	  \+ Args = [],
          \+ Cs = [_] -> % note: only these predicates can be indexed...
            KeyTypes = ~get_keytypes(Cs, PredId),
            missing_typedic(BasicKeyTypes = ~typedic.keytypes(KeyTypes)),
            DefCs = ~default_clauses(Cs, KeyTypes, BasicKeyTypes),
            % TODO: do not do the index if Indexed = false or clauses = [_]
            ( distribute_badindex(BasicKeyTypes, DefCs) ->
                % not worth indexing (but at least, remove clauses that are going to fail in the guard, anyway)
                Cs2 = ~not_null_clauses(Cs, KeyTypes),
                PredId.set_code(icode(a, Args, or(Cs2)))
            ; call((
                preds :: accum(SubPreds),
                distribute(BasicKeyTypes, PredId, KeyTypes, Cases0),
                speckey(PredId, DefCs, DefGoal)
              )),
              Index = index(Cases0, [DefGoal]),
              exp.preddic__register_preds(SubPreds),
              PredId.set_code(icode(a, Args, Index))
            )
        ; true
        ).

get_keytypes([], _) := [].
get_keytypes([C|Cs], PredId) := [KeyType|~get_keytypes(Cs, PredId)] :-
        % TODO: get entry/exit using ptoc__analyze
        % TODO: why?
        % TODO: obtain a list of repeated any types (indeed, we should analyze, run index and reanalize) if the default entry does not exist and the predicate is not external
	intr :: absint <- sht_analyzer,
	AbsDef = ~intr.get_usermemo(PredId),
	analyze_guard(PredId, AbsDef, C, KeyType).

:- '$ctxprj'(distribute_badindex/2, []).
distribute_badindex(BasicKeyTypes, DefCs) :-
        ( BasicKeyTypes = [] ->
            true
        ; BasicKeyTypes = [_], DefCs = [] ->
            true
        ).

% TODO: classify each clause, instead of traversing the clause list for each basic key type
{
:- fluid preds :: accum.
distribute([], _, _, []) :- !.
distribute([BasicKeyType|BasicKeyTypes], PredId, Ks, [case(BasicKeyType, [Goal])|Cases]) :-
	trust(PredId instance_of predicate_x),
        Code = ~PredId.code,
        Code = icode(a, _Args, or(Cs)),
        Cs2 = ~filter_noncontradictory(Cs, Ks, BasicKeyType),
%        errlog:trace([dist_under(BasicKeyType, Cs2)]),  
        speckey(PredId, Cs2, Goal), % TODO: only spec the guard!
        distribute(BasicKeyTypes, PredId, Ks, Cases).

speckey(_, [], Goal) :- !,
        Goal = ~fail_new.
speckey(PredId, Clauses, Goal) :- !,
	trust(PredId instance_of predicate_x),
        Code = ~PredId.code,
        Code = icode(a, Args, _),
        AuxPredId = ~PredId.clone([indexed = false, do_not_check_events = true]),
        AuxPredId.set_code(icode(a, Args, or(Clauses))),
        preds.add(AuxPredId),
        Goal = ~strgoal.new_n(AuxPredId, Args).
}.

filter_noncontradictory([], [], _) := [] :- !.
filter_noncontradictory([C|Cs], [K|Ks], Type) := [C|~filter_noncontradictory(Cs, Ks, Type)] :-
        \+ missing_typedic(typedic.contradictory(K, Type)), !.
filter_noncontradictory([_|Cs], [_|Ks], Type) := ~filter_noncontradictory(Cs, Ks, Type).

default_clauses([], [], _) := [] :- !.
default_clauses([C|Cs], [K|Ks], KeyTypes) := [C|~default_clauses(Cs, Ks, KeyTypes)] :-
	not_consequence(KeyTypes, K), !.
default_clauses([_|Cs], [_|Ks], KeyTypes) := ~default_clauses(Cs, Ks, KeyTypes) :- !.

not_null_clauses([], []) := [] :- !.
not_null_clauses([C|Cs], [K|Ks]) := [C|~not_null_clauses(Cs, Ks)] :-
        missing_typedic(typedic.not_bottom(K)), !.
not_null_clauses([_|Cs], [_|Ks]) := ~not_null_clauses(Cs, Ks).

not_consequence([], _) :- !.
not_consequence([KeyType|KeyTypes], K) :-
        \+ missing_typedic(typedic.consequence(K, KeyType)),
        not_consequence(KeyTypes, K).

% ---------------------------------------------------------------------------
%
% Definition of a term:
% a) a variable is term
% b) a structure made with a name and a list of terms is a term.
%
% T is the set of all the terms.
%
% Definition of a type: 2^T (a boolean algebra with its usual operations /\ and \/).
% 
% The lattice relation A =< B is defined as A /\ B = A
% 
% Simplification of type checks:
%    Let A be the type of a variable X and C the checked type (if typeof X is C then ...).
%    C can be replaced by 'true' if A =< C  [A is consequence of C]
%    C can be replaced by 'false' if A =< not C  [A is contradictory with C]
%

% This implementation uses 'any' for anything or a list of pairs.

% ---------------------------------------------------------------------------

%condtrace(X) :- errlog:trace(X).
%condtrace(_).
%condtraceon(X) :- errlog:trace(X).

% ---------------------------------------------------------------------------

% TODO: This is the superclass of the three classes
:- static is_normtype/1.
:- '$ctxprj'(is_normtype/1, []).
is_normtype(X) :- X instance_of modedtype.
is_normtype(X) :- X instance_of vtype.
is_normtype(X) :- X instance_of fnctype.

:- class vtype {
    % Type associated to a variable
    % TODO: do the association as part of the abs domain, not the type
    :- '$statemodel'(single). % note: this module manages self directly
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    :- constructor new_/0.
    new_ :- ~self = typeof(_).
    :- constructor new_n_/1.
    new_n_(N) :- ~self = typeof(N).

    :- static meta_predicate instance_of__(out(vtype)).
    instance_of__(X) :- var(X), !, fail.
    instance_of__(typeof(_)).

    name := N :- ~self = typeof(N).
}.

:- class fnctype {
    :- '$statemodel'(single). % note: this module manages self directly
    :- '$raw_state'.

    % TODO: missing instance_of__/1

    :- constructor new_/2.
    new_(NA, Ts) :- ~self = '$fnc'(NA, Ts).

    :- static meta_predicate instance_of__(out(fnctype)).
    instance_of__(X) :- var(X), !, fail.
    instance_of__(X) :- X = '$fnc'(_, _).

    functor := NA :- ~self = '$fnc'(NA, _).
    args := Args :- ~self = '$fnc'(_, Args).
}.

% Moded types
:- class modedtype {
    :- '$statemodel'(single).
    :- '$raw_state'.
    
    % TODO: missing instance_of__/1

    :- constructor new_/2.
    new_(Var, Nva) :-
        ~self = Id,
        Id = ~'$mut__init'('$m'(_Name, Var, Nva)).

    :- constructor new_nva_/1.
    new_nva_(Nva) :- new_([], Nva).
    :- constructor new_var_/0.
    new_var_ :- new_('$t_any', []).
    :- constructor new_any_/1.
    new_any_(Nva) :- new_('$t_any', Nva).

    :- constructor new_nva_f1_/2.
    new_nva_f1_(F, T) :- new_([], ['$f'(F, [T])]).

    :- static meta_predicate instance_of__(out(modedtype)).
    instance_of__(Id) :-
        V = ~'$mut__value'(Id),
	nonvar(V), V = '$m'(_, _, _).

    decomp(Var, Nva) :-
        ~self = Id,
        '$m'(_Name, Var, Nva) = ~'$mut__value'(Id).
    name := Name :-
        ~self = Id,
        '$m'(Name, _, _) = ~'$mut__value'(Id).

    is_unktype :-
        decomp(Var, Nva),
	Var = [], Nva = ['$f'(N, _)],
	N = unk.
}.

% ---------------------------------------------------------------------------
% Dump analysis results

:- '$ctxprj'(get_dumper/1, []).
get_dumper(Dumper) :-
	Dumper = ~sht_dumper.new.

}.

:- class sht_dumper {
    :- extends abs_dumper.
    :- attr typecount :: m_int.
    :- attr seentypes :: u_dic.

    :- constructor new_/0.
    new_ :-
        ~typecount = 0,
	~seentypes = _.

    {
    :- fluid exp :: module_exp.
    pred(PredId) :-
        ShtDef = ~get_usermemo(PredId),
	trust(ShtDef instance_of shtdef),
        CallTypes = ~ShtDef.call_types,
        ExitTypes = ~ShtDef.exit_types,
	call((
	  pendingtypes :: accum(PendingTypes),
          display('  calltypes: '), dump_lambda(CallTypes), nl,
          display('  exittypes: '), dump_lambda(ExitTypes), nl
        )),
	display('Types: '), nl,
	pending_types(PendingTypes).

    pending_types([]) :- !.
    pending_types([X|Xs]) :- pending_type(X), pending_types(Xs).

    pending_type(X) :-
        trust(X instance_of modedtype),
        call((
          pendingtypes :: accum(PendingTypes),
          seentypes.get(~X.name, TypeNumber),
          display('  t'), display(TypeNumber), display(' := '),
          dump_type_def(X),
          nl
        )),
        pending_types(PendingTypes).

    {
    :- fluid pendingtypes :: accum.
    % dump type definition
    dump_type_def(X) :-
        trust(X instance_of modedtype),
        X.decomp(XVar, XNva),
	( XVar = '$t_any', XNva = '$t_any' ->
	    display('_')
	; XVar = '$t_any', XNva = [] ->
	    display('~var')
	; ( XVar = '$t_any' ->
	      display('~var'), display(' | ')
	  ; true
	  ),
	  ( XNva = '$t_reclist'(X2) -> display('#reclist('), type_dump(X2), display(')')
	  ; XNva = '$t_any' -> display('~nonvar')
	  ; t__dump(XNva)
	  )
	).

    % dump a lambda
    dump_lambda(X) :- atom(X), !, display(X).
    dump_lambda(X) :- t__dump_args(X).

    % dump type name and store in pending types if necessary
    type_dump(X) :-
        trust(X instance_of modedtype),
        TypeName = ~X.name,
        ( seentypes.get(TypeName, TypeNumber) ->
            true
        ; TypeNumber = ~typecount,
          typecount.inc(1),
          seentypes.lookup(TypeName, TypeNumber),
          pendingtypes.add(X)
        ),
        display('~t'), display(TypeNumber).

    t__dump_args(Xs) :- display('['), t__dump_args__2(Xs), display(']').

    t__dump_args__2([]) :- !.
    t__dump_args__2([X]) :- !, type_dump(X).
    t__dump_args__2([X|Xs]) :- type_dump(X), display(', '), t__dump_args__2(Xs).

    t__dump([]) :- !.
    t__dump(['$f'(Kx, Tx)]) :- !,
        t__dump_1(Kx, Tx).
    t__dump(['$f'(Kx, Tx)|Xs]) :-
        t__dump_1(Kx, Tx),
        display(' | '),
        t__dump(Xs).

    t__dump_1(N/0, _) :- !,
        display(N).
    t__dump_1(Kx, Tx) :-
        ( Kx = N/_ -> display(N) ; display('#'), display(Kx) ),
        display('('),
        t__dump_args__2(Tx),
        display(')').
    }.
    }.
}.

% ---------------------------------------------------------------------------
% A simple value/key table

{
:- '$all_static'.
% TODO: This should be a submodule
/*
keyval_empty := [].

% get value
get_value(X, Xs) := Value :- member(X0/Value, Xs), X0 == X, !.

% set value
set_value(X, Value, Xs0) := [X/Value|Xs] :- select(X0/_, Xs0, Xs), X0 == X, !.
set_value(X, Value, Xs) := [X/Value|Xs].
*/

keyval_empty := _.

% get value
get_value(X, Xs) := V :- V = ~dic_get(Xs, X), !.

% set value
set_value(X, Value, Xs0) := Xs :- Xs = ~dic_replace(Xs0, X, Value).
}.

% ---------------------------------------------------------------------------
% Equivalence classes (data structure used for sharing and equality)

% TODO: this implementation is not efficient
% TODO: implement another version that uses Prolog variables to implement the union-find algorithm (using mutables?)

{
:- '$all_static'.
% TODO: This should be a submodule
equiv_empty := [].

% make Xs be in the same equivalence class
equiv_set([], Eq) := Eq :- !.
equiv_set([_], Eq) := Eq :- !.
equiv_set([X, Y], Eq) := ~equiv_set2(X, Y, Eq) :- !.
equiv_set([X, Y|Ys], Eq) := ~equiv_set([Y|Ys], ~equiv_set2(X, Y, Eq)) :- !.

% make X and Y be in the same equivalence class
equiv_set2(X, Y, Eq) := Eq :-
        equiv_in(X, Y, Eq), !.
equiv_set2(X, Y, Eq0) := Eq :-
        X instance_of vtype, XName = ~X.name,
        Y instance_of vtype, YName = ~Y.name,
        ( inselect(XName, Eq0, XEquiv, Eq1) -> true ; XEquiv = [XName], Eq1 = Eq0 ),
        ( inselect(YName, Eq1, YEquiv, Eq2) -> true ; YEquiv = [YName], Eq2 = Eq1 ),
        XYEquiv = ~append(XEquiv, YEquiv),
        Eq = [XYEquiv|Eq2].

% as inmember 
inselect(X, Ss0, Xs, Ss) :- select(Xs, Ss0, Ss), eqmember(X, Xs).

% variables equivalent to X
% TODO: other way to do that: select an equiv type representative...
equiv_of(Xt, Eq) :=
        ( Xt instance_of vtype, Equiv0 = ~inmember(~Xt.name, Eq) ?
            ~to_typeof(Equiv0)
        | [Xt]
        ).

to_typeof([]) := [].
to_typeof([X|Xs]) := [~vtype.new_n(X)|~to_typeof(Xs)].

% X and Y are equivalent
equiv_in(Xt, Yt, Eq) :-
	Xt instance_of vtype, Yt instance_of vtype,
	XName = ~Xt.name, YName = ~Yt.name,
        ( XName == YName ->
            true
        ; Equiv = ~inmember(XName, Eq),
          eqmember(YName, Equiv)
        ).

% aux predicates

% member Xs so that X is eqmember of Xs
inmember(X, Ss) := Xs :- member(Xs, Ss), eqmember(X, Xs), !.

% exact (==/2) member 
eqmember(X, Xs) :- member(X0, Xs), X0 == X, !.

}.


