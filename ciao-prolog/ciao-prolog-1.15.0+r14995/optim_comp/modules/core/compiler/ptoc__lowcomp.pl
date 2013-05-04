:- module(_, [], [compiler(complang)]).

:- doc(title, "Low level code compiler").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module implements the translation of @em{Prolog}
   to a basic low level code. This code can be processed or optimized
   before being sent to a proper @em{backend} to generate an
   executable object or some other intermediate code.").

%:- use_module(library(format)).
:- use_module(library(dict)).
:- use_module(library(lists)).

:- use_module(compiler(errlog)).
:- use_module(.(module_exp)).
:- use_module(.(module_ipexp)).
:- use_module(.(ptoc__tbl)).
:- use_module(.(ptoc__props)).
:- use_module(.(ptoc__analyze)).
:- use_module(.(sht_analyzer)).
:- use_module(compiler(memoize)). % because of module_ipexp

:- use_module(compiler(meta_syntax)).

:- use_module(.(trivial_analyzer)).
:- use_module(.(idet_analyzer)).
:- use_module(.(ptoc__ins)).
:- use_module(.(ptoc__jump_opt)).

:- include(.(absint__interface)).

% TODO: Use input/output arguments from the beginning to avoid 'mode
%   filtering'?
%
% TODO: Create hash tables at compile time? To implement switch:
%   generate two versions, one with computed gotos and other with C
%   switch.
%
% TODO: Indexing where the clause of each key is a single call Do not
%   copy the clause: that should reduce the code size; leave the
%   current implementation that does key-specialization as a optional
%   compilation flag.
%
% TODO: Include predicate properties about 'liveregs'? Maybe at this
%   point all the code could be translated to clauses, without need of
%   using the pr representation.
%
% TODO: Move more analysis to high lever analysis.
%
% TODO: Generalization of C interface: checks and translations in
%   native boxed imptypes.
%
% TODO: Replace all the code list transformations by transform (done?).
%
% TODO: Ensure that no 'cond' is used.
%
% TODO: Replace Arity in some instructions (heap, gc) with a liveset
%   structure (partially done), since x registers may contain holes.
%
% TODO: Verify that optimal code is generated for the old choice_y (no
%   temporary unifications should be done).
%
% TODO: Variables with a temporal register as mem, whose lifetime does
%   not cross a call that requires and-binarization or does not cross
%   a possible garbage collection or stack expansion can be assigned a
%   local cvar(_) mem.

% ---------------------------------------------------------------------------
:- doc(subsection, "Add 'register' property").
% ---------------------------------------------------------------------------
% (i.e. the predicate must be registered in the predicate table)

{
:- fluid exp :: module_exp.

:- public reg_analyze/1.
reg_analyze([]) :- !.
reg_analyze([PredId|Xs]) :-
	trust(PredId instance_of predicate_x),
	( CompMode = ~PredId.get_prop(compmode) ->
	  ( CompMode = bytecode ->
	      icode(a, _, or(Cs)) = ~PredId.code,
	      % TODO: index is ignored??!
	      reg_analyze_cs(Cs)
          ; CompMode = lowcomp ->
	      ( vs_public = ~PredId.get_prop(visibility) ->
		  PredId.set_prop(register, true)
	      ; true
	      )
	  ; true
          )
	; true
	),
	reg_analyze(Xs).

reg_analyze_cs([]) :- !.
reg_analyze_cs([X|Xs]) :- reg_analyze_body(X), reg_analyze_cs(Xs).

reg_analyze_body([]) :- !.
reg_analyze_body([X|Xs]) :- reg_analyze_body__2(X), reg_analyze_body(Xs).

reg_analyze_body__2(G) :-
	trust(G instance_of strgoal),
	GId = ~G.predid,
	~G.name = MF/A,
	% TODO: replace by 'local defined'
	( exp.expanded_decl(MF, A, _) -> true ),
	CompMode = ~GId.get_prop(compmode),
	CompMode = lowcomp,
	!,
	GId.set_prop(register, true).
reg_analyze_body__2(_).
}.
	
% ---------------------------------------------------------------------------

numbers(A, A) := [] :- !.
numbers(A0, A) := [A0|~numbers(A1, A)] :- A1 is A0+1.

max(A, B) := A :- A >= B, !.
max(_, B) := B.

min(A, B) := A :- A =< B, !.
min(_, B) := B.

% ---------------------------------------------------------------------------
:- doc(subsection, "Indexing").
% ---------------------------------------------------------------------------

% The indexing over the first argument (X0) 
% -------------------------------------------
%
% Let P be the predicate:  P :- C1 ; C2 ; C3 ... ; Cn
% Where each clause Ci is a list of literals. 
%
% The key of a clause is the type of the argument X0 just before a
% literal with side-effects or a cut is executed.
%
% The indexing creates a tree-like structure which given the type of
% de argument X0, goes straight to the list of compatible clauses.
% 
% Thus, the indexing has these steps:
%
%  1 - analysis of the type of the first argument until a side-effect
%      literal or cut (will be the clause key)
%
%  2 - creation of the tree-like structure with the collected keys
%
%  3 - distribution of the clauses along the index tree (preserving
%      the order) [a clause is inserted in a leaf if the clause key is
%      not contradictory with the leaf key]
%
% The type used as key must have a granularity optimal for generating
% a O(1) clause dispatching code.
%
% [hint: improved analysis improves the indexing; the analysis could
% be done by ciaopp?]
%
% [uses the same information that is needed for the specialization]
%
% The tree has a default case which contais all the clauses whose key
% is consequence of neither of the index keys.

% 'Versioning'
% ------------
%
% With 'key-versioning', a new clause is generated for each tree leaf
% and the leaf key is added to the X0 argument. In this way, it is
% ensured that no duplicated work is made.
%
% With 'alternative-versioning', the next alternative is fixed (useful
% for direct fails).
%
% [implementation state: both; disadvantages: bigger code]
%
% TODO: Add option to activate/deactivate (per predicate).

% ---------------------------------------------------------------------------
:- doc(section, "Predicate compilation").
% ---------------------------------------------------------------------------

{
:- fluid exp :: module_exp.
:- fluid preds :: accum.

:- public lowcomp_pred/1.
lowcomp_pred(PredId0) :-
	trust(PredId0 instance_of predicate_x),
	call((
	  maybeproceed :: any <- no,
          PredsB3 = ~lowcomp_pred__2(PredId0)
        )),
	MainPredName = ~PredId0.name,
        call((
          allpreds :: any <- PredsB3,
	  subpreddic :: any,
          ptoc__jump_opt:analyze_subpreds(MainPredName),
	  PredsB4 = ~(ptoc__jump_opt:unfold_subpreds(PredsB3))
        )),
	% Lowcomp prepass before ImProlog compilation
	% TODO: see prepass_preds' todo
	prepass_preds(PredsB4),
	emit_preds(PredsB4).

{
:- fluid maybeproceed :: any.
:- '$ctxprj'(lowcomp_pred__2/2, [exp, maybeproceed]).
lowcomp_pred__2(PredId0) := Preds5 :-
	p :: predpass <- ~predpass_g.new,
	p.transform(PredId0),
	call(( preds :: accum(Preds5), binarize_or(PredId0) )),
	!.
lowcomp_pred__2(PredId0) := _ :-
	trust(PredId0 instance_of predicate_x),
	errlog:bug(['lowcomp_pred__2 failed for ', ~PredId0.name]),
	fail.
}.

:- '$ctxprj'(emit_preds/1, [preds]).
emit_preds([]).
emit_preds([X|Xs]) :- preds.add(X), emit_preds(Xs).
}.

% ---------------------------------------------------------------------------
:- doc(section, "Generic predicate compilation pass").
% ---------------------------------------------------------------------------

:- class predpass {
    {
    :- virtual prev/1.
    :- virtual curr/1.
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid maybeproceed :: any.
    transform(PredId) :-
	trust(PredId instance_of predicate_x),
	%
	icode(Mode, _, _) = ~PredId.code,
	% Make sure that PredId is at input stage
        InputMode = ~prev,
	( Mode = InputMode ->
	    true
	; call(( 
 	    p :: predpass <- ~get_predpass(InputMode),
	    p.transform(PredId)
	  ))
	),
	% Transform PredId
        call((
          altstate :: m_any,
	  common :: any,
          (Args2, Code1) = ~x_prepred(PredId),
          Code = ~x_predcode(Code1)
        )),
        TargetMode = ~curr,
        PredId.set_code(icode(TargetMode, Args2, Code)).
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid common :: any.
    {
    :- fluid altstate :: m_any + u.
    x_predcode(or(Cs)) := or(~x_alts__2(Cs)) :- !.

    x_alts__2([]) := [].
    x_alts__2([C0|Cs0]) := [C|Cs] :- !,
	lastalt :: any <- ( Cs0 = [] ? yes | no ),
	C = ~x_alt(C0),
	Cs = ~x_alts__2(Cs0).

    x_cases([]) := [] :- !.
    x_cases([case(KeyType, Cs)|Xs0]) := [case(KeyType, Code)|Xs] :-
	Code = ~x_case(Cs), Xs = ~x_cases(Xs0).

    x_case(Cs) := Code :- or([Code]) = ~x_predcode(or([Cs])).

    x_switch__2(I0) := I :-
	I0 = switch_on_index(A, Cases0, DefCode0),
	!,
	trust(Cases0 instance_of termunk),
	trust(DefCode0 instance_of termunk),
	Cases1 = ~x_cases(~Cases0.value),
	DefCode1 = ~x_case(~DefCode0.value),
	I = switch_on_index(A, ~termunk.new(Cases1), ~termunk.new(DefCode1)).
    x_switch__2(I) := I.

    % like prmap, but applying x_switch__2
    prmap_sw(c(d(Ia, Ib), Xs)) := c(d(~prmap_sw(Ia), ~prmap_sw(Ib)), ~prmap_sw(Xs)) :- !.
    prmap_sw(c(s(X), Xs)) := c(s(~x_switch__2(X)), ~prmap_sw(Xs)) :- !.
    prmap_sw(t(Cont)) := t(Cont).
    }.

    {
    :- fluid altstate :: m_any.
    :- fluid lastalt :: any.
    :- virtual x_alt/2.
    }.
    {
    :- fluid altstate :: m_any + d.
    :- fluid maybeproceed :: any.
    :- virtual x_prepred/2.
    }.
    }.
}.

% Get the predicate pass that produces the required mode
:- discontiguous get_predpass/2.

% ---------------------------------------------------------------------------
:- doc(section, "Generate initial low level code").
% ---------------------------------------------------------------------------

get_predpass(c) := ~predpass_c.new.
:- class predpass_c {
    :- extends predpass.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    :- static prev/1.
    prev := a.
    :- static curr/1.
    curr := c.

    {
    :- fluid exp :: module_exp.
    :- fluid maybeproceed :: any.
    :- fluid altstate :: m_any + d.
    :- fluid common :: any.
    x_prepred(PredId) := (Args2, Code1) :-
	trust(PredId instance_of predicate_x),
	icode(_, Args0, Code0) = ~PredId.code,
	Modes = ~PredId.get_prop(argmodes),
	Mems = ~callee_argmems(PredId),
	% TODO: use argunboxs!!!!!!
	% TODO: do not read this!! set definition here (or even in backend_c)!!
	ImpTypes = ~PredId.get_prop(argimptypes),
	InMems = ~filter_mode(Mems, Modes, in),
	InImpTypes = ~filter_mode(ImpTypes, Modes, in),
	In0 = ~var_new_args(InMems, InImpTypes),
	call((
          intr :: absint <- sht_analyzer,
	  ShtDef = ~intr.get_usermemo(PredId),
	  trust(ShtDef instance_of shtdef)
        )),
	CallTypes = ~ShtDef.get_call_types,
	Args = ~ziptypes(Args0, CallTypes),
	InTypes = ~filter_mode(CallTypes, Modes, in),
	In = ~ziptypes(In0, InTypes),
	%
	% TODO: Types of CodeIn1 have no type (will be typed as var later)
	CodeIn1 = ~filter_mode(Args0, Modes, in),
	Out0 = ~filter_mode(Args, Modes, out), % TODO: right types?
	ProceedPredId = ~PredId.new_proceed,
	Argspr = argspr(In, CodeIn1, Out0, ~maybeproceed, ProceedPredId),
	%
        Code1 = ( Code0 = index(_, _) ? or([[Code0]]) | Code0 ),
	Args2 = ~replace_mode(Args, Modes, in, In),
	altstate <- altstatec(yes),
	~common = common(Argspr).
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid lastalt :: any.
    :- fluid altstate :: m_any.
    :- fluid common :: any.
    x_alt([index(Cases0, DefCs)]) := c0c(Code, no_explicit_cut) :- !,
	% TODO: put A inside index structure (remember that it has to be updated in analysis...)
	~common = common(argspr([A|_], _, _, _, _)),
	Code = ~single(~x_switch__2(switch_on_index(A, ~termunk.new(Cases0), ~termunk.new(DefCs)))),
	altstate <- altstatec(no).
    x_alt(Code1b) := Code :-
	~altstate = altstatec(FirstAlt),
	%
	~common = common(argspr(In, CodeIn1b, CodeOutb, MaybeProceed, ProceedPredId)),
	% TODO: kludge: do not share any variable between alternatives (if not, the same variable may have different mem assignments in the generated code, and impcomp does not like it)
	( FirstAlt = no, ~lastalt = yes ->
	    add_last_cut(Code1b, Code1b0)
	; Code1b0 = Code1b
	),
	copy_term_shattr(ttt(Code1b0, CodeIn1b, CodeOutb), ttt(Code1, CodeIn1, CodeOut)),
	% TODO: merge the following two preds
	get_choice(Code1, MaybeChoice),
	analyze_cut(MaybeChoice, Code1, AbsCut),
        Proceed0 = ~strgoal.new_n(ProceedPredId, CodeOut),
	Proceed = ~Proceed0.addp(proceed_at, MaybeProceed),
	Code2 = ~append(Code1, [Proceed]),
	Code30 = ~gen_code(In, CodeIn1, Code2),
	Code = c0c(Code30, AbsCut),
	altstate <- altstatec(no).
    }.
}.

var_new_args([], []) := [] :- !.
var_new_args([R|Rs], [IT|ITs]) := [~var_new_arg(R, IT)|~var_new_args(Rs, ITs)] :- !.

var_new_arg(Mem, IT) := V :-
	Mem2 = ( Mem = cvar ? cvar(_)
	       | Mem = cvar(Name) ? cvar(Name)
	       | Mem
	       ),
	V0 = ~termvar.new,
	V1 = ~V0.addp(mem, Mem2),
	V = ~V1.addp(imptype, IT).

% TODO: Make binarize work with code before onlyregs, do onlyregs as
%   last pass, change emit_emulator and foreign_interface to use the c
%   backend, unify analysis and preprocess of bytecode and ptoc pred.

% ---

% Add a cut in the last clause.
% TODO: Do this in compiler__expand instead?
{
:- fluid exp :: module_exp.
add_last_cut(LastC0, LastC) :-
	( LastC0 = [G|C00],
	  trust(G instance_of strgoal),
	  ~G.name = 'basiccontrol:$caller_choice'/1 ->
	    ~G.args = [Choice0],
	    trust(Choice0 instance_of termvar),
	    Choice = ~Choice0.addp(type, ~type_norm(smallint)),
	    LastC = [G, ~cut_new(Choice)|C00]
	% TODO: This last cut is necessary even if we fail (to remove
	%   the choice point). Remove those lines if everything is OK.
%	; LastC0 = [G|_],
%         trust(X instance_of strgoal),
%	  ~G.name = 'basiccontrol:fail'/0 ->
%	    % nothing if we fail
%	    LastC = LastC0
	; Choice0 = ~termvar.new,
	  Choice = ~Choice0.addp(type, ~type_norm(smallint)),
	  G = ~caller_choice_new(Choice),
	  LastC = [G, ~cut_new(Choice)|LastC0]
	).
}.

{
:- fluid exp :: module_exp.
get_choice([G|_], just(Choice)) :-
	trust(G instance_of strgoal),
	~G.name = 'basiccontrol:$caller_choice'/1,
	!,
	~G.args = [Choice].
get_choice(_, no).

% key used for indexing
type_to_key(KeyType) := Key :-
	Type = ~type_inverse(KeyType),
	( Type = atomic(Cons) ->
	    '$absmach'(Absmach),
	    Absmach.functorcons(Cons/0, Key)
	; Type = str(Functor) ->
	    '$absmach'(Absmach),
	    Absmach.functorcons(Functor, Key)
	; Type = var ->
	    Key = var
	; Type = list ->
	    Key = list
	; errlog:bug(['cannot get key from type ', KeyType]), fail
	).
}.

% If the predicate leaves a choice point on exit, it is none of the
% predicate choice points (Precondition: the clause have cuts).
%
% TODO: Use pr and move just before g_code? (not sure).
% TODO: Rename AbsCut by 'preserves choice points'
%
% Analyze the uses of cut:
%
%  - cut_to_previous_choice: the first cut in the clause uses
%    previous_choice.
%
%  - cut_to_other_choice: the first cut does not use previous_choice
%    (since the cut updates previous_choice, later cuts may use
%    previous_choice or other choice without any problems).
%
%  - no_cut: there is no explicit cut in the clause (but called
%    predicates may cut the current choice using meta cuts)
{
:- fluid exp :: module_exp.
analyze_cut(MaybeChoice, Code) :=
	( Cut = ~first_cut(Code),
	  trust(Cut instance_of strgoal) ?
	    ( MaybeChoice = just(Choice),
	      trust(Choice instance_of termvar),
	      ~Cut.args = [Choice0],
	      trust(Choice0 instance_of termvar),
	      ~Choice0.name == ~Choice.name ?
	        cut_to_previous_choice
	    | cut_to_other_choice
	    )
	| no_explicit_cut
	).

first_cut([G|_]) := G :-
	trust(G instance_of strgoal),
	~G.name = 'basiccontrol:$cut'/1, !.
first_cut([_|Gs]) := ~first_cut(Gs).
}.

% Add the mem and grp attributes

{
:- fluid memdic :: u_dic.
add_mem_goal('$sub$'(G, PredId)) := '$sub$'(~add_mem_goal(G), PredId) :- !.
add_mem_goal(X) := X :- X instance_of termunk, !.
add_mem_goal(X) := ~X.addp(mem, Mem) :- X instance_of termvar, !,
	memdic.lookup(~X.name, Mem).
add_mem_goal(X) := ~X.set_args(~add_mem_xs(~X.args)) :-
	trust(X instance_of strgoal).

add_mem_xs([]) := [].
add_mem_xs([G|Gs]) := [~add_mem_x(G)|~add_mem_xs(Gs)].

add_mem_x(X) := X :- X instance_of termunk, !.
add_mem_x(X) := ~X.addp(mem, Mem) :- X instance_of termvar, !,
	memdic.lookup(~X.name, Mem).
add_mem_x(X) := ~X.set_args(~add_mem_xs(~X.args)) :-
	trust(X instance_of termstr).
}.

:- class initdic {
    % Dictionary to annotate initialized variables during code
    % generation.
    :- '$statemodel'(single).
    :- '$raw_state'.

    :- constructor empty_/0.
    empty_.

    mark_list([]).
    mark_list([X|Xs]) :- mark(X), mark_list(Xs).

    mark(U) :- U instance_of termvar, !, dic_lookup(~self, ~U.name, yes).
    mark(_).

    % Get the initialization state of the variable.
    marked(U) := true :-
	trust(U instance_of termvar),
	_ = ~dic_get(~self, ~U.name), !.
    marked(_) := false :- !.
}.

% Flat a list of goals: only very simple (annotated) unifications.

:- op(650, yfx, [(.&.)]).
:- fun_eval((.&.)/2).
A .&. B := ~compose(A, B).
nop := ~cont(success).

{
:- fluid exp :: module_exp.
gen_code(In, IArgs0, Gs) := Code :-
	memdic :: u_dic,
	initd :: initdic <- ~initdic.empty,
	IArgs = ~add_mem_xs(IArgs0),
	initd.mark_list(In),
	% TODO: is reverse needed?
	call((
	  code :: accum(Code0b),
          get_args(~ziptypesvar(~reverse(IArgs)), ~reverse(In)),
	  gen_goals(Gs)
        )),
	% Remove dead code (code after fail) % TODO: is that necessary??? analysis should do that... right?
	(Code1, FailCode) = ~forward_takeneg(~compl(Code0b), ins_is_fail),
	Code = ( FailCode = ~nop ? Code1 | Code1 .&. ~single(call(~fail_new)) ).

ins_is_fail(I) :- ~ins_name(I) = 'basiccontrol:fail'/0.

ins_is_proceed(call(G)) :-
	true = ~goal_prop(is_proceed, G).
}.

{
:- fluid exp :: module_exp.
% TODO: Precondition: all elements in the list are variables
%       <- not really.
ziptypes([], []) := [] :- !.
ziptypes([X|Xs], [T|Ts]) := [~X.addp(type, T)|~ziptypes(Xs, Ts)] :- X instance_of termvar, !.
ziptypes([X|Xs], [_|Ts]) := [X|~ziptypes(Xs, Ts)].
}.

{
:- fluid exp :: module_exp.
% TODO: Precondition: all elements in the list are variables
ziptypesvar([]) := [].
ziptypesvar([X|Xs]) := [~X.addp(type, ~type_norm(var))|~ziptypesvar(Xs)] :-
	trust(X instance_of termvar).
}.

{
:- fluid exp :: module_exp.
map_aterm_type([]) := [].
map_aterm_type([X|Xs]) := [~aterm_type(X)|~map_aterm_type(Xs)].
}.

{
:- fluid exp :: module_exp.
:- fluid code :: accum.
:- fluid initd :: initdic.
:- fluid memdic :: u_dic.
gen_goals([]).
gen_goals([G|Gs]) :-
	( gen_goal(G) ->
	    true
	; errlog:bug(['gen_goal failed for ', G]), fail
	),
	gen_goals(Gs).

gen_goal(G) :-
	trust(G instance_of strgoal),
	~G.name = 'term_basic:$varmem'/2, !,
	~G.args = [X, Mem],
	X2 = ~add_mem_x(X),
	% TODO: Not really a inline C (unk) value but a memory value.
	trust(X2 instance_of termvar),
	trust(Mem instance_of termunk),
	~X2.getp(mem) = ~Mem.value.
gen_goal(G) :- _ = ~trust_domain(G), !.
gen_goal(G) :- is_unify(G), !,
	G2 = ~add_mem_goal(G),
	trust(G2 instance_of strgoal),
	~G2.args = [U, V],
	gen_unify(U, V).
gen_goal(G) :- is_instance(G), !,
	G2 = ~add_mem_goal(G),
	trust(G2 instance_of strgoal),
	~G2.args = [U, V],
	gen_instance(U, V).
gen_goal(G) :-
	G2 = ~add_mem_goal(G),
	gen_call(G2).
}.

% umode (for structure flatting):
% - get is a top-down flat: better for reading a structure
%     X=f(a) ----> X=f(T),T=a
% - put is a bottom-up flat: better for writing a structure
%     X=f(a) ----> T=a,X=f(T)

% TODO: An improved unification optimizer [READ AGAIN]
%   With the current compilator the generated code of:
%     p :- X=[1,2,3], g(X).
%   and
%     p :- g([1,2,3]) may differ.
%
%   The idea for an improved compilator is to, given a sequence of
%   unordered basic subsitutions (X<-f(A,B...) or X<-atom or X<-Y)
%   generate an optimal flatted sequence.
%
%   The optimal flatted sequence is described by this rules:
%   (notation: ti is a term, X is a variable, f is a functor, a is an
%   atom)
%
%     - if X is known to be fresh it's better to move all the
%       Y<-... before X<-f(..., Y, ...)
%
%     - if X is not known to be fresh (is not fresh or nothing is
%       known) it's better to move all the Y<-... after X<-f(..., Y,
%       ...)
%
%   Collapsing rules:
%     - replace X by a if X<-a and X is an argument of a structure.
%     - X<-f(..., Z), Z<-t goes X<-f(..., t)
%     [and more...]

{
:- fluid exp :: module_exp.
:- fluid initd :: initdic.
:- fluid code :: accum.
gen_instance(U, V) :-
	% Use constructor that remembers unboxed value.
	%
	% Note: that constructor is not useful in deeper instances
	%   because we do not have access to the variable.
	%
	% TODO: for homogeneity, change something in how other types
	%   (e.g. floats) are build or consulted in deeper instances?
        exp.pragma(unbox_cons),
	trust(V instance_of termstr),
	'$absmach'(Absmach),
	Absmach.functorcons(~V.name, FC),
	functorcons__is_constant(FC),
	% TODO: hardwired!!
	FC = float(Value),
	!,
	( Cons = ~type_prop(cons, ~type_norm(float)) ->
	    true
	; errlog:bug(['cons for ', float, ' not found'])
	),
	ConsId = ~predicate_x.reg_new(Cons),
	gen_call(~strgoal.new_n(ConsId, [~termunk.new(Value), U])).
gen_instance(U, V) :-
	Is = ~flat_instance(U, V),
	gen_instance__2(Is).

gen_instance__2([]) :- !.
gen_instance__2([X|Xs]) :- gen_flatinstance(X), gen_instance__2(Xs).

% TODO: Do not split unifications, just put together and use the
%   following rule:
%     x x x x y y y z z z z z 
%   can be grouped as
%     x x x x | y y y | z z z z z
%   if y does not use the mode and z does set or forget it (or there
%   is no z).
%
% TODO: Flat before code generation; flat before analysis if can
%   reorder the unifications later.
%
gen_flatinstance(u([A-B|Xs])) :-
	Conts = ( Xs = [] ? ~successconts | ~anyconts ),
	call((
          code :: accum(Code0),
	  instance2i(A, B, Conts),
	  gen_flatinstance__2(Xs)
        )),
	code.add(comprw(Code0)).

instance2i(A, B, Conts) :-
	instance2i_3(A, ~initd.marked(A), B, Conts),
	initd.mark_list([A]).

gen_flatinstance__2([A-B|Xs]) :- !,
	Conts = ( Xs = [] ? ~successconts | ~anyconts ),
	( B instance_of termvar ->
	    InitB = ~initd.marked(B),
	    call((
              code :: accum(ReadCode),
	      gen_unify__3(A, true, B, InitB),
	      setmode(read, Conts)
	    )),
	    call((
              code :: accum(WriteCode),
	      trust(A instance_of termvar),
              gen_unify__3(~A.addp(type, ~type_norm(var)), false, B, InitB),
	      setmode(write, Conts)
	    )),
	    Inits = [A, B]
	; call(( code :: accum(ReadCode), instance2i_3(A, true, B, Conts) )),
	  call(( code :: accum(WriteCode), instance2i_3(A, false, B, Conts) )),
	  Inits = [A]
	),
	initd.mark_list(Inits),
	code.add(moderw(ReadCode, WriteCode)),
	gen_flatinstance__2(Xs).
gen_flatinstance__2([]).
}.

% Split the unification is a list of pairs A-B, where B can be an
% structure with free variables or other variable.
{
:- fluid exp :: module_exp.
flat_instance(U, V) := Is :-
	% Note: 'put' mode is used only when we know that it will not
	% fail, we do not want to build a big term and then discard it
	% (that is why the more relaxed condition that allows other
	% types not compatible with the functor of V is not used here)
	trust(U instance_of termvar),
	FlatMode = ( typemode_is_var(~U.getp(type)) ? put | get ),
	flat_instance_2(U, V, FlatMode, Is1, Hs),
	Stem = u(Hs),
	Is = ( FlatMode = get ? ~append([Stem], Is1) | ~append(Is1, [Stem]) ).

% TODO: unif__2 in 'sht_analyzer' shares a lot of things with this code.
flat_instance_2(V, X, FlatMode, Is1, Hs) :-
	% VType should be the type of V after doing functor(V,N,A) where {X.get_functor(N, A)}
	trust(V instance_of termvar),
	Vt = ~V.getp(type),
	trust(X instance_of termstr),
	XFunctor = ~X.name,
	VMayBeVar = ( typemode_may_be_var(Vt) ? true | false ),
        ( Ats = ~type_nonvar_args(Vt, XFunctor) -> true ; Ats = no ),
	( VMayBeVar = true, Ats == no ->
	    % may be a 'var' but not the required functor
	    VArgTypes = ~type_nonvar_args(~type_norm(fnc_var(XFunctor)), XFunctor)
	; Ats \== no,
	  VArgTypes = ( VMayBeVar = true ? ~orvars(Ats) | Ats )
	),
	As = ~X.args,
	Bs = ~flat_term_bs(As, V),
	X1 = ~X.set_args(Bs),
	Hs = [V-X1|Hs0],
	flat_term_args(As, Bs, FlatMode, VArgTypes, Is1, Hs0).

% TODO: Share with sht_analyzer operation.
%:- '$ctxprj'(orvars/2, []).
orvars([]) := [].
orvars([T|Ts]) := [~type_norm((T;var))|~orvars(Ts)].

flat_term_bs(Xs, V) := ~flat_term_bs_2(Xs, 1, V).

% TODO: Use a zipvars?
flat_term_bs_2([], _, _) := [] :- !.
flat_term_bs_2([_|Xs], I, V) := [~B.addp(type, ~type_norm(var))|Bs2] :- !,
	% formally, Bs are vars until the term is unified
	B = ~var_subarg_new(V, I),
	I1 is I + 1,
	Bs2 = ~flat_term_bs_2(Xs, I1, V).

:- '$ctxprj'(var_subarg_new/3, []).
:- meta_predicate var_subarg_new(?, ?, out(termvar)).
var_subarg_new(_X, I) := V :-
	V0 = ~termvar.new,
	V = ~V0.addp(mem, n('a_s', I)).

flat_term_args(Xs0, Bs0, FlatMode, VArgTypes0, Is, Hs) :-
	append(Xs, [X], Xs0), single_cell_args(Xs), \+ single_cell_arg(X), !,
	append(Bs, [B], Bs0),
	trust(B instance_of termvar),
	append(VArgTypes, [VArgType], VArgTypes0),
	flat_term_args_2(Xs, Bs, FlatMode, VArgTypes, Is2, Hs2p),
	append(Is2, IsA, Is),
	append(Hs2p, HsAp0, Hs),
	BType = VArgType,
	flat_instance_2(~B.addp(type, BType), X, FlatMode, IsA, HsAp0).
flat_term_args(Xs, Bs, FlatMode, VArgTypes, Is, Hs) :-
	flat_term_args_2(Xs, Bs, FlatMode, VArgTypes, Is, Hs).

flat_term_args_2([], [], _, [], [], []) :- !.
flat_term_args_2([X|Xs], [B|Bs], FlatMode, [VArgType|VArgTypes], Is3, Hs) :- !,
	trust(B instance_of termvar),
	BType = VArgType,
	( X instance_of termunk ->
	    T2 = X, Is = []
	; single_cell_arg(X) ->
	    T2 = X, Is = []
	; T0 = ~termvar.new,
	  T = ~T0.addp(mem, _),
	  ( FlatMode = get ->
	      % temp is unified after parent unification
	      TType = BType,
	      TType2 = ~type_norm(var) % on parent it is fresh
	  ; % temp is unified before parent unification
	    TType = ~type_norm(var),
	    TType2 = ~aterm_type(X) % on parent is bound
	  ),
	  T2 = ~T.addp(type, TType2),
	  Is = ~flat_instance(~T.addp(type, TType), X)
	),
	B1 = ~B.addp(type, BType),
	B2 = ~B1.addp(scope, none),
	Hs = [B2-T2|Hs0],
	Is3 = ~append(Is, Is2),
	flat_term_args_2(Xs, Bs, FlatMode, VArgTypes, Is2, Hs0).
}.

single_cell_args([]) :- !.
single_cell_args([X|Xs]) :- single_cell_arg(X), !, single_cell_args(Xs).

single_cell_arg(X) :- X instance_of termvar, !. 
single_cell_arg(X) :-
	'$absmach'(Absmach),
	trust(X instance_of termstr),
	Absmach.functorcons(~X.name, FC),
	functorcons__single_cell(FC),
	!.

{
:- fluid exp :: module_exp.
:- fluid initd :: initdic.
:- fluid code :: accum.
gen_call(Ga) :-
	( Ga = '$sub$'(G, GPredId) ->
	    MaybeGPred = yes(GPredId)
	; G = Ga,
	  MaybeGPred = no
	),
	trust(G instance_of strgoal),
	Args = ~G.args,
	GId = ~G.predid,
	Mems = ~caller_argmems(GId),
	Modes = ~GId.get_prop(argmodes),
	In = ~filter_mode(Args, Modes, in),
	InMems = ~filter_mode(Mems, Modes, in),
	%
	IVars = ~ziptypes(~vars_list(In, InMems), ~map_aterm_type(In)),
	% Replace output arguments by fresh variables
	call((
          intr :: absint <- sht_analyzer,
	  ShtDef = ~intr.get_usermemo(GId),
	  trust(ShtDef instance_of shtdef)
        )),
	Out0 = ~filter_mode(Args, Modes, out), 
	OutMems = ~filter_mode(Mems, Modes, out),
	OVars0 = ~vars_list(Out0, OutMems),
	OVarsBefore = ~ziptypesvar(OVars0),
	Vars2 = ~replace_mode(~replace_mode(Args, Modes, in, IVars), 
	                      Modes, out, OVarsBefore),
	%
	call((
          scope :: any <- bellow(FrameSize),
	  put_args(In, ~ziptypesvar(IVars))
        )),
	G2a = ~G.set_args(Vars2),
	G2b = ~G2a.addp(frame_live_size, FrameSize),
	G2 = ~add_liveness_att(G2b),
	trust(G2 instance_of strgoal),
	G3 = ( MaybeGPred = yes(GPredId2) ? ~G2.addp(sub, GPredId2) | G2 ),
	emit_ins(call(G3)),
	initd.mark_list(Vars2),
	% If the predicate does not fail, unify output variables with
	% output arguments
	( ShtDef.is_bottom ->
	    true
	; ExitTypes = ~ShtDef.get_exit_types,
	  OVars = ~ziptypes(OVars0, ~filter_mode(ExitTypes, Modes, out)),
	  get_args(~ziptypesvar(Out0), OVars)
	).
}.

{
:- fluid exp :: module_exp.
add_liveness_att(G) := G2 :-
	trust(G instance_of strgoal),
	true = ~goal_prop(needs_liveness_info, G), !,
	G1 = ~G.addp(heap, _),
	G2 = ~G1.addp(live_set, _).
add_liveness_att(G) := G :- !.
}.

{
:- fluid exp :: module_exp.
:- fluid initd :: initdic.
:- fluid code :: accum.

{
:- fluid scope :: any.
% TODO: Unify with bytecode__compiler:c_goal_args.
put_args(Args, Vars) :-
	put_args_nv(Args, Vars),
	put_args_v(Args, Vars).

put_args_nv([], []) :- !.
put_args_nv([Arg|Args], [Var|Vars]) :-
	( Arg instance_of termstr ->
	    gen_instance(Var, Arg)
	; true
	),
	put_args_nv(Args, Vars).

put_args_v([], []) :- !.
put_args_v([Arg|Args], [Var|Vars]) :-
	( Arg instance_of termvar ->
	    trust(Var instance_of termvar),
	    gen_unify(~Var.addp(scope, ~scope), Arg)
	; true
	),
	put_args_v(Args, Vars).
}.

get_args([], []) :- !.
get_args([Arg|Args], [Var|Vars]) :-
	get_args_2(Arg, Var),
	get_args(Args, Vars).

get_args_2(Arg, Var) :- Arg instance_of termvar, !,
	gen_unify(Var, Arg).
get_args_2(Arg, Var) :-
	errlog:bug(['get argument failed for variable ', Arg, Var]), fail.

gen_unify(U, V) :- U == V, !.
gen_unify(U, V) :-
	gen_unify__3(U, ~initd.marked(U), V, ~initd.marked(V)),
	initd.mark_list([U, V]).

:- '$ctxprj'(gen_unify__3/4, [exp, code]).
gen_unify__3(U, true, V, false) :- !,
	emit_ins(move(U, V)).
gen_unify__3(U, false, V, true) :- !,
	emit_ins(move(V, U)).
gen_unify__3(U, true, V, true) :- !,
	emit_ins(unify(V, U)).
gen_unify__3(U, false, V, false) :-
	trust(U instance_of termvar),
	trust(V instance_of termvar),
	( allocated_temp(U) ->
	    % temporal variables has the lowest initialization priority
	    emit_ins(init(V)),
	    emit_ins(move(~V.addp(fresh, true), U))
	; allocated_temp(V) ->
	    % temporal variables has the lowest initialization priority
	    emit_ins(init(U)),
	    emit_ins(move(~U.addp(fresh, true), V))
        ; coalesce_vars(U, V) -> % coalesce
	    % TODO: Check that coalescing here does not affect the
	    %   same variables in other clauses.
	    %% errlog:trace(['warning: ', coalesce_in_gen_unify(U,V)]),
            % TODO: The coalesce should be done when possible, but
            %   sometimes one variables has the scope prop and the other
            %   does not, what should be done in these cases?
	    emit_ins(init(U))
        ; emit_ins(init(U)),
	  emit_ins(move(~U.addp(fresh, true), V))
	).

% TODO: this does not seem very clean
:- '$ctxprj'(coalesce_vars/2, [exp]).
coalesce_vars(U, V) :-
	trust(U instance_of termvar),
	trust(V instance_of termvar),
%	errlog:trace([coalescing_vars(U, V)]),
	~U.name = ~V.name, % the names unify
	~U.getp(mem) = ~V.getp(mem), % the mems unify
	% TODO: check that scope test is correct
	( ~U.getp(scope) = UScope -> true ; UScope = no_scope ),
	( ~V.getp(scope) = VScope -> true ; VScope = no_scope ),
	UScope == VScope,
	% the types are the same
	% TODO: Instead, check that types are compatible.
	type_equal(~U.getp(type), ~V.getp(type)).
%	errlog:trace([coalesced(U, V)]).
}.

% TODO: Study performance difference between using a read(...) after
%   the load(..) and removing the part that loads the 'a_s' register
%   or leave it unchanged.
%
% TODO: Do other translation step to ease specialization?

anyconts := read-write.
successconts := success-success.

{
:- fluid code :: accum.
setmode(read, Read-_Write) :- code.add(~cont(Read)).
setmode(write, _Read-Write) :- code.add(~cont(Write)).

emit_ins(X) :- code.add(~single(X)).
}.

{
:- fluid exp :: module_exp.
:- fluid code :: accum.
instance2i_3(A, true, B, Conts) :- !,
	trust(A instance_of termvar),
	trust(B instance_of termstr),
	( typemode_is_var(~A.getp(type)) ->
	    T0 = ~termvar.new,
	    T = ~T0.addp(mem, _),
	    T1 = ~T.addp(type, ~type_norm(var)),
	    T2a = ~T.addp(type, ~aterm_type(B)),
	    T2 = ~T2a.addp(dereferenced, true),
	    instance2i_3(T1, false, B, ~successconts),
	    emit_ins(unify(A, T2)),
	    ( 0 = ~B.arity ->
	        setmode(read, Conts)
	    ; setmode(write, Conts)
	    )
	; typemode_is_nonvar(~A.getp(type)) ->
	    '$absmach'(Absmach),
	    BName = ~B.name,
	    Absmach.functorcons(BName, FC),
	    Type = ~type_norm(fnc(BName)),
	    ( type_consequence(~A.getp(type), Type) ->
	        true
	    ; type_contradictory(~A.getp(type), Type) ->
	        emit_ins(call(~fail_new))
	    ; emit_ins(call(~check_test_str_new(A, ~termunk.new(FC))))
	    ),
	    ( functorcons__is_constant(FC) ->
	        setmode(read, Conts)
	    ; emit_ins(read(A, ~termunk.new(FC), ~termunk.new(~funcall('$vargv'('a_s'))))),
	      setmode(read, Conts)
	    )
	; ( 0 = ~B.arity ->
	      NextConts = ~successconts,
	      call(( code :: accum(CodeRead), instance2i_3(~force_type(A, nonvar), true, B, NextConts) )),
	      call(( code :: accum(CodeWrite), instance2i_3(~force_type(A, var), true, B, NextConts) )),
	      emit_ins(switchrw(isnonvar(A))),
	      code.add(moderw(CodeRead, CodeWrite)),
	      setmode(read, Conts)
	  ; NextConts = Conts,
	    call(( code :: accum(CodeRead), instance2i_3(~force_type(A, nonvar), true, B, NextConts) )),
	    call(( code :: accum(CodeWrite), instance2i_3(~force_type(A, var), true, B, NextConts) )),
	    emit_ins(switchrw(isnonvar(A))),
	    code.add(moderw(CodeRead, CodeWrite))
	  )
	).
instance2i_3(U, false, V, Conts) :- !,
	'$absmach'(Absmach),
	trust(V instance_of termstr),
	VName = ~V.name,
	Absmach.functorcons(VName, FC),
	MaybeArgument = ( functorcons__is_constant(FC) ? no | just('a_s') ),
	emit_ins(load(U, ~termunk.new(MaybeArgument), ~termunk.new(FC))),
	setmode(write, Conts).
}.

% TODO: Type is not evaluated nor processed, right?
% TODO: Maybe it is better to implement a 'intersect with negated'
%   instead of using nonvar.
{
:- fluid exp :: module_exp.
force_type(A, Type) := ~A.addp(type, ~type_norm((~A.getp(type), Type))) :-
	trust(A instance_of termvar).
}.

% Get a list of temp variables for the arguments.
vars_list([], []) := [] :- !.
vars_list([A|As], [R|Rs]) := [~vars_list__2(A, R)|~vars_list(As, Rs)].

vars_list__2(A, Mem) := V :-
	Mem2 = ( Mem = cvar ? _
	       | Mem = cvar(_) ? _
	       | Mem
	       ),
	ImpType = ~aterm_imptype(A),
	V0 = ~termvar.new,
	V1 = ~V0.addp(mem, Mem2),
	V = ~V1.addp(imptype, ImpType).

aterm_imptype(X) := ImpType :-
	ImpType0 = ~aterm_imptype__2(X),
	( var(ImpType0) ->
	    errlog:bug(['var ', X, ' has empty imptype, assuming tagged!']),
	    ImpType = tagged
	; ImpType = ImpType0
	).

aterm_imptype__2(X) := ~get_imptype(X) :- X instance_of termvar, !.
% TODO: The following clause is not really correct.
aterm_imptype__2(X) := '$unknown' :- X instance_of termunk, !.
aterm_imptype__2(_) := tagged.

% ---------------------------------------------------------------------------
:- doc(section, "Unbox/box analysis and processing").
% ---------------------------------------------------------------------------

get_predpass(u) := ~predpass_u.new.
:- class predpass_u {
    :- extends predpass.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    :- static prev/1.
    prev := c.
    :- static curr/1.
    curr := u.

    {
    :- fluid exp :: module_exp.
    :- fluid maybeproceed :: any.
    :- fluid altstate :: m_any + d.
    :- fluid common :: any.
    :- static x_prepred/2.
    x_prepred(PredId) := (Args, Code0) :-
	trust(PredId instance_of predicate_x),
	icode(_, Args, Code0) = ~PredId.code,
	% TODO: Treatment of output arguments should be done in
	%   proceed compilation.
	% TODO: During code generation, do box of boxed input and put
	%   that in the cache. It is too complex to be done here by
	%   hand.
	% TODO: put argunboxs property in proceed (see module_exp?).
	altstate <- none,
	~common = common.
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid lastalt :: any.
    :- fluid altstate :: m_any.
    :- fluid common :: any.
    x_alt(Code0) := Code :-
	Code0 = c0c(Code30, AbsCut),
	call((
	  bcode :: accum <- Code30,
	  bcode <- ~prmap_sw(~bcode),
	  add_bxs,
	  Code3 = ~bcode
	)),
	Code = c0u(Code3, AbsCut).
    }.
}.

{
:- fluid exp :: module_exp.
:- fluid bcode :: accum.
% Add unboxs
% TODO: this uses a cache and a inverse-function cache (which is new)
add_bxs :-
	state :: m_any <- (~tbl_empty)-(~tbl_empty),
	% TODO: Improve notation.
%	bcode <- ~((~bxs_transform.new).map(~bcode)).
	tr :: bxs_transform <- ~bxs_transform.new,
	bcode <- ~tr.map(~bcode).
}.

:- class bxs_transform {
    :- extends forward_transform.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    {
    :- fluid exp :: module_exp.
    {
    :- fluid code :: accum.
    :- fluid state :: m_any.
    % (override)
    :- static transfer/1.
    transfer(X) :- 
        add_bx(X),
	bx_cache_update(X).
    }.

    {
    :- fluid state :: m_any + d.
    % (override)
    :- static meet/2.
    meet(UnboxCacheA-BoxCacheA, UnboxCacheB-BoxCacheB) :-
	UnboxCache = ~tbl_intersection(UnboxCacheA, UnboxCacheB, meet__2),
	BoxCache = ~tbl_intersection(BoxCacheA, BoxCacheB, meet__2),
	state <- UnboxCache-BoxCache.
   
    :- static meet__2/3.
    :- '$ctxprj'(meet__2/3, []).
    meet__2(ValA, ValB) := ValA :- ValA == ValB, !.
    }.
    }.
}.

{
:- fluid exp :: module_exp.
:- fluid code :: accum.
:- fluid state :: m_any.
% TODO: Study if it is worth caching update in every box and unbox
%   operation (i.e. so that foo(X,X,X) only unboxes X once).
add_bx(I) :-
	Args = ~ins_args(I),
	Unboxs = ~ins_argunboxs(I),
	Modes = ~ins_argmodes(I),
	In = ~filter_mode(Args, Modes, in),
	InUnboxs = ~filter_mode(Unboxs, Modes, in),
	call(( box_ops :: accum(UnBoxOps), bx_in_args(In, InUnboxs, In2) )),
	do_box_ops(UnBoxOps),
	Args2 = ~replace_mode(Args, Modes, in, In2),
	( ins_bottom_shtdef(I) ->
	    % TODO: This is not completelly correct.
	    % TODO: Problem: imptype of output arguments cannot be
	    %   inferred if ExitTypes is bottom... remove output
	    %   arguments in those cases?
	    emit_ins(~ins_set_args(I, Args2))
	; Out = ~filter_mode(Args, Modes, out),
	  OutUnboxs = ~filter_mode(Unboxs, Modes, out),
	  ExitTypes = ~ins_exittypes(I),
	  OutT = ~ziptypes(Out, ~filter_mode(ExitTypes, Modes, out)),
	  call(( box_ops :: accum(BoxOps), bx_out_args(Out, OutT, OutUnboxs, Out2) )),
	  Args3 = ~replace_mode(Args2, Modes, out, Out2),
	  emit_ins(~ins_set_args(I, Args3)),
	  do_box_ops(BoxOps)
	).
}.

{
:- fluid exp :: module_exp.
:- fluid state :: m_any.
:- fluid box_ops :: accum.
% TODO: Can arguments be something that is not a var?
bx_in_args([], []) := [].
bx_in_args([A|As], [Unbox|Unboxs]) := [A2|As2] :-
	A2 = ( A instance_of termvar ? ~bx_in_arg(A, Unbox) | A ),
	As2 = ~bx_in_args(As, Unboxs).

bx_in_arg(A, Unbox) := A2 :-
	trust(A instance_of termvar),
	( Unbox = true ->
	    ( CachedA = ~bx_get_cache(unboxed, A) ->
	        A2 = CachedA
	    ; Type = ~A.getp(type),
	      ImpType = ~bx_unbox_imptype(Type),
	      OpName = ~bx_op(unbox, Type),
	      T0 = ~termvar.new,
	      T1 = ~T0.addp(mem, _),
	      T2 = ~T1.addp(imptype, ImpType),
	      % TODO: Unboxed attribute and imptype are very similar;
	      %   one of them may not be needed.
	      T = ~T2.addp(unboxed, true),
	      A3 = ~T.addp(type, ~type_norm(var)), % arg before ins
	      A2 = ~T.addp(type, Type), % arg after box
	      box_ops.add(unbox(A, A3, OpName))
	    )
	; A2 = A
	).

bx_out_args([], [], []) := [].
bx_out_args([A|As], [AT|ATs], [Unbox|Unboxs]) := [A2|As2] :-
	trust(AT instance_of termvar),
	A2 = ( A instance_of termvar ? ~bx_out_arg(A, ~AT.getp(type), Unbox) | A ),
	As2 = ~bx_out_args(As, ATs, Unboxs).

bx_out_arg(A, OutType, Unbox) := A2 :-
	( Unbox = true ->
	    ( CachedA = ~bx_get_cache(boxed, A) -> % TODO: Always false?
	        A2 = CachedA
	    ; Type = OutType,
	      ImpType = ~bx_unbox_imptype(Type),
	      OpName = ~bx_op(box, Type),
	      T0 = ~termvar.new,
	      T1 = ~T0.addp(mem, _),
	      T2 = ~T1.addp(imptype, ImpType),
	      T = ~T2.addp(unboxed, true),
	      A3 = ~T.addp(type, Type), % arg before box
	      A2 = ~T.addp(type, ~type_norm(var)), % arg after ins
	      box_ops.add(box(A3, A, OpName))
	    )
	; A2 = A
	).
}.

{
:- fluid exp :: module_exp.
:- fluid code :: accum.
:- fluid state :: m_any.
do_box_ops([]).
do_box_ops([unbox(A, A3, OpName)|Bs]) :-
	bx(unboxed, A, A3, OpName),
	do_box_ops(Bs).
do_box_ops([box(A3, A, OpName)|Bs]) :-
	bx(boxed, A3, A, OpName),
	do_box_ops(Bs).
}.

{
:- fluid exp :: module_exp.
bx_unbox_imptype(Type0) := ImpType :-
	( ImpType = ~type_prop(imptype, Type0) ->
	    true
	; errlog:bug(['cannot get unboxed imptype for ', Type0]),
	  fail
	).

% TODO: Bx is box/unbox here, while boxed/unboxed in other code, adopt
%   the first notation everywhere.
bx_op(Bx, Type0) := OpName :-
	( OpName = ~type_prop(Bx, Type0) ->
	    true
	; % TODO: This should be an error.
	  errlog:bug(['unbox operation for native type ', Type0, ' not found']),
	  fail
	).
}.

{
:- fluid exp :: module_exp.
:- fluid code :: accum.
:- fluid state :: m_any.
bx(Bx, Source, Target, OpName) :-
	trust(Target instance_of termvar),
	Target0 = ~Target.addp(type, ~type_norm(var)),
	OpId = ~predicate_x.reg_new(OpName),
	emit_ins(call(~strgoal.new_n(OpId, [Source, Target0]))),
	bx_cache(Bx, Source, Target).
}.

inv_bx(unboxed, boxed).
inv_bx(boxed, unboxed).

{
:- fluid exp :: module_exp.
:- fluid state :: m_any.
bx_cache(boxed, Unboxed, Boxed) :-
	trust(Unboxed instance_of termvar),
	trust(Boxed instance_of termvar),
	~state = UnboxCache0-BoxCache0,
	UnboxCache = ~tbl_update(UnboxCache0, ~Boxed.name, Unboxed),
	BoxCache = ~tbl_update(BoxCache0, ~Unboxed.name, Boxed),
	state <- UnboxCache-BoxCache.
bx_cache(unboxed, Boxed, Unboxed) :-
	trust(Unboxed instance_of termvar),
	trust(Boxed instance_of termvar),
	~state = UnboxCache0-BoxCache0,
	UnboxCache = ~tbl_update(UnboxCache0, ~Boxed.name, Unboxed),
	BoxCache = ~tbl_update(BoxCache0, ~Unboxed.name, Boxed),
	state <- UnboxCache-BoxCache.
}.

{
:- fluid exp :: module_exp.
:- fluid state :: m_any + u.
bx_get_cache(unboxed, A) := A2 :-
	trust(A instance_of termvar),
	~state = UnboxCache-_,
	A2 = ~tbl_get(UnboxCache, ~A.name).
bx_get_cache(boxed, A) := A2 :-
	trust(A instance_of termvar),
	~state = _-BoxCache,
	A2 = ~tbl_get(BoxCache, ~A.name).
}.

{
:- fluid exp :: module_exp.
:- fluid state :: m_any.
bx_cache_update(move(A, B)) :- !,
	% If f(A)=A2, and B is copied with the value of A, then
	% f(B)=A2 too.
	trust(A instance_of termvar),
	trust(B instance_of termvar),
	~state = UnboxCache0-BoxCache0,
	( A2 = ~tbl_get(UnboxCache0, ~A.name) ->
	    UnboxCache = ~tbl_update(UnboxCache0, ~B.name, A2)
	; UnboxCache = UnboxCache0
	),
	( A3 = ~tbl_get(BoxCache0, ~A.name) ->
	    BoxCache = ~tbl_update(BoxCache0, ~B.name, A3)
	; BoxCache = BoxCache0
	),
	state <- UnboxCache-BoxCache.
bx_cache_update(I) :- ins_nounboxmod(I), !.
bx_cache_update(_I) :- state <- (~tbl_empty)-(~tbl_empty).
}.

{
:- fluid exp :: module_exp.
ins_nounboxmod(I) :-
	% TODO: Optimize using todo[Wed Dec 28 01:35:25 CET 2005]
	% Cleaning the cache implies that the blocks that this
	% instruction separates will not share common unboxed/boxed
	% variables, because it is not safe or it is not possible:
	%
        %  - after executing a predicate that cleans the X registers
	%
        %  - after executing a predicate that needs and-binarization
        %    (no local variables survive that...)
	%
        %  - after executing a predicate that needs liveinfo (e.g. a
        %    garbage collection will invalidate unboxed values that
        %    contains pointers to the heap)
        ( ins_clear_regs(I)
	; ins_needs_cframe(I)
	; ins_uses_successcont(I)
	),
	!,
	fail.
ins_nounboxmod(_).
}.

% ---------------------------------------------------------------------------
:- doc(section, "Deref analysis and processing").
% ---------------------------------------------------------------------------

get_predpass(d) := ~predpass_d.new.
:- class predpass_d {
    :- extends predpass.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    :- static prev/1.
    prev := u.
    :- static curr/1.
    curr := d.

    {
    :- fluid exp :: module_exp.
    :- fluid maybeproceed :: any.
    :- fluid altstate :: m_any + d.
    :- fluid common :: any.
    :- static x_prepred/2.
    % Note: Treatment of output arguments is done when the proceed
    %   instruction is called.
    x_prepred(PredId) := (Args2, Code0) :-
	trust(PredId instance_of predicate_x),
	icode(_, Args, Code0) = ~PredId.code,
	Modes = ~PredId.get_prop(argmodes),
	Derefs = ~PredId.get_prop(argderefs),
	In = ~filter_mode(Args, Modes, in),
	InDerefs = ~filter_mode(Derefs, Modes, in),
	%
	call((
          state :: m_any,
	  deref_callsubst(In, InDerefs),
	  % Anotate dereferenced arguments
	  In2 = ~deref_anot_xs(In),
	  Args2 = ~replace_mode(Args, Modes, in, In2),
	  %
	  altstate <- none,
	  ~common = common(~state)
	)).
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid lastalt :: any.
    :- fluid altstate :: m_any.
    :- fluid common :: any.
    x_alt(Code0) := Code :-
	Code0 = c0u(Code30, AbsCut),
	call((
          bcode :: m_any <- Code30,
	  bcode <- ~prmap_sw(~bcode),
	  ~common = common(CallSubst),
	  deref_analyze(CallSubst),
	  % TODO: 'add_derefs' should be done before or after this
	  %   block. Can be placed in the code generation?
	  %
	  % TODO: Dangerous: the mem of a fixed mem argument of a call
	  %   can be altered! Don't worry if non n(_, _) or y(_), but
	  %   deref should be in put_args.
	  add_derefs,
	  Code3 = ~bcode
        )),
	Code = c0d(Code3, AbsCut).
    }.
}.

% If A is a nonvar, once it is dereferenced it remains dereferenced
% forever.  This analysis does not takes in account dereferenced var.

{
:- fluid exp :: module_exp.
:- fluid state :: m_any + d.
deref_callsubst(In0, InDerefs) :-
	% Set dereferenced arguments (using InDerefs).
	state <- ~tbl_empty,
	deref_set_xs(~filter_deref(In0, InDerefs)).
}.

{
:- fluid exp :: module_exp.
:- fluid bcode :: m_any.
deref_analyze(CallSubst) :-
	state :: m_any <- CallSubst,
	tr :: deref_transform <- ~deref_transform.new,
	bcode <- ~tr.map(~bcode).
}.

:- class deref_transform {
    :- extends forward_transform.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    {
    :- fluid exp :: module_exp.
    :- fluid code :: accum.
    :- fluid state :: m_any.
    % (override)
    :- static transfer/1.
    transfer(I) :-
	( transfer__2(I) -> true
	; errlog:bug(['deref analyze failed for ', I]),
	  fail
	).

    :- static transfer__2/1.
    transfer__2(I) :- I = move(X, Y), deref_in(X), !,
	deref_set_x(Y),
	emit_ins(I).
    transfer__2(I) :-
	Args = ~ins_args(I),
	Derefs = ~ins_argderefs(I),
	Modes = ~ins_argmodes(I),
	In = ~filter_mode(Args, Modes, in),
	Args2 = ~replace_mode(Args, Modes, in, ~deref_anot_xs(In)),
	emit_ins(~ins_set_args(I, Args2)),
	( ins_bottom_shtdef(I) ->
	    true
	; ExitTypes = ~ins_exittypes(I),
	  Out = ~ziptypes(~filter_mode(Args, Modes, out), ~filter_mode(ExitTypes, Modes, out)),
	  OutDerefs = ~filter_mode(Derefs, Modes, out),
	  deref_set_xs(~filter_deref(Out, OutDerefs))
	).
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid state :: m_any.
    % (override)
    :- static meet/2.
    :- '$ctxprj'(meet/2, [exp, d(state)]).
    meet(DicA, DicB) :-
        state <- ~tbl_intersection(DicA, DicB, meet__2).

    :- static meet__2/3.
    :- '$ctxprj'(meet__2/3, []).
    meet__2(ValA, ValB) := ValA :- ValA == ValB, !.
    }.
}.

{
:- fluid exp :: module_exp.
filter_deref([X|Xs], [true|Ys]) := [X|~filter_deref(Xs, Ys)] :- mode_is_nonvar(X), !. % nonvar: no alias or sharing problems
filter_deref([_|Xs], [_|Ys]) := ~filter_deref(Xs, Ys) :- !.
filter_deref([], []) := [] :- !.
}.

{
:- fluid state :: m_any.
deref_set_xs([]).
deref_set_xs([X|Xs]) :- deref_set_x(X), deref_set_xs(Xs).	

deref_set_x(X) :-
	trust(X instance_of termvar),
	state <- ~tbl_update(~state, ~X.name, 1).

:- '$ctxprj'(deref_in/1, [u(state)]).
deref_in(X) :-
	trust(X instance_of termvar),
	_ = ~tbl_get(~state, ~X.name).
}.

{
:- fluid state :: m_any + u.
deref_anot_xs([]) := [].
deref_anot_xs([X|Xs]) := [~deref_anot_x(X)|~deref_anot_xs(Xs)].

deref_anot_x(X) := ~X.addp(dereferenced, true) :-
	X instance_of termvar,
	deref_in(X), !.
deref_anot_x(X) := X.
}.

% TODO: Dereference set is the Choice variable (how can I get it?).
%   move A B -> if A is in deref set, then insert B in deref set 
%   call As -> annotate with deref set

% Add derefs
{
:- fluid exp :: module_exp.
:- fluid bcode :: m_any.
add_derefs :-
	state :: m_any <- ~tbl_empty,
	tr :: add_derefs_transform <- ~add_derefs_transform.new,
	bcode <- ~tr.map(~bcode).
}.

:- class add_derefs_transform {
    :- extends forward_transform.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    {
    :- fluid exp :: module_exp.
    :- fluid code :: accum.
    :- fluid state :: m_any.
    % (override)
    :- static transfer/1.
    transfer(X) :-
	add_deref(X),
	update(X).

    :- '$ctxprj'(update/1, [exp, state]).
    :- static update/1.
    update(move(A, B)) :- !,
	% B is dereferenced only if A is dereferenced
	trust(A instance_of termvar),
	trust(B instance_of termvar),
	( _A = ~tbl_get(~state, ~A.name) ->
	    % A is dereferenced, B will be dereferenced
	    B2 = ~B.addp(dereferenced, true),
	    state <- ~tbl_update(~state, ~B.name, B2)
	; % A is not dereferenced, B will not be dereferenced
	  state <- ~tbl_remove(~state, ~B.name)
	).
    update(I) :- ins_noderefmod(I), !.
    update(_I) :- state <- ~tbl_empty.

    :- static add_deref/1.
    add_deref(I) :-
	I = unify(A, B),
	avoid_deref_unif(A, B), !,
	emit_ins(I).
    add_deref(I) :-
	Args = ~ins_args(I),
	Derefs = ~ins_argderefs(I),
	Modes = ~ins_argmodes(I),
	In = ~filter_mode(Args, Modes, in),
	InDerefs = ~filter_mode(Derefs, Modes, in),
	deref_args(In, InDerefs, In2),
	Args2 = ~replace_mode(Args, Modes, in, In2),
	emit_ins(~ins_set_args(I, Args2)).

    :- '$ctxprj'(avoid_deref_unif/2, [exp]).
    :- static add_deref_unif/2.
    avoid_deref_unif(A, B) :-
	trust(A instance_of termvar),
	trust(B instance_of termvar),
	~A.name == ~B.name, !.
    avoid_deref_unif(A, _B) :-
	trust(A instance_of termvar),
        ~A.getp(mem) = Mem, nonvar(Mem), Mem = y(_),
	~A.getp(fresh) = true, !.
    avoid_deref_unif(_A, B) :- 
	trust(B instance_of termvar),
        ~B.getp(mem) = Mem, nonvar(Mem), Mem = y(_),
	~B.getp(fresh) = true, !.
    avoid_deref_unif(A, B) :-
	~get_imptype(A) = tagged,
	~get_imptype(B) = tagged,
	( varvaluetype(A) ->
	    fail
	; varvaluetype(B) ->
	    fail
	; one_cell_types(A, B) ->
	    fail
	; true % do not put derefs in the default case!
	).

%    :- '$ctxprj'(deref_args/3, [code, state]).
    :- static deref_args/2.
    deref_args([], []) := [].
    deref_args([A|As], [Deref|Derefs]) := [A2|As2] :-
	A2 = ( A instance_of termvar, Deref = true ?
	         ~deref_arg(A)
	     | A
	     ),
	As2 = ~deref_args(As, Derefs).

%    :- '$ctxprj'(deref_arg/2, [code, state]).
    :- static deref_arg/2.
    deref_arg(A, A2) :-
	trust(A instance_of termvar),
	( A1 = ~tbl_get(~state, ~A.name) ->
	    trust(A1 instance_of termvar),
	    A2 = ~A1.addp(type, ~A.getp(type))
	; A2 = ~dereference(A),
	  state <- ~tbl_update(~state, ~A.name, A2)
	).
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid state :: m_any + d.
    % (override)
    :- static meet/2.
    meet(CacheA, CacheB) :-
	state <- ~tbl_intersection(CacheA, CacheB, meet__2).

    :- static meet__2/3.
    :- '$ctxprj'(meet__2/3, []).
    meet__2(ValA, ValB) := ValA :- ValA == ValB, !.
    }.
}.

{
% TODO: reuse registers... how?
:- fluid exp :: module_exp.
:- fluid code :: accum.
:- static dereference/2.
dereference(A) := A2 :- aterm_is_dereferenced(A), !, A2 = A.
dereference(A) := T2 :-
	trust(A instance_of termvar),
	T0 = ~termvar.new,
	T = ~T0.addp(mem, _),
	T1 = ~T.addp(type, ~type_norm(var)),
	T2a = ~T.addp(dereferenced, true),
	T2b = ~T2a.addp(type, ~A.getp(type)),
	T2 = ~T2b.addp(vartype, ~vt(A)),
	emit_ins(deref(A, T1)).
}.

% A term is dereferenced if it is not a variable or is a fresh
% variable or is a dereferenced variable.
aterm_is_dereferenced(A) :-
	\+ A instance_of termvar, !.
aterm_is_dereferenced(A) :-
	trust(A instance_of termvar),
	~A.getp(fresh) = true, !.
aterm_is_dereferenced(A) :-
	trust(A instance_of termvar),
	~A.getp(dereferenced) = true, !.
aterm_is_dereferenced(A) :-
	% TODO: Can it be determined by the type and boxing status?
	\+ ~get_imptype(A) = tagged, !.

vt(A) := nonstack :- is_subarg(A), !.
vt(_) := any :- !.

% ---------------------------------------------------------------------------
:- doc(section, "xcache, register allocation, heap analysis, ... (see g_code)").
% ---------------------------------------------------------------------------

get_predpass(g) := ~predpass_g.new.
:- class predpass_g {
    :- extends predpass.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    :- static prev/1.
    prev := d.
    :- static curr/1.
    curr := g.

    {
    :- fluid exp :: module_exp.
    :- fluid maybeproceed :: any.
    :- fluid altstate :: m_any + d.
    :- fluid common :: any.
    :- static x_prepred/2.
    x_prepred(PredId) := (Args, Code0) :-
	trust(PredId instance_of predicate_x),
	icode(_, Args, Code0) = ~PredId.code,
	Modes = ~PredId.get_prop(argmodes),
	In2 = ~filter_mode(Args, Modes, in),
	CheckEvents = ( true = ~PredId.get_prop(do_not_check_events) ? no
		      | yes
		      ),
	FailInfo = failinfo(Modes, Args),
	%
	altstate <- altstate(no_choice, yes),
	~common = common(CheckEvents, In2, FailInfo).
%	{ errlog:trace([b]) },
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid lastalt :: any.
    :- fluid altstate :: m_any.
    :- fluid common :: any.
    x_alt(Code0) := Code :-
        ~common = common(CheckEvents, In2, FailInfo),
	~altstate = altstate(ChoiceState, FirstAlt),
        AltPos = ( ~lastalt = yes ? last_alt
		 | FirstAlt = yes ? first_alt
		 | middle_alt
		 ),
	Code0 = c0d(Code3, AbsCut),
	ChoiceInfo = choiceinfo(ChoiceState, AltPos, AbsCut, ChoiceIsNeeded),
	call((
          inargs :: any <- In2,
	  choiceinfo :: any <- ChoiceInfo,
	  failinfo :: any <- FailInfo,
	  checkevents :: any <- CheckEvents,
	  bcode :: m_any <- Code3,
	  g_code,
	  Code4 = ~bcode
        )),
	call((
          % TODO: Generalize indexing as a hiord call where the
          %   predicate is choosen dynamically?
	  common :: any <- common(no,In2,FailInfo),
	  Code5 = ~prmap_sw(Code4)
        )),
	% TODO: Do not use nextalt(Addr), use Addr directly.
	Code = ~add_fail_cont(nextalt(no_restore), Code5),
	( ChoiceIsNeeded = yes, ChoiceState = no_choice -> 
	    ChoiceState2 = choice_unknown, FirstAlt2 = no
	; ChoiceState2 = ChoiceState, FirstAlt2 = no
	),
	altstate <- altstate(ChoiceState2, FirstAlt2).
    }.
}.

{
:- fluid exp :: module_exp.
:- fluid inargs :: any.
:- fluid choiceinfo :: any.
:- fluid failinfo :: any.
:- fluid checkevents :: any.
:- fluid bcode :: m_any.

% TODO: Does coalesce variables affect variables in source term?
%   (copy_term/2 should be necessary, but it also makes copies of
%   variables defining labels and makes tests like imp_c fail)
%   (i.e. copy_term((In,Body0), (In2,Body2))).

g_code :-
        % TODO: Do CSE optimization? (include it in xcache?)
	% TODO: To get dereferenced values, do deref analysis to
	%   transform derefs into moves?
	xcache,
	add_neck, 
	delay_moves,
	allocate_frame_vars,
	fill_loc,
	pre_update_frame_vars,
	remove_useless_ins,
	assign_temp_registers,
	remove_useless_cut, % TODO: kludge
	mark_last_ins,
	add_var_inits,
	heap_usage,
	speccalls,
	add_choice_ins,
	add_check_events.
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "X-cache Propagation").
% Replace X by Y if Y is precolored and contains the value of X.
% ---------------------------------------------------------------------------

{
:- fluid exp :: module_exp.
:- fluid bcode :: m_any.
xcache :-
	state :: m_any <- ~xcache_new,
	tr :: xcache_transform <- ~xcache_transform.new,
	bcode <- ~tr.map(~bcode).
}.

:- class xcache_transform {
    :- extends forward_transform.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    {
    % (override)
    :- fluid exp :: module_exp.
    :- fluid code :: accum.
    :- fluid state :: m_any.
    :- static transfer/1.
    transfer(load(U, MaybeArguments, V)) :- !,
        trust(V instance_of termunk),
	( \+ is_subarg(U),
	  V1 = ~V.value,
	  functorcons__is_constant(V1),
	  W = ~xcache_replace_cons(V1) ->
	    emit_ins(move(W, U)),
	    xcache_alias_load(U, ~V.value)
	; emit_ins(load(U, MaybeArguments, V)),
	  xcache_alias_load(U, ~V.value)
	).
    transfer(move(U,V)) :- !,
	U2 = ~xcache_replace_var(U),
	emit_ins(move(U2,V)),
	xcache_alias_bind(U, V).
    transfer(I) :- !,
	Args = ~ins_args(I),
	Modes = ~ins_argmodes(I),
	In = ~filter_mode(Args, Modes, in),
	In2 = ~xcache_replace_vars(In),
	Args2 = ~replace_mode(Args, Modes, in, In2),
	emit_ins(~ins_set_args(I, Args2)),
	( ins_clear_regs(I) ->
	    state <- ~xcache_new
	; true % no xcache update
	).
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid state :: m_any + d.
    % (override)
    :- static meet/2.
    meet(CacheA, CacheB) :-
    	state <- ~tbl_intersection(CacheA, CacheB, meet__2).

    :- static meet__2/3.
    :- '$ctxprj'(meet__2/3, []).
    meet__2(U-ValA, _-ValB) := U-ValA :- ValA == ValB, !.
    }.
}.

{
:- fluid exp :: module_exp.
:- fluid state :: m_any + u.
xcache_replace_vars([V|Vs]) := [~xcache_replace_var(V)|~xcache_replace_vars(Vs)] :- !.
xcache_replace_vars([]) := [].

xcache_replace_var(U) := ~V.addp(type, ~U.getp(type)) :-
	U instance_of termvar,
	V-var(Grp0) = ~tbl_values(~state),
	trust(V instance_of termvar),
	Grp = ~U.name, Grp0 == Grp, !.
xcache_replace_var(U) := U :- !.

xcache_replace_cons(V) := U2 :-
	Val = ~functorcons__constant_value(V), 
	U-cons(Val) = ~tbl_values(~state), !,
	trust(U instance_of termvar),
	U2 = ~U.addp(type, ~type_norm(atomic(Val))).
}.

% Update the cache with a list of Variable-Value or remove(Variable)
{
:- fluid state :: m_any.
xcache_alias_bind(U, V) :- allocated_temp(U), !,
	trust(V instance_of termvar),
	xcache_update_alias__2(U-var(~V.name)).
xcache_alias_bind(U, V) :- allocated_temp(V), !,
	trust(U instance_of termvar),
	xcache_update_alias__2(V-var(~U.name)).
xcache_alias_bind(_, _).

xcache_alias_load(U, V) :- allocated_temp(U), !, xcache_alias_load_value(U, V).
xcache_alias_load(_, _).

xcache_alias_load_value(U, V) :- functorcons__is_constant(V), !,
	xcache_update_alias__2(U-cons(~functorcons__constant_value(V))).
xcache_alias_load_value(U, _) :- !, xcache_update_alias__2(remove(U)).

xcache_update_alias__2(remove(U)) :- !,
	trust(U instance_of termvar),
	state <- ~tbl_remove(~state, ~U.getp(mem)).
xcache_update_alias__2(C) :- C = U-_, !,
	trust(U instance_of termvar),
	state <- ~tbl_update(~state, ~U.getp(mem), C).
}.

% Empty cache
xcache_new := ~tbl_empty.

% ---------------------------------------------------------------------------

is_subarg(U) :-
	trust(U instance_of termvar),
	~U.getp(mem) = Mem, nonvar(Mem), Mem = n(_, _).
allocated(U) :-
	trust(U instance_of termvar),
	Mem = ~U.getp(mem), nonvar(Mem).
unallocated(U) :-
	trust(U instance_of termvar),
	Mem = ~U.getp(mem), var(Mem).
unallocated_temp(U) :-
	trust(U instance_of termvar),
	Mem = ~U.getp(mem),
	( var(Mem) -> true ; Mem = x(I), var(I) ).
allocated_temp(U) :-
	trust(U instance_of termvar),
	allocated(U), \+ ~U.getp(mem) = n(_, _), \+ ~U.getp(mem) = y(_).
allocated_ny(A) :-
	trust(A instance_of termvar),
	allocated(A), ( ~A.getp(mem) = n(_, _) ; ~A.getp(mem) = y(_) ), !.

% ---------------------------------------------------------------------------
:- doc(subsection, "Add neck").
% ---------------------------------------------------------------------------

:- pred add_neck/0 # "Insert the @tt{neck} instruction in the correct
  place using the following rules:

@begin{itemize}

@item Must be placed before an unavoidable write to a precolored register.

@item A complete choice instruction after a instruction which cannot fail can
be moved backwards (and the number of temporary registers will be reduced).

@end{itemize}".

% TODO: What happens with derefs?
% TODO: Maybe we can insert the neck at the begining and delay it in
%   other pass.
{
:- fluid exp :: module_exp.
:- fluid inargs :: any.
:- fluid choiceinfo :: any.
%trace_exp(M) :- Exp = ~exp, ( Exp = none -> errlog:trace([me(M,Exp)]) ; functor(Exp, F, N), errlog:trace([me(M,F,N)]) ).
:- fluid bcode :: m_any.
add_neck :-
	~choiceinfo = choiceinfo(ChoiceState, AltPos, AbsCut, ChoiceIsNeeded),
	Body = ~bcode,
	(Pre0, Post0) = ~noprecut_prefix(Body),
	( AltPos = last_alt ->
	    ChoiceIsNeeded = no
	; Cut = ~prhead(Post0), ins_is_cut(Cut) ->
	    Post = ~prtail(Post0), 
	    Safe = ~nop,
	    % OK, because none of prenocut creates choice points, so
	    % this cut really cuts.
	    % TODO: Perhaps fails with "metacut(X), !(X)...".
	    CompleteChoice = neck(~termunk.new(trick_to_preserve_args), ~inargs),
	    ( ChoiceState = no_choice,
	      AbsCut = cut_to_previous_choice,
	      prforall(Pre0, ro_i) ->
	        % TODO: Remove update_default_choice if no cuts are
		% left.
	        %
	        % Only no_choice, but choice_unknown affects the next
		% clause state recovery and can be taken into
		% account.
	        ChoiceIsNeeded = no,
		Pre2 = ~complw([Pre0, ~single(dummy_cut)])
	    ; ChoiceIsNeeded = yes,
	      Pre2 = ~complw([Pre0, ~single(Cut)])
	    ),
	    bcode <- ~complw([Pre2, ~single(CompleteChoice), Safe, Post])
	; (Pre1, Safe) = ~safe_suffix(Pre0),
	  Post0 = Post, 
	  CompleteChoice = neck(~termunk.new(ChoiceState), ~inargs),
	  ChoiceIsNeeded = yes,
	  Pre2 = Pre1,
	  bcode <- ~complw([Pre2, ~single(CompleteChoice), Safe, Post])
	).
}.

{
:- fluid exp :: module_exp.
ins_is_cut(I) :- ~ins_name(I) = 'basiccontrol:$cut'/1.

% Prefix of instructions that holds noprecut_ins.
noprecut_prefix(Xs) := ~forward_take(Xs, noprecut_ins).

% No cut and nopreins.
noprecut_ins(X) :-
%	trace_exp(z0(X)),
	\+ ins_is_cut(X),
%	trace_exp(z1(X)),
	nopre_ins(X).

% No fail, no proceed, no need_neck and no write precolored.
nopre_ins(I) :- 
%	trace_exp(a(I)),
	\+ ins_is_fail(I),
%	trace_exp(b(I)),
	\+ ins_is_proceed(I),
%	trace_exp(c(I)),
	\+ ins_needs_neck(I),
%	trace_exp(d(I)),
	\+ write_precolored(I).
%	trace_exp(e(I)).

write_precolored(I) :-
	member(X, ~ins_defined(I)),
	trust(X instance_of termvar),
	\+ ~X.getp(mem) == default_choice, % TODO: check...
	allocated_temp(X), !.

% The instruction does not use the heap, trail or registers already
% used as arguments (that is, it does not modifies the state) does not
% use registers, nor heap, trail or choice stacks.
%
% TODO: Does it have something to do with nosideeffdet? move to
%   ptoc__ins.
ro_i(I) :-
	\+ write_precolored(I),
	%
	\+ ins_clear_regs(I),
	HeapUsage = ~ins_heap_usage(I),
	( HeapUsage = max(0) -> true
	; HeapUsage = info % TODO: right?
	),
	\+ ins_uses_trail(I),
	\+ ins_modifies_choice_on_success(I),
	\+ ins_uses_failcont(I).

% Biggest suffix (safe)*
safe_suffix(Xs) := ~backward_take(Xs, safe).

% The instruction does not fail.

% TODO: Verify that it 'does not fail' is really the property
%   described here.
% TODO: Move to ptoc__ins.pl, give other name, move to properties?
safe(move(_, _)) :- !.
safe(init(_)) :- !.
safe(load(_, _, _)) :- !.
safe(unify(A, _)) :-
	trust(A instance_of termvar),
	typemode_is_var(~A.getp(type)), !.
safe(unify(_, A)) :-
	trust(A instance_of termvar),
	typemode_is_var(~A.getp(type)), !.
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Remove Unnecessary Cuts").
% TODO: peephole optimizing?
% ---------------------------------------------------------------------------

% TODO: Should this pass be performed before gencode?
{
:- fluid exp :: module_exp.
:- fluid choiceinfo :: any.
:- fluid bcode :: m_any.
remove_useless_cut :-
	( ~choiceinfo = choiceinfo(no_choice, last_alt, _, _) ->
	    call((
	      common :: any <- nothing,
	      bcode <- ~prmap(~bcode, remove_useless_cut_2)
            ))
	; true
	).
}.

{
:- fluid exp :: module_exp.
%:- fluid p :: predpass.
remove_useless_cut_2(I, _) := dummy_cut :-
	ins_is_cut(I),
	~ins_args(I) = [Choice],
	trust(Choice instance_of termvar),
	~Choice.getp(mem) = default_choice, !.
remove_useless_cut_2(I, _) := I.
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Delay moves").
% This optimization reduces the number of temporary registers
% ---------------------------------------------------------------------------

% TODO: This pass is inefficient.
{
:- fluid exp :: module_exp.
:- fluid bcode :: m_any.
delay_moves :-
	Code = ~bcode,
	(Moves, Code2) = ~forward_take(Code, is_move),
        % TODO: what happens with derefs???
	(Pre0, Post0) = ~forward_take(Code2, nopre_ins),
	(Unsafe, Mid) = ~safe_suffix(Pre0),
	Post = Mid .&. Post0,
        %
	call((
          common :: any <- Unsafe,
	  (Safe1b, Safe1a) = ~prsplit(Moves, can_be_delayed)
        )),
	bcode <- ~complw([Safe1a, Unsafe, Safe1b, Post]).
}.

% TODO: what happens with derefs???
{
:- fluid exp :: module_exp.
:- fluid common :: any.
can_be_delayed(move(_, D)) :-
	trust(D instance_of termvar),
	Unsafe = ~common,
	\+ ( prmember(I, Unsafe),
	     member(U, ~ins_used(I)),
	     trust(U instance_of termvar),
	     ~D.name == ~U.name ).
}.

% TODO: exp is not necessary here
{
:- fluid exp :: module_exp.
is_move(move(_, _)).
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Allocate frame variables").
% The variable which stores the register of each variable is
% instantiated.
% ---------------------------------------------------------------------------

{
:- fluid exp :: module_exp.
:- fluid bcode :: m_any.
allocate_frame_vars :-
	Body = ~bcode,
	call((
          chunk :: m_any <- 0,
	  list :: m_any <- [],
	  get_occurrences(Body),
	  ~list = List
        )),
	call((
          n :: m_int <- 0,
	  allocate_frame_vars_2(~reverse(List)),
	  ~n = N
        )),
	bcode <- ~fill_lvs(Body, N).
}.

{
:- fluid n :: m_int.
allocate_frame_vars_2([]).
allocate_frame_vars_2([L|List]) :-
	allocate_frame_var(L),
	allocate_frame_vars_2(List).

allocate_frame_var(occs(_, Mem, Occs)) :- !,
	( Occs = several ->
	    N0 = ~n,
	    Mem = y(N0),
	    n.inc(1) 
	; Mem = _
	).
allocate_frame_var(frame_live_size(N)) :- !, N = ~n.
}.

{
:- fluid exp :: module_exp.
:- fluid chunk :: m_any.
:- fluid list :: m_any.
get_occurrences(Xs) :-
	state :: m_any <- ~chunk,
	global :: m_any <- ~list,
	tr :: occ_analyze <- ~occ_analyze.new,
	tr.analyze(Xs),
	chunk <- ~state,
	list <- ~global.
}.

:- class occ_analyze {
    :- extends backward_analyze_g.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    {
    :- fluid exp :: module_exp.
    :- fluid state :: m_int.
    :- fluid global :: m_any.
    % (override)
    :- static transfer/1.
    transfer(X) :-
	( ins_should_trim_frame(X) ->
	    LiveSize = ~ins_frame_live_size(X),
	    global <- [frame_live_size(LiveSize)| (~global)]
	; true
	),
	record_occurrences_args(~ins_defined(X)),
	( ins_clear_regs(X) -> % increment chunk number
	    % TODO: rename state by chunknumber
	    state.inc(1)
	; true
	),
	record_occurrences_args(~ins_used(X)).
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid state :: m_int + d.
    % (override)
    :- static meet/2.
    meet(A, B) :- state <- ~max(A, B).
    }.
}.

{
:- fluid state :: m_any + u.
:- fluid global :: m_any.
record_occurrences(X) :-
	X instance_of termvar,
	var(~X.getp(mem)),
	~get_imptype(X) = tagged, % TODO: why? document
	!,
	record_occurence(X).
record_occurrences(X) :- X instance_of termstr, !,
	record_occurrences_args(~X.args).
record_occurrences(_).

record_occurrences_args(Xs) :-
	maplist(([u(state), global] -> ''(X) :- record_occurrences(X)), Xs).

record_occurence(X) :-
	% update the occs of the var (if present)
	List = ~record_occurence_2(~global, X), !,
	global <- List.
record_occurence(X) :-
	% insert the occs
	trust(X instance_of termvar),
	Mem = ~X.getp(mem),
	global <- [occs(~X.name, Mem, ~state)|(~global)].
}.

% TODO: alias 'state' by 'chunk'
{
:- fluid state :: m_any + u.
record_occurence_2([occs(Name, Mem, Occs0)|List], X) := [occs(Name, Mem, Occs)|List] :-
	trust(X instance_of termvar),
	Name0 = ~X.name, Name == Name0,
	% Occs is the chunk number where the variable lives or 'several' if 
	% it lives in several chunks.
	~state = ChunkNumber,
	Occs = ( Occs0 = ChunkNumber ? Occs0 | several ).
record_occurence_2([L|List], X) := [L|~record_occurence_2(List, X)] :- !.
}.

{
:- fluid exp :: module_exp.
fill_lvs(Xs, N) := Xs2 :-
	state :: m_any <- N,
	tr :: fill_lvs_transform <- ~fill_lvs_transform.new,
	Xs2 = ~tr.map(Xs).
}.

:- class fill_lvs_transform {
    :- extends forward_transform.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    {
    :- fluid exp :: module_exp.
    {
    :- fluid code :: accum.
    :- fluid state :: m_any.
    % (override)
    :- static transfer/1.
    transfer(X) :-
	emit_ins(X),
	( ins_should_trim_frame(X) ->
	    % reset with a new frame size
	    state <- ~ins_frame_live_size(X)
        ; FrameSize = ~state,
	  ( FrameSize = ~ins_frame_live_size(X) ->
	      true % fill (unify) frame size
	  ; true
	  )
        ).
    }.
    {
    :- fluid state :: m_any + d.
    % (override)
    :- static meet/2.
    meet(FrameSize, FrameSize) :- !, state <- FrameSize.
    meet(_, _) :- errlog:bug(['fill_lvs_meet failed! (frame size changed in a condition?)']), fail.
    }.
    }.
}.

% ---------------------------------------------------------------------------
:- doc(section, "Analizes and annotates the safeness of the variables").
%   - analyze safeness of variables (whether a variable is local or global)
%   - mark the variables which need to be globalized
% ---------------------------------------------------------------------------

% TODO: improve the analysis of locality of variables (i.e. whether a
%   fresh variable is in the framestack or not)
%
% Loc values:
%   cached_r -> may be a stack variable, but will live after ret
%   cached_y(I) -> may be the stack variable y(I)
%                  (otherwise, it is not a stack variable...)
%   cached_heap -> cannot be a stack variable           

{
:- fluid exp :: module_exp.

{
:- fluid inargs :: any.
:- fluid bcode :: m_any.
fill_loc :-
	state :: m_any <- ~tbl_empty,
	tr :: fill_loc_transform <- ~fill_loc_transform.new,
	set_locs(~ziploc(~inargs, cached_r)),
	bcode <- ~tr.map(~bcode).
}.

:- class fill_loc_transform {
    :- extends forward_transform.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    {
    :- fluid exp :: module_exp.
    :- fluid code :: accum.
    :- fluid state :: m_any.
    :- static transfer/1.
    transfer(I) :-
	set_locs(~ins_loc(I)),
	fill_globalize(I).
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid state :: m_any + d.
    :- static meet/2.
    meet(A, B) :- state <- ~tbl_union(A, B, mix_cached).
    }.
}.

{
:- fluid state :: m_any + u.
ins_loc(I) := Locs :- Locs = ~ins_loc__2(I), !.
ins_loc(I) := _ :- errlog:bug(['ins_loc failed for ', I]), fail.

ins_loc__2(init(U)) := [U-cached_y(J)] :-
	trust(U instance_of termvar),
	allocated(U),
	~U.getp(mem) = y(J), !.
ins_loc__2(move(U, V)) := [V- ~get_loc(U)] :- \+ is_subarg(U), \+ is_subarg(V), !.
ins_loc__2(deref(U, V)) := [V- ~get_loc(U)] :- \+ is_subarg(U), \+ is_subarg(V), !.
% TODO: Actually, is a cached_heap (see the fill_loc_instance_2 code,
%   this hack is for preserving the generated code with previous
%   temporary variable insertion method).
ins_loc__2(load(U, _, _)) := [U-cached_y(J)] :-
	trust(U instance_of termvar),
	allocated(U),
	~U.getp(mem) = y(J), !.
ins_loc__2(load(U, _, _)) := [] :- is_subarg(U), !.
ins_loc__2(I) := ~ziploc(~ins_defined(I), cached_heap) :- !.
}.

:- '$ctxprj'(ziploc/3, []).
ziploc([], _) := [].
ziploc([V|Vs], Loc) := [V-Loc|~ziploc(Vs, Loc)].

:- '$ctxprj'(mix_cached/3, []).
mix_cached(_, X) := X :- X = cached_y(_), !.
mix_cached(X, _) := X :- X = cached_y(_), !.
mix_cached(_, X) := X :- X = cached_r, !.
mix_cached(X, _) := X :- X = cached_r, !.
mix_cached(_, _) := cached_heap.

% V (var) needs to be globalized if:
%
%  - the destination variable (not shown here) needs a global
%    variable, and
%
%  - the source variable (V) may be a nonheap variable (from the
%    input) or a Y variable (if the destination variable is the
%    argument of a call, the source variable is only globalized if the
%    Y variable will not live after the call).
%
% If V needs to be globalized but has a nonvar type, then only deref
% is needed.

% TODO: This is not an usual deref.
{
:- fluid code :: accum.
:- fluid state :: m_any + u.
fill_globalize(move(V, U)) :-
	trust(U instance_of termvar),
	Globalize = ~do_globalize(~get_loc(V), ~U.getp(scope)), !,
	V1 = ~dereference(V),
	Globalize2 = ( mode_may_be_var(V) ? Globalize
		     | deref_only
		     ),
	emit_ins(globalize(~termunk.new(Globalize2), V1, U)).
fill_globalize(I) :-
	emit_ins(I).
}.

% Type of globalization needed, if needed.
% Scope: minimum amount of frame alive during the use of V.
:- '$ctxprj'(do_globalize/3, []).
do_globalize(cached_r, none) := yes :- !.
do_globalize(cached_y(_), none) := yes :- !.
do_globalize(cached_y(K), bellow(LiveSize)) := if_unsafe :- K >= LiveSize, !.

mode_may_be_var(X) :-
	trust(X instance_of termvar),
	typemode_may_be_var(~X.getp(type)).

mode_is_nonvar(X) :-
	trust(X instance_of termvar),
	typemode_is_nonvar(~X.getp(type)).
}.

% TODO: This should be a class for locations.
{
:- fluid state :: m_any.

:- '$ctxprj'(set_locs/1, [state]).
set_locs([]) :- !.
set_locs([X-Loc|Locs]) :- set_loc(X, Loc), set_locs(Locs).

:- '$ctxprj'(set_loc/2, [state]).
set_loc(V, Loc) :-
	trust(V instance_of termvar),
	state <- ~tbl_update(~state, ~V.name, Loc).

:- '$ctxprj'(get_loc/2, [u(state)]).
get_loc(V) := ~tbl_get(~state, ~V.name) :-
	trust(V instance_of termvar).
}.

% ---------------------------------------------------------------------------

% Note: put a trail and move with update_frame_vars? 

{
:- fluid exp :: module_exp.
:- fluid bcode :: m_any.
pre_update_frame_vars :-
	bcode <- ~prflatmap(~bcode, pre_update_frame_vars_2).
}.

{
:- fluid exp :: module_exp.
:- fluid code :: accum.
pre_update_frame_vars_2(move(U, V)) :-
	trust(U instance_of termvar),
	trust(V instance_of termvar),
	~U.getp(mem) = MemA, nonvar(MemA), MemA = y(I),
	~V.getp(mem) = MemB, nonvar(MemB), MemB = y(J), (I >= J ; \+ ~U.getp(fresh) = true),
	!,
	emit_ins(init(V)),
	emit_ins(unify(V, U)).
pre_update_frame_vars_2(load(U, MaybeArguments, V)) :-
	trust(U instance_of termvar),
	trust(V instance_of termunk),
	~U.getp(mem) = MemA, nonvar(MemA), MemA = y(_),
	!,
	T0 = ~termvar.new,
	T = ~T0.addp(mem, _),
	T1 = ~T.addp(type, ~type_norm(var)),
	'$absmach'(Absmach),
	Absmach.functorcons__functor(~V.value, NA),
	T2 = ~T.addp(type, ~type_norm(fnc(NA))),
	emit_ins(init(U)),
	emit_ins(load(T1, MaybeArguments, V)),
	emit_ins(unify(~U.addp(dereferenced, true), ~T2.addp(dereferenced, true))).
pre_update_frame_vars_2(I) :-
	emit_ins(I).
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Analyze and Remove Unnecessary Instructions").
% TODO: peephole optimizing?
% ---------------------------------------------------------------------------

{
:- fluid exp :: module_exp.
:- fluid bcode :: m_any.
remove_useless_ins :-
	Body0 = ~bcode,
	call((
	  state :: m_any <- [],
	  tr :: useless_ins_analyze <- ~useless_ins_analyze.new,
          tr.analyze(Body0), ~state = Needed
	)),
	call((
          state :: m_any <- Needed,
	  bcode <- ~prflatmap_ro(Body0, remove_useless_ins__2)
	)).
}.

:- class useless_ins_analyze {
    :- extends backward_analyze.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    {
    :- fluid exp :: module_exp.
    % (override)
    :- fluid state :: m_any.
    :- static transfer/1.
    transfer(X) :-
	get_needed_2(~ins_def_use(X), ~isideeffdet(X)).

    % note: state is Needed
    :- static get_needed_2/2.
    get_needed_2(move(A, B), _) :-
	trust(A instance_of termvar),
	trust(B instance_of termvar),
        ~A.name == ~B.name, !. % trick: do not assign color if the move is useless
    get_needed_2(move(A, B), _) :- !, get_needed_2(usedef([A], [B]), nosideeffdet).
    get_needed_2(usedef(Us, Ds), sideeffdet) :- !, add_needed(Ds), add_needed(Us).
    get_needed_2(usedef(Us, Ds), nosideeffdet) :- any_needed(Ds), !, add_needed(Us).
    get_needed_2(_, _).

    :- static add_needed/1.
    add_needed([]) :- !.
    add_needed([X|Xs]) :- add_needed_2(X), add_needed(Xs).

    :- static add_needed_2/1.
    add_needed_2(X) :-
	trust(X instance_of termvar),
        ( member(Grp, ~state), Grp == ~X.name ->
	    true
	; state <- [~X.name|(~state)]
	).

    % (override)
    :- '$ctxprj'(meet/2, [exp, d(state)]).
    :- static meet/2.
    meet(NeededA, NeededB) :- merge_needed_1(NeededA, NeededB, Needed), state <- Needed.

    :- '$ctxprj'(merge_needed_1/3, [exp]).
    :- static merge_needed_1/3.
    merge_needed_1([], Needed) := Needed :- !.
    merge_needed_1([X|Xs], Needed) := ~merge_needed_1(Xs, ~merge_needed_2(X, Needed)).

    :- '$ctxprj'(merge_needed_2/3, []).
    :- static merge_needed_3/2.
    merge_needed_2(X, Needed) := Needed :- member(Grp, Needed), Grp == X, !.
    merge_needed_2(X, Needed) := [X|Needed].
    }.
}.

{
:- fluid exp :: module_exp.
:- fluid code :: accum.
:- fluid state :: m_any + u.
remove_useless_ins__2(X) :-
	( useless_ins(X) -> true ; emit_ins(X) ).
}.

{
:- fluid exp :: module_exp.
:- fluid state :: m_any.
:- '$ctxprj'(useless_ins/1, [exp, u(state)]).
useless_ins(X) :-
    	ins_nosideeffdet(X),
    	Ds = ~ins_defined(X),
    	\+ any_needed(Ds).

:- '$ctxprj'(any_needed/1, [u(state)]).
any_needed([X|_]) :- is_needed(X), !.
any_needed([_|Xs]) :- any_needed(Xs).

:- '$ctxprj'(is_needed/1, [u(state)]).
is_needed(X) :- allocated_ny(X), !.
is_needed(X) :-
	trust(X instance_of termvar),
	member(Grp, ~state), Grp == ~X.name.
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Assign registers to temporary variables").
% ---------------------------------------------------------------------------

% Use the Chaitin's algorithm based on graph coloration, but here the number
% of registers is unlimited, no spill is required and some nodes are 
% precolored.
% 
% Notes about the current implementation:
%   * Register coalescing is quite limited.
%   * The interference graph is not implemented as a graph but as a list of 
%     lifetimes.
%   * Variables cannot be reused (in order to do that, a different node
%     must be used in each lifetime): SSA (static single assignment)

{
:- fluid exp :: module_exp.
:- fluid inargs :: any.
:- fluid bcode :: m_any.
assign_temp_registers :-
	Body = ~bcode,
	Arcs = ~build_arcs(~inargs, Body),
	Arcs2 = ~coalesce_moves(Body, Arcs),
	Body1 = ~assign_tempmem(Body),
	Body2 = ~assign_cvarmem(Body1),
	_ = ~assign_colors(Body2, Arcs2),
	bcode <- ~remove_coalesced_ins(Body2).
}.

:- pred ins_def_use(+I, -DU) # "@var{DU} is the use/definition
structure for the instruction @var{I}.".

% TODO: move to ptoc__ins
{
:- fluid exp :: module_exp.
ins_def_use(move(A, B)) := move(A, B) :- \+ allocated_ny(A), \+ allocated_ny(B), !.
ins_def_use(I) := usedef(~ins_used(I), ~ins_defined(I)).

isideeffdet(I) :=
	( ins_nosideeffdet(I) ? nosideeffdet
	| sideeffdet
	).
}.

% ---------------------------------------------------------------------------

{
:- fluid exp :: module_exp.
build_arcs(In, Body) := Arcs :-
	state :: lifetime_map <- ~lifetime_map.empty,
	global :: color_graph <- ~color_graph.empty,
	%
	tr :: lifetime_analyze <- ~lifetime_analyze.new,
        tr.analyze(Body),
	% TODO: state metatype is lost! this is why it is reintroduced here
	State = ~state,
	call((
	  state :: lifetime_map <- State,
	  state.ds(In),
	  state.check_undefined
	)),
	~global = Arcs.
}.

:- class lifetime_analyze {
    :- extends backward_analyze_g.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    {
    :- fluid exp :: module_exp.
    {
    :- fluid state :: lifetime_map.
    :- fluid global :: m_any.
    % (override)
    :- static transfer/1.
    transfer(I) :-
    	( state.transfer(~ins_def_use(I)) -> true
    	; errlog:bug(['lifetime_transfer failed for ', I]),
    	  fail
    	),
	( ins_needs_liveness_info(I) ->
    	    ~ins_live_set(I) = ~state.current_live_set
    	; true
	).
    }.

    {
    :- fluid state :: lifetime_map.
    % (override)
    :- static meet/2.
    meet(MapA, MapB) :- state <- ~lifetime_map.merge(MapA, MapB).
    }.
    }.
}.

:- class lifetime_map {
    :- attr used :: m_any.
    :- attr used_defined :: m_any.

    :- constructor empty_/0.
    empty_ :- ~used = [], ~used_defined = [].

    :- constructor merge_/2.
    merge_(MapA, MapB) :-
        trust(MapA instance_of lifetime_map),
        trust(MapB instance_of lifetime_map),
        used <- ~setadd(~MapA.used, ~MapB.used),
        used_defined <- ~setadd(~MapA.used_defined, ~MapB.used_defined),
        merge_check.

    :- constant merge_check/0.
    merge_check :-
        member(X, ~used), member(Y, ~used_defined), Y == X, !,
        errlog:bug(['uninitialized variable or redefinition in one brach ', X]), fail.
    merge_check.

    :- constant is_used/1.
    is_used(A) :-
	trust(A instance_of termvar),
        member((X, _), ~used),
	X == ~A.name, !.

    :- constant is_defined/1.
    is_defined(A) :-
	trust(A instance_of termvar),
        member((X, _), ~used_defined),
	X == ~A.name, !.

    :- constant check_undefined/0.
    check_undefined :-
        ( ~used = [] ->
            true
        ; errlog:bug(['used without prior definition? ', ~used]), fail
        ).

    :- constant current_live_set/1.
    current_live_set := ~as_vars(~used).

    % (private)
    :- static as_vars/2.
    as_vars([]) := [].
    as_vars([(Grp, Mem)|Ts]) := [X|~as_vars(Ts)] :-
        X0 = ~termvar.new,
	X = ~X0.addp(mem, Mem),
        ~X.name = Grp.

    {
    :- fluid global :: color_graph.
    transfer(move(U, D)) :- !, d(D), u(U).
    transfer(usedef(Us, Ds)) :- !, ds(Ds), us(Us).

    ds([]) :- !.
    ds([X|Xs]) :- d(X), ds(Xs).

    d(A) :- allocated_ny(A), !.
    d(A) :-
	trust(A instance_of termvar),
        is_defined(A), !, errlog:bug(['redefined? ', ~A.name]), fail.
    d(A) :-
	trust(A instance_of termvar),
        used_defined <- [(~A.name, ~A.getp(mem))|(~used_defined)],
        Used = ~used, used <- Used2,
        ( select((X, _), Used, Used2), X == ~A.name -> true ; Used2 = Used ).

    us([]) :- !.
    us([X|Xs]) :- u(X), us(Xs).

    u(A) :- allocated_ny(A), !.
    u(A) :- is_defined(A), !,
	trust(A instance_of termvar),
        errlog:bug(['used prior to definition? ', ~A.name]), fail.
    u(A) :- is_used(A), !.
    u(A) :-
	trust(A instance_of termvar),
        Used = ~used,
        global.add_arcs((~A.name, ~A.getp(mem)), Used),
        used <- [(~A.name, ~A.getp(mem))|Used].
    }.
}.

:- class color_graph {
    % K-colorability graph for register assignment
    :- '$statemodel'(pair).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor empty_/0.
    empty_ :- self <- [].

    % add new arcs from (Grp,Mem) to each of the list To
    add_arcs((Grp, Mem), To) :-
        Arcs = ~self,
        ( select(t(Grp0, _, OldTo), Arcs, Arcs2), Grp == Grp0 ->
            TotalTo = ~setadd(OldTo, To),
            NewTo = ~setsub(OldTo, To)
        ; NewTo = To,
          TotalTo = To,
          Arcs2 = Arcs
        ),
	self <- [t(Grp, Mem, TotalTo)|~add_arcs_2(Arcs2, NewTo, (Grp, Mem))].

    % (private)
    :- static add_arcs_2/4.
    add_arcs_2([], _, _) := [].
    add_arcs_2([t(Grp, Mem, Xs)|Ts], To, X) :=
           [t(Grp, Mem, [X|Xs])|~add_arcs_2(Ts, To, X)] :-
        member((Grp0, _), To), Grp0 == Grp, !.
    add_arcs_2([T|Ts], To, X) := [T|~add_arcs_2(Ts, To, X)] :- !.

    % TODO: Why not 'constant'? It is never changed.
    assign_colors_2(move(A, B)) :-
	trust(A instance_of termvar),
	trust(B instance_of termvar),
	% Trick: do not assign color if the move is useless.
	~A.name == ~B.name, !.
    assign_colors_2(move(A, B)) :- !, assign_colors_2(usedef([A], [B])).
    assign_colors_2(usedef(Us, Ds)) :- !,
        assign_color_xs(Us),
        assign_color_xs(Ds).
    assign_colors_2(_) :- !. % TODO: Something here?

    :- constant assign_color_xs/1.
    assign_color_xs([]) :- !.
    assign_color_xs([X|Xs]) :- !,
        assign_color(X),
        assign_color_xs(Xs).

    :- constant assign_color/1.
    assign_color(A) :-
        ( unallocated_temp(A) ->
	    trust(A instance_of termvar),
            ~A.getp(mem) = ~free_color(~A.name)
        ; true
        ).

    % Smallest register that is not used by any of the adjacent nodes.
    :- constant free_color/2.
    free_color(Grp) := Mem :-
        Mem0 = ~color,
        \+ colored_neighbor(Grp, Mem0), !,
	% Needed because Mem should be var before testing the
	% possible conflict.
        Mem = Mem0.

    :- constant colored_neighbor/2.
    colored_neighbor(Grp, Mem) :-
        (_, Mem2) = ~arcs_adjacent(Grp), Mem == Mem2, !.

    :- static color/1.
    color := ~color_2(0).

    :- static color_2/2.
    color_2(I) := x(I).
    color_2(I) := ~color_2(I1) :- I1 is I + 1.

    % (Node, Color) adjacents to GrpA node
    :- constant arcs_adjacent/2.
    arcs_adjacent(Grp) := X :-
        member(t(Grp0, _, Xs), ~self), Grp == Grp0, !,
        member(X0, Xs),
        X0 = (GrpX, _),
        \+ GrpX == Grp,
        X = X0.

    % Safe if doesn't affect the K-colorability of the graph
    %
    % (from http://www.math.grin.edu/~rebelsky/Courses/CS362/2002F/2001S/Outlines/outline.33.html)
    %    * Briggs heuristic: Coalesce a and b if no neighbor of the
    %      combined node will have k or more edges.
    %    o This does seem safe, doesn't it? After coalescing, we can
    %      still remove all neighbors, so we haven't changed the
    %      colorability of the graph.
    %    * The George heuristic: Coalesce a and b if, for all
    %      neighbors n of a, either n already interferes with b or n
    %      has degree less than k.
    %    o Hmm ... analysis of this is harder. We need to consider the
    %      graphs that result after removing insignificant degree
    %      neighbors.
    %
    % As we have precolored nodes and K=+inf, the rule is:
    %    * A and B (precolored with N) can be coalesced if none of the
    %      adjacents of A is precolored with N

    :- constant can_coalesce/2.
    can_coalesce(A, B) :-
        trust(A instance_of termvar),
	trust(B instance_of termvar),
        \+ ~A.name == ~B.name,
        ( unallocated_temp(A), allocated_temp(B) -> % only B precolored
            \+ conflict(~A.name, ~B.name, ~B.getp(mem))
        ; allocated_temp(A), unallocated_temp(B) -> % only A precolored
            \+ conflict(~B.name, ~A.name, ~A.getp(mem))
        ; unallocated_temp(A), unallocated_temp(B) % none precolored
        ).

    % The var with group GrpA is adjacent to some var with mem M, that is not in group GrpB
    :- constant conflict/3.
    conflict(GrpA, GrpB, MemB) :-
        (Grp, Mem) = ~arcs_adjacent(GrpA), Mem == MemB, \+ Grp == GrpB, !.

    % Merge two nodes in the graph
    arcs_coalesce(GrpA, GrpB) :-
        ~arcs_lift_selt(GrpA) = t(_, MemA, XsA), % select
        ~arcs_lift_selt(GrpB) = t(_, MemB, XsB), % select
        Xs = ~merge_arcs_to(XsB, XsA), % hmmm not so easy
	% TODO: check that coalescing does not affect variables with the same name from other clauses
	% errlog:trace([warning_arcs_coalesce(GrpA,GrpB)]),
        GrpA = GrpB,
        MemA = MemB,
        self <- [t(GrpA, MemA, Xs)|(~self)].

    arcs_lift_selt(Grp) := T :-
        ~self = Arcs,
        select(T, Arcs, Arcs2),
        T = t(Grp0, _, _), Grp0 == Grp, !,
        self <- Arcs2.
    arcs_lift_selt(_) := t(_, _, []).

    % (private)
    :- static merge_arcs_to/3.
    merge_arcs_to([], Ys) := Ys :- !.
    merge_arcs_to([X|Xs], Ys) := ~merge_arcs_to(Xs, ~merge_arcs_to_2(X, Ys)).

    % (private)
    :- static merge_arcs_to_2/3.
    merge_arcs_to_2(X, Ys) := Ys :- member(X0, Ys), X0 == X, !.
    merge_arcs_to_2(X, Ys) := [X|Ys].
}.

% :- module set {
setadd([],As,As) :- !.
setadd([X|Xs],As,Bs) :- member(X0,As), X0 == X, !, setadd(Xs,As,Bs).
setadd([X|Xs],As,[X|Bs]) :- setadd(Xs,As,Bs).

setsub([],As,As) :- !.
setsub([X|Xs],As,Bs) :- select(X0,As,As2), X0 == X, !, setsub(Xs,As2,Bs).
setsub([_|Xs],As,Bs) :- setsub(Xs,As,Bs).
% }.

% ---------------------------------------------------------------------------

% TODO: Pending tasks here:
%  - remove vars, unify names and insert the var <- if you want
%    coalesce two uncolored nodes.

{
:- fluid exp :: module_exp.
coalesce_moves(Body, Arcs) := Arcs2 :-
	global :: m_any <- Arcs,
	tr :: coalesce_pass <- ~coalesce_pass.new,
	tr.pass(Body),
	Arcs2 = ~global.
}.

:- class coalesce_pass {
    :- extends forward_pass_g.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    {
    :- fluid exp :: module_exp.
    {
    :- fluid global :: color_graph.
    % (override)
    :- static transfer/1.
    transfer(I) :-
    	( ~ins_def_use(I) = move(A, B), global.can_coalesce(A, B) ->
	    trust(A instance_of termvar),
	    trust(B instance_of termvar),
    	    global.arcs_coalesce(~A.name, ~B.name),
    	    ~A.getp(mem) = ~B.getp(mem)
    	; true
    	).
    }.
    }.
}.

% assign colors to the nodes
{
:- fluid exp :: module_exp.
assign_colors(Xs, Arcs) := Arcs2 :-
	global :: m_any <- Arcs,
	tr :: assign_colors_pass <- ~assign_colors_pass.new,
	tr.pass(Xs),
	~global = Arcs2.
}.

:- class assign_colors_pass {
    :- extends backward_pass_g.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    {
    :- fluid exp :: module_exp.
    % TODO: choose a transform to insert decl code?...
    % note: global is Arcs
    {
    :- fluid global :: color_graph.
    % (override)
    :- static transfer/1.
    transfer(I) :-
    	UseDef = ~ins_def_use(I),
    	global.assign_colors_2(UseDef).
    }.
    }.
}.

% ---------------------------------------------------------------------------
% Mark variables that will be required in livesets so that they will
% be assigned X registers (the rest of temporal variables will be
% assigned cvar registers)

{
:- fluid exp :: module_exp.
assign_tempmem(Xs) := Xs2 :-
	Xs2 = ~prflatmap(Xs, assign_tempmem_1).
{
:- fluid code :: accum.
assign_tempmem_1(I) :-
        ( ins_needs_liveness_info(I) ->
	    Set = ~ins_live_set(I),
	    assign_tempmem_xs(Set)
	; true
	),
	emit_ins(I).
}.
}.

assign_tempmem_xs([]).
assign_tempmem_xs([X|Xs]) :-
	assign_tempmem_x(X),
	assign_tempmem_xs(Xs).

assign_tempmem_x(A) :-
	trust(A instance_of termvar),
	unallocated(A),
	tagged = ~get_imptype(A),
	!,
	~A.getp(mem) = x(_).
assign_tempmem_x(_) :- !.

{
:- fluid exp :: module_exp.
assign_cvarmem(Xs) := Xs2 :-
	Xs2 = ~prflatmap(Xs, assign_cvarmem_1).
{
:- fluid code :: accum.
assign_cvarmem_1(I) :-
        UseDef = ~ins_def_use(I),
	call(( cvars :: accum(Cvars), assign_cvarmem_2(UseDef) )),
	decl_cvars(Cvars),
	emit_ins(I).

{
:- fluid cvars :: accum.
:- '$ctxprj'(assign_cvarmem_2/1, [cvars]).
assign_cvarmem_2(move(A, B)) :-
	assign_cvarmem_2(usedef([A], [B])).
assign_cvarmem_2(usedef(_Us, Ds)) :-
	assign_cvarmem_xs(Ds).

:- '$ctxprj'(assign_cvarmem_xs/1, [cvars]).
assign_cvarmem_xs([]).
assign_cvarmem_xs([X|Xs]) :- assign_cvarmem_x(X), assign_cvarmem_xs(Xs).

% TODO: note: coloring of cvar mems is up to the C compiler...
:- '$ctxprj'(assign_cvarmem_x/1, [cvars]).
assign_cvarmem_x(A) :-
	trust(A instance_of termvar),
	( unallocated(A) -> ~A.getp(mem) = mvar(_) ; true ),
	!,
	( ~A.getp(mem) = mvar(_) -> cvars.add(A) ; true ).
}.

:- '$ctxprj'(decl_cvars/1, [code]).
decl_cvars([]).
decl_cvars([X|Xs]) :-
	emit_ins(mvardecl(X)),
	decl_cvars(Xs).
}.
}.

% ---------------------------------------------------------------------------

% TODO: exp should not be necessary here
{
:- fluid exp :: module_exp.
remove_coalesced_ins(Xs) := Xs2 :- Xs2 = ~prflatmap(Xs, remove_coalesced_ins_f).
% TODO: exp is not necessary in remove_coalesced_ins_f
{
:- fluid code :: accum.
remove_coalesced_ins_f(move(U, V)) :-
	trust(U instance_of termvar),
	trust(V instance_of termvar),
	~U.getp(mem) == ~V.getp(mem), !.
remove_coalesced_ins_f(I) :- emit_ins(I).
}.
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Mark Last Instruction").
% ---------------------------------------------------------------------------

% Success continuation:
%
%  builtin: it makes a push (i.e. via C stack) of continuation.
%
%  default: it uses the cont register - a frame is needed if the
%    previous cont is required (we pass a next continuation which will
%    require the current continuation) - if the next continuation is a
%    proceed, we can pass directly the current continuation oas next
%    continuation (proceed is like an identity for the continuations)
%
% TODO: document fail cont
%
%  builtin: it makes a push (i.e. via C stack) of continuation.
%
%  default: it uses the cont register.
%
% - A frame is needed if the previous cont is required (we pass a next
%   continuation which will require the current continuation).
%
% - If the next continuation is a proceed, we can pass directly the
%   current continuation as next continuation (proceed is like an
%   identity for the continuations).
	
% Note: information about a particular call:
%   ~G.getp(contmem) = push

% any - call with framelivesize=0 - calls without contmem = x
{
:- fluid exp :: module_exp.
:- fluid bcode :: m_any.
mark_last_ins :-
	Is = ~bcode,
	(Is00, I2) = ~prlast(Is), 
	(Is0, I) = ~prlast(Is00), 
	ins_is_proceed(I2),
	ins_clear_regs(I),
	~ins_frame_live_size(I) = 0, !,
	bcode <- Is0 .&. (~single(~ins_mark_last(I)) .&. ~single(~ins_mark_last(I2))).
mark_last_ins :-
	Is = ~bcode,
	(Is0, I2) = ~prlast(Is),
	ins_is_proceed(I2), !,
	bcode <- Is0 .&. ~single(~ins_mark_last(I2)).
mark_last_ins.
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Variable Initialization and Stack Management").
% (requires: mark_last_ins)
%
% Add variable initialization, frame creation, frame destruction
% and frame trimming instructions.
% ---------------------------------------------------------------------------

% TODO: Some goals require complete frames to be executed; 'frame
%   state' contains the initial 'frame size', so that it is not
%   necessary to fill every cell from the beginning and frames can be
%   completed later.

{
:- fluid exp :: module_exp.
:- fluid bcode :: m_any.
add_var_inits :-
	state :: m_any <- no_frame,
	tr :: add_var_inits_transform <- ~add_var_inits_transform.new,
	bcode <- ~tr.map(~bcode).
}.

:- class add_var_inits_transform {
    :- extends forward_transform.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    {
    :- fluid exp :: module_exp.
    :- fluid code :: accum.
    :- fluid state :: m_any.
    % (override)
    :- static transfer/1.
    transfer(I) :- ins_last(I), !,
    	FrameState0 = ~state,
    	ensure_no_frame,
    	( ~frame_completion(FrameState0) = frame_complete, ins_is_proceed(I) ->
    	    % add a check_events here: a frame was necessary, that implies a call, which may have consumed the ensured heap space, so the check_events test is necessary
    	    Args = ~ins_args(I), % TODO: correct?
    	    emit_ins(check_events(Args))
    	; true
    	),
	emit_ins(I).
    transfer(I) :-
    	FrameState0 = ~state,
    	update_frame(I),
    	FrameState1 = ~state,
    	( Ys = ~get_y_vars(~ins_defined(I)), \+ Ys = [] ->
    	    check_can_output_y_vars(I),
    	    ( ~frame_completion(FrameState1) = frame_complete ->
    	        update_frame_vars(I)
    	    ; ensure_frame,
    	      init_mark_vars(Ys),
    	      emit_ins(I)
    	    )
    	; ins_is_cut(I), ~frame_completion(FrameState0) = frame_complete ->
    	    emit_ins(validate_local_top),
    	    emit_ins(I)
    	; ins_is_cut(I), ~frame_completion(FrameState0) = no_frame ->
    	    emit_ins(invalidate_local_top),
    	    emit_ins(I)
    	; emit_ins(I)
    	),
    	( \+ max(0) = ~ins_frame_usage(I), ~frame_completion(FrameState1) = frame_complete ->
    	    % a frame is required and the called goal may do a stack overflow (thus frame cached register must be updated)
    	    emit_ins(recover_frame) % recover cached frame variable
    	; true
    	).

    % Update the instructions with the new initialization state of the
    % Y variables.
    %
    % Note: if the frame is completed, replace the move or init by a
    %   unify or move; else remember that the var is initialized and
    %   avoid its initialization when completing the frame).
    :- static update_frame_vars/1.
    :- '$ctxprj'(update_frame_vars/1, [code]).
    update_frame_vars(init(A)) :-
	trust(A instance_of termvar),
	~A.getp(mem) = y(_), !.
    update_frame_vars(move(A, B)) :-
	trust(B instance_of termvar),
    	~B.getp(mem) = y(_), !, % TODO: hmmm a bit dirty...
    	emit_ins(unify(A, ~B.addp(fresh, true))).
    update_frame_vars(I) :- !,
    	emit_ins(I).

    % Instructions that can have Y registers as output (this is
    % precondition).
    %
    % TODO: Study if it is useful to consider other instructions here
    %   (of course, changing it will affect much more code).
    :- static check_can_output_y_vars/1.
    :- '$ctxprj'(check_can_output_y_vars/1, []).
    check_can_output_y_vars(init(_)) :- !.
    check_can_output_y_vars(move(_, _)) :- !.
    check_can_output_y_vars(I) :-
    	errlog:bug(['instruction ', I,
    	          ' cannot have Y registers as output arguments']),
    	fail.

    :- static get_y_vars/2.
    :- '$ctxprj'(get_y_vars/2, []).
    get_y_vars([X|Xs]) := [Y|~get_y_vars(Xs)] :-
	trust(X instance_of termvar),
        ~X.getp(mem) = y(Y), !.
    get_y_vars([_|Xs]) := ~get_y_vars(Xs).
    get_y_vars([]) := [].

    % Frame state:
    %   frame_complete: frame created and complete
    %   frame_incomplete(_): frame created
    %   no_frame: no frame created
    :- static frame_completion/2.
    :- '$ctxprj'(frame_completion/2, []).
    frame_completion(FrameState) := no_frame :- FrameState = no_frame, !.
    frame_completion(FrameState) := frame_incomplete :- FrameState = frame_incomplete(_), !.
    frame_completion(FrameState) := frame_complete :- FrameState = frame_complete, !.

    :- static init_mark_vars/1.
    :- '$ctxprj'(init_mark_vars/1, [state]).
    init_mark_vars([]).
    init_mark_vars([X|Xs]) :- init_mark_var(X), init_mark_vars(Xs).

    :- static init_mark_var/1.
    :- '$ctxprj'(init_mark_var/1, [state]).
    init_mark_var(_) :- ~state = frame_complete, !.
    init_mark_var(Y) :- ~state = frame_incomplete(Ys), !,
    	state <- frame_incomplete(~intset_insert(Ys, Y)).

    :- static ensure_frame/0.
    :- '$ctxprj'(ensure_frame/0, [code, state]).
    ensure_frame :-
    	( ~state = no_frame ->
    	    emit_ins(alloc),
    	    state <- frame_incomplete([])
    	; true
    	).

    % TODO: Rework until cframe.
    :- static update_frame/1.
    update_frame(I) :-
    	( ins_should_trim_frame(I) ->
    	    ensure_frame,
    	    complete_or_trim_frame(~ins_frame_live_size(I))
    	; ins_clear_regs(I) ->
    	    ensure_frame,
    	    cframe(~ins_frame_live_size(I))
    	; ins_needs_cframe(I) ->
    	    cframe(~ins_frame_live_size(I))
    	; true
    	).

    :- static complete_or_trim_frame/1.
%    :- '$ctxprj'(complete_or_trim_frame/1, [state, code, exp]).
    complete_or_trim_frame(FrameSize) :-
    	( ~state = frame_complete ->
    	    emit_ins(trim_frame(~termunk.new(FrameSize)))
    	; cframe(FrameSize)
    	).

    :- static cframe/1.
%    :- '$ctxprj'(cframe/1, [state, code, exp]).
    cframe(FrameSize) :-
    	( ~state = frame_incomplete(Ys) ->
    	    init_frame_vars(~numbers(0, FrameSize), Ys),
    	    emit_ins(cframe(~termunk.new(FrameSize))),
    	    state <- frame_complete
    	; true
    	).

    :- static ensure_no_frame/0.
    :- '$ctxprj'(ensure_no_frame/0, [state, code]).
    ensure_no_frame :-
    	Frame0 = ~state,
    	( Frame0 = no_frame ->
    	    true
    	; Frame0 = frame_complete ->
    	    emit_ins(dealloc)
    	),
    	state <- no_frame.

    :- static init_frame_vars/2.
    :- '$ctxprj'(init_frame_vars/2, [code, exp]).
    init_frame_vars([], _) :- !.
    init_frame_vars([X|Xs], Inits) :- intset_in(X, Inits), !,
    	init_frame_vars(Xs, Inits).
    init_frame_vars([X|Xs], Inits) :- 
    	T0 = ~termvar.new,
	T1 = ~T0.addp(mem, y(X)),
	T = ~T1.addp(type, ~type_norm(var)),
    	emit_ins(init(T)),
    	init_frame_vars(Xs, Inits).
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid state :: m_any + d.
    % (override)
    :- static meet/2.
    meet(FrameStateA, FrameStateB) :-
    	( FrameStateA == FrameStateB ->
    	    state <- FrameStateA
    	; errlog:bug(['cannot merge ', FrameStateA, ' ', FrameStateB]), fail
    	).
    }.
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Heap-usage Analysis").
% This pass fills the 'Heap' attributes in goals with estimated heap
% usage. Eventually, instructions are generated to make sure that
% enough heap is allocated to build terms and call builtins.
% ---------------------------------------------------------------------------

% TODO: Try to redesign this part.
{
:- fluid exp :: module_exp.
:- fluid inargs :: any.
:- fluid bcode :: m_any.
heap_usage :-
	Body = ~bcode,
	Kind = ~code_uses_frame(Body),
	Body2 = ~insert_ensure_heap(~inargs, Body, Kind),
	( Kind = unit -> % TODO: why? because of the check_events that are added later?
	    '$absmach'(Absmach),
	    Absmach.heap_pad_size(cont, Heap0)
	; Heap0 = 0
	),
	_ = ~analyze_heap_usage(Body2, Heap0),
	bcode <- ~filter_heap_usage(Body2).
}.

% TODO: Why?
code_uses_frame(Xs) := nounit :- prmember(alloc, Xs), !.
code_uses_frame(_) := unit.

{
:- fluid exp :: module_exp.
:- fluid inargs :: any.
:- fluid choiceinfo :: any.
:- fluid checkevents :: any.
:- fluid bcode :: m_any.
add_check_events :-
	~choiceinfo = choiceinfo(ChoiceState, AltPos, _, _),
	Body3 = ~bcode,
	Kind = ~code_uses_frame(Body3),
	( needs_check_events(ChoiceState, Kind, AltPos, Body3) ->
	    bcode <- ~single(check_events(~inargs)) .&. Body3
	; true
	).
}.

% TODO: Sure? Can it be improved?
{
:- fluid exp :: module_exp.
:- fluid checkevents :: any.
needs_check_events(ChoiceState, Kind, AltPos, Xs) :-
	( ~checkevents = no ->
	    fail
	; ChoiceState = no_choice, AltPos = last_alt ->
	    ( Kind = unit ->
	        % Because it needs a frame (and a frame stack
	        % expansion before the frame is created may be
	        % required).
	        true
	    ; needs_check_events__2(Xs) ->
	        true
	    ; fail
	    )
	; ChoiceState = no_choice, AltPos = first_alt ->
	    true
	; fail
	).
}.

% There exist a instruction that needs ensure_heap.
{
:- fluid exp :: module_exp.
needs_check_events__2(Xs) :-
	prmember(X, Xs),
	  \+ ins_does_not_need_ensure_heap(X).

% The instruction does not require ensure heap from the caller.
ins_does_not_need_ensure_heap(X) :-
	HeapUsage = ~ins_heap_usage(X),
	( HeapUsage = max(0) -> true % no requires heap
	; HeapUsage = info -> true % does special heap treatment
	; HeapUsage = unknown % does an internal ensure_heap
	).

insert_ensure_heap(Args, Code, Kind) :=
	~single(pre_ensure_heap(Kind, Args, _)) .&. ~insert_ensure_heap_2(Code).

insert_ensure_heap_2(Xs) := Xs2 :-
	Xs2 = ~prflatmap(Xs, insert_ensure_heap_3).
% TODO: The arity can be nonzero! (if the instruction is not a call)?
{
:- fluid code :: accum.
insert_ensure_heap_3(I) :-
	\+ ins_last(I),
	% TODO: This means that the instruction does an internal
	%   ensure_heap.
	HeapUsage = ~ins_heap_usage(I),
	HeapUsage = unknown,
	!,
	Args = ~ins_args(I),
	Modes = ~ins_argmodes(I),
	ExitTypes = ~ins_exittypes(I),
	( ins_bottom_shtdef(I) ->
	    emit_ins(I)
	; % The input arguments for the next chunk will be Out.
	  % TODO: this is not always true! (<- when not?)
	  Out = ~ziptypes(~filter_mode(Args, Modes, out),
	                  ~filter_mode(ExitTypes, Modes, out)),
	  emit_ins(I),
	  emit_ins(pre_ensure_heap(cont, Out, _))
	).
insert_ensure_heap_3(I) :-
	emit_ins(I).
}.
}.

% Analyze the maximum 'uncontrolled' heap usage:
%
%  - Traverse the program in reverse order counting the heap usage of
%    each instruction.
%
%  - Reset heap usage if a goal/instruction which can do heap
%    expansion/GC.
{
:- fluid exp :: module_exp.
analyze_heap_usage(Xs, Heap0) := Heap :-
	state :: m_any <- Heap0,
	tr :: heap_usage_analyze <- ~heap_usage_analyze.new,
	tr.analyze(Xs),
	~state = Heap.
}.

:- class heap_usage_analyze {
    :- extends backward_analyze.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    {
    :- fluid exp :: module_exp.
    % Note: state is Heap
    {
    :- fluid state :: m_any.
    % (override)
    :- static transfer/1.
    transfer(pre_ensure_heap(_, _, Size)) :- !,
    	% Unify Size with the input Heap.
    	Size = ~state,
    	state <- 0. % Reset heap usage.
    transfer(I) :-
    	% Annotate the instruction with the heap usage at this point.
    	Heap0 = ~state,
    	( ins_needs_liveness_info(I) ->
    	    ~ins_heap(I) = Heap0
    	; true % TODO: is this true or convenient? Leave the heap prop
	       % unbound.
    	),
    	HeapUsage = ~ins_heap_usage(I),
    	( HeapUsage = unknown ->
	    % Reset heap usage (heap checks are done internally)
    	    state <- 0
    	; HeapUsage = info ->
    	    true % Do nothing % TODO: is this correct?
    	; HeapUsage = max(Heap1),
    	  Heap is Heap0+Heap1,
    	  state <- Heap
    	).
    }.

    {
    :- fluid state :: m_any + d.
    % (override)
    :- static meet/2.
    meet(A, B) :- state <- ~max(A, B).
    }.
    }.
}.

{
:- fluid exp :: module_exp.
% TODO: exp is not necessary in filter_heap_usage_2
filter_heap_usage(Xs) := Xs2 :- Xs2 = ~prflatmap(Xs, filter_heap_usage_2).
{
:- fluid code :: accum.
filter_heap_usage_2(pre_ensure_heap(Kind, Args, Size)) :- !,
	'$absmach'(Absmach),
	( Absmach.heap_pad_size(Kind, HeapUnitSize), Size > HeapUnitSize ->
	    emit_ins(ensure_heap(completed_choice, Size, Args))
	; true
	).
filter_heap_usage_2(I) :-
	emit_ins(I).
}.
}.

% TODO: ensure_heap ins is missing in ptoc__ins.
% TODO: pre_ensure_heap ins is missing in ptoc__ins.

% ---------------------------------------------------------------------------
:- doc(subsection, "Specialize using props mem, fresh, etc. ").
% ---------------------------------------------------------------------------

{
:- fluid exp :: module_exp.
:- fluid bcode :: m_any.
speccalls :-
	bcode <- ~prflatmap(~bcode, speccalls_1).
}.

{
:- fluid exp :: module_exp.
{
:- fluid code :: accum.
speccalls_1(unify(A, B)) :- !, spec_unify(A, B).
speccalls_1(X) :- emit_ins(X).

spec_unify(A, B) :-
	trust(A instance_of termvar),
	trust(B instance_of termvar),
	~A.name == ~B.name, !.
spec_unify(A0, B0) :-
	% Fresh = var type, dereferenced and not referenced by anyone
	% (the variable was initialized but we don't care about its
	% value, except for trailing).
	( (A0, B0) = (A, B) ; (B0, A0) = (A, B) ),
	trust(A instance_of termvar),
        ~A.getp(fresh) = true, ~A.getp(mem) = y(_), !,
	Trail = call(~trail_if_conditional_new(A)),
	X1 = move(B, ~A.delp(fresh)),
	emit_ins(Trail),
	emit_ins(X1).
% TODO: Patching ptoc props of unify is not a good idea.
spec_unify(A, B) :-
	ImpTypes = [~get_imptype(A), ~get_imptype(B)],
	UnifyPredId = ~predicate_x.reg_new('term_basic:$unify'/2),
	ImpSpecialize = ~UnifyPredId.get_prop(impspecialize),
	member(on(ImpTypes, SpecializedAtom), ImpSpecialize),
	!,
	SpecId = ~predicate_x.reg_new(SpecializedAtom/2),
	X = call(~strgoal.new_n(SpecId, [A, B])),
	emit_ins(X).
/*
spec_unify(A, B) :- % Test! disabled spec unify
	~get_imptype(A) = tagged,
	~get_imptype(B) = tagged,
	!,
	X = call(~unify(A, B)),
	emit_ins(X).
*/
spec_unify(A, B) :-
	~get_imptype(A) = tagged,
	~get_imptype(B) = tagged,
	!,
	( varvaluetype(A) ->
%	    X = call(~unify(A, B))
	    X = call(~bind_new(A, B))
	; varvaluetype(B) ->
%	    X = call(~unify(A, B))
	    X = call(~bind_new(B, A))
	; one_cell_types(A, B) ->
	    % TODO: Add different equality checks for NUM or ATM?
	    %   (blob types are compared in a different way).
	    X = call(~equal_new(A, B))
	; X = call(~unify(A, B))
	),
	emit_ins(X).
spec_unify(A, B) :-
	errlog:bug(['unknown unification method for ', A, ' and ', B]),
	fail.
}.

% Simplified type for bind operations.
varvaluetype(A) :-
	trust(A instance_of termvar),
        typemode_is_var(~A.getp(type)).

% A and B are nonvars, and at least one of them is represented in one
% single cell.
one_cell_types(A, B) :-
	trust(A instance_of termvar),
	trust(B instance_of termvar),
	AType = ~A.getp(type),
	BType = ~B.getp(type),
	typemode_is_nonvar(AType),
	typemode_is_nonvar(BType),
	OneCellType = ~type_norm((smallint;atom)),
	( type_consequence(AType, OneCellType) -> true
	; type_consequence(BType, OneCellType)
        ).
}.

% ---------------------------------------------------------------------------
% nondetblocks ('or' code) -> detblocks (push_choice, continuations, etc.)
% TODO: include orbinarization here (done but not polished...)
% ---------------------------------------------------------------------------

{
:- fluid exp :: module_exp.
n_switch(Code3, Code) :-
	% TODO: is that necessary if I run the global_jump_opt??
	call(( code :: accum(Code4), emit_jumps(Code3) )),
	call((
	  code :: accum(Code0),
	  n_switch__2(Code4, Extra),
	  emit_code(Extra)
        )),
	Code = ~jump_opt(Code0).
}.

{
:- fluid exp :: module_exp.
:- fluid code :: accum.
n_switch__2([], []).
n_switch__2([X0|Xs], Extra) :-
	X0 = switch_on_index(A1, Cases0, DefCode0),
	!,
	DefaultL = local(_),
	trust(Cases0 instance_of termunk),
	trust(DefCode0 instance_of termunk),
	Cases1 = ~Cases0.value,
	DefCode1 = ~DefCode0.value,
	VarCases1 = ~filter_case_by_type(Cases1, ~type_norm(var)),
	ListCases1 = ~filter_case_by_type(Cases1, ~type_norm(list)),
	OtherCases1 = ~filter_case_by_type(Cases1, ~type_norm((str;atomic))),
	call((
          code :: accum(Extra),
	  subindex_single(VarCases1, DefaultL, VarL),
	  subindex_single(ListCases1, DefaultL, ListL),
	  label_cases(OtherCases1, OtherCases2),
	  code.add(label(DefaultL)),
	  emit_jumps(DefCode1)
        )),
	X = switch_on_index(A1, ~termunk.new(VarL), ~termunk.new(ListL), ~termunk.new(OtherCases2), ~termunk.new(DefaultL)),
	code.add(X),
	n_switch__2(Xs, _Extra).
n_switch__2([X|Xs], Extra) :- !,
	code.add(X),
	n_switch__2(Xs, Extra).

:- '$ctxprj'(filter_case_by_type/3, [exp]).
filter_case_by_type([], _) := [] :- !.
filter_case_by_type([Case|Cases], Type) := [Case|~filter_case_by_type(Cases, Type)] :-
	Case = case(KeyType, _),
%	errlog:trace([filtering(Type)]),
%	( errlog:trace([kt_______(~'$mut__value'(KeyType))]) -> true
%	; errlog:trace([kt______g(KeyType)])
%	),
	\+ type_contradictory(KeyType, Type), !.
%	errlog:trace([added]).
filter_case_by_type([_|Cases], Type) := ~filter_case_by_type(Cases, Type) :- !.

:- '$ctxprj'(subindex_single/3, [code]).
subindex_single(Cases1, DefaultL, Label) :-
	( Cases1 = [] ->
	    Label = DefaultL
	; Cases1 = [case(_, CaseCode)] ->
	    Label = local(_),
	    code.add(label(Label)),
	    emit_jumps(CaseCode)
	; errlog:bug(['wrong cases in subindex_single ', Cases1]), fail
	).

label_cases([], []).
label_cases([case(KeyType, Code0)|Xs], [Key-L1|Cases]) :-
	Key = ~type_to_key(KeyType),
	L1 = local(_), 
	code.add(label(L1)),
	emit_jumps(Code0),
	label_cases(Xs, Cases).
}.

% TODO: Simplify, look at choiceinfo.
{
:- fluid choiceinfo :: any.
:- fluid failinfo :: any.
:- fluid bcode :: m_any.
add_choice_ins :-
	~failinfo = failinfo(Modes, Args), 
	( ~choiceinfo = choiceinfo(no_choice, AltPos, _, yes), \+ AltPos = last_alt ->
	    In = ~filter_mode(Args, Modes, in),
	    Code = ~bcode,
	    Code1 = ~single(push_choice(In)) .&. Code,
	    bcode <- ~single(update_default_choice) .&. Code1
	; ~choiceinfo = choiceinfo(no_choice, _, AbsCut, _),
	  AbsCut = cut_to_previous_choice ->
	    % TODO: do not update previous_choice if there is no cut (or similar)
	    Code = ~bcode,
	    bcode <- ~single(update_default_choice) .&. Code
	; true
	).
}.

% TODO: Add failcont arguments to remove the dummy neck (<- check
%   again).
% TODO: Analyze before adding the neck to remove the dummy_cut.
% TODO: Not very clean: it needs semidet or det calls in the
%   if-then-elses (gotos) and proceeds followed by nothing requires:
%   the next code passes do not have to change the properties
%   uses_trail modifies_choice_on_success, nor add push_choice, neck,
%   dummy_cut instructions.

{
:- fluid exp :: module_exp.
add_fail_cont(Fail, Is) := Is2 :-
	state :: m_any <- Fail,
	tr :: add_fail_transform <- ~add_fail_transform.new,
	Is2 = ~tr.map(Is).
}.

:- class add_fail_transform {
    :- extends forward_transform.
    :- '$statemodel'(single).
    :- '$raw_state'.

    % TODO: Missing instance_of__/1

    :- constructor new_/0.
    new_.

    {
    :- fluid exp :: module_exp.
    :- fluid code :: accum.
    :- fluid state :: m_any.
    % (override)
    :- static transfer/1.
    transfer(I) :-
    	~state = Fail,
    	% TODO: Rename by fail_annotate?
    	( I = call(G) ->
	    trust(G instance_of strgoal),
    	    emit_ins(call(~G.addp(fail, Fail)))
    	; emit_ins(I)
    	),
    	next_fail_cont(I).
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid state :: m_any + d.
    % (override)
    :- static meet/2.
    meet(FailA, FailB) :- FailA == FailB, !, state <- FailA.
    meet(nextalt(RestoreA), nextalt(RestoreB)) :- merge_restore(RestoreA, RestoreB, RestoreC), !,
    	state <- nextalt(RestoreC).
    meet(FailA, FailB) :-
    	errlog:bug(['cannot merge fail conts ', FailA, ' ', FailB]),
    	fail.
    }.
}.

merge_restore(X, X, X) :- !.
merge_restore(restore_mem, no_restore, restore_mem) :- !.
merge_restore(no_restore, restore_mem, restore_mem) :- !.

% TODO: Generate the alternatives as they are needed? (do not do an
%   extra pass to check).
%
%   Fail if the choice changes: a choice point can be created or
%   (semidet preserves the fail continuation because we are
%   considering only the success branch)
%
% TODO: This is not complete.
% TODO: See add_choice_ins, deduce this first clause using that info
%   instead of push_choice ins.
{
:- fluid exp :: module_exp.
:- fluid state :: m_any.
next_fail_cont(I) :- state <- ~next_fail_cont__2(I, ~state).

:- '$ctxprj'(next_fail_cont__2/3, [exp]).
next_fail_cont__2(push_choice(_), nextalt(_)) := nextalt(restore_mem) :- !.
next_fail_cont__2(neck(_, _), nextalt(_)) := nextalt(restore_all) :- !.
next_fail_cont__2(dummy_cut, nextalt(_)) := fail :- !.
next_fail_cont__2(I, nextalt(_)) := fail :- ins_modifies_choice_on_success(I), !.
next_fail_cont__2(I, nextalt(Restore)) := nextalt(~enable_restore_mem(Restore)) :- ins_uses_trail(I), !.
next_fail_cont__2(_, Fail) := Fail.
}.

% TODO: document:
% Restore state operations:
%   no_restore (the state does not need to be restored)
%   restore_mem (untrail operation is needed)
%   restore_all (untrail operation + restore registers is needed)

enable_restore_mem(no_restore) := restore_mem :- !.
enable_restore_mem(restore_mem) := restore_mem :- !.
enable_restore_mem(restore_all) := restore_all :- !.

% or-binarization: replace the local labels
%   no_restore
%   restore_mem
%   restore_all
% by the actual label corresponding to the global block.

% TODO: document: move where other properties are defined
impinlinealts(semidet).
impinlinealts(det).

% TODO: Use other properties (id_fail_cont) instead of Imp
% Note: alt list cannot be empty
{
:- fluid exp :: module_exp.
:- fluid maybeproceed :: any.
:- fluid preds :: accum.
binarize_or(PredId) :-
	trust(PredId instance_of predicate_x),
	icode(g, Args, Code) = ~PredId.code,
	Imp = ~PredId.get_prop(imp),
	call((
          preds :: accum(PredsR),
	  code :: accum(X2),
	  binarize_orcode__2(PredId, Args, Code, Imp)
        )),
	binarize_andcode(PredId, Args, X2),
	emit_preds(PredsR).

{
:- fluid code :: accum.
binarize_orcode__2(PredId, Args, or(Alts), Imp) :- !,
	Alts = [X0|Xs],
	n_switch(X0, X),
	OrArgs = orargs(PredId, Args, Imp, ~maybeproceed, Xs),
	call((
          failcontdic :: fcd <- ~fcd.new,
	  % If there are no more clauses, fail conts are redirected to
	  % fail.
	  HasNext = ( Xs = [] ? no | yes ),
	  % TODO: Instead of using hasnext, try to avoid placing
	  %   nextalt in the last alt.
	  call((
            orargs :: any <- OrArgs,
	    hasnext :: any <- HasNext,
	    insert_set_fail_cont(X)
          )),
	  emit_fcdes(~failcontdic.active_conts)
        )).
}.
}.

{
:- fluid code :: accum.
:- fluid preds :: accum.
emit_fcdes([]).
emit_fcdes([E|Es]) :-
	trust(E instance_of fcde),
	E.emit,
	emit_fcdes(Es).
}.

{
:- fluid code :: accum.
emit_code([]).
emit_code([X|Xs]) :- code.add(X), emit_code(Xs).
}.

{
:- fluid exp :: module_exp.
:- fluid failcontdic :: fcd.
no_restore_pred(OrArgs) :-
	( ~failcontdic.no_restore = EntN, var(EntN) ->
	    OrArgs = orargs(PredId, Args, Imp, MaybeProceed, Xs2),
	    trust(PredId instance_of predicate_x),
	    NewPredId = ~PredId.clone([]),
	    call((
              maybeproceed :: any <- MaybeProceed,
	      preds :: accum(PredsR),
	      code :: accum(X2),
	      binarize_orcode__2(NewPredId, Args, or(Xs2), Imp)
            )),
	    EntN = ~ent_def(NewPredId, Imp, [], Args, X2, PredsR)
	; true
	).

restore_mem_pred(OrArgs) :-
	( ~failcontdic.restore_mem = EntM, var(EntM) ->
	    no_restore_pred(OrArgs),
	    OrArgs = orargs(PredId, Args, Imp, MaybeProceed, _),
	    trust(PredId instance_of predicate_x),
	    NewPredId = ~PredId.clone([]),
            % TODO: Later, try to generate simpler code and pass
	    %   through reg assignment, etc. Maybe do this
	    %   binarization before.
	    % TODO: Untested! It may fail.
	    % TODO: Should be NewPredId instead of PredId? This code
	    % can be incorrect when Out regs are different (no data
	    % movements here...), until we use the reg assgn and code
	    % gen to generate this gluecode.
	    (PPArgs, PPOut) = ~prd_tempargs(PredId, Args),
	    call(( 
              code :: accum(Code1),
	      code.add(restore_mem),
	      EntN = ~failcontdic.lookup(no_restore),
	      call((
                maybeproceed :: any <- MaybeProceed,
		EntN.callcont(PPArgs, PPOut, Imp, NewPredId)
              ))
            )),
	    EntM = ~ent_def(NewPredId, Imp, [], Args, Code1, [])
	; true
	).

restore_all_pred(OrArgs) :-
	( ~failcontdic.restore_all = EntA, var(EntA) ->
	    no_restore_pred(OrArgs),
	    OrArgs = orargs(PredId, Args, Imp, MaybeProceed, _),
	    trust(PredId instance_of predicate_x),
	    % Remove input arguments because those are obtained from
	    % the choicepoint.
	    NewPredId = ~PredId.clone_nullin([]),
	    Modes = ~PredId.get_prop(argmodes),
	    InArgs = ~filter_mode(Args, Modes, in), % TODO: params ignored
	    OutArgs = ~filter_mode(Args, Modes, out),
            % TODO: Maybe this can be generalized.
	    % TODO: should be NewPredId instead of PredId? This code
	    % can be incorrect when Out regs are different (no data
	    % movements here...), until we use the reg assgn and code
	    % gen to generate this gluecode.
	    (PPArgs, PPOut) = ~prd_tempargs(PredId, Args),
	    In3 = ~filter_mode(PPArgs, Modes, in),
	    call(( 
              code :: accum(Code2),
	      code.add(restore_all(In3)),
	      EntN = ~failcontdic.lookup(no_restore),
              call((
                maybeproceed :: any <- MaybeProceed,
		EntN.callcont(PPArgs, PPOut, Imp, NewPredId)
              ))
            )),
	    EntA = ~ent_def(NewPredId, Imp, InArgs, OutArgs, Code2, [])
	; true
	).
}.

{
:- fluid exp :: module_exp.
ent_def(NewPredId, Imp, InArgs, OutArgs, Code, PredsR) := Ent :-
	( impinlinealts(Imp) ->
	    ( PredsR = [] -> true ; errlog:bug(['predsr must be empty']) ),
	    Ent = ~fcde.inline_new(NewPredId, Code)
	; call((
            preds :: accum <- Preds,
            binarize_andcode(NewPredId, OutArgs, Code),
	    ~preds = PredsR
	  )),
	  Ent = ~fcde.new(NewPredId, InArgs, OutArgs, Preds)
	).

% TODO: It won't be necessary when binarization is done before
%   register assignment.
prd_tempargs(PredId, Args0) := (Args, Out) :-
	trust(PredId instance_of predicate_x),
	Modes = ~PredId.get_prop(argmodes),
	% TODO: Args0 should be a list of new vars?
	Out0 = ~filter_mode(Args0, Modes, out),
	Mems0 = ~caller_argmems(PredId), % TODO: sure?
	OutMems0 = ~filter_mode(Mems0, Modes, out),
	% TODO: Types are simply ignored here.
	% TODO: This is a hack. We set unused X regs to store the cvar
	%   args; we should use register allocation to do that?
        rendic(OutMems0, RenDic, ~maxreg(Mems0)),
	OutMems = ~renametotemp(OutMems0, RenDic),
	Out = ~prd_setmems(Out0, OutMems),
	Args = ~replace_mode(Args0, Modes, out, Out).
}.

prd_setmems([], [], []).
prd_setmems([X0|Xs0], [Mem|Mems], [X|Xs]) :-
	trust(X0 instance_of termvar),
	X = ~X0.addp(mem, Mem),
	prd_setmems(Xs0, Mems, Xs).

% TODO: What is this?
renametotemp([], _) := [] :- !.
renametotemp([X|Xs], Dic) := [~renametotemp_2(X, Dic)|~renametotemp(Xs, Dic)] :- !.

renametotemp_2(X, Dic) := Y :- Y = ~dic_get(Dic, X), !.
renametotemp_2(X, _) := X :- !.

rendic([], _, _) :- !.
rendic([X|Xs], RenDic, I) :-
	rendic_2(X, RenDic, I, I1),
	rendic(Xs, RenDic, I1).

rendic_2(default_choice, _, I, I) :- !.
rendic_2(x(_), _, I, I) :- !.
rendic_2(X, RenDic, I, I) :- 
	_ = ~dic_get(RenDic, X), !.
rendic_2(X, RenDic, I, I1) :- !,
	dic_lookup(RenDic, X, x(I)), !,
	I1 is I + 1.

% TODO: Move with other props.
% TODO: Document: code where ~X.getp(fail) contains a callable.
impusesfailalt(nondet).
impusesfailalt(semidet).

{
:- fluid exp :: module_exp.
goal_uses_nextalt(G) :- ins_is_proceed(call(G)), !,
	true = ~goal_prop(uses_failcont, G).
goal_uses_nextalt(G) :-
	CalledImp = ~goal_prop(effective_imp, G),
	impusesfailalt(CalledImp).
}.

% Replace the local labels (no_restore, restore_mem, restore_all) by
% the actual label corresponding to the global block.
%
% TODO: Do a more general predicate to replace high order calls.
%
% TODO: Not very clean: needs semidet or det calls in the
%   if-then-elses (gotos) and proceeds followed by nothing.
%
{
:- fluid exp :: module_exp.
:- fluid failcontdic :: fcd.
:- fluid orargs :: any.
:- fluid hasnext :: any.
:- fluid code :: accum.
insert_set_fail_cont([]).
insert_set_fail_cont([I|Is]) :-
	insert_set_fail_cont__2(I),
	insert_set_fail_cont(Is).

% The goal needs the cont in the sr register .
% TODO: Hide fail if predicate does not use alts?
insert_set_fail_cont__2(call(G)) :-
	trust(G instance_of strgoal),
	Fail = ~G.getp(fail),
	Fail = nextalt(Next),
%	goal_uses_nextalt(G),
	!,
	( ( \+ goal_uses_nextalt(G) ; ~hasnext = no ) ->
	    code.add(call(~G.addp(fail, fail)))
	; ( Next = no_restore -> no_restore_pred(~orargs)
	  ; Next = restore_mem -> restore_mem_pred(~orargs)
	  ; Next = restore_all -> restore_all_pred(~orargs)
	  ),
	  Ent = ~failcontdic.lookup(Next),
	  ( true = ~goal_prop(uses_failcont, G) ->
	      % TODO: nextalt should have no input arguments if the
	      %   following holds.
              set_fail_cont(Ent), 
	      code.add(call(~G.addp(fail, fail)))
	  ; I2 = ~Ent.invoke,
	    code.add(call(~G.addp(fail, I2)))
	  )
        ).
insert_set_fail_cont__2(I) :-
	code.add(I).

set_fail_cont(Ent) :-
	trust(Ent instance_of fcde),
	FailCont = ~Ent.failcont,
	( FailCont = fail ->
	    % TODO: Is this bad? Maybe we should insert the
	    % setfailcont after the fail next is modified. The only
	    % problem would be that we have a fail next pointing to
	    % no_restore and we don't complete a choice point with
	    % restoreall info (<- check this).
	    true
	; code.add(set_fail_cont(FailCont))
	).
}.

% Fail continuation
:- class fcde {
    :- attr predid :: any.
    :- attr failcont :: any.
    :- attr invoke :: any.
    :- attr aux :: any.

    % TODO: Check? "In" should be the arguments stored in the choice
    %   point.
    % TODO: Ensure that no_restore, etc. cannot be here (or at least,
    %   when it has cvar arguments, right?).
    {
    :- fluid exp :: module_exp.
    :- constructor new_/4.
    % A fail continuation 
    new_(PredId, InArgs, OutArgs, Preds) :-
        % TODO: Ensure that restore_all is invoked correctly (without
        %   args).
	~predid = PredId,
	~failcont = failcont(PredId, InArgs),
	~invoke = ~to_last_call(call(~fcdgoal(PredId, OutArgs))),
	~aux = auxpreds(Preds).
    }.

    % A fail continuation that will be inlined
    :- constructor inline_new_/2.
    inline_new_(PredId, Code) :-
	Label = local(_),
	~predid = PredId,
	~failcont = bad,
	~invoke = jump(Label),
	~aux = auxinline([label(Label)|Code]).

    {
    :- fluid code :: accum.
    :- fluid preds :: accum.
    % Dump the code of the fail continuation
    emit :-
	Aux = ~aux,
	( Aux = auxpreds(Preds) ->
	    emit_preds(Preds)
	; Aux = auxinline(Is) ->
	    emit_code(Is)
	; true
	).
    }.

    {
    :- fluid exp :: module_exp.
    :- fluid maybeproceed :: any.
    :- fluid code :: accum.
    % Invoke the fail continuation
    callcont(Args, Out, Imp, NewPredId) :-
        trust(NewPredId instance_of predicate_x),
	( impinlinealts(Imp) ->
	    Invoke = ~invoke,
	    code.add(Invoke)
	; % TODO: Put something else in proceed_at if known.
	  ProceedPredId = ~NewPredId.new_proceed,
	  FcdId = ~predid,
	  A0 = ~fcdgoal(FcdId, Args),
	  code.add(call(A0)),
	  B0 = ~fcdgoal(ProceedPredId, Out),
	  B1 = ~B0.addp(proceed_at, ~maybeproceed),
	  code.add(call(B1))
	).
    }.
}.

:- meta_predicate fcdgoal(?, ?, out(strgoal)).
fcdgoal(PredId, Args) := G2 :-
	G0 = ~strgoal.new_n(PredId, Args),
	G1 = ~G0.addp(fail, fail),
	G2 = ~G1.addp(frame_live_size, 0).

:- class fcd {
    :- attr no_restore :: fcde.
    :- attr restore_mem :: fcde.
    :- attr restore_all :: fcde.

    :- constructor new_/0.
    new_.

    active_conts := ~filter_nonvar([~no_restore, ~restore_mem, ~restore_all]).

    :- static filter_nonvar/2.
    filter_nonvar([]) := [].
    filter_nonvar([X|Xs]) := Ys :-
	( var(X) -> Ys = Ys0 ; Ys = [X|Ys0] ),
	Ys0 = ~filter_nonvar(Xs).

    :- meta_predicate lookup(?, out(fcde)).
    lookup(no_restore) := ~no_restore.
    lookup(restore_mem) := ~restore_mem.
    lookup(restore_all) := ~restore_all.
}.

% ---------------------------------------------------------------------------
% and-binarization: 
%   - add binlastcall prop (last_call mark)
%   - binarize and add success continuations
% ---------------------------------------------------------------------------

{
:- fluid exp :: module_exp.
:- fluid preds :: accum.
binarize_andcode(PredId, Args, Code) :-
	trust(PredId instance_of predicate_x),
	PredId.set_code(icode(b, Args, Code1)),
	Imp = ~PredId.get_prop(imp),
	preds.add(PredId),
	call((
          callerimp :: any <- Imp,
	  pred_id :: predicate_x <- PredId,
	  predargs :: any <- Args,
	  code :: accum(Code1),
	  binarize_andcode_2(Code)
        )).
{
:- fluid callerimp :: any.
:- fluid pred_id :: predicate_x.
:- fluid predargs :: any.
:- fluid code :: accum.
binarize_andcode_2([]) :- !.
binarize_andcode_2([I0, I1|Code1]) :-
	\+ ins_is_proceed(I0),
	ins_accept_last_call(I0),
	ins_is_proceed(I1),
	Imp = ~ins_call_imp(I0),
	~callerimp = Imp,
	MaybeProceed = ~proceed_cont(I1),
	% lastcall is only valid if we are using the successcont.
	( MaybeProceed = justp(_) -> ins_uses_successcont(I0) ; true ),
	!,
	( Ys = ~getout(I0),
	  Ys2 = ~getin(I1),
	  same_mems(Ys, Ys2) ->
	    % the output of the last call is the input of the proceed
	    %
	    % Note: compare mems, not the vars! The vars may be
	    % different because of some removed move instructions (a
	    % move from a variable to a different variable in the same
	    % mem is removed)
	    true
	; errlog:trace(['warning: input of proceed is not the output of the last call ', call_proceed(I0, I1)])
	),
	( Fail = ~get_fail(I0),
	  Fail2 = ~get_fail(I1),
	  Fail == Fail2 ->
	    true
	; errlog:trace(['warning: fail cont of proceed is not the fail cont of the last call ', call_proceed(I0, I1)])
	),
	bin_add_lastcall(I0, MaybeProceed),
	% TODO: It should end here, but Code1 may contain local labels
	%   (<- try to avoid this?)
	binarize_andcode_2(Code1).
% TODO: Is correct code emitted when last call cannot be introduced?
%   It seems that next_insn is modified even if no frame is being
%   allocated.
binarize_andcode_2([I0|Code1]) :-
	\+ ins_is_proceed(I0),
	ins_uses_successcont(I0),
	I0 = call(I0G),
	trust(I0G instance_of strgoal),
	PrevId = ~I0G.predid,
	Imp = ~ins_call_imp(I0),
	~callerimp = Imp,
	!,
	PredId = ~pred_id, Args = ~predargs,
	trust(PredId instance_of predicate_x),
	% the continuation
	I0Out = ~getout(I0),
	ContPredId = ~PredId.clone_chain(PrevId, []),
	Modes0 = ~PredId.get_prop(argmodes),
	OutArgs = ~filter_mode(Args, Modes0, out),
	NewArgs = ~append(I0Out, OutArgs),
	% the call
	FrameLiveSize = ~ins_frame_live_size(I0),
	% TODO: This is necessary even when proceed_at is known
	%   because it stores the frame live size.
	% TODO: Add a property to the subpred?
	I1 = set_success_cont(successcont(ContPredId, FrameLiveSize)),
	code.add(I1),
	bin_add_lastcall(I0, justp(pred(NewArgs, ContPredId))),
	% Binarized continuation
	binarize_andcode(ContPredId, NewArgs, [recover_frame|Code1]).
binarize_andcode_2([I|Code]) :-
	ins_is_proceed(I),
	justp(Cont) = ~proceed_cont(I),
	!,
	( Cont = pred(ContIn, ContPredId) ->
	    I1 = ~strgoal.new_n(ContPredId, ContIn),
	    I2 = ~to_last_call(call(~I1.addp(fail, fail)))
	; Cont = label(Label) ->
	    I2 = jump(Label)
	; errlog:bug(['unknown proceed cont ', Cont]),
	  fail
	),
	( imp_may_create_choices(~callerimp) ->
	    % TODO: Optimize, do not invalidate if it is not
	    % necessary.
	    code.add(invalidate_local_top)
	; true
	),
	code.add(I2),
	binarize_andcode_2(Code).
binarize_andcode_2([I|Code]) :- !,
	bin_add_call(I),
	binarize_andcode_2(Code).
}.
}.

same_mems([], []).
same_mems([X|Xs], [Y|Ys]) :-
	trust(X instance_of termvar),
	trust(Y instance_of termvar),
	~X.getp(mem) == ~Y.getp(mem),
	same_mems(Xs, Ys).

% TODO: Is this correct? I think it is not based on that. (I had lot
%   of segfaults when that thing was not active).
imp_may_create_choices(nondet).
imp_may_create_choices(semidet).
imp_may_create_choices(det).

proceed_cont(I) := MaybeProceed :-
	I = call(G),
	trust(G instance_of strgoal),
	MaybeProceed = ~G.getp(proceed_at),
	!.
proceed_cont(_) := no.

% TODO: Try to unfold also here (no continuation pointer is required
%   in these predicates, just take care to do a good proceed.
{
:- fluid exp :: module_exp.
:- fluid preds :: accum.
:- fluid code :: accum.
bin_add_call(I0) :-
	I0 = call(G),
	trust(G instance_of strgoal),
	~G.getp(sub) = SubpredId,
	!,
	Label = local(_),
%	errlog:trace(['compunfolding ', Name]),
	call((
          maybeproceed :: any <- justp(label(Label)),
          lowcomp_pred_unfold(SubpredId)
        )),
	code.add(label(Label)).
bin_add_call(I) :-
	code.add(I).

bin_add_lastcall(I0, MaybeProceed) :-
	I0 = call(G),
	trust(G instance_of strgoal),
	~G.getp(sub) = SubpredId,
	!,
	% TODO: Do expansion. Be careful with input and output cvars.
	%   This and the previous one could replace imacro. imacro
	%   predicates would be just predicates defined in C that are
	%   forced to be unfolded and whose code is not emitted.
	call((
          maybeproceed :: any <- MaybeProceed,
	  lowcomp_pred_unfold(SubpredId)
        )).
bin_add_lastcall(I0, _) :-
	I = ~to_last_call(I0),
	code.add(I).
}.

% TODO: Input name is not used because it is the same that appears in
%   the Pred.
{
:- fluid exp :: module_exp.
:- fluid maybeproceed :: any.
:- fluid preds :: accum.
:- fluid code :: accum.
lowcomp_pred_unfold(PredId) :-
	trust(PredId instance_of predicate_x),
	Preds = ~lowcomp_pred__2(PredId),
	% Find PredId in the compilation output, remove it and append
	% its code to the main block of the output of lowcomp
	% compilation.
	Name = ~PredId.name,
	( select(PredId2, Preds, Preds2),
	  trust(PredId2 instance_of predicate_x),
	  Name = ~PredId2.name,
	  icode(b, _, Is) = ~PredId2.code ->
	    true
	; fail
	),
	Comment = inline_c(~block_comment(PredId2)),
	emit_code([Comment|Is]),
	emit_preds(Preds2).
}.

{
:- fluid exp :: module_exp.
getin(call(G)) := In :-
	trust(G instance_of strgoal),
	Args = ~G.args,
	GId = ~G.predid,
	Modes = ~GId.get_prop(argmodes),
	In = ~filter_mode(Args, Modes, in).

getout(call(G)) := Out :-
	trust(G instance_of strgoal),
	Args = ~G.args,
	GId = ~G.predid,
	Modes = ~GId.get_prop(argmodes),
	Out = ~filter_mode(Args, Modes, out).

ins_call_imp(call(X)) := Imp :-
	trust(X instance_of strgoal),
	GId = ~X.predid,
	Imp = ~GId.get_prop(effective_imp).

:- '$ctxprj'(to_last_call/2, []).
to_last_call(call(G)) := call(~G.addp(binlastcall, true)) :-
	trust(G instance_of strgoal).

:- '$ctxprj'(get_fail/2, []).
get_fail(call(G)) := ~G.getp(fail) :-
	trust(G instance_of strgoal).

% TODO: Move to a prop?
% TODO: macros are not easily expanded if they are transformed in last
%   calls; although det and semidet do not necessarily need to be
%   lastcalls the block optimizer works better if they are.
% TODO: explain the reason of the last TODO
ins_accept_last_call(call(G)) :-
	trust(G instance_of strgoal),
	GId = ~G.predid,
	_MacroDef = ~GId.get_prop(impmacro),
	!,
	fail.
ins_accept_last_call(_).
}.

% ---------------------------------------------------------------------------

get_imptype(A) := T :-
	trust(A instance_of termvar),
	T = ~A.getp(imptype), !.
get_imptype(_) := tagged. % default value

% ---------------------------------------------------------------------------
:- doc(subsection, "Preprocessing before translation to C").
% TODO: Is that an optimized bytecode postprocessing or a
%   preprocessing before translation to C?
% TODO: Change name? Move to a different file?
% ---------------------------------------------------------------------------

%:- public prepass_preds/1.
{
:- fluid exp :: module_exp.
prepass_preds([]) :- !.
prepass_preds([PredId|Xs0]) :-
	trust(PredId instance_of predicate_x),
	Code = ~PredId.code,
	( Code = icode(b, Args, Body) ->
	    Mems = ~callee_argmems(PredId),
	    CvarArgs0 = ~filter_cvar(Args, Mems),
	    pretranslate_argsbd(CvarArgs0, CvarArgs, _),
	    prepass(Body, Body1),
	    Comment = ~block_comment(PredId),
	    Code1 = vcode(Comment, CvarArgs, (globvardefs,Body1)),
	    PredId.set_code(Code1)
        ; true
	),
	prepass_preds(Xs0).

prepass(Xs, (Body, PACode)) :-
	prepass_getpa(Xs, Rest, PACode),
	prepass__1(Rest, Body).

prepass__1([X|Xs], Code) :- X = label(local(Label)), !,
	prepass_getpa(Xs, Rest, PACode),
	Code = ((Label = ~funcall('$predabs'([], all, PACode))), XsCode),
	prepass__1(Rest, XsCode).
prepass__1([X], Code) :- !,
	Code = ~prepass__2(X).
prepass__1([X|Xs], Code) :- !,
	XCode = ~prepass__2(X),
	prepass__1(Xs, XsCode),
	( XCode = true -> Code = XsCode
	; XsCode = true -> Code = XCode
	; Code = (XCode, XsCode)
	).
prepass__1([], Code) :- !, Code = true.

prepass_getpa(Xs, Rest, Code) :-
	( Xs = [] ; Xs = [label(_)|_] ), !,
	Rest = Xs,
	Code = true.
prepass_getpa([X], Rest, Code) :- !,
	Rest = [],
	Code = ~prepass__2(X).
prepass_getpa([X|Xs], Rest, Code) :-
	XCode = ~prepass__2(X),
	prepass_getpa(Xs, Rest, XsCode),
	( XCode = true -> Code = XsCode
	; XsCode = true -> Code = XCode
	; Code = (XCode, XsCode)
	).

prepass__2(switch_on_index(X, Y, U, V, W)) := Code :- !,
	trust(V instance_of termunk),
	Code = ~pretranslate_goal(sw_on_index(X, Y, U, ~V.value, W)).
prepass__2(neck(ChoiceState, _)) := Y :-
	trust(ChoiceState instance_of termunk),
	trick_to_preserve_args = ~ChoiceState.value, !,
	% TODO: Why? Not very clean.
	Y = true.
prepass__2(neck(ChoiceState, As)) := Y :- !,
	trust(ChoiceState instance_of termunk),
	Y = ~pretranslate_goal(~c_neck(~ChoiceState.value, As)).
prepass__2(globalize(Globalize, A, B)) := Y :- !,
	trust(Globalize instance_of termunk),
	Y = ~pretranslate_goal(~c_globalize(~Globalize.value, A, B)).
prepass__2(dummy_cut) := Y :- !, Y = true.
prepass__2(init(A)) := Y :- !,
	Y = ~pretranslate_goal(~c_init(A)).
prepass__2(jump(X)) := Y :- !,
	Y = ~pretranslate_goal(jump(X)).
prepass__2(cjump(Cond, X, Y)) := Code :- !,
	Code = ~pretranslate_goal(cjump(Cond, X, Y)).
prepass__2(call(G)) := Y :-
	trust(G instance_of strgoal),
	~G.name = Name,
	Name = 'term_basic:$bind'/2,
	!,
	~G.args = [A, B],
	Y = ~pretranslate_goal(bind(A, B)).
prepass__2(call(G)) := Y :-
	trust(G instance_of strgoal),
	~G.name = 'basiccontrol:fail'/0,
	!,
	Y = ~prepass__2(~G.getp(fail)).
prepass__2(call(G)) := Y :- !,
	trust(G instance_of strgoal),
	( true = ~goal_prop(needs_liveness_info, G) ->
	    Y = (~pretranslate_goal(set_liveinfo(liveinfo_set(~G.getp(heap), ~G.getp(live_set)))), Y2)
	; Y = Y2
	),
	( ~G.getp(fail) = jump(Label) ->
	    G2 = ~G.addp(fail, ~prepass_jump(Label))
	; G2 = G
	),
	trust(G2 instance_of strgoal),
	Args = ~G2.args,
	G2Id = ~G2.predid,
	( MacroDef = ~G2Id.get_prop(impmacro) ->
	    copy_term_shattr(MacroDef, imacro_def(Args2, R)),
	    Y2 = ~pretranslate_goal(inline_i(Args, Args2, R, ~G2.getp(fail)))
	; Y2 = ~pretranslate_goal(call(G2))
	).
prepass__2(X) := ~pretranslate_goal(X).
}.

prepass_jump(Label) := '$vcall'(X) :-
	( Label = local(X) -> true
	; trust(Label instance_of termunk),
	  ~Label.value = local(X)
	).

pretranslate_jump(Label) := '$vvivcall'(X) :-
	( Label = local(X) -> true
	; trust(Label instance_of termunk),
	  ~Label.value = local(X)
	).

block_comment(PredId) := M :-
	trust(PredId instance_of predicate_x),
	M = comment_msg([~~(~PredId.name)]).

% TODO: Move to other pass, not in onlyregs.
% Not preceded by a cut and without a choice of a previous
% alternative:
c_neck(no_choice, As) := neck(As) :- !.
% Not preceded by a cut and with a choice of a previous alternative
% (may be still in shallow mode):
c_neck(choice_unknown, As) := maybe_neck(As) :- !.

% TODO: Move to other pass, not in onlyregs.
c_init(A) := _ :-
	trust(A instance_of termvar),
	\+ ~get_imptype(A) = tagged, !,
	errlog:bug(['do not know how to initialize a non tagged variable']),
	fail.
c_init(A) := inits(A) :-
	trust(A instance_of termvar),
	~A.getp(mem) = y(_), !.
c_init(A) := consh(A) :-
	trust(A instance_of termvar),
	~A.getp(mem) = n(_, _), !.
c_init(A) := inith(A) :- !.

% TODO: Reuse globalized values.
% TODO: Move to other pass, not in onlyregs.
c_globalize(if_unsafe, B1, A) := globalize_if_unsafe(B1, A) :- !.
c_globalize(yes, B1, A) := globalize_to_arg(B1, A) :- !.
c_globalize(deref_only, B1, A) := move(B1, A) :- !.

% TODO: Remove Fail field earlier if goal is known not to fail?
{
:- fluid exp :: module_exp.
pretranslate_goal(nop, true) :- !.
pretranslate_goal(call(G), R) :-
	trust(G instance_of strgoal),
	true = ~goal_prop(is_proceed, G), !,
	GId = ~G.predid,
	Modes = ~GId.get_prop(argmodes),
	Mems = ~caller_argmems(GId),
	InMems = ~filter_mode(Mems, Modes, in),
        Args = ~G.args,
	In = ~filter_mode(Args, Modes, in),
	CvarIn = ~filter_cvar(In, InMems),
	pretranslate_argsbd(CvarIn, CvarIn2, BR),
	R = (BR,invalidate_local_top,'$vvproceed'(CvarIn2)).
pretranslate_goal(call(G), R) :-
	trust(G instance_of strgoal),
	Name = ~G.name,
	GId = ~G.predid,
	% TODO: Not very clean. Maybe it can be stored in the frame
	%   live size.
%	errlog:trace([a(G)]),
	Modes = ~GId.get_prop(argmodes),
	Mems = ~caller_argmems(GId),
	InMems = ~filter_mode(Mems, Modes, in),
	OutMems = ~filter_mode(Mems, Modes, out),
	CallImp = ~GId.get_prop(effective_imp),
%	errlog:trace([b(Modes, Mems, CallImp, Label)]),
	Args = ~G.args,
	In = ~filter_mode(Args, Modes, in),
	Out = ~filter_mode(Args, Modes, out),
	CvarIn = ~filter_cvar(In, InMems),
	CvarOut = ~filter_cvar(Out, OutMems),
%	errlog:trace([c(Modes, Mems, CallImp, Label)]),
	( Name = NameMF/NameA,
	  exp.expanded_import(NameMF, NameA, _),
	  % TODO: this is related to pred_label, depending on what is
	  % the entry of the predicate we should generate proto here
	  % or not. Define a ... requirepredlabel? (that registers it)
	  % and definepredlabel?
	  \+ true = ~GId.get_prop(impreg)
	  ->
	    % Include foreign proto.
	    % TODO: Move as argument of vicallr, vilast_call, etc.
	    R = ('$defnatproto'(Name), R1b)
	; R = R1b
	),
	R1b = (BR1,BR2,R1),
	pretranslate_argsbd(CvarIn, CvarIn1, BR1),
	pretranslate_argsb(CvarOut, CvarOut1, BR2),
	( ~G.getp(binlastcall) = true ->
	    ( CallImp = det, CvarOut1 = [_] ->
	        % TODO: Used in exp.pl, correct? It says that a
	        %   builtin function can be a lastcall (should I
	        %   invalidate_local_top?).
	        Ra = '$vicallr'(det, GId, CvarIn1, CvarOut1, '$vifail'),
		% TODO: Strange, should I invalidate_local_top?
		Rb = '$vvproceed'([]),
		R1 = (Ra, Rb)
	    ; R1 = '$vilast_call'(CallImp, GId, CvarIn1, CvarOut1)
	    )
	; Fail0 = ~G.getp(fail),
	  pretranslate_goal(Fail0, Fail),
	  ( CallImp = semidet_re, CvarOut1 = [OutVar] ->
	      Ra = '$vicallr'(det, GId, CvarIn1, [OutVar], erroralt),
	      Rb = (v__erroneous('@'(OutVar)) -> Fail ; true),
	      R1 = (Ra, Rb)
	  ; CallImp = detcut ->
	      R1 = '$vicallr'(det, GId, CvarIn1, CvarOut1, Fail)
	  ; R1 = '$vicallr'(CallImp, GId, CvarIn1, CvarOut1, Fail)
	  )
	).
pretranslate_goal(jump(X), R) :- !,
	R = ~pretranslate_jump(X).
pretranslate_goal(cjump(Cond0, X, Y), R) :- !,
	pretranslate_goal(Cond0, Cond),
	R = ( Cond -> ~pretranslate_jump(X) ; ~pretranslate_jump(Y) ).
pretranslate_goal(ifnot(Def, Fail), R) :- !,
	pretranslate_goal(Def, Def2),
	pretranslate_goal(Fail, Fail2),
	R = ( \+ Def2 -> Fail2 ; true ).
pretranslate_goal(sw_on_index(X, Y, U, V, W), R) :- !,
	R = ~pretranslate_sw_on_index(X, Y, U, V, W).
pretranslate_goal(mvardecl(X), BR) :- !,
	trust(X instance_of termvar),
	Mem = ~X.getp(mem),
	Y0 = ~X.name,
	Mem = mvar(N),
%	errlog:trace([md(Y0,Mem)]),
	ImpType0 = ~get_imptype(X),
%	BR = (Y0 = ~funcall(mut(ImpType0))).
	% TODO: '$vimut' should not be used, but there is a bug in
	%   impcomp (if two predabs share the same var, and it is
	%   declared as a mut, declarations are either duplicated or
	%   created with different C identifiers)
%	BR = ('$inline_c'([], [], comment_msg(['patata y0:', Y0, ' origmvar:', Mem, ' mvar:', _Mem])), Y0 = ~funcall('$vimut'(N, ImpType0))).
	BR = (Y0 = ~funcall('$vimut'(N, ImpType0))).
pretranslate_goal('$vcall'(Label), '$vvivcall'(Label)) :- !.
pretranslate_goal('<-'(X0, Y0), (BR,'<-'(~funcall('$trust_typed'(X, mut(ttt))), ~funcall('$trust_typed'(Y, ttt))))) :- !, % TODO: fix types
	pretranslate_argsbm([X0, Y0], [v, d], [X, Y], BR).
%
pretranslate_goal(globalize_if_unsafe(X0, Y0), (BR,v__globalize_if_unsafe(X, Y))) :- !,
	pretranslate_argsbm([X0, Y0], [d, v], [X, Y], BR).
pretranslate_goal(globalize_to_arg(B0, A0), (BR,v__globalize_to_arg(B, A))) :- !,
	pretranslate_argsbm([B0, A0], [d, v], [B, A], BR).
pretranslate_goal(deref(X0, Y0), (BR,v__deref(X, Y))) :- !,
	pretranslate_argsbm([X0, Y0], [d, v], [X, Y], BR).
pretranslate_goal(load(A0, MaybeArgs0, B0), (BR,v__load(A, MaybeArgs, B))) :- !,
	pretranslate_argsbm([A0, MaybeArgs0, B0], [v, d, d], [A, MaybeArgs, B], BR).
pretranslate_goal(read(X0, Type0, As0), (BR,v__read(X, Type, As))) :- !,
	pretranslate_argsbm([X0, Type0, As0], [d, d, v], [X, Type, As], BR).
%pretranslate_goal(move(X0, Y0), ('$inline_c'([], [], comment_msg([bar(X0,Y0)])), BR,v__move(X, Y))) :- !,
pretranslate_goal(move(X0, Y0), (BR,v__move(X, Y))) :- !,
	pretranslate_argsbm([X0, Y0], [d, v], [X, Y], BR).
pretranslate_goal(inits(A0), (BR,v__inits(A))) :- !,
	pretranslate_argsbm([A0], [v], [A], BR).
pretranslate_goal(inith(A0), (BR,v__inith(A))) :- !,
	pretranslate_argsbm([A0], [v], [A], BR).
%pretranslate_goal(consh(A0), ('$inline_c'([], [], comment_msg([foo(A0)])), BR,v__consh(A))) :- !,
pretranslate_goal(consh(A0), (BR,v__consh(A))) :- !,
	pretranslate_argsbm([A0], [v], [A], BR).
pretranslate_goal(inline_cut(A0), (BR,v__inline_cut(A))) :- !,
	% TODO: It should not pass a mutable to cut, but a tagged!
	pretranslate_argsbm([A0], [v], [A], BR).
pretranslate_goal(inline_trail_if_conditional(A0), (BR,v__inline_trail_if_conditional(A))) :- !,
	pretranslate_argsbm([A0], [v], [A], BR).
%
pretranslate_goal(foreign(N, Xs), G) :- !,
	G0 =.. [N|Xs],
	pretranslate_goal(G0, G).
pretranslate_goal(inline_i(Args, Args2, R, Fail0), (BR,R1)) :- !,
	pretranslate_argsb(Args, Args2, BR),
	( Fail0 = fail -> R1 = R
	; pretranslate_goal(Fail0, Fail1),
	  R1 = (R->true;Fail1)
	).
pretranslate_goal(inline_c(R), '$inline_c'([], [], R)) :- !.
pretranslate_goal(fail, fail) :- !.
pretranslate_goal(globvardefs, globvardefs) :- !.
pretranslate_goal(cteq(X0, Y0), (BR,'$vcccteq'(X, Y))) :- !,
	pretranslate_argsbd([X0,Y0], [X,Y], BR).
pretranslate_goal(cteqvt(X0, Y0), (BR,'$vcccteqvt'(X, Y))) :- !,
	pretranslate_argsbd([X0,Y0], [X,Y], BR).
pretranslate_goal(bind(A, B), (BR,v__bind(A3, B3))) :- !,
	pretranslate_argsbd([A,B], [A2,B2], BR),
	TypeA = ~get_imptype2(A),
	TypeB = ~get_imptype2(B),
	A3 = ~funcall('$trust_typed'(A2, TypeA)),
	B3 = ~funcall('$trust_typed'(B2, TypeB)).
pretranslate_goal(restore_args(Args0), (BR,v__restore_args(Args))) :- !,
	pretranslate_argsbd(Args0, Args, BR).
pretranslate_goal(neck(Args0), (BR,v__neck(Args))) :- !,
	pretranslate_argsbd(Args0, Args, BR).
pretranslate_goal(maybe_neck(Args0), (BR,v__maybe_neck(Args))) :- !,
	pretranslate_argsbd(Args0, Args, BR).
pretranslate_goal(X, (BR,X2)) :-
	X =.. [N|Xs],
	pretranslate_argsbd(Xs, Xs1, BR),
	X1 =.. [N|Xs1],
	( pretranslate_goal__2(X1, X2) ->
	    true
	; X2 = X1
	).

% The tagged type of a var.
get_imptype2(A) := T :-
	trust(A instance_of termvar),
	T = ~A.getp(imptype), !.
get_imptype2(A) := ~derefvar_taggedtype(A) :-
	trust(A instance_of termvar),
	~A.getp(dereferenced) = true, !.
get_imptype2(A) := ~freshvar_taggedtype(A) :-
	trust(A instance_of termvar),
	~A.getp(fresh) = true, !.
get_imptype2(_A) := tagged. % default value

% TODO: Study relationship between dereferenced and fresh, remove cva
%   cases when possible.
derefvar_taggedtype(A) := VT :-
	trust(A instance_of termvar),
	AType = ~A.getp(type),
	VT = ( typemode_is_var(AType) ?
	         % nonstack_var can be a heap var or a constrained var
	         % but never a stack var.
	         ( is_subarg(A) ? nonstackreftagged
		 | ~A.getp(vartype) = nonstack ? nonstackreftagged
		 | reftagged
		 )
	     | type_consequence(AType, ~type_norm(list)) ? lsttagged
	     | type_consequence(AType, ~type_norm(str)) ? strtagged
	     | type_consequence(AType, ~type_norm(bignum)) ? strtagged
	     | type_consequence(AType, ~type_norm(float)) ? strtagged
	     | type_consequence(AType, ~type_norm(atom)) ? atmtagged
	     | type_consequence(AType, ~type_norm(smallint)) ? numtagged
	     | typemode_is_nonvar(AType) ? nonreftagged
	     | tagged
	     ).

% TODO: Study relationship between dereferenced and fresh, remove cva
%   cases when possible.
freshvar_taggedtype(A) := VT :-
	trust(A instance_of termvar),
	VT = ( ~A.getp(mem) = y(_) ? svatagged
	     | is_subarg(A) ? nonstackreftagged
	     | ~A.getp(vartype) = nonstack ? nonstackreftagged
	     | reftagged
	     ).

% TODO: Use a version of SwitchOnVar that does not dereference.
pretranslate_sw_on_index(A0, Var, List, Xs, Def) := R :-
	trust(A0 instance_of termvar),
	AType = ~A0.getp(type),
	pretranslate_argsb([A0], [A], BR),
	R = (BR, Temp = ~funcall(initmut(tagged, '@'(A))),
	     T0 = ~funcall(newmut(tagged)),
	     OtherLabel = ~funcall('$predabs'([], all, ~pretranslate_sw_on_tagged(T0, Xs, Def))),
	     R1),
	Rvar = ~pretranslate_jump(Var),
	Rcons = ('<-'(T0, '@'(Temp)),'$vvivcall'(OtherLabel)),
	Rlst = ~pretranslate_jump(List),
	Rstr = ('<-'(T0, ~funcall(tagged_to_head_functor('@'(A)))),'$vvivcall'(OtherLabel)),
	Rnva = ( constagged('@'(Temp)) -> Rcons
	       ; lsttagged('@'(Temp)) -> Rlst
	       ; strtagged('@'(Temp)) -> Rstr
	       ),
	( typemode_is_var(AType) ->
	    R1 = Rvar
	; type_consequence(AType, ~type_norm((atom;list))) ->
	    R1 = ( lsttagged('@'(Temp)) -> Rlst ; Rcons)
	; R1 = ( reftagged('@'(Temp)) -> Rvar
	       ; Rnva
	       )
        ).

pretranslate_sw_on_tagged(T0, Xs, Def) := R :-
	hashtab_length_threshold(MinLength),
	( ~length(Xs) < MinLength ->
	    % use linear search
	    R = ~linear_tagged_search(Xs, Def, T0)
	; % use a hash table
	  R = ((I = ~funcall(initmut(intmach, ~funcall('$tagged_hashtab_get'(Xs, '@'(T0)))))), '$switch'('@'(I), ~tagged_search_cases(Xs, 1), ~pretranslate_jump(Def)))
	).

:- '$ctxprj'(tagged_search_cases/3, []).
tagged_search_cases([], _I) := [].
tagged_search_cases([_-Label|Xs], I) := [Y|Ys] :-
	I1 is I + 1,
	Y = case(I, ~pretranslate_jump(Label)),
	Ys = ~tagged_search_cases(Xs, I1).

% Length threshold for use a hash table.
% TODO: What is the good value here?
:- '$ctxprj'(hashtab_length_threshold/1, []).
hashtab_length_threshold(3).

:- '$ctxprj'(linear_tagged_search/4, []).
linear_tagged_search([], Def, _T0) := ~pretranslate_jump(Def) :- !.
linear_tagged_search([Key-Label|Xs], Def, T0) :=
	( '@'(T0) == ~funcall('$trust_typed'(~funcall('$varg'(str(Functor))), tagged)) ->
	    ~pretranslate_jump(Label)
	; ~linear_tagged_search(Xs, Def, T0)
	) :- Key = str(Functor), !.
linear_tagged_search([Cons-Label|Xs], Def, T0) :=
	( v__inline_test('@'(T0), Cons2) ->
	    ~pretranslate_jump(Label)
	; ~linear_tagged_search(Xs, Def, T0)
	) :-
         ( Cons = smallint(I) ->
	    Cons2 = ~funcall(make_small(~funcall('$trust_typed'(I, intval))))
	; Cons2 = ~funcall('$varg'(Cons))
	).

pretranslate_goal__2(erroralt, v__erroralt) :- !. % TODO: fictional definition
pretranslate_goal__2(inline_test(A, B), v__inline_test(A, B)) :- !.
pretranslate_goal__2(inline_equal(A, B), v__inline_equal(A, B)) :- !.
pretranslate_goal__2(check_events(Args), v__check_events(Args)) :- !.
pretranslate_goal__2(ensure_heap(X, Size, Args), v__ensure_heap(X, Size, Args)) :- !.
pretranslate_goal__2(set_choice, v__set_choice) :- !.
pretranslate_goal__2(trim_frame(FrameSize), v__trim_frame(FrameSize)) :- !.
pretranslate_goal__2(recover_frame, v__recover_frame) :- !.
pretranslate_goal__2(invalidate_local_top, v__invalidate_local_top) :- !.
pretranslate_goal__2(validate_local_top, v__validate_local_top) :- !.
pretranslate_goal__2(set_liveinfo(X), v__set_liveinfo(X)) :- !.
pretranslate_goal__2(set_success_cont(Successcont), v__set_success_cont(Successcont)) :- !.
pretranslate_goal__2(set_fail_cont(Failcont), v__set_fail_cont(Failcont)) :- !.
pretranslate_goal__2(update_default_choice, v__update_default_choice) :- !.
pretranslate_goal__2(push_choice(Args), v__push_choice(Args)) :- !.
pretranslate_goal__2(restore_all(Args), v__restore_all(Args)) :- !.
pretranslate_goal__2(restore_mem, v__restore_mem) :- !.
pretranslate_goal__2(alloc, v__alloc) :- !.
pretranslate_goal__2(cframe(FrameSize), v__cframe(FrameSize)) :- !.
pretranslate_goal__2(dealloc, v__dealloc) :- !.
pretranslate_goal__2(erroneous(X), v__erroneous(X)) :- !.
pretranslate_goal__2(isnonvar(A), v__isnonvar(A)) :- !.
pretranslate_goal__2(test(A, B), v__test(A, B)) :- !.
pretranslate_goal__2(equal(B, A), v__equal(B, A)) :- !.

pretranslate_argsb([], [], true) :- !.
pretranslate_argsb([X|Xs], [Y|Ys], BR) :- !,
	pretranslate_argb(X, v, Y, BR1),
	BR = ( BR1 = true ? BR0 | (BR1,BR0) ),
	pretranslate_argsb(Xs, Ys, BR0).

pretranslate_argsbd([], [], true) :- !.
pretranslate_argsbd([X|Xs], [Y|Ys], BR) :- !,
	pretranslate_argb(X, d, Y, BR1),
	BR = ( BR1 = true ? BR0 | (BR1,BR0) ),
	pretranslate_argsbd(Xs, Ys, BR0).

pretranslate_argsbm([], [], [], true) :- !.
pretranslate_argsbm([X|Xs], [M|Ms], [Y|Ys], BR) :- !,
	pretranslate_argb(X, M, Y, BR1),
	BR = ( BR1 = true ? BR0 | (BR1,BR0) ),
	pretranslate_argsbm(Xs, Ms, Ys, BR0).

pretranslate_argb(X, Mode, Y, BR) :- !,
	( X instance_of termvar ->
	    Mem = ~X.getp(mem),
	    Y0 = ~X.name,
	    var(Y0), % TODO: This is always true, right?
	    ( Mem = cvar(N) ->
	        ( Mode = v ->
		    errlog:bug(['cannot get mutable value of cvar variable ', X]),
		    fail
		; Mode = d ->
		    ImpType0 = ~get_imptype(X),
		    ImpType = ~get_imptype2(X),
		    Y = Y0,
		    Y1 = Y0,
	            % TODO: Generate check_cvarmem only when necessary.
		    BR = ('$check_cvarmem'(Y0, N, ImpType0), '$trust_type'(Y1, ImpType))
		)
	    ; Mem = mvar(N) ->
	        ImpType0 = mut(~get_imptype(X)),
		ImpType = ~get_imptype2(X),
		Y = ( Mode = v ? Y0 | Mode = d ? '@'(Y0) ),
		Y1 = '@'(Y0),
	        % TODO: Generate check_mvarmem only when necessary.
	        BR = ('$check_mvarmem'(Y0, N, ImpType0), '$trust_type'(Y1, ImpType))
	    ; ( Mem = default_choice -> ImpType0 = mut(tagged), ImpType = numtagged, MemExpr = ~funcall('$custommem_default_choice')
	      ; Mem = n(A,I) -> ImpType0 = mut(tagged), ImpType = ~get_imptype2(X), MemExpr = ~funcall(v__n(A,I))
	      ; Mem = x(I) -> ImpType0 = mut(tagged), ImpType = ~get_imptype2(X), MemExpr = ~funcall('$custommem_x'(I))
	      ; Mem = y(I) -> ImpType0 = mut(tagged), ImpType = ~get_imptype2(X), MemExpr = ~funcall(v__y(I))
	      ),
	      Y = ( Mode = v ? Y0 | Mode = d ? '@'(Y0) ),
	      Y1 = '@'(Y0),
	      % TODO: Generate check_evalmem only when necessary.
	      BR = ('$check_evalmem'(Y0, MemExpr, ImpType0), '$trust_type'(Y1, ImpType))
	    )
	; BR = true,
	  pretranslate_arg(X, Y)
	).

pretranslate_args([], []) :- !.
pretranslate_args([X|Xs], [Y|Ys]) :- !,
	pretranslate_arg(X, Y),
	pretranslate_args(Xs, Ys).

pretranslate_arg(X, X) :- var(X), !.
pretranslate_arg(X, X) :-
	% TODO: Already translated; remove this clause?
	X = ~funcall(_), !.
pretranslate_arg(X, ~funcall('$varg'(X))) :- number(X), !.
pretranslate_arg(X, Y) :- X instance_of termstr, !,
	'$absmach'(Absmach),
	NA = ~X.name,
	Absmach.functorcons(NA, FC),
	pretranslate_arg(FC, Y).
pretranslate_arg(X, Y) :- X instance_of termunk, !,
	trust(X instance_of termunk),
	pretranslate_arg(~X.value, Y).
pretranslate_arg(smallint(X), Y) :- !,
	Y = ~funcall(make_small(~funcall('$trust_typed'(X, intval)))).
pretranslate_arg(callpad, Y) :- !,
	Y = ~funcall(v__callpad).
pretranslate_arg(float_value(X), Y) :- !,
	trust(X instance_of termunk),
	Y = ( X instance_of termunk ? ~pretranslate_arg(~X.value)
	    | X = ~funcall(_) ? X
	    ).
pretranslate_arg(bignum_value(X), Y) :- !,
	trust(X instance_of termunk),
	Y = ( X instance_of termunk ? ~pretranslate_arg(~X.value)
	    | X = ~funcall(_) ? X
	    ).
pretranslate_arg(type_functor(X), Y) :- !,
	Y = ( X instance_of termunk ?
	        ( str(Functor) = ~X.value ?
	            ~pretranslate_arg(str(Functor))
		| ~pretranslate_arg(~X.value)
		)
	    | X = ~funcall('$varg'(str(Functor))) ?
	        ~pretranslate_arg(str(Functor))
	    | X = ~funcall(_) ? X
	    ).
pretranslate_arg(float(X), ~funcall('$varg'(float(X)))) :- !.
pretranslate_arg(bignum(X), ~funcall('$varg'(bignum(X)))) :- !.
pretranslate_arg(atom(X), ~funcall('$varg'(atom(X)))) :- !.
pretranslate_arg(str(X), ~funcall('$varg'(str(X)))) :- !.
pretranslate_arg(liveinfo_set(HeapSize, LiveSet), ~funcall('$vargliveinfo_set'(HeapSize, LiveSet))) :- !.
pretranslate_arg(failcont(Next, NextIn), ~funcall('$vargfailcont'(Next, NextIn))) :- !.
pretranslate_arg(nullfailcont(As0), ~funcall('$vargnullfailcont'(As0))) :- !.
pretranslate_arg(successcont(Next, FrameLiveSize), ~funcall('$vargsuccesscont'(Next, FrameLiveSize))) :- !.
pretranslate_arg(nullsuccesscont(FrameLiveSize0), ~funcall('$vargnullsuccesscont'(FrameLiveSize0))) :- !.
pretranslate_arg(execcont(Label), ~funcall('$vargexeccont'(Label))) :- !.
pretranslate_arg(predtblentry(X), ~funcall('$vargpredtblentry'(X))) :- !.
pretranslate_arg(type_size(T), ~funcall('$vargtype_size'(T))) :- !.
pretranslate_arg(type_arity(X0), ~funcall('$vargtype_arity'(X0))) :- !.
pretranslate_arg(X, ~funcall('$varg'(X))).
}.

% ---------------------------------------------------------------------------
:- doc(subsection, "Schemata of analysis and transform passes over code").
% ---------------------------------------------------------------------------

% TODO: Change representation? [a,b,d(d,e)] -> t(a,t(b,merge(t(d,s0),t(e,s0)))).

% Forward analysis-and-transform pass
:- class forward_transform {
    {
    :- fluid exp :: module_exp.
    {
    :- fluid state :: m_any.
    map(t(Cont), t(Cont)) :- !.
    map(c(d(Ia, Ib), Is), c(d(Ia1, Ib1), Is1)) :- !,
	S0 = ~state,
	map_s(S0, Ia, Ia1, Sa),
	map_s(S0, Ib, Ib1, Sb),
	meet(Sa, Sb),
	map(Is, Is1).
    map(c(s(I), Is), Code2) :- !,
	call(( code :: accum(I1), transfer(I) )),
	map(Is, Is1),
	Code2 = ~compose(~compl(I1), Is1).
    }.
    map_s(S0, Ib, Ib1, Sb) :-
        state :: m_any <- S0,
	map(Ib, Ib1),
	Sb = ~state.

    {
    :- fluid code :: accum.
    :- fluid state :: m_any.
    :- virtual transfer/1.
    }.
    {
    :- fluid state :: m_any + d.
    :- virtual meet/2.
    }.
    }.
}.

% Backward analysis-and-transform pass, updating 'global'
:- class backward_analyze_g {
    {
    :- fluid exp :: module_exp.
    {
    :- fluid state :: m_any.
    :- fluid global :: m_any.
    analyze(t(_)) :- !.
    analyze(c(d(Ia, Ib), Xs)) :- !,
	analyze(Xs),
	S0 = ~state,
	analyze_s(S0, Ia, Sa),
	analyze_s(S0, Ib, Sb),
	meet(Sa, Sb).
    analyze(c(s(X), Xs)) :- !,
	analyze(Xs),
	transfer(X).
    }.

    {
    :- fluid global :: m_any.
    analyze_s(S0, X, Sb) :-
        state :: m_any <- S0,
	analyze(X),
	Sb = ~state.
    }.

    {
    :- fluid state :: m_any.
    :- fluid global :: m_any.
    :- virtual transfer/1.
    }.
    {
    :- fluid state :: m_any + d.
    :- virtual meet/2.
    }.
    }.
}.

% Backward analysis-only pass
% (like backward_analyze_g, but without 'global')
:- class backward_analyze {
    {
    :- fluid exp :: module_exp.
    {
    :- fluid state :: m_any.
    analyze(t(_)) :- !.
    analyze(c(d(Ia, Ib), Xs)) :- !,
	analyze(Xs),
	S0 = ~state,
	analyze_s(S0, Ia, Sa),
	analyze_s(S0, Ib, Sb),
	meet(Sa, Sb).
    analyze(c(s(X), Xs)) :- !,
	analyze(Xs),
	transfer(X).
    }.

    {
    analyze_s(S0, X, Sb) :-
        state :: m_any <- S0,
	analyze(X),
	Sb = ~state.
    }.

    {
    :- fluid state :: m_any.
    :- virtual transfer/1.
    }.
    {
    :- fluid state :: m_any + d.
    :- virtual meet/2.
    }.
    }.
}.

% Backward pass, updating 'global'
:- class backward_pass_g {
    {
    :- fluid exp :: module_exp.
    :- fluid global :: m_any.
    pass(t(_)) :- !.
    pass(c(d(Ia, Ib), Xs)) :- !,
	pass(Xs),
	pass(Ia),
	pass(Ib).
    pass(c(s(X), Xs)) :- !,
	pass(Xs),
	transfer(X).

    :- virtual transfer/1.
    }.
}.

% Forward pass, updating 'global'
:- class forward_pass_g {
    {
    :- fluid exp :: module_exp.
    :- fluid global :: m_any.
    pass(t(_Cont)) :- !.
    pass(c(d(Ia, Ib), Is)) :- !,
	pass(Ia),
	pass(Ib),
	pass(Is).
    pass(c(s(I), Is)) :- !,
	transfer(I),
	pass(Is).

    :- virtual transfer/1.
    }.
}.

:- mixin program_ctx {
    :- fluid exp :: module_exp.
}.

% TODO: The tail of a tree is a set of leafs, the head is a tree.

% Forward pass, taking elements while the condition holds
%
% TODO: cond is conservative (a cond is taken if condition holds
%   for both branches).
% TODO: NOTE: the last todo implies that forward_take(not cond) and
%   forward_takeneg(cond) are not equivalent
%
%:- public forward_take/3.
{
:- fluid exp :: module_exp.
:- meta_predicate forward_take(_, pred(1, program_ctx), _).
forward_take(c(d(Ia, Ib), As), C) := (c(d(Ia, Ib), Xs), Ys) :- prforall(Ia, C), prforall(Ib, C), !, (Xs, Ys) = ~forward_take(As, C).
forward_take(Xs, _) := (t(success), Xs) :- Xs = c(d(_, _), _), !.
forward_take(c(s(A), As), C) := (c(s(A), Xs), Ys) :- C(A), !, (Xs, Ys) = ~forward_take(As, C).
forward_take(Xs, _) := (t(success), Xs).
}.

% Forward pass, taking elements while the condition does not hold
%
% TODO: cond is conservative (a cond is taken if condition does not
%   hold for both branches).
% TODO: NOTE: the last todo implies that forward_take(not cond) and
%   forward_takeneg(cond) are not equivalent
%
%:- public forward_takeneg/3.
{
:- fluid exp :: module_exp.
:- meta_predicate forward_takeneg(_, pred(1, program_ctx), _).
forward_takeneg(c(d(Ia, Ib), As), C) := (c(d(Ia, Ib), Xs), Ys) :- \+ prforall(Ia, C), \+ prforall(Ib, C), !, (Xs, Ys) = ~forward_takeneg(As, C).
forward_takeneg(Xs, _) := (t(success), Xs) :- Xs = c(d(_, _), _), !.
forward_takeneg(c(s(A), As), C) := (c(s(A), Xs), Ys) :- \+ C(A), !, (Xs, Ys) = ~forward_takeneg(As, C).
forward_takeneg(Xs, _) := (t(success), Xs).
}.

% Backward pass, taking elements (from the tail) while the condition
% holds
%
%:- public backward_take/3.
% TODO: cond is conservative.
{
:- fluid exp :: module_exp.
:- meta_predicate backward_take(_, pred(1, program_ctx), _).
backward_take(Xs, C) := (Ns, Ys) :-
	T = ~backward_take_2(Xs, C),
	( T = closed(Ns, Ys) ->
	    true
	; T = open(Ns, Ys) ->
	    true
	; fail
	).

:- meta_predicate backward_take_2(_, pred(1, program_ctx), _).
backward_take_2(t(Cont), _) := open(t(success), t(Cont)) :- !.
backward_take_2(c(d(Ia, Ib), Xs), C) := T3 :- !,
	T2 = ~backward_take_2(Xs, C),
	T3 = ( T2 = open(Ns, Ys), prforall(Ia, C), prforall(Ib, C) ?
	         open(Ns, c(d(Ia, Ib), Ys))
	     | T2 = open(Ns, Ys) ?
	         closed(c(d(Ia, Ib), Ns), Ys)
	     | T2 = closed(Ns, Ys) ?
	         closed(c(d(Ia, Ib), Ns), Ys)
	     ).
backward_take_2(c(s(X), Xs), C) := T3 :- !,
	T2 = ~backward_take_2(Xs, C),
	T3 = ( T2 = open(Ns, Ys), C(X) ?
	         open(Ns, c(s(X), Ys))
	     | T2 = open(Ns, Ys) ?
	         closed(c(s(X), Ns), Ys)
	     | T2 = closed(Ns, Ys) ?
	         closed(c(s(X), Ns), Ys)
	     ).
}.

% Split the expression in one holding C and other not holding C.
%
%:- public prsplit/3.
%
% TODO: This is rare, and it only works without conds.
{
:- meta_predicate prsplit(_, pred(1, prsplit_op), _).
:- fluid exp :: module_exp.
:- fluid common :: any.
prsplit(c(d(_, _), _), _) := _ :- !, errlog:bug(['prsplit does not work with conds']), fail.
prsplit(c(s(A), As), C) := (c(s(A), Xs), Ys) :- C(A), !, (Xs, Ys) = ~prsplit(As, C).
prsplit(c(s(A), As), C) := (Xs, c(s(A), Ys)) :- !, (Xs, Ys) = ~prsplit(As, C).
prsplit(t(Cont), _) := (t(success), t(Cont)) :- !.
}.

:- mixin prsplit_op {
    :- fluid exp :: module_exp.
    :- fluid common :: any.
}.

% Forall elements the condition holds.
%:- public prforall/2.
{
:- fluid exp :: module_exp.
:- meta_predicate prforall(_, pred(1, program_ctx)).
prforall(Xs, C) :- \+ ( prmember(X, Xs), \+ C(X) ).
}.

%:- public prmap/3. (using predpass)
{
:- meta_predicate prmap(_, pred(3, prmap_op), _).
:- fluid exp :: module_exp.
:- fluid common :: any.
prmap(c(d(Ia, Ib), Xs), F) := c(d(~prmap(Ia, F), ~prmap(Ib, F)), ~prmap(Xs, F)) :- !.
prmap(c(s(X), Xs), F) := c(s(~F(X, Common)), ~prmap(Xs, F)) :- !, Common = ~common.
prmap(t(Cont), _) := t(Cont).
}.

:- mixin prmap_op {
    :- fluid exp :: module_exp.
}.

% Like flat(map()) with a common u(state) fluid var.
%:- public prflatmap_ro/3.
{
:- meta_predicate prflatmap_ro(_, pred(1, flatmap_ro_op), _).
:- fluid exp :: module_exp.
:- fluid state :: m_any + u.
prflatmap_ro(c(d(Ia, Ib), Xs), F) := c(d(~prflatmap_ro(Ia, F), ~prflatmap_ro(Ib, F)), ~prflatmap_ro(Xs, F)) :- !.
prflatmap_ro(c(s(X), Xs), F) := ~compose(~compl(Code), ~prflatmap_ro(Xs, F)) :- !,
	call(( code :: accum(Code), F(X) )).
prflatmap_ro(t(Cont), _) := t(Cont).
}.

:- mixin flatmap_ro_op {
    :- fluid state :: m_any + u.
    :- fluid exp :: module_exp.
    :- fluid code :: accum.
}.

% Like flat(map())
%:- public prflatmap/3.
{
:- fluid exp :: module_exp.
:- meta_predicate prflatmap(_, pred(1, flatmap_op), _).
prflatmap(c(d(Ia, Ib), Xs), F) := c(d(~prflatmap(Ia, F), ~prflatmap(Ib, F)), ~prflatmap(Xs, F)) :- !.
prflatmap(c(s(X), Xs), F) := ~compose(~compl(Code), ~prflatmap(Xs, F)) :- !,
	call(( code :: accum(Code), F(X) )).
prflatmap(t(Cont), _) := t(Cont).
}.

:- mixin flatmap_op {
    :- fluid exp :: module_exp.
    :- fluid code :: accum.
}.

%:- public prmember/2.
prmember(X, c(d(Ia, Ib), _)) :- ( prmember(X, Ia) ; prmember(X, Ib) ).
prmember(X, c(d(_, _), Xs)) :- prmember(X, Xs).
prmember(X, c(s(X), _)).
prmember(X, c(s(_), Xs)) :- prmember(X, Xs).

%:- public compose/3.
compose(A, B) := C :-
	( C = ~compose_2(A, B) ->
	    true
	; errlog:bug(['compose failed with ', A, ' ', B]), fail
	).

compose_2(t(success), Xs) := Xs :- !.
compose_2(c(d(Ia, Ib), Xs), Ys) := c(d(Ia, Ib), ~compose_2(Xs, Ys)) :- !.
compose_2(c(s(X), Xs), Ys) := c(s(X), ~compose_2(Xs, Ys)).

% Like compose, but also composes write and read.
%:- public compose_wide/3.
compose_wide(A, B) := C :-
	( C = ~compose_wide_2(A, B) ->
	    true
	; errlog:bug(['compose_wide failed with ', A, ' ', B]), fail
	).

compose_wide_2(t(success), Xs) := Xs :- !.
compose_wide_2(c(d(Ia, Ib), Xs), Ys) := c(d(Ia, Ib), ~compose_wide_2(Xs, Ys)) :- !.
compose_wide_2(c(s(X), Xs), Ys) := ~codecons_wide(X, ~compose_wide_2(Xs, Ys)).

codecons_wide(X, Y) := ~single_d(X, Y) :- Y = c(d(_, _), _), \+ X = switchrw(_), !.
codecons_wide(X, Y) := c(s(X), Y).

single_d(X, Y) := c(d(c(s(X), t(read)), c(s(X), t(write))), Y).

subscont(t(Cont), Cont0, Xs) := Xs :- !,
	( Cont = Cont0 ->
	    true
	; errlog:bug(['subscont failed ', Cont, ' ', Cont0]), fail
	).
subscont(c(d(Ia, Ib), t(Cont0)), Cont, Ys) := c(d(~subscont(Ia, Cont, success), ~subscont(Ib, Cont, success)), Ys) :- \+ Cont0 = Cont, !. % cond should be a codecont... ??
subscont(c(d(R, W), Xs), Cont, Ys) := c(d(R, W), ~subscont(Xs, Cont, Ys)) :- !.
subscont(c(s(X), Xs), Cont, Ys) := c(s(X), ~subscont(Xs, Cont, Ys)).

%:- public complw/2.
% TODO: Simplify and optimize.
complw([]) := t(success) :- !.
complw([X|Xs]) := Code :- !,
	Code = ~complw_(Xs, X).

complw_([X|Xs], Root) := ~complw_(Xs, ~compose_wide(Root, X)) :- !.
complw_([], Root) := Root.

%:- public compl/2.
% TODO: Simplify and optimize.
compl([]) := t(success) :- !.
compl([comprw(X)|Xs]) := Code :- !,
	Code = ~compl_(Xs, ~comprw(X)).
compl([X|Xs]) := Code :- !,
	Code = ~compl_(Xs, X).

compl_([comprw(X)|Xs], Root) := ~compl_(Xs, ~compose(Root, ~comprw(X))) :- !.
compl_([X|Xs], Root) := ~compl_(Xs, ~compose(Root, X)) :- !.
compl_([], Root) := Root.

%:- public comprw/2.
% TODO: Simplify and optimize.
comprw([X|Xs]) := Code :- !,
	Code = ~comprw2(Xs, X).

comprw2([moderw(A,B)|Xs], Root) := ~comprw2(Xs, ~composerw(Root, ~moderw(~comprw(A), ~comprw(B)))) :- !.
comprw2([X|Xs], Root) := ~comprw2(Xs, ~composerw(Root, X)) :- !.
comprw2([], Root) := Root.

moderw(R, W) := c(d(R, W), t(success)).

composerw(X, c(d(R, W), t(success))) := ~whererw_2(X, R, W) :- !.
composerw(X, Y) := ~compose(X, Y).

whererw_2(X, R, _) := ~subscont(X, read, R) :- pruniquecont(X, read), !.
whererw_2(X, _, W) := ~subscont(X, write, W) :- pruniquecont(X, write), !.
whererw_2(X, R, W) := ~compose(X, c(d(R, W), t(success))).

pruniquecont(t(Cont), Cont) :- !.
pruniquecont(c(d(R, W), t(Cont0)), Cont) :- \+ Cont0 = Cont, !,
	pruniquecont(R, Cont),
	pruniquecont(W, Cont).
pruniquecont(c(d(_, _), Xs), Cont) :- !, pruniquecont(Xs, Cont).
pruniquecont(c(s(_), Xs), Cont) :- pruniquecont(Xs, Cont).

%:- public cont/2.
cont(Cont) := t(Cont).

%:- public prhead/2.
prhead(c(s(X), _)) := X.

%:- public prtail/2.
prtail(c(s(_), Xs)) := Xs.

%:- public prlast/2.
% TODO: The same than forward_take.
prlast(c(s(X), t(Cont))) := (t(success), X) :- !,
	( Cont = success ->
	    true
	; errlog:bug(['prlast does not work in this moment with a Cont different from success']), fail
	).
prlast(c(d(R, W), Xs)) := (c(d(R, W), Ys), Z) :- !, (Ys, Z) = ~prlast(Xs).
prlast(c(s(X), Xs)) := (c(s(X), Ys), Z) :- (Ys, Z) = ~prlast(Xs).

%:- public single/2.
single(X) := c(s(X), t(success)).

%c(s(A), envrw(cccnd(R, W), Ds0))
%c(envrw(A, cccnd({R}, {W})), Ds0)

% ---------------------------------------------------------------------------

%:- public emit_jumps/1.
{
:- fluid code :: accum.
emit_jumps(Xs) :-
	ld :: labeldic <- ~labeldic.new,
	LabelS = ~ld.success,
	emit_jumps_2(Xs),
	code.add(label(LabelS)).
{
:- fluid ld :: labeldic.
:- '$ctxprj'(emit_jumps_2/1, [code,u(ld)]).
emit_jumps_2(c(X0, Xs0)) :-
	( Xs0 = c(d(_, _), _) ->
	    D2 = ~ld,
	    call((
              ld :: labeldic <- D2,
	      LabelS = ~ld.update_success,
	      emit_jumps_rw(X0, Xs0, Xs)
            )),
	    code.add(label(LabelS)),
	    emit_jumps_2(Xs)
	; emit_jumps_2(X0),
	  emit_jumps_2(Xs0)
	).
emit_jumps_2(t(LabelId)) :- !,
	code.add(jump(~ld.lookup(LabelId))).
emit_jumps_2(s(switchrw(X))) :- !, 
	code.add(cjump(X, ~ld.read, ~ld.write)).
emit_jumps_2(s(X)) :- !,
	code.add(X).

:- '$ctxprj'(emit_jumps_rw/3, [code,u(ld)]).
emit_jumps_rw(A, c(d(R, W), Ds0), Ds2) :- !,
	emit_jumps_rw(e(A, d(R, W)), Ds0, Ds2).
emit_jumps_rw(e(X, d(R, W)), Xs0, Xs) :- !,
	D2 = ~ld,
	call((
          ld :: labeldic <- D2,
          LabelR = ~ld.update_read,
          LabelW = ~ld.update_write,
	  emit_jumps_rw(X, Xs0, Xs)
        )),
	code.add(label(LabelR)),
	emit_jumps_2(R),
	code.add(label(LabelW)),
	emit_jumps_2(W).
emit_jumps_rw(X, Xs, Xs) :-
	emit_jumps_2(X).
}.
}.

:- class labeldic {
    :- attr success :: m_any.
    :- attr read :: m_any.
    :- attr write :: m_any.

    :- constructor new_/0.
    new_ :-
	~success = local(_),
	~read = local(_),
	~write = local(_).

    :- constant lookup/2.
    lookup(success) := ~success.
    lookup(read) := ~read.
    lookup(write) := ~write.

    update_success := L :- L = local(_), success <- L.
    update_read := L :- L = local(_), read <- L.
    update_write := L :- L = local(_), write <- L.
}.
