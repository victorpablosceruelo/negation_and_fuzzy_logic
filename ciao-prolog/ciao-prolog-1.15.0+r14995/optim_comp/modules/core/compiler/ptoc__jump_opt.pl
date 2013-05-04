:- module(_, [], [compiler(complang)]).

:- doc(title, "Control-flow graph tools").
:- doc(author, "Jose F. Morales").

:- use_module(library(lists)).
%:- use_module(library(format)).
:- use_module(library(dict)).

:- use_module(compiler(errlog)).
:- use_module(compiler(module_exp)).
:- use_module(.(ptoc__props)).

% Notes:
%   CFG: control-flow graph

% ---------------------------------------------------------------------------
% Analyze the predicate list and obtain a dictionary with the name of
% the subpredicates

% TODO: the predicates prefixed with new_* are duplicated; can they be
%       merged with the other version?

{
:- fluid exp :: module_exp.

:- public new_analyze_subpreds/2.
new_analyze_subpreds(Preds, SubpredDic) :-
	allpreds :: any <- Preds,
	cfg :: digraph <- ~digraph.empty,
	new_gblock_arcs_1(Preds),
	call((
          subpreddic :: u_dic <- SubpredDic,
	  new_find_subpreds(Preds)
	)).
{
:- fluid allpreds :: any.
:- fluid cfg :: digraph.
new_gblock_arcs_1([]).
new_gblock_arcs_1([PredId|Xs]) :-
	trust(PredId instance_of predicate_x),
	Code1 = ~PredId.code,
	call((
          from_id :: predicate_x <- PredId,
	  new_gblock_arcs_2(Code1)
        )),
	new_gblock_arcs_1(Xs).
{
:- fluid from_id :: predicate_x.
new_gblock_arcs_2(Code1) :-
	% TODO: ignoring other code that is not a icode(b) is not correct
	% TODO: use analysis entries!
	( CompMode = ~from_id.get_prop(compmode),
	  CompMode = lowcomp ->
	    % Mark arcs from unknown predicates to this predicate (if the predicate is registered it can be accessed from multiple locations)
	    ( true = ~from_id.get_prop(register) ->
		cfg.add(unknown, ~from_id)
	    ; true
	    ),
	    % Mark arcs from this predicate to called predicates
	    Code1 = icode(a, _, Code2),
	    ( Code2 = or(Cs) ->
		new_global_arcs__alts(Cs)
	    ; Code2 = index(Cases0, DefGoal0) ->
		new_global_arcs__cases(Cases0),
		new_global_arcs__conj(DefGoal0)
	    )
	; true
	).

new_global_arcs__cases([]).
new_global_arcs__cases([case(_, X0)|Xs0]) :-
	new_global_arcs__conj(X0),
	new_global_arcs__cases(Xs0).

new_global_arcs__alts([]).
new_global_arcs__alts([X|Xs]) :-
	new_global_arcs__conj(X),
	new_global_arcs__alts(Xs).

new_global_arcs__conj([]).
new_global_arcs__conj([X|Xs]) :-
	new_global_arcs__goal(X),
	new_global_arcs__conj(Xs).

new_global_arcs__goal(G) :-
	trust(G instance_of strgoal),
	~G.name = NA,
	!,
	cfg.add(~from_id, ~pred_maybe_lookup(NA)).
new_global_arcs__goal(_). % TODO: when?
}.
}.
}.

:- public mixin ctx_subpreds {
    :- fluid exp :: module_exp.
    :- fluid allpreds :: any.
    :- fluid subpreddic :: u_dic.
}.

{
:- extends ctx_subpreds.
:- fluid cfg :: digraph.
new_find_subpreds([]).
new_find_subpreds([PredId|Xs]) :-
	trust(PredId instance_of predicate_x),
	Code1 = ~PredId.code,
	call((
          from_id :: predicate_x <- PredId,
	  new_find_subpreds_1(Code1)
        )),
	new_find_subpreds(Xs).
{
:- fluid from_id :: predicate_x.
new_find_subpreds_1(Code1) :-
	% TODO: ignoring other code is not correct...
	( CompMode = ~from_id.get_prop(compmode),
	  CompMode = lowcomp ->
	    % TODO: index is ignored??!
	    Code1 = icode(a, _, Code2),
	    ( Code2 = or(Cs) ->
	        new_find_subpreds__alts(Cs)
	    ; Code2 = index(Cases0, DefGoal0) ->
		new_find_subpreds__cases(Cases0),
		new_find_subpreds__conj(DefGoal0)
	    )
	; true
	).

new_find_subpreds__cases([]) :- !.
new_find_subpreds__cases([case(_KeyType, X)|Xs]) :-
	new_find_subpreds__conj(X),
	new_find_subpreds__cases(Xs).

new_find_subpreds__alts([]) :- !.
new_find_subpreds__alts([X|Xs]) :-
	new_find_subpreds__conj(X),
	new_find_subpreds__alts(Xs).

new_find_subpreds__conj([]) :- !.
new_find_subpreds__conj([X|Xs]) :-
	trust(X instance_of strgoal),
	( ~X.name = Name,
	  PredId = ~pred_lookup(Name),
	  trust(PredId instance_of predicate_x),
	  new_maybe_subpred(~from_id, PredId) ->
	    subpreddic.lookup(~PredId.name, yes)
%	    PredId.set_prop(subpred, true)
	; true
	),
	new_find_subpreds__conj(Xs).
}.
}.

% TODO: use ctxprj (it does not work because of incctx pair+u)
{
:- fluid exp :: module_exp.
:- fluid cfg :: digraph + u.
% detects if a predicate is a subpredicate: it is called only at one program point and its definition can be unfolded
new_maybe_subpred(FromId, PredId) :-
	new_can_unfold(FromId, PredId),
	new_single_source(PredId, FromId).
}.

{
:- fluid exp :: module_exp.
new_can_unfold(FromId, PredId) :-
        % TODO: still cannot unfold predicates with cvar mems
	trust(PredId instance_of predicate_x),
	trust(FromId instance_of predicate_x),
	Code = ~PredId.code,
	Code = icode(a, _, _),
	CompMode = ~PredId.get_prop(compmode),
	CompMode = lowcomp,
	% same implementation mode
	Imp = ~PredId.get_prop(imp),
	FromImp = ~FromId.get_prop(imp),
	Imp = FromImp,
	% TODO: still no supported
	Modes = ~PredId.get_prop(argmodes),
	Mems = ~PredId.get_prop(argmems),
	InMems = ~filter_mode(Mems, Modes, in),
	InCvarMems = ~filter_cvar(InMems, InMems),
	( InCvarMems = [] -> true
	; errlog:bug(['warning: predicates with argmode=in argmem=cvar mems are not (new)unfolded yet:', ~PredId.name]),
	  fail
	),
	OutMems = ~filter_mode(Mems, Modes, out),
	OutCvarMems = ~filter_cvar(OutMems, OutMems),
	( OutCvarMems = [] -> true
	; errlog:bug(['warning: predicates with argmode=out argmem=cvar mems are not (new)unfolded yet:', ~PredId.name]),
	  fail
	).
}.

% TODO: innefficient
{
:- fluid exp :: module_exp.
:- fluid allpreds :: any.
pred_lookup(Name) := PredId :-
	member(PredId, ~allpreds),
	trust(PredId instance_of predicate_x),
	Name0 = ~PredId.name,
	Name == Name0,
	!.

pred_maybe_lookup(Name) := 
	( PredId0 = ~pred_lookup(Name) ? PredId0
	| '$$external$$'
	).
}.

% TODO: optimize using the following abstraction: use 'many' or 'single(From)'
{
:- fluid cfg :: digraph + u.
new_single_source(Node, OriginId) :-
	FromList = ~cfg.predecessors(Node), FromList = [OriginId0], OriginId == OriginId0.
}.

% TODO: unfold more predicates, not only subpreds, if declarations are provided...

{
:- extends ctx_subpreds.

:- public new_unfold_subpreds/2.
% TODO: try to propagate return pointer or doing some equivalent optimization (like unfolding before...)
new_unfold_subpreds([]) := [] :- !.
new_unfold_subpreds([PredId|Xs]) := ~new_unfold_subpreds(Xs) :-
	trust(PredId instance_of predicate_x),
	subpreddic.get(~PredId.name, _),
	!.
new_unfold_subpreds([X|Xs]) := [X|~new_unfold_subpreds(Xs)] :- !,
	new_unfold_subpreds__pred(X).

new_unfold_subpreds__pred(PredId) :-
	% TODO: index(...) predicate are ignored
	trust(PredId instance_of predicate_x),
	Code = ~PredId.code,
	CompMode = ~PredId.get_prop(compmode),
	CompMode = lowcomp,
	!,
	Code = icode(a, Args, Code2),
	( Code2 = or(Cs0) ->
	    call(( alts :: accum(Cs), new_unfold_subpreds__alts(Cs0) )),
	    NCode2 = or(Cs)
	; Code2 = index(Cases0, DefGoal0) ->
	    call(( cases :: accum(Cases), new_unfold_subpreds__cases(Cases0) )),
	    call(( code :: accum(DefGoal), new_unfold_subpreds__conj(DefGoal0) )),
	    NCode2 = index(Cases, DefGoal)
	),
        PredId.set_code(icode(a, Args, NCode2)).
new_unfold_subpreds__pred(_).

{
:- fluid cases :: accum.
new_unfold_subpreds__cases([]).
new_unfold_subpreds__cases([case(KeyType,X)|Xs]) :-
	call(( code :: accum(X2), new_unfold_subpreds__conj(X) )),
	cases.add(case(KeyType,X2)),
	new_unfold_subpreds__cases(Xs).
}.

{
:- fluid alts :: accum.
new_unfold_subpreds__alts([]) :- !.
new_unfold_subpreds__alts([X|Xs]) :-
	call(( code :: accum(X2), new_unfold_subpreds__conj(X) )),
	alts.add(X2),
	new_unfold_subpreds__alts(Xs).
}.

{
:- fluid code :: accum.
new_unfold_subpreds__conj([]).
new_unfold_subpreds__conj([X|Xs]) :-
	trust(X instance_of strgoal),
	( ~X.name = Name,
	  PredId = ~pred_lookup(Name),
	  trust(PredId instance_of predicate_x),
	  subpreddic.get(~PredId.name, _) ->	    
	    new_unfold_subpreds__pred(PredId),
	    code.add('$sub$'(X, PredId)) % TODO: note that it is similar to '$or$'
	; code.add(X)
	),
	new_unfold_subpreds__conj(Xs).
}.

}.

% ---------------------------------------------------------------------------
% Analyze the predicate list and obtain a dictionary with the name of the subpredicates

{
:- extends ctx_subpreds.

:- public analyze_subpreds/1.
analyze_subpreds(MainPredName) :-
	cfg :: digraph <- ~digraph.empty,		      
	gblock_arcs_1(~allpreds),
	% TODO: a kludge!!
	MainPredId = ~pred_lookup(MainPredName),
	cfg.add('$$external1$$', MainPredId),
	cfg.add('$$external2$$', MainPredId),
	find_subpreds(~allpreds).
{
:- fluid cfg :: digraph.
:- '$ctxprj'(gblock_arcs_1/1, [cfg, exp, allpreds]).
gblock_arcs_1([]).
gblock_arcs_1([PredId|Xs]) :-
	% TODO: ignoring other code that is not a icode(b) is not correct
	trust(PredId instance_of predicate_x),
	( icode(b, _, Code) = ~PredId.code ->
	    call((
              from_id :: any <- PredId,
	      global_code_arcs(Code)
            ))
	; true
	),
	gblock_arcs_1(Xs).

{
:- fluid from_id :: any.
:- '$ctxprj'(global_code_arcs/1, [cfg, exp, allpreds, from_id]).
global_code_arcs([]).
global_code_arcs([X|Xs]) :-
	global_code_arcs_2(X),
	global_code_arcs(Xs).

:- '$ctxprj'(global_code_arcs_2/1, [cfg, exp, allpreds, from_id]).
global_code_arcs_2(set_success_cont(successcont(NextId, _))) :- !,
	cfg.add(~from_id, NextId).
global_code_arcs_2(set_fail_cont(failcont(NextId, _))) :- !,
	cfg.add(~from_id, NextId).
global_code_arcs_2(call(G)) :- !,
	trust(G instance_of strgoal),
	~G.name = GName,
	GId = ~pred_maybe_lookup(GName),
	cfg.add(~from_id, GId),
	( ~G.getp(fail) = Fail ->
	    global_code_arcs_2(Fail)
	; true
	).
global_code_arcs_2('$bsub$'(_SubName, Ys)) :- !,
	global_code_arcs(Ys).
global_code_arcs_2(_).
}.

:- '$ctxprj'(find_subpreds/1, [u(cfg), exp, allpreds, subpreddic]).
find_subpreds([]).
find_subpreds([PredId|Xs]) :-
	trust(PredId instance_of predicate_x),
	( icode(b, _, Code) = ~PredId.code ->
	    find_subpreds__conj(Code)
	; true
	),
	find_subpreds(Xs).

:- '$ctxprj'(find_subpreds__conj/1, [u(cfg), exp, allpreds, subpreddic]).
find_subpreds__conj([]) :- !.
find_subpreds__conj([X|Xs]) :-
	( X = call(G) ->
            trust(G instance_of strgoal),
	    ( ~G.getp(binlastcall) = true,
	      ~G.getp(fail) = fail,
	      % TODO: get the pred directly from G?
	      ~G.name = Name,
	      PredId = ~pred_lookup(Name),
	      maybe_subpred(PredId) ->
	        trust(PredId instance_of predicate_x),
	        subpreddic.lookup(~PredId.name, yes)
	    ; true
	    ),
	    ( ~G.getp(fail) = Fail ->
	        find_subpreds__conj([Fail])
	    ; true
	    )
	; X = '$bsub$'(_SubName, Ys) ->
	    find_subpreds__conj(Ys)
	; true
	),
	find_subpreds__conj(Xs).
}.

}.

{
:- fluid exp :: module_exp.
:- fluid cfg :: digraph + u.
% detects if a predicate is a subpredicate: it is called only at one program point and its definition can be unfolded
maybe_subpred(PredId) :-
	trust(PredId instance_of predicate_x),
	can_unfold(PredId),
	\+ true = ~PredId.get_prop(register),
	single_source(PredId).
}.

{
:- fluid exp :: module_exp.
can_unfold(PredId) :-
        % TODO: still cannot unfold predicates with cvar mems
	trust(PredId instance_of predicate_x),
	icode(b, _, _) = ~PredId.code,
	% no cvar
	Modes = ~PredId.get_prop(argmodes),
	Mems = ~PredId.get_prop(argmems),
	InMems = ~filter_mode(Mems, Modes, in),
	InCvarMems = ~filter_cvar(InMems, InMems),
	( InCvarMems = [] -> true
	; errlog:bug(['predicates with cvar mems are not unfolded yet:', ~PredId.name]),
	  fail
	).
% TODO: ok? out mems no problem here?
%	OutMems = ~filter_mode(Mems, Modes, out),
%	( OutMems = [] -> true
%	; errlog:bug(['predicates out args are not unfolded yet:', Name])
%	).
}.

% ---------------------------------------------------------------------------
% Unfold subpredicates

{
:- extends ctx_subpreds.

:- public unfold_subpreds/2.
% TODO: try to propagate return pointer or doing some equivalent optimization (like unfolding before...)
unfold_subpreds([]) := [] :- !.
unfold_subpreds([PredId|Xs]) := ~unfold_subpreds(Xs) :-
	trust(PredId instance_of predicate_x),
	subpreddic.get(~PredId.name, _),
	!.
unfold_subpreds([X|Xs]) := [X|~unfold_subpreds(Xs)] :- !,
	unfold_subpreds__pred(X).

unfold_subpreds__pred(PredId) :-
	trust(PredId instance_of predicate_x),
	icode(b, BlockArgs, Code0) = ~PredId.code, !,
	call(( code :: accum(Code), unfold_subpreds__code(Code0) )),
	PredId.set_code(icode(b, BlockArgs, Code)).
unfold_subpreds__pred(_).

{
:- fluid code :: accum.
unfold_subpreds__code([]) :- !.
unfold_subpreds__code([X|Xs]) :-
	( X = call(G),
	  trust(G instance_of strgoal),
	  ~G.getp(binlastcall) = true,
	  ~G.getp(fail) = fail,
	  Name = ~G.name,
	  % TODO: get pred from G?
	  PredId = ~pred_lookup(Name),
	  trust(PredId instance_of predicate_x),
	  subpreddic.get(~PredId.name, _) ->
	    icode(b, _, Code00) = ~PredId.code,
	    unfold_subpreds__code(Code00)
	; % unfold the failure
	  X = call(G),
	  trust(G instance_of strgoal),
	  ~G.getp(fail) = Fail,
	  Fail = call(G2),
	  trust(G2 instance_of strgoal),
	  ~G2.getp(binlastcall) = true,
	  ~G2.getp(fail) = fail,
	  % TODO: get pred form G2?
	  Name = ~G2.name,
	  PredId = ~pred_lookup(Name),
	  trust(PredId instance_of predicate_x),
	  subpreddic.get(~PredId.name, _) ->
	    icode(b, _, Code00) = ~PredId.code,
	    FailCode = local(_),
	    SuccessCode = local(_),
	    code.add(call(~G.addp(fail, jump(FailCode)))),
	    code.add(jump(SuccessCode)),
	    code.add(label(FailCode)),
	    unfold_subpreds__code(Code00),
	    code.add(label(SuccessCode))
	; X = '$bsub$'(SubName, Ys) ->
	    call(( code :: accum(Ys2), unfold_subpreds__code(Ys) )),
	    code.add('$bsub$'(SubName, Ys2))
	; code.add(X)
	),
	unfold_subpreds__code(Xs).
}.
}.

{
:- fluid cfg :: digraph + u.
single_source(Node) :-
	FromList = ~cfg.predecessors(Node), FromList = [X], X \== unknown. % TODO: what is unknown???
}.

% ---------------------------------------------------------------------------

{
:- fluid exp :: module_exp.

:- public jump_opt/2.
:- pred jump_opt(+Low, -Low2) :: list(term) * list(term) # "@var{Low2}
   is the result of optimizing the jumps in @var{Low}.".
jump_opt(Xs0) := Ys :-
	RootLabel = local(_),
	Xs1 = ~merge_labels([label(RootLabel)|Xs0]),
	call(( blocks :: accum(Xs2), code_to_blocks(Xs1) )),
	call((
          cfg :: digraph <- ~digraph.empty,			      
          block_arcs_1(Xs2),
	  Used = ~cfg.descendants_dic(RootLabel),
	  call((
            used :: u_dic <- Used,
            blocks :: accum(Xs3),
	    remove_unreachable_blocks(Xs2)
          )),
	  call(( code :: accum(Ys1), linearize(Xs3) )),
	  call(( code :: accum(Ys2), remove_superfluous_jumps(Ys1) )),
	  Ys2 = [label(RootLabel)|Ys]
	)).
}.

% ---------------------------------------------------------------------------

% TODO: this works because every jump is followed by a label and every label preceded by a jump
merge_labels([label(A), jump(B)|Xs]) := ~merge_labels(Xs) :- A = local(A0), B = local(B0), var(A0), var(B0), A = B, !.
%merge_labels([label(A), jump(B)|Xs]) := [jump(B)|~merge_labels(Xs)] :- A = local(A0), B = local(B0), var(A0), var(B0), A = B, !.
merge_labels([X|Xs]) := ~merge_labels(Xs) :- X = label(_), Xs = [X|_], !.
merge_labels([X|Xs]) := [X|~merge_labels(Xs)] :- !.
merge_labels([]) := [] :- !.

% ---------------------------------------------------------------------------
% Obtain the list of CFG-nodes from the linear code representation

{
:- fluid exp :: module_exp.
:- fluid blocks :: accum.
code_to_blocks([]).
code_to_blocks([label(Name)|Code]) :-
	code_to_blocks_2(Code, Code1, Rest),
	blocks.add(lblock(Name, ~remove_after_jump(Code1))),
	code_to_blocks(Rest).
}.

code_to_blocks_2([], [], []) :- !.
code_to_blocks_2(Code, [jump(Name)], Code) :- Code = [label(Name)|_], !.
code_to_blocks_2([I|Code], [I|Code1], Code0) :- !, code_to_blocks_2(Code, Code1, Code0).

% do a little of dead code elimination
{
:- fluid exp :: module_exp.
remove_after_jump([X|_Xs]) := [X] :- is_jump(X), !.
remove_after_jump([X|Xs]) := [X|~remove_after_jump(Xs)] :- !.
remove_after_jump([]) := [] :- !.

% TODO: wrong!!! I DO NOT USE THESE INSTRUCTIONS HERE!
%is_jump(fail(_)) :- !.
%is_jump(proceed(_, _, _)) :- !.
is_jump(call(G)) :-
	trust(G instance_of strgoal),
	~G.getp(binlastcall) = true, !.
is_jump(call(G)) :-
	trust(G instance_of strgoal),
	~G.getp(proceed_at) = _, !.
is_jump(call(G)) :-
	trust(G instance_of strgoal),
	~G.name = 'basiccontrol:fail'/0, !.
is_jump(jump(_)) :- !.
is_jump(cjump(_, _, _)) :- !.
is_jump(switch_on_index(_, _, _, _, _)) :- !.
}.
	
% ---------------------------------------------------------------------------
% Linearize as a list of instructions the code represented in blocks

{
:- fluid code :: accum.
linearize([]).
linearize([lblock(Name, Code)|Xs]) :-
	code.add(label(Name)),
	linearize__2(Code),
	linearize(Xs).

linearize__2([]).
linearize__2([X|Xs]) :- code.add(X), linearize__2(Xs).
}.

% ---------------------------------------------------------------------------

{
:- fluid cfg :: digraph.
block_arcs_1([]).
block_arcs_1([lblock(Name, Ys)|Xs]) :-
	call((
          from :: any <- Name,
	  code_arcs(Ys)
        )),
	block_arcs_1(Xs).

{
:- fluid from :: any.
code_arcs([]) :- !.
code_arcs([X|Xs]) :-
	code_arcs_2(X),
	code_arcs(Xs).

code_arcs_2(jump(X)) :- !,
	code_arcs_add(X).
code_arcs_2(cjump(_, X, Y)) :- !,
	code_arcs_add(X),
	code_arcs_add(Y).
code_arcs_2(switch_on_index(_, Y, Z, V, W)) :- !,
	trust(Y instance_of termunk),
	trust(Z instance_of termunk),
	trust(V instance_of termunk),
	trust(W instance_of termunk),
	code_arcs_add(~Y.value),
	code_arcs_add(~Z.value),
	cases_arcs(~V.value),
	code_arcs_add(~W.value).
code_arcs_2(_).

cases_arcs([_-X|Xs]) :- !,
	code_arcs_add(X),
	cases_arcs(Xs).
cases_arcs([]).

code_arcs_add(To) :- To = local(_), !,
	cfg.add(~from, To).
code_arcs_add(_).
}.
}.

% ---------------------------------------------------------------------------

{
:- fluid used :: u_dic.
:- fluid blocks :: accum.
remove_unreachable_blocks([]).
remove_unreachable_blocks([X|Xs]) :-
	( X = lblock(Name, _), \+ used.get(Name, _) ->
	    true
	; blocks.add(X)
	),
	remove_unreachable_blocks(Xs).
}.

% ---------------------------------------------------------------------------

{
:- fluid cfg :: digraph + u.
:- fluid code :: accum.
remove_superfluous_jumps([jump(A), label(B)|Xs]) :- A == B, has_single_source(B), !,
	remove_superfluous_jumps(Xs).
remove_superfluous_jumps([X|Xs]) :- !, code.add(X), remove_superfluous_jumps(Xs).
remove_superfluous_jumps([]).
}.

{
:- fluid cfg :: digraph + u.
has_single_source(Node) :-
	FromList = ~cfg.predecessors(Node), FromList = [X], X \== unknown. % TODO: what is unknown???
}.

% ---------------------------------------------------------------------------
% A digraph implementation.
%   It uses a pair of From->[To] and To->[From] tables (library(dict))
%   to enable fast access to both successors and predecessors.

% TODO: create a version that uses a mutvar safe dictionary!
%   library(dict) does not work with attributes variables because the
%   order relation between variables changes as variables get its
%   attributes removed/modified.  A solution is to store the predicate
%   name as hash value and store the real predicate name in the
%   attributes.

:- class digraph {
    :- attr forward :: m_dic # "Forward arcs".
    :- attr backward :: m_dic # "Backward arcs".

    :- constructor empty_/0.
    empty_.

    % Add an arc from From to To
    add(From, To) :-
            forward <- ~setadd(From, To, ~forward),
            backward <- ~setadd(To, From, ~backward).

    % (private)
    :- '$ctxprj'(setadd/4, []).
    :- static setadd/4.
    setadd(A, B, Set0, Set) :- dic_get(Set0, A, Xs), !, dic_replace(Set0, A, [B|Xs], Set).
    setadd(A, B, Set, Set) :- dic_lookup(Set, A, [B]).

    % Ds is the list of successors of vertex V
    :- constant successors/2.
    successors(V) := Ws :- forward.get(V, Ws), !.
    successors(_) := [] :- !.

    % FromList is the list of vertexes from which To can be accessed
    :- constant predecessors/2.
    predecessors(V) := Ws :- backward.get(V, Ws), !.
    predecessors(_) := [] :- !.

    % WsDic contains all the descendants of V
    :- constant descendants_dic/2.
    descendants_dic(V) := WsDic :-
            nodes :: m_dic,
	    descendants_1(V),
	    ~nodes = WsDic.
    % (private)
    {
    :- fluid nodes :: m_dic.
    :- constant descendants_1s/1.
    descendants_1s([]).
    descendants_1s([V|Vs]) :- descendants_1(V), descendants_1s(Vs).

    :- constant descendants_x/1.
    descendants_1(V) :- nodes.get(V, _), !.
    descendants_1(V) :-
            nodes.lookup(V, marked),
            descendants_1s(~successors(V)).
    }.
}.
