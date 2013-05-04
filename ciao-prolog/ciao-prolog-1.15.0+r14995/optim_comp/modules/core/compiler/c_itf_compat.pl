:- module(c_itf_compat, [], [dcg, fsyntax]).

:- use_module(compiler(errlog)).
:- use_module(compiler(memoize)).
:- use_module(compiler(store)).
:- use_module(compiler(module_pli)).
:- use_module(compiler(module_exp)).
:- use_module(compiler(module_deps)).
:- use_module(compiler(module_ideps)).
:- use_module(compiler(module_sym)).
:- use_module(library(lists)).
:- use_module(library(aggregates)).
:- use_module(library(dict)).

% TODO: wrong! use global vars to store memoize... 
:- initialization(init).
:- data memo/1. 

init :-
	Errs = ~(errlog:new),
	Errs.add(verbose(off)),
	Memo = ~(memoize:new(Errs)),
	asserta_fact(memo(Memo)).

:- meta_predicate get_memo(out(memoize)).
get_memo(Memo) :-
	memo(Memo).

:- export(get_code_and_related_assertions_opts/6).
% TODO: this code is not very clean
get_code_and_related_assertions_opts(Uspec,Opts,M,Base,Suffix,Dir) :-
	cleanup_everything,
	store:find_source(Uspec, Spec),
	get_code_and_related_assertions_opts0(Spec,Opts,M,Base,Suffix,Dir).

get_code_and_related_assertions_opts0(Spec,Opts,M,Base,Suffix,Dir) :-
	Info = info(M,Base,Suffix,Dir),
	get_memo(Memo),
	Memo.enter,
	( read_one_and_related(Spec, Opts, Info) ->
	    Ok = yes
	; cleanup_everything,
	  Ok = no
	),
	Memo.leave,
	Ok = yes.

read_one_and_related(Spec, Opts, Info) :-
	read_one(Spec, Opts, Info),
	Info = info(_,Base,_,_),
	( imported(Base, IUspec, _),
	  \+ module_spec(_, IUspec),
	  store:find_source(IUspec, ISpec),
	  read_one(ISpec, Opts, _),
	  fail
	; true
	).

% TODO: not all the clauses and assertions are necessary for secondary modules
read_one(Spec, Opts, Info) :-
        ( io(ref, Spec, I),
	  translate(I, Spec, Opts, Info) ->
	    Ok = yes
	; Ok = no
	),
	io(unref, Spec, I),
	Ok = yes.

io(Op, Spec, I) :-
	get_memo(Memo),
	I = i(Pli, Deps, Exp, IDeps, Sym),
	io__2(Op, Memo, split__src, Spec, Pli, Bad),
	io__2(Op, Memo, split, Spec, Deps, Bad),
	io__2(Op, Memo, expand__src, Spec, Exp, Bad),
	io__2(Op, Memo, expand, Spec, IDeps, Bad),
	io__2(Op, Memo, expand__sym, Spec, Sym, Bad),
	( var(Bad) -> true ; io(unref, Spec, I), fail ).

io__2(ref, Memo, Type, Spec, Ref, Bad) :-
	trust(Memo instance_of memoize),
	functor(Action, Type, 1),
	arg(1, Action, Spec),
	( Memo.eval(Action, Ref) ->
	    true
	; Bad = yes % and leave Ref unbound
	).
io__2(unref, _Memo, _Type, _Spec, Ref, _) :-
	( nonvar(Ref) -> % instantiated if loaded
	    '$inst_destroy'(Ref)
	; true
	).

translate(I, Spec, _Opts, Info) :-
	Info = info(M,Base,Suffix,Dir),
	I = i(Pli, Deps, Exp, IDeps, Sym),
	trust(Deps instance_of module_deps),
	trust(Pli instance_of module_pli),
	trust(IDeps instance_of module_ideps),
	trust(Exp instance_of module_exp),
	trust(Sym instance_of module_sym),

	store:addr(prolog_source(Spec), Name),
	absolute_file_name(Name, '', '.pl', '.', _AbsFile, _AbsBase, Dir),
	substract_pl(Name, Base, Suffix),

	M = Exp.defines_module,

	% note: do not alter this order!
	% Direct imports (not reexports)
	( Deps.import(ImpSpec),
	  ( Pli.should_be_usermod(ImpSpec) ->
	      store:denormalized_spec(ImpSpec, ImpUspec),
	      % imported module (imported as user module)
	      assertz_fact(adds(Base, ImpUspec))
	  ; store:denormalized_spec(ImpSpec, ImpUspec),
	    % imported module (not imported as user module)
	    assertz_fact(uses_file(Base, ImpUspec))
	  ),
	  fail
	; true
	),
	% Included files
	( Deps.include(IncSpec),
	  store:denormalized_spec(IncSpec, IncUspec),
	  assertz_fact(includes(Base, IncUspec)),
	  fail
	; true
	),
	% Any imported module (including reexports)
        ( IDeps.imported(IM, ISpec),
	  store:denormalized_spec(ISpec, IUspec),
	  base_name(IUspec, IBase),
	  ( defines_module(IBase, _) ->
	      true
	  ; defines_module(IBase, IM),
	    module_spec(IM, IUspec),
	    imported(Base, IUspec, IM)
	  ),
	  fail
	; true
	),
	% Defined predicates
        ( Pli.def_pred(F,A,PubProps),
	  PubProps = pub_props(Def0, Meta, Visibility, _IsProp, _Context),
	  olddef(Def0, Def),
	  \+ internal_predicate(F, A),
	  assertz_fact(defines(Base,F,A,Def,Meta)),
	  ( Visibility = vs_multifile ->
	      assertz_fact(def_multifile(Base,F,A,Def))
	  ; true
	  ),
	  fail
	; true
	),
	% Predicate declarations (for Ciaopp)
	( Exp.expanded_decl(MF, A, PubProps),
	  PubProps = pub_props(Def0, Meta, Visibility, _, _),
	  olddef(Def0, Def),
	  \+ internal_predicate__pp(MF, A),
	  assertz_fact(defines__pp(Base,MF,A,Def,Meta)),
	  ( Visibility = vs_multifile ->
	      assertz_fact(def_multifile__pp(Base,MF,A,Def))
	  ; true
	  ),
	  fail
	; true
	),
	% Imported predicates (including those due to reexports)
	( Exp.expanded_import(MF, A, PubProps),
	  unexpand_module(MF, EM, F),
	  ( Exp.expanded_import_from(MF, A, IM) ->
	      true
	  ; IM = EM % not reexported 
	  ),
	  PubProps = pub_props(Def0, Meta, _, _, _),
	  olddef(Def0, Def),
	  module_spec(IM, UFile),
	  module_spec(EM, EndFile),
	  % EndFile not used in autodoc
	  assertz_fact(imports_pred(Base,UFile,F,A,Def,Meta,EndFile)),
	  assertz_fact(imports_pred__pp(Base,UFile,MF,A,Def,Meta,EndFile)),
	  fail
	; true
	),
	% Exported predicates
        ( Sym.exports(F, A, _MF),
	  % DetType and Meta are not used in autodoc
	  ( defines(Base,F,A,Def,Meta) ->
	      true
	  ; imports_pred(Base,_,F,A,Def,Meta,_) ->
	      true
	  ),
	  assertz_fact(exports(Base,F,A,Def,Meta)),
	  atom_concat(M,':',M2),
	  atom_concat(M2,F,MF),
	  assertz_fact(exports__pp(Base,MF,A,Def,Meta)),
	  fail
	; true
	),
	% use_module's
        ( imports_directives(Decl,Pli),
	  %Loc = loc([], Src, Ln0, Ln1),
	  Dict = [], Src = '', Ln0 = 0, Ln1 = 0,
	  assertz_fact(clause_read(Base,0,Decl,Dict,Src,Ln0,Ln1)),
	  assertz_fact(clause_read__pp(Base,0,Decl,Dict,Src,Ln0,Ln1)),
	  fail
	; true
	),
	% Clauses
	( Preds = Exp.preds,
	  member(P, Preds),
	  P = p(_, _, Cs),
	  member(C, Cs),
	  unexpand_clause(C, C2),
	  C2 = c(Head, Body, Loc),
	  Loc = loc(Dict, Src, Ln0, Ln1),
	  % TODO: Head = 0 for comments ??
	  assertz_fact(clause_read(Base,Head,Body,Dict,Src,Ln0,Ln1)),
	  fail
	; true
	),
	% Clauses (expanded)
	( Preds = Exp.preds,
	  member(P, Preds),
	  P = p(_, _, Cs),
	  member(C, Cs),
	  pp_unexpand_clause(C, C2),
	  C2 = c(Head, Body, Loc),
	  Loc = loc(Dict, Src, Ln0, Ln1),
	  % TODO: Head = 0 for comments ??
	  assertz_fact(clause_read__pp(Base,Head,Body,Dict,Src,Ln0,Ln1)),
	  fail
	; true
	),
	% Declarations (user decls, comments, etc.)
	% TODO: are all declarations here?
        ( Pli.decl(Decl,Loc),
	  Loc = loc(Dict, Src, Ln0, Ln1),
	  assertz_fact(clause_read(Base,1,Decl,Dict,Src,Ln0,Ln1)),
	  assertz_fact(clause_read__pp(Base,1,Decl,Dict,Src,Ln0,Ln1)),
	  fail
	; true
	),
	% Assertions
	( Exp.expanded_assertion(A),
	  unexpand_assertion(A, A2),
	  A2 = a(Status, Type, NAss, Loc),
	  Loc = loc(Dict, S, LB,LE),
	  assertion_body(AssrtP,_,_,_,_,_,NAss),
	  assertz_fact(assertion_read(AssrtP,M,Status,Type,NAss,Dict,S,LB,LE)),
	  fail
	; true
	),
	% Assertions
	( Exp.expanded_assertion(A),
	  pp_unexpand_assertion(A, A2),
	  A2 = a(Status, Type, NAss, Loc),
	  Loc = loc(Dict, S, LB,LE),
	  assertion_body(AssrtP,_,_,_,_,_,NAss),
	  assertz_fact(assertion_read__pp(AssrtP,M,Status,Type,NAss,Dict,S,LB,LE)),
	  fail
	; true
	),
	% End
	true.

imports_directives(D, Pli) :-
	trust(Pli instance_of module_pli),
	findall((ImpSpec, F/A), Pli.get_imports(ImpSpec, F, A), Xs),
	cluster_pairs(Xs, Ys),
	member((ImpSpec2, Preds), Ys),
	  store:denormalized_spec(ImpSpec2, ImpUspec3),
	  D = (use_module(ImpUspec3, Preds)),
	  fail.
imports_directives(D, Pli) :-
	trust(Pli instance_of module_pli),
	Pli.get_imports_all(ImpSpec),
	  store:denormalized_spec(ImpSpec, ImpUspec2),
	  D = (use_module(ImpUspec2)).

unexpand_clause(c(Head0, Body0, Loc), c(Head, Body, Loc)) :-
	unexpand(Head0, Head),
	unexpand(Body0, Body). 

pp_unexpand_clause(c(Head0, Body0, Loc), c(Head, Body, Loc)) :-
	pp_unexpand(Head0, Head),
	pp_unexpand(Body0, Body). 

unexpand_assertion(a(Status, Type, Ass0, Loc),
	           a(Status, Type, Ass, Loc)) :-
       	assertion_body(PD0,DP0,CP0,AP0,GP0,CO,Ass0),
        unexpand(PD0, PD),
	unexpand(DP0, DP),
	unexpand(CP0, CP),
	unexpand(AP0, AP),
	unexpand(GP0, GP),
       	assertion_body(PD,DP,CP,AP,GP,CO,Ass).

pp_unexpand_assertion(a(Status, Type, Ass0, Loc),
	           a(Status, Type, Ass, Loc)) :-
       	assertion_body(PD0,DP0,CP0,AP0,GP0,CO,Ass0),
        pp_unexpand(PD0, PD),
	pp_unexpand(DP0, DP),
	pp_unexpand(CP0, CP),
	pp_unexpand(AP0, AP),
	pp_unexpand(GP0, GP),
       	assertion_body(PD,DP,CP,AP,GP,CO,Ass).

olddef(bytecode, static) :- !.
olddef(X, X) :- !.

:- export(base_name/2).
base_name(Uspec, Base) :-
	store:find_source(Uspec, Spec),
	store:addr(prolog_source(Spec), Name),
	substract_pl(Name,Base,'.pl').

cleanup_everything :-
	cleanup_c_itf_data,
	cleanup_code_and_related_assertions.

:- export(cleanup_c_itf_data/0).
cleanup_c_itf_data :-
	retractall_fact(imported(_,_,_)),
	retractall_fact(module_spec(_,_)),
	retractall_fact(uses_file(_,_)),
	retractall_fact(adds(_,_)),
	retractall_fact(includes(_,_)),
	retractall_fact(def_multifile(_,_,_,_)),
	retractall_fact(def_multifile__pp(_,_,_,_)),
	retractall_fact(defines(_,_,_,_,_)),
	retractall_fact(defines__pp(_,_,_,_,_)),
	retractall_fact(defines_module(_,_)),
	retractall_fact(exports(_,_,_,_,_)),
	retractall_fact(exports__pp(_,_,_,_,_)),
	retractall_fact(imports_pred(_,_,_,_,_,_,_)),
	retractall_fact(imports_pred__pp(_,_,_,_,_,_,_)).

:- export(cleanup_code_and_related_assertions/0).
cleanup_code_and_related_assertions :-
	retractall_fact(clause_read(_,_,_,_,_,_,_)),
	retractall_fact(clause_read__pp(_,_,_,_,_,_,_)),
	retractall_fact(assertion_read(_,_,_,_,_,_,_,_,_)),
	retractall_fact(assertion_read__pp(_,_,_,_,_,_,_,_,_)).

:- export(uses_file/2).
:- data uses_file/2.
:- export(adds/2).
:- data adds/2.
:- export(includes/2).
:- data includes/2.
:- export(def_multifile/4).
:- data def_multifile/4.
:- export(def_multifile__pp/4).
:- data def_multifile__pp/4.
:- export(defines/5).
:- data defines/5.
:- export(defines__pp/5).
:- data defines__pp/5.
:- export(defines_module/2).
:- data defines_module/2.
:- export(exports/5).
:- data exports/5.
:- export(exports__pp/5).
:- data exports__pp/5.
:- export(imports_pred/7).
:- data imports_pred/7.
:- export(imports_pred__pp/7).
:- data imports_pred__pp/7.
:- export(assertion_read/9).
:- data assertion_read/9.
:- export(assertion_read__pp/9).
:- data assertion_read__pp/9.
:- export(clause_read/7).
:- data clause_read/7.
:- export(clause_read__pp/7).
:- data clause_read__pp/7.

% private
:- data module_spec/2.
% private
:- data imported/3.

substract_pl(FPL,F,'.pl') :-
	atom_concat(F,'.pl',FPL),
	!. %% it ends in .pl
%% else, it does not end in .pl
substract_pl(F,F,'').

% TODO: use and fill this types (see lpdoc autodoc.pl)
:- export(miscopt/1).
miscopt(_).
:- export(miscopts/1).
miscopts(_).

:- reexport(compiler(assertions__common), [assertion_body/7]).

:- export(use_module/1).
use_module(_). % disabled by now, it is used to include dynamic paths
:- export(set_libs/2).
set_libs(_,_). % disabled by now, it is used to include dynamic paths

% ---------------------------------------------------------------------------
% TODO: meta-data predicates (make it configurable)

internal_predicate('$primitive_meta_predicate',2).
internal_predicate('$current_module',1).
internal_predicate('$ldlibs',1).
internal_predicate('$multifile',3).
internal_predicate('$load_libs',0).
internal_predicate('$meta_args',2).
internal_predicate('$u',2).
internal_predicate('$initialization',1).
internal_predicate('$on_abort',1).
internal_predicate('$imports',5).
internal_predicate('$defines',3).

internal_predicate('$context',4).
internal_predicate('$specmod',2).
internal_predicate('$exports',4).
internal_predicate('$imports_all',2).

internal_predicate__pp('multifile:$primitive_meta_predicate',2).
internal_predicate__pp('multifile:$current_module',1).
internal_predicate__pp('multifile:$ldlibs',1).
internal_predicate__pp('multifile:$multifile',3).
internal_predicate__pp('multifile:$load_libs',0).
internal_predicate__pp('multifile:$meta_args',2).
internal_predicate__pp('multifile:$u',2).
internal_predicate__pp('multifile:$initialization',1).
internal_predicate__pp('multifile:$on_abort',1).
internal_predicate__pp('multifile:$imports',5).
internal_predicate__pp('multifile:$defines',3).

internal_predicate__pp('multifile:$context',4).
internal_predicate__pp('multifile:$specmod',2).
internal_predicate__pp('multifile:$exports',4).
internal_predicate__pp('multifile:$imports_all',2).

% ---------------------------------------------------------------------------
% TODO: FIX!!! THIS CODE IS WRONG!!!! (missing meta_pred info)

unexpand(G, G) :- var(G), !.
unexpand(G0, G) :-
        G0 =.. [MF|Xs0],
        % that is not correct, we should use meta_predicate info
        ( atom(MF), unexpand_module(MF, M, F) ->
            true
        ; M = '', F = MF
        ),
        unexpand__2(Xs0, Xs),
        G =.. [F|Xs].

unexpand_module(MF, M, F) :-
        atom_codes(MF, MFCodes),
        append(MCodes, ":"||FCodes, MFCodes), !,
        atom_codes(M, MCodes),
        atom_codes(F, FCodes).

unexpand__2([], []).
unexpand__2([X0|Xs0], [X|Xs]) :-
        unexpand(X0, X),
        unexpand__2(Xs0, Xs).

% ---------------------------------------------------------------------------
% TODO: FIX!!! THIS CODE IS WRONG!!!! (missing meta_pred info)
% TODO: merge with simple unexpand 

pp_unexpand(G, G) :- var(G), !.
pp_unexpand(G0, G) :-
	pp_unexpand__goal(G0, G1), !,
	pp_unexpand(G1, G).
pp_unexpand(G0, G) :-
        G0 =.. [F|Xs0],
        % that is not correct, we should use meta_predicate info
        pp_unexpand__2(Xs0, Xs),
        G =.. [F|Xs].

pp_unexpand__2([], []).
pp_unexpand__2([X0|Xs0], [X|Xs]) :-
        pp_unexpand(X0, X),
        pp_unexpand__2(Xs0, Xs).

pp_unexpand__goal('basiccontrol:,'('rt_exp:rt_module_exp'(X,_,_,_,_,_,Y),C), C) :- !, % TODO: really dirty... this expansion should be done after?!? (or use other notation... hmmm perhaps some escape term in the actual argument?? 
	X = Y.
pp_unexpand__goal('basiccontrol:,'(A,B), (A,B)) :- !.
pp_unexpand__goal('basiccontrol:;'(A,B), (A;B)) :- !.
pp_unexpand__goal('basiccontrol:->'(A,B), (A->B)) :- !.
pp_unexpand__goal('aggregates:^'(X,A), (X^A)) :- !.
pp_unexpand__goal('basiccontrol:\\+'(A), (\+ A)) :- !.
pp_unexpand__goal('basiccontrol:if'(A,B,C), if(A,B,C)) :- !.
pp_unexpand__goal('basiccontrol:!', !) :- !.
%pp_unexpand__goal('basiccontrol:true', true) :- !.
pp_unexpand__goal('$:'('PA'(_,_,NP1)), NP) :-
	NP1 =.. [N,_|As], !,
	NP =.. [N|As].
pp_unexpand__goal('$:'(A), A) :- !.

% ---------------------------------------------------------------------------

:- use_package(hiord).
:- use_module(compiler(global_pass)).

:- export(process_file/7).
:- meta_predicate
        process_file(+, +, +, pred(1), pred(1), pred(1), pred(1)).
% TODO: StopP is ignored
% TODO: RedoP is ignored
% TODO: SkipP is ignored
% TODO: Mode is ignored
% TODO: Type is ignored
/*
% TODO: this is wrong...
process_file(USpec, _Mode, _Type, TreatP, _StopP, _SkipP, _RedoP) :-
	store:find_source(Uspec, Spec),
	cleanup_everything,
	get_memo(Memo),
	reachable_module_set([Spec], Memo, AllSpecs),
	( member(RelatedSpec, AllSpecs),
	    Opts = [],
	    get_code_and_related_assertions_opts0(RelatedSpec,Opts,_M,Base,_Suffix,_Dir),
	    TreatP(Base),
	    fail
	; true
	).
*/
process_file(Uspec, _Mode, _Type, TreatP, _StopP, _SkipP, _RedoP) :-
	% TODO: remove trace message
%	display(user_error, Uspec), nl(user_error),
	store:find_source(Uspec, Spec),
	cleanup_everything,
	Opts = [],
	get_code_and_related_assertions_opts0(Spec,Opts,_M,Base,_Suffix,_Dir),
	TreatP(Base),
	% TODO: delete_module_data and delete_file_data after treatment... is the following equivalent?
	cleanup_everything.

% ---------------------------------------------------------------------------

% TODO: very similar to cluster_clauses/bag_to_list in action__compile!!
% e.g. [(a,0),(b,3),(a,1),(b,5)] -> [(a,[0,1]),(b,[3,5])]
cluster_pairs(Xs0, Xs) :-
	pairs_to_bag(Xs0, _Bag, Bag, Order),
	bag_to_list(Order, Bag, Xs, []).

bag_to_list([], _Bag) --> [].
bag_to_list([Key|Keys], Bag) -->
	{ Xs0 = ~dic_get(Bag, Key) },
	{ Xs = ~reverse(Xs0) },
	[(Key, Xs)],
	bag_to_list(Keys, Bag).

pairs_to_bag([], Bag, Bag, []).
pairs_to_bag([X|Xs], Bag0, Bag, Order) :-
	pairs_to_bag__2(X, Bag0, Bag1, Order, Order0),
	pairs_to_bag(Xs, Bag1, Bag, Order0).

pairs_to_bag__2((Key, Value), Bag0, Bag, Order, Order0) :-
	bag_put(Bag0, Key, Value, Bag, Order, Order0).
pairs_to_bag__2(_, Bag, Bag, Order, Order).

bag_put(Bag0, Key, Value, Bag, Order, Order) :-
	Values = ~dic_get(Bag0, Key), !,
	Bag = ~dic_replace(Bag0, Key, [Value|Values]).
bag_put(Bag, Key, Value, Bag, [Key|Order], Order) :-
	dic_lookup(Bag, Key, [Value]).
