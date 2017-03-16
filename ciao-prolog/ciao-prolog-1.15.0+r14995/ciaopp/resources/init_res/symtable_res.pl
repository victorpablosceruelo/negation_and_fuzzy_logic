:- module(symtable_res,
	    [
		literal_property/10,
		insert_symbol_entry/3,
		insert_symbol_field/4,
		find_symbol_field/4,
		find_symbol_field_clause/3,
		find_symbol_entry/3,
		get_input_arglist_from_st/3,
		remove_literal_keys/2,
		current_symbol_resources/3
	    ], [assertions, resources(inferres_decl)]).

:- use_module(library(hiordlib)).
:- use_module(library(terms_vars),              [varset/2]).
:- use_module(library(lists),                   [select/3]).
:- use_module(resources(init_res(builtin_res)), [find_point_trusted_field/10]).
:- use_module(resources(size_res(clause_res)),  [ith_clause_literal/3]).
:- use_module(resources(resources_basic)).

:- doc(module,
"
This module contains the procedures for handling the symbol table.

The structure of the symbol table:
	st(Pred/Arity,clause,mode,measure,mutex,det,size,solution,time,domain)
").

:- pred insert_symbol_entry(ST, Pred, Entry) :: list(ST)
	: (predname(Pred)) + not_fails #
"Insert an entry for predicate Pred in the symbol table.  If it is
already in, return the entry as Entry; otherwise, an entry Entry for
Pred is inserted and returned.".

insert_symbol_entry(ST, Pred, Entry) :-
	var(ST),
	!,
	Entry = st(Pred, _, _, _, _, _, _, _, _, _),
	ST = [Entry|_].
insert_symbol_entry([E|ST], Pred, Entry) :-
	(
	    E = st(Pred, _, _, _, _, _, _, _, _, _) ->
	    Entry = E
	;
	    insert_symbol_entry(ST, Pred, Entry)
	).

insert_symbol_clause(ClauseList, Clause) :-
	var(ClauseList),
	!,
	ClauseList = [Clause|_].
insert_symbol_clause(ClauseList, Clause) :-
	nonvar(ClauseList),
	ClauseList = [C|CList],
	(
	    C == Clause ->
	    true
	;
	    insert_symbol_clause(CList, Clause)
	).

:- pred insert_symbol_field(ST, Pred, Type, Symbol) :: list(ST)
	: (predname(Pred), symbol_field(Type)) + not_fails.

insert_symbol_field(ST, Pred, Type, Symbol) :-
	insert_symbol_field_(Type, ST, Pred, Symbol).

%
%  Insert a clause for predicate Pred into the symbol table.
%

insert_symbol_field_(clause, ST, Pred, Clause) :-
	insert_symbol_entry(ST, Pred,
	    st(Pred, ClauseList, _, _, _, _, _, _, _, _)),
	insert_symbol_clause(ClauseList, Clause).


%
%  Insert a declaration for predicate Pred into the symbol table.
%

insert_symbol_field_((mode), ST, Pred, Mode) :-
	insert_symbol_entry(ST, Pred,
	    st(Pred, _, Mode, _, _, _, _, _, _, _)).
insert_symbol_field_(measure, ST, Pred, Measure) :-
	insert_symbol_entry(ST, Pred,
	    st(Pred, _, _, Measure, _, _, _, _, _, _)).
insert_symbol_field_(mutex, ST, Pred, Mutex) :-
	insert_symbol_entry(ST, Pred,
	    st(Pred, _, _, _, Mutex, _, _, _, _, _)).
insert_symbol_field_(det, ST, Pred, Det) :-
	insert_symbol_entry(ST, Pred,
	    st(Pred, _, _, _, _, Det, _, _, _, _)).
insert_symbol_field_(size, ST, Pred, Size) :-
	insert_symbol_entry(ST, Pred,
	    st(Pred, _, _, _, _, _, Size, _, _, _)).
insert_symbol_field_(relation, ST, Pred, Solution) :-
	insert_symbol_entry(ST, Pred,
	    st(Pred, _, _, _, _, _, _, Solution, _, _)).
% insert_symbol_field_(time, ST, Pred, Time) :-
% 	insert_symbol_entry(ST, Pred,
% 	    st(Pred, _, _, _, _, _, _, _, Time, _)).
insert_symbol_field_(domain, ST, Pred, Domain) :-
	insert_symbol_entry(ST, Pred,
	    st(Pred, _, _, _, _, _, _, _, _, Domain)).
insert_symbol_field_(resources(Resources), ST, Pred, [Costs]) :-
	insert_symbol_entry(ST, Pred,
	    st(Pred, _, _, _, _, _, _, _, ResourceList, _)),
	current_symbol_resources(Resources, Costs, ResourceList).

current_symbol_resources([],                   [],           _).
current_symbol_resources([Resource|Resources], [Cost|Costs], ResourceList0) :-
	select(res(Resource, Cost), ResourceList0, ResourceList),
	!,
	current_symbol_resources(Resources, Costs, ResourceList).

%
%  Find the entry for predicate Pred in the symbole table.
%  If it is in, return the entry as Entry;
%  otherwise, Entry is returned as a variable.
%

find_symbol_entry(ST, _, _) :-
	var(ST),
	!.
find_symbol_entry(ST, Pred, Entry) :-
	nonvar(ST),
	(
	    ST = [Entry|_],
	    Entry = st(Pred, _, _, _, _, _, _, _, _, _) ->
	    true
	;
	    ST = [E|S],
	    E = st(Pred1, _, _, _, _, _, _, _, _, _),
	    Pred \== Pred1,
	    find_symbol_entry(S, Pred, Entry)
	).

%
%  Find a field for predicate Pred in symbol table.
%

:- comp find_symbol_field_clause/3 + is_det.
% rtcheck -- EMM
find_symbol_field_clause(ST, Pred, ClauseList) :-
	find_symbol_field(ST, Pred, clause, ClauseKeyList),
	clause_list(ClauseKeyList, ClauseList).

:- push_prolog_flag(unused_pred_warnings, no).
:- use_module(program(clidtypes), [bodykey/1]).
:- pop_prolog_flag(unused_pred_warnings).

:- use_module(program(clidlist), [inverse_rewrite_source_body/2]).

:- comp clause_list/2 + is_det.
% rtcheck -- EMM
clause_list(ClauseKeyList, ClauseKeyList) :-
	var(ClauseKeyList),
	!.
clause_list([ClauseKey: _Key|ClauseKeyList], [Clause|ClauseList]) :-
	remove_literal_keys(ClauseKey, Clause),
	clause_list(ClauseKeyList, ClauseList).

remove_literal_keys((A :- B0), Clause) :-
	inverse_rewrite_source_body(B0, B),
	!,
	(B == true -> Clause = A ; Clause = (A :- B)).
remove_literal_keys((:- D), (:- D)) :- !.
remove_literal_keys(A,      A).

:- pred find_symbol_field/4 :: list(symbol_entry) * predname * symbol_field
	* term + not_fails.
find_symbol_field(ST, Pred, Type, Field) :-
	find_symbol_entry(ST, Pred, Entry),
	find_entry_field(Type, Entry, Pred, Field).

find_entry_field(clause, Entry, Pred, ClauseList) :-
	Entry = st(Pred, ClauseList, _, _, _, _, _, _, _, _).
find_entry_field((mode), Entry, Pred, Mode) :-
	Entry = st(Pred, _, Mode, _, _, _, _, _, _, _).
find_entry_field(measure, Entry, Pred, Measure) :-
	Entry = st(Pred, _, _, Measure, _, _, _, _, _, _).
find_entry_field(mutex, Entry, Pred, Mutex) :-
	Entry = st(Pred, _, _, _, Mutex, _, _, _, _, _).
find_entry_field(det, Entry, Pred, Det) :-
	Entry = st(Pred, _, _, _, _, Det, _, _, _, _).
find_entry_field(size, Entry, Pred, Size) :-
	Entry = st(Pred, _, _, _, _, _, Size, _, _, _).
find_entry_field(relation, Entry, Pred, Solution) :-
	Entry = st(Pred, _, _, _, _, _, _, Solution, _, _).
% find_entry_field(time, Entry, Pred, Time) :-
% 	Entry = st(Pred, _, _, _, _, _, _, _, Time, _).
find_entry_field(resources(Resources), Entry, Pred, [Costs]) :-
% 	Time and resource are the same thing
	Entry = st(Pred, _, _, _, _, _, _, _, ResourceList, _),
	current_symbol_resources(Resources, Costs, ResourceList).
find_entry_field(domain, Entry, Pred, Domain) :-
	Entry = st(Pred, _, _, _, _, _, _, _, _, Domain).
find_entry_field(resource(Resource), Entry, Pred, [Cost]) :-
	Entry = st(Pred, _, _, _, _, _, _, _, ResourceList, _),
	member(res(Resource, Cost), ResourceList),
	!.

next_lit_ppkey(LitNum, ClausePPKey, Key0, Key) :-
	LitNum1 is LitNum + 1,
	ith_clause_literal(LitNum1, ClausePPKey, LitPPKey) ->
	lit_ppkey(LitPPKey, _, PPKey),
	(
	    PPKey = noinfo ->
	    next_lit_ppkey(LitNum1, ClausePPKey, Key0, Key)
	;
	    Key = PPKey
	)
    ;
	Key0 = Key.

:- pred literal_property/10 :: list(bottom_entry) * list(symbol_entry)
	* callable * clause_ppkey_t * atm * bodykey * nnegint * symbol_field *
	approx * term # "Get a property of a literal from symbol table.".
literal_property(BT, ST, Lit, ClausePPKey, Key, PPKey, LitNum, PropName,
	    Approx, Prop) :-
	functor(Lit, F, A),
	LitName = F/A,
%	clause_key(ClauseKey, ClausePPKey, Key),
% 	clause_head_body(ClausePPKey, Head, Body),
% 	source_clause(_Key, clause(Head, Body), dic(Vars, _)),
% 	clause_body(ClausePPKey, Body),
	next_lit_ppkey(LitNum, ClausePPKey, Key, PPKey1),
	varset(ClausePPKey, Vars),
	find_point_trusted_field(PropName, BT, ST, Lit, Vars, PPKey, PPKey1,
	    LitName, Approx, Prop0),
	(
	    ground(Prop0) ->
	    Prop = Prop0
	;
	    find_symbol_field(ST, LitName, PropName, Prop1),
	    combine_results(PropName, Prop0, Prop1),
	    (
		ground(Prop0) ->
		Prop = Prop0
	    ;
		insert_element(bt(LitName, Approx, PropName, BTE), BT),
% 		**** here there are problems with compound_resources -- EMM ***
		bottom(PropName, Approx, BTE, Key-LitName, Prop0, Prop)
	    )
	).

get_input_arglist_from_st(ST, Pred, InArgList) :-
	find_symbol_field(ST, Pred, mode, Modes),
	create_input_arglist(Modes, 1, InArgList).

create_input_arglist([],           _,      []) :- !.
create_input_arglist([Mode|Modes], ArgNum, AList0) :-
	NewArgNum is ArgNum + 1,
	add_if_input(Mode, ArgNum, AList0, AList),
	create_input_arglist(Modes, NewArgNum, AList).

add_if_input(+, ArgNum, [ArgNum|AList], AList) :- !.
add_if_input(_, _,      AList,          AList).

:- use_module(resources(res_assrt_defs(resources_lib)), [trust_default/3]).

bottom(resources(Resources), Ap, BTE, _, [TLitComp], [TLitComp]) :-
	map(Resources, bottom_res_each(Ap, BTE), TLitComp).
bottom(resource(Resource), Ap, BTE, _, Comp, Comp) :-
	bottom_res_each(Resource, Ap, BTE, Comp).
bottom(det,      Ap, _,   _, _, [Det]) :- approx_bottom(Ap, Det).
bottom(relation, Ap, _,   _, _, Rel) :- approx_bottom(Ap, Rel).
bottom(size,     Ap, BTE, P, C, C) :-
	approx_bottom(Ap, Bot),
	bottom_size(C, P, 1, BTE, Bot).

bottom_size([],     _, _,  _,   _).
bottom_size([S|Ss], P, N0, BTE, Bot) :-
	bottom_size_each(S, P, N0, BTE, Bot),
	N is N0 + 1,
	bottom_size(Ss, P, N, BTE, Bot).

bottom_size_each(S, C-L, N0, BTE, Bot) :-
	(
	    var(S) ->
	    S = Bot,
	    insert_element(bs(C, L, N0), BTE)
	;
	    true
	).

% By defult, the number of solutions is inf
bottom_res_each(Resource, Approx, BTE, Comp) :-
	var(Comp) ->
	(
	    trust_default(Approx, Resource, Comp) ->
	    PreType = default
	;
	    approx_bottom(Approx, Comp),
	    PreType = top
	),
	insert_element(be(Resource, PreType, Comp), BTE)
    ;
	true.
