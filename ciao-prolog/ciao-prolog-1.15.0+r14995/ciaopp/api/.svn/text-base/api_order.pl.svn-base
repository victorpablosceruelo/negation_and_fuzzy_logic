:- module(api_order,
	    [% CLEAN
		cleanup_order/0,
% PREDICATES
		pr_order_clean/0,
		pr_order_set/1,
		pr_order_get/1,
		pr_order_add/2,
		pr_order_erase/1,
% CLAUSES
		cl_order_get/2,
		cl_order_set/2,
		cl_order_erase/2,
		cl_order_add/2,
		cl_order_update/1,
		keep_order/3],
	    [assertions, regtypes]).

%:- use_module(library(aggregates)).
:- use_module(library(lists), [append/3]).
%:- use_module(library(terms), [atom_concat/2]).

:- use_module(program(itf_db), [curr_file/2]).

:- use_module(library(ddlist)).
:- use_module(ciaopp(api(api_internal_types))).
:- use_module(ciaopp(api(api_base))).
:- use_module(ciaopp(api(api_predcl))).

:- use_package(.(api_internal_dec)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   CLEAN UP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cleanup_order :-
	retractall_fact(clause_order(_, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   PREDICATE ORDER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- data pr_order/1.

:- pred pr_order_clean
# "Removes all information about clauses order.".

pr_order_clean :-
	retractall_fact(pr_order(_)),
	asserta_fact(pr_order([])).

:- pred pr_order_set(X) : list(X, t_cls_ppi_id)
# "Set list @var{X} as the clause order.".

pr_order_set(X) :-
	retractall_fact(pr_order(_)),
	asserta_fact(pr_order(X)).

:- pred pr_order_get(X) : var(X) => list(X, t_cls_ppi_id)
# "Returns in @var{X} the clause key order.".

pr_order_get(X) :-
	current_fact(pr_order(X)),
	!.
pr_order_get([]).

:- pred pr_order_add(K, NK) : (t_cls_ppi_id(K), t_cls_ppi_id(NK))
# "Add @var{NK} after @var{K} key.".

:- pred pr_order_add(K, NK) : (atom(K), t_cls_ppi_id(NK))
# "If @var{K} is 'begin' or 'end', @var{NK} key is set the first or
last in the clause order.".

pr_order_add(begin, Key) :-
	!,
	retract_fact(pr_order(X)),
	(member_and_remove(Key, X, XC) -> true ; XC = X),
	asserta_fact(pr_order([Key|XC])).
pr_order_add(end, Key) :-
	!,
	retract_fact(pr_order(X)),
	(member_and_remove(Key, X, XC) -> true ; XC = X),
	append(XC, [Key], KeyX),
	asserta_fact(pr_order(KeyX)).
pr_order_add(AfterThisKey, Key) :-
	retract_fact(pr_order(X)),
	(member_and_remove(Key, X, XC) -> true ; XC = X),
	create_from_list(XC, DDL),
	pr_order_add_internal(DDL, AfterThisKey, Key, NDDL),
	to_list(NDDL, NewOrder),
	asserta_fact(pr_order(NewOrder)).

:- push_prolog_flag(multi_arity_warnings, off).

:- pred pr_order_add(K, NK, DDL, NDDL)
	: (t_cls_ppi_id(K), t_cls_ppi_id(NK), ddlist(DDL))
	=> ddlist(NDDL)
# "Add @var{NK} in @var{DDL} after @var{K} key. @var{NDDL} is the
  result".

:- pred pr_order_add(A, NK, DDL, NDDL)
	: (atom(A), t_cls_ppi_id(NK), ddlist(DDL))
	=> ddlist(NDDL)
# "If @var{A} is 'begin' or 'end', @var{NK} key is set the first or
last in the clause order.".

pr_order_add(begin, Key, DDL, NDDL) :-
	insert_begin(DDL, Key, NDDL).
pr_order_add(end, Key, DDL, NDDL) :-
	insert_end(DDL, Key, NDDL).
pr_order_add(after(AKey), Key, DDL, NDDL2) :-
	( pr_order_put_this_key_on_top(DDL, AKey, NDDL1)
	->
	    true
	;
	    error_message(
		"INTERNAL_ERROR: pr_order_add: Key ~w does not belong " ||
		"to the predicate order list",
		[AKey]),
	    NDDL1 = DDL
	),
	insert_after(NDDL1, Key, NDDL2).
pr_order_add(before(AKey), Key, DDL, NDDL2) :-
	( pr_order_put_this_key_on_top(DDL, AKey, NDDL1)
	->
	    true
	;
	    error_message(
		"INTERNAL_ERROR: pr_order_add: Key ~w does not belong " ||
		"to the predicate order list",
		[AKey]),
	    NDDL1 = DDL
	),
	insert(NDDL1, Key, NDDL2).
pr_order_add(AfterThisKey, Key, DDL, NDDL) :-
	pr_order_add_internal(DDL, AfterThisKey, Key, NDDL).

% 	error_message( "INTERNAL ERROR: update_predicate_order: " ||
% 		       "Found not recogniced ~w when adding clause.", [Where] ).



% :- pred pr_order_add_implicit(K, DDL, NDDL)
% 	: (t_cls_ppi_id(K), ddlist(DDL))
% 	=> ddlist(NDDL)
% # "Add @var{K} before the current pointer of @var{DDL}.".

% :- pred pr_order_add_implicit(A, DDL, NDDL)
% 	: (ddlist(DDL))
% 	=> ddlist(NDDL)
% # "@var{A} has to be 'begin(K)' or 'end(K)', where K is of the type
%   t_cls_ppi_id. K is added at the beginning or end of @var{DDL}
%   wihtout modifiying the current pointer.".

% pr_order_add_implicit(begin(K), O, NO) :-
% 	!,
% 	insert_begin(O, K, NO).
% pr_order_add_implicit(end(K), O, NO) :-
% 	!,
% 	insert_end(O, K, NO).
% pr_order_add_implicit(K, O, NO) :-
% 	insert(O, K, NO).


:- pred pr_order_erase(K)
	: (t_cls_ppi_id(K))
# "Erase @var{K} from the program order list.".

pr_order_erase(Key) :-
	retract_fact(pr_order(X)),
	create_from_list(X, DDL),
	ddlist:remove_all_elements(DDL, Key, NDDL),
	to_list(NDDL, NewOrder),
	asserta_fact(pr_order(NewOrder)).

/*
:- pred pr_order_erase(K, DDL, NDLL)
	: (t_cls_ppi_id(K), ddlist(DDL))
	=> ddlist(NDLL)
# "Erase @var{K} from @var{DDL}.".

pr_order_erase(Key, DDL, NDDL) :-
	ddlist:remove_all_elements(DDL, Key, NDDL).
*/

:- pop_prolog_flag(multi_arity_warnings).

:- pred pr_order_put_this_key_on_top(DDL, K, NDDL)
	: (ddlist(DDL), t_cls_ppi_id(K))
	=> ddlist(NDDL)
# "Advance or get back @var{DDL} pointer till @var{K} is on
  top. Result is @var{NDDL}. Fails if @var{k} does not belong to
  @var{DDL}.".

pr_order_put_this_key_on_top(DDL, Key, NDDL) :-
	pr_order_advance_till_key(DDL, Key, NDDL),
	top(NDDL, Key),
	!.
pr_order_put_this_key_on_top(DDL, Key, NDDL) :-
	pr_order_go_back_till_key(DDL, Key, NDDL),
	top(NDDL, Key).

:- pred pr_order_advance_till_key(DDL, K, NDDL)
	: (ddlist(DDL), t_cls_ppi_id(K))
	=> ddlist(NDDL)
# "Advance @var{DDL} pointer till find @var{K} key. Result is
  @var{NDDL}.".

pr_order_advance_till_key(DDL, Key, DDL) :-
	top(DDL, Key),
	!.
pr_order_advance_till_key(DDL, Key, NNDDL) :-
	next(DDL, NDDL),
	!,
	pr_order_advance_till_key(NDDL, Key, NNDDL).
pr_order_advance_till_key(DDL, _, DDL).

:- pred pr_order_go_back_till_key(DDL, K, NDDL)
	: (ddlist(DDL), t_cls_ppi_id(K))
	=> ddlist(NDDL)
# "Advance @var{DDL} pointer till find @var{K} key. Result is
  @var{NDDL}.".

pr_order_go_back_till_key(DDL, Key, DDL) :-
	top(DDL, Key),
	!.
pr_order_go_back_till_key(DDL, Key, NNDDL) :-
	prev(DDL, NDDL),
	!,
	pr_order_advance_till_key(NDDL, Key, NNDDL).
pr_order_go_back_till_key(DDL, _, DDL).

:- pred pr_order_add_internal(DDL, AfterThis, K, NDDL)
	: (ddlist(DDL), t_cls_ppi_id(AfterThis), t_cls_ppi_id(K))
	=> ddlist(NDDL)
# "Inserts the @var{K} in @var{DDL} after @var{AftherThis} key. If
  @var{AfterThis} key is not found, then it is inserted at the
  end. Note that the @var{DDL} pointer is modified pointing at
  @var{AfterThis}".

pr_order_add_internal(DDL, AfterThisKey, Key, NDDL) :-
	pr_order_advance_till_key(DDL, AfterThisKey, ADDL),
	insert_after(ADDL, Key, NDDL).

/*
:- pred pr_order_pass_same_clause_key(DDL, ClauseK, NDDL)
	: (ddlist(DDL), t_cls_ppi_id(AfterThis))
	=> ddlist(NDDL)

# "Put @var{DDL} pointer pointing at last clause of the same predicate
  of @var{ClauseK}. If no @var{ClauseKey} is found, it points at the
  end.".

% We cannot use last_clause/1 from p_unit, but I am not sure wether
% it is a good idea or not. Imagine we delete the last clause => pointer
% will run till the end.
pr_order_pass_same_clause_key(DDL, ClKey, NDDL) :-
	clid2data(ClKey, F, A, _),
	pr_order_pass_same_clause_key_1(DDL, F, A, NDDL).

% Move till we find the 1st clause Key that match
pr_order_pass_same_clause_key_1(DDL, F, A, NDDL) :-
	top(DDL, Top),
	clid2data(Top, F, A, _),
	pr_order_pass_same_clause_key_2(DDL, F, A, NDDL),
	!.
pr_order_pass_same_clause_key_1(DDL, F, A, NDDL) :-
	next(DDL, Next),
	!,
	pr_order_pass_same_clause_key_1(Next, F, A, NDDL).
pr_order_pass_same_clause_key_1(DDL, _, _, DDL).

% Move WHILE we read the same clause key
pr_order_pass_same_clause_key_2(DDL, F, A, NDDL) :-
	(
	    next(DDL, Next),
	    top(Next, Top),
	    clid2data(Top, F, A, _),
	    pr_order_pass_same_clause_key_2(Next, F, A, NDDL)
	->
	    true
	;
	    NDDL = DDL
	).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   CLAUSE ORDER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- data clause_order/2.

:- doc(clause_order/2, "clause_order( t_cls_key , list(t_lit_ppi_id) )").

:- pred cl_order_set(PredKey, ClsKeys)
	: (t_pred_ppi_id(PredKey), list(ClsKeys, t_cls_ppi_id))
# "Assetrs the list of clause keys (@var{ClsKeys}) for a given
  @var{PredKey} vapredicate key".

cl_order_set(P, PO) :-
	retractall_fact(clause_order(P, _)),
	assertz_fact(clause_order(P, PO)).

:- pred cl_order_add(Key, ClsID)
	: (term(Key), t_cls_ppi_id(ClsID))

# "Add clause program point info @var{ClsID} to predicate @var{Key}".

cl_order_add(Key, ClsID) :-
	(retract_fact(clause_order(Key, Cl)) -> true ; Cl = []),
	!,
	append(Cl, [ClsID], NewCls),
	assertz_fact(clause_order(Key, NewCls)).
cl_order_add(Key, ClsID) :-
	error_message("cl_order_add: invalid key ~p (2nd arg: ~p).",
	    [Key, ClsID]).

:- pred cl_order_erase(Key, ClsID)
	: (term(Key), t_cls_ppi_id(ClsID))

# "Remove clause program point info @var{ClsID} from predicate
  @var{Key}. If it is the last clause, the predicate that contains the
  clause is also removed from order list.".

cl_order_erase(Key, ClsID) :-
	(retract_fact(clause_order(Key, Cl)) -> true ; Cl = []),
	!,
	remove(ClsID, Cl, NewCls),
% Note, the locater and clause has to be removed by something else!
	(
	    NewCls = []
	->
	    pr_order_erase(Key)
	;
	    assertz_fact(clause_order(Key, NewCls))
	).
cl_order_erase(Key, ClsID) :-
	error_message("cl_order_erase: invalid key ~p (2nd arg: ~p).",
	    [Key, ClsID]).

:- pred cl_order_get(PredKey, ClsKeys)
	: (term(PredKey), var(ClsKeys))
	=> list(ClsKeys, t_cls_ppi_id)
# "Return in @var{ClsKeys} the list of clause keys of @var{PredKey}
  vapredicate key".

cl_order_get(P, ClausesKeys) :-
	current_fact(clause_order(P, ClausesKeys)),
	!.
cl_order_get(P, _) :-
	error_message(
	    "cl_order_get: It seems predicate ~p does not exist.",
	    [P]),
	fail.

:- pred cl_order_get_internal(Cls, PO)
	: (t_cls(Cls), var(PO))
%        => list( ClsKeys , t_cls_ppi_id )

# "INTERNAL USE ONLY. Return the clause order in @var{PO} for a given
  clause @var{Cls}. This is used only in @pred{keep_order/2}
  predicate".

cl_order_get_internal(Cls, po(Key, ClausesKeys, _)) :-
	Cls = cls${key => Key},
	nonvar(Key),
	current_fact(clause_order(Key, ClausesKeys)),
	!.
cl_order_get_internal(Cls, po(Key, [ID], _)) :-
	Cls = cls${id => ID, key => Key}.

:- pred cl_order_update(IncrementalClOrder)
	: list(IncrementalClOrder)

# "@var{IncrementalClOrder} is a list of terms po(Key,List,Where),
  where Key is the Predicate Key (example: qsort(_._)), and @var{List}
  is the list of clauses id (example qsort/2/1, qsort/2/2). Where is
  used to specify a relative order, look at where_order type for more
  information.".

cl_order_update(IncrementalClOrder) :-
	pr_order_get(CurrentClOrder),
	create_from_list(CurrentClOrder, DDL_Order),
	assert_new_clause_order(IncrementalClOrder, DDL_Order, NewOrder),
	to_list(NewOrder, NList),
	asserta_fact(pr_order(NList)).

assert_new_clause_order([], PredOrder, PredOrder).
% CASE 0: The predicate has already 1 clause, so it in the pr order list!
assert_new_clause_order([po(Key, List, _Where)|Ps], PO, NPO2) :-
	retract_fact(clause_order(Key, _)),
	!,
% Should Where overwrite the current position of the precate
% if different??
	asserta_fact(clause_order(Key, List)),
	assert_new_clause_order(Ps, PO, NPO2).
% CASE 1: The predicate is absolutely new!
assert_new_clause_order([po(Key, List, Where)|Ps], PO, NPO2) :-
	!,
	(curr_file(_, Mod) -> add_defined_pred(Key, Mod) ; true),
	(var(Where) ->        Where=end ;                  true),
	pr_order_add(Where, Key, PO, NPO1),
	asserta_fact(clause_order(Key, List)),
	assert_new_clause_order(Ps, NPO1, NPO2).

:- pred keep_order(OrderModifier, CurrentOrder, NewOrder)
	: (t_acls(OrderModifier), list(CurrentOrder, order_element))
	=> list(CurrentOrder, order_element)

# "Modifies the list @var{CurrentOrder} according to
  @var{OrderModifier} and return the result in @var{NewOrder}".

keep_order(a(Where, Cls), CurrentOrder, NewOrder) :-
	Cls = cls${key => Key, id => ID},
	(
	    member_and_remove(po(Key, ClOrder, Expected), CurrentOrder, NO)
	->
	    (
		Where = Expected
	    ->
		true
	    ;
% --- ONLY whether possible error flag is active!!!
		error_message(
"The order of the clause ~p (~p) is not the same with
                  respect to ~w (~w)",
		    [Cls, Where, Key, Expected])
	    )
	;
	    cl_order_get_internal(Cls, po(Key, ClOrder, _)),
	    NO = CurrentOrder
	),
	append_if_not_member(ClOrder, ID, ClOrder_K),
	NewOrder = [po(Key, ClOrder_K, Where)|NO].
keep_order(e(Cls), CurrentOrder, NewOrder) :-
	Cls = cls${key => Key, id => ID},
	(
	    member_and_remove(po(Key, ClOrder, Where), CurrentOrder, NO)
	->
	    true
	;
	    cl_order_get_internal(Cls, po(Key, ClOrder, Where)),
	    NO = CurrentOrder
	),
	remove(ID, ClOrder, ClOrder_K),
	NewOrder = [po(Key, ClOrder_K, Where)|NO].
keep_order(u(Cls), CurrentOrder, NO2) :-
	Cls = cls${id => ID},
	keep_order(u(ID, Cls), CurrentOrder, NO2).
keep_order(a(Cls), CurrentOrder, NO2) :-
	keep_order(a(_, Cls), CurrentOrder, NO2).
keep_order(u(ID, Cls), CurrentOrder, NO2) :-
	ClsOrig = cls${id => ID, key => Key},
	Cls = cls${key => RenKey},
	get_clause(ClsOrig),
%	(display( cmp( RenKey , Key ) ),nl,nl; display( puto_error ) , nl,fail),
	(
	    RenKey = Key
	->
% The head has no changed, neither the order so
	    CurrentOrder = NO2
	;
	    ( member(po(RenKey, _, ID_Expected), CurrentOrder),
		nonvar(ID_Expected)
	    -> Key2 = ID_Expected ; Key2 = after(Key) ),
%	    display( erasing( ID ) ),nl,
%	    display( adding( after( Key2 ) , RenKey ) ),nl,nl,
	    keep_order(e(ClsOrig),   CurrentOrder, NO1),
	    keep_order(a(Key2, Cls), NO1,          NO2)
	).

:- regtype where_order/1
# "This type is used to specify the relative order of a clause or a
  predicate. The possible values can be:
@enumerate
 @item begin Insert at the begining of the list
 @item end   Insert at the end of the list
 @item after(K) Insert just after the key K
 @item before(K) Insert just before the key K".

where_order(begin).
where_order(end).
where_order(after(AKey)) :- t_cls_key(AKey).
where_order(before(AKey)) :- t_cls_key(AKey).

:- regtype order_element/1
# "This is is used to store the order modifiers. It is internal to
  this module only.".

order_element(po(Key, List, Where)) :-
	t_cls_key(Key),
	list(List, t_lit_ppi_id),
	where_order(Where).
