:- module(p_canonical, [
		to_simple_assrt/0,
		module_to_simple_assrt/1,
		current_module_to_simple_assrt/0,
		compound_to_simple_assrt/2,
		compound_to_simple_assrt_same_pred/2
	    ], [api(ciaopp_api), assertions]).

:- use_module(library(assertions(assertions_props)), [assrt_status/1]).

:- use_module(library(aggregates), [findall/3]).


% to_simple_assrt.

% --- in the future
% low_level_to_api( L , L ).

to_simple_assrt :-
	module_to_simple_assrt(_).

current_module_to_simple_assrt :-
	get_key_predicates([internal], P),
	transform_assertions_key(P).

module_to_simple_assrt(M) :-
	get_all_assertions_key_of_module(M, L),
	transform_assertions_key(L).

transform_assertions_key(L) :-
	member(Key, L),
	get_assertions(Key, as${type => pred}, true, L),
	compound_to_simple_assrt_same_pred__(L, CannonAs, []),
	process_simple_assrt(L, CannonAs),
	fail.
transform_assertions_key(_).

process_simple_assrt(L, CannonAs) :-
	erase_assertions(L),
	add_assertions(CannonAs),
	add_commented_assertions(L).

compound_to_simple_assrt(L, CannonAs) :-
	compound_to_simple_assrt__(L, CannonAs, []),
	!.

compound_to_simple_assrt__([], A,   A).
compound_to_simple_assrt__(L,  NAs, TTNAs) :-
% Get predicate assertions from the same pred
	gather_pred_assertions_with_same_head(L, As, L_left),
	get_only_pred_assertions(As, PredAs, NAs, As_Tleft),
	compound_to_simple_assrt_same_pred__(PredAs, As_Tleft, TNAs),
	compound_to_simple_assrt__(L_left, TNAs, TTNAs).

compound_to_simple_assrt_same_pred(A, NA) :-
	get_only_pred_assertions(A, Pred, NA, NA1),
	compound_to_simple_assrt_same_pred__(Pred, NA1, []).

compound_to_simple_assrt_same_pred__([], A,     A) :- !.
compound_to_simple_assrt_same_pred__(As, Calls, T) :-
%	join_pred_assertions( As , Call ),
	separate_and_join_assertions(As, Calls, L),
	do_success_comp(As, L, T).

gather_pred_assertions_with_same_head([],     [],     []).
gather_pred_assertions_with_same_head([L|Ls], [L|As], L_left) :-
	L = as${head => H},
	gather_pred_assertions_with_same_head__(Ls, H, As, L_left).

get_only_pred_assertions([],     [],          B, B).
get_only_pred_assertions([A|As], [A|RPredAs], B, TB) :-
	A = as${type => pred},
	!,
	get_only_pred_assertions(As, RPredAs, B, TB).
get_only_pred_assertions([A|As], PredAs, [A|B], TB) :-
	get_only_pred_assertions(As, PredAs, B, TB).

gather_pred_assertions_with_same_head__([],     _, [],     []).
gather_pred_assertions_with_same_head__([L|Ls], H, [L|As], L_left) :-
	L = as${head => H2},
	\+ \+ H = H2,
	!,
	gather_pred_assertions_with_same_head__(Ls, H, As, L_left).
gather_pred_assertions_with_same_head__([L|Ls], H, As, [L|L_left]) :-
	gather_pred_assertions_with_same_head__(Ls, H, As, L_left).

do_success_comp([],    A, A).
do_success_comp([A|B], L, TailOut) :-
	pred_to_success(A, L, L1),
	pred_to_comp(A, L1, L2),
	do_success_comp(B, L2, TailOut).

pred_to_success(As, Suc, Suc__) :-
	As = as${
	    status => Status,
	    call => Call,
	    succ => Succ
	},
	(
	    Succ == []
	->
	    Suc = Suc__
	;
	    decide_status(Status, success(SStatus)),
	    SAs = as${
		status => SStatus,
		type => success,
		call => Call,
		compat => [],
		succ => Succ,
		comp => []
	    },
	    copy_the_rest(As, SAs),
	    Suc = [SAs|Suc__]
	).

pred_to_comp(As, Suc, Suc__) :-
	As = as${
	    status => Status,
	    call => Call,
	    comp => Comp
	},
	(
	    Comp == []
	->
	    Suc = Suc__
	;
	    decide_status(Status, comp(CoStatus)),
	    CAs = as${
		status => CoStatus,
		type => comp,
		call => Call,
		compat => [],
		succ => [],
		comp => Comp},
	    copy_the_rest(As, CAs),
	    Suc = [CAs|Suc__]
	).

copy_the_rest(A, B) :-
	A = as${locator => L, comment => C, fromwhere => F,
	    head => H, dic => D},
	B = as${locator => L, comment => C, fromwhere => F,
	    head => H, dic => D}.

separate_and_join_assertions(L, A, TA) :-
	findall(S, assrt_status(S), Status),
	separate_and_join_assertions__(Status, L, A, TA).

separate_and_join_assertions__([],     _, A,    A).
separate_and_join_assertions__([S|Ss], L, Call, TA) :-
	gather_pred_assertions_of_same_status(L, S, LS),
	(
	    LS == []
	->
	    Call = TailA
	;
	    join_pred_assertions(LS, A),
	    (
		A = as${call => []}
	    ->
		Call = TailA
	    ;
		Call = [A|TailA]
	    )
	),
	separate_and_join_assertions__(Ss, L, TailA, TA),
	!.
separate_and_join_assertions__([S|Ss], L, A, TA) :-
	error_message("INTERNAL ERROR: When gathering pred assertions"||
	    " of status ~w: ~p. Skiping...", [S, L]),
	separate_and_join_assertions__(Ss, L, A, TA).

gather_pred_assertions_of_same_status([],     _Status, []).
gather_pred_assertions_of_same_status([A|As], Status,  [A|Ls]) :-
	A = as${status => Status},
	!,
	gather_pred_assertions_of_same_status(As, Status, Ls).
gather_pred_assertions_of_same_status([_|As], Status, Ls) :-
	gather_pred_assertions_of_same_status(As, Status, Ls).

:- pred join_pred_assertions(L, A) : (list(L, t_as), var(A)) => (t_as(A))
# "Given a list of assertions, @var{L}, a calls assertions is returned
  on @var{A}. All list 'pred' assertions member of the list have to
  have the same status".

join_pred_assertions(L, A2) :-
	L = [LA|_],
	LA = as${
	    status => Status
	},
	decide_status(Status, calls(PredStatus)),
	A = as${
	    status => PredStatus,
	    type => calls,
	    call => Calls,
	    succ => [],
	    compat => [],
	    comp => [],
	    comment => [],
	    fromwhere => read
	},
	join_pred_assertions__(L, A),
	$~(A, as${call => Calls2}, A2),
	list_of_list_to_calls_body(Calls, Calls2).

list_of_list_to_calls_body([[]], []).
list_of_list_to_calls_body([A],  A) :-
	list(A),
	!.
list_of_list_to_calls_body(A, [B]) :-
	list_of_list_to_calls_body__(A, B).

list_of_list_to_calls_body__([A],    A) :- !.
list_of_list_to_calls_body__([A|As], (A;Bs)) :-
	list_of_list_to_calls_body__(As, Bs).

join_pred_assertions__([A], As) :-
	!,
	As = as${
	    head => Head,
	    call => [Calls],
	    dic => Dic,
	    locator => Loc
	},
	A = as${
	    head => Head,
	    call => Calls,
	    dic => Dic,
	    locator => Loc
	}.

join_pred_assertions__([B|Bs], As) :-
	join_pred_assertions__(Bs, AsR),
	B = as${
	    head => B_Head,
	    call => B_Calls,
	    dic => B_Dic,
	    locator => B_Loc
	},
	AsR= as${
	    head => As_Head,
	    call => As_Calls,
	    dic => As_Dic,
	    locator => As_Loc
	},
	As = as${
	    head => A_Head,
	    call => A_Calls,
	    dic => A_Dic,
	    locator => A_Loc
	},

	copy_term((B_Head, B_Dic, As_Head, As_Dic, B_Calls, As_Calls),
	    (A_Head, B_Dic2, A_Head, As_Dic2, B_Calls2, As_Calls2)),

	add_if_different(B_Calls2, As_Calls2, A_Calls),
	join_loc(B_Loc, As_Loc, A_Loc),
	join_dic(B_Dic2, As_Dic2, A_Dic).

add_if_different(A, L, L) :-
	member(AA, L),
	AA == A,
	!.
add_if_different(A, L, [A|L]).

join_loc(loc${module => M,
		file => F,
		line_begin=> ALB,
		line_end => ALE
	    },
	    loc${line_begin=> BLB,
		line_end => BLE
	    },
	    loc${module => M,
		file => F,
		line_begin=> CLB,
		line_end => CLE
	    }) :-
	min(ALB, BLB, CLB),
	max(ALE, BLE, CLE).

join_dic([],     A, A).
join_dic([B|Bs], A, AS) :-
	join_dic(Bs, A, AT),
	(
	    is_in_dic(AT, B)
	->
	    AS = AT
	;
	    AS = [B|AT]
	).

is_in_dic([A|As], B) :-
	A = (_=VA),
	B = (_=VB),
	(
	    VB == VA
	->
	    true
	;
	    is_in_dic(As, B)
	).

max(X, Y, X) :-
	X > Y,
	!.
max(_X, Y, Y).

min(X, Y, X) :-
	X < Y,
	!.
min(_X, Y, Y).

:- pred decide_status(S, PredS) :: assrt_status * term
# "For a given status @var{S} from a pred assertions, the status of
  calls and success are returned on @var{PredS}.".

decide_status(true, calls(true)) :- !.
decide_status(true, success(true)) :- !.
decide_status(true, comp(true)) :- !.

decide_status(trust, calls(check)) :- !.
decide_status(trust, success(trust)) :- !.
decide_status(trust, comp(trust)) :- !.

decide_status(check, calls(check)) :- !.
decide_status(check, success(check)) :- !.
decide_status(check, comp(check)) :- !.

decide_status(X, calls(X)) :- member(X, [false, checked]), !.
decide_status(X, success(X)) :- member(X, [false, checked]), !.
decide_status(X, comp(X)) :- member(X, [false, checked]), !.

decide_status(X, Y) :-
	error_message("decide_status: no entry for: ~w (output: ~w)", [X, Y]),
	fail.
