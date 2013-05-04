:- module(time_basic_res,
	    [
		wamcode_literal/4,
		wamcode_size/3
	    ], [assertions]).

:- doc(author, "Edison Mera").

:- doc(module, "This module contains additional operations to
   measure the size of a literal following several models.").

:- use_module(library(lists)).

:- test wamcode_size(Clause, WamCode, Size) :
	(
	    Clause = ('fib:fib'(0, 0) :- !),
	    WamCode = [get_constant_x0(0), get_constant(0, 1), cutb, neck(2),
		proceed]
	) =>
	(
	    Size = [0, 5]
	) # "Applying to fib/2".

:- test wamcode_size(Clause, WamCode, Size) :
	(
	    Clause = ( 'append:append'([H|X], Y, [H|Z]) :-
		'append:append'(X, Y, Z) ),
	    WamCode = [ifshallow,
		get_list_x0,
		unify_x_variable(4),
		unify_x_variable(3),
		get_list(2),
		unify_x_value(4),
		neck(3),
		unify_x_variable(2),
		put_x_value(3, 0),
		execute('append:append'/3),
		else,
		get_list_x0,
		unify_x_variable(3),
		unify_x_variable(0),
		get_list(2),
		unify_x_value(3),
		unify_x_variable(2),
		execute('append:append'/3),
		endif]
	) =>
	(
	    Size = [0, 9]
	) # "Applying to append/3".

:- test wamcode_size(Clause, WamCode, Size) :
	(
	    Clause = ( 'hanoi:hanoi'(N, A, B, C, M) :-
		N1 is N - 1,
		'hanoi:hanoi'(N1, A, C, B, M1),
		'hanoi:hanoi'(N1, B, A, C, M2),
		'hanoi:append'(M1, [mv(A, C)], T),
		'hanoi:append'(T, M2, M) ),
	    WamCode = [ifshallow, neck(5), else, endif, allocate,
		get_y_variable(2, 4), get_y_variable(5, 3), get_y_variable(7, 2
		), get_y_variable(4, 1), init([0, 1, 3, 6]), true(8),
		function_1(
		    2, 5, 0, 0, 4), get_y_first_value(6, 5), put_y_value(6, 0),
		put_x_value(
		    3, 2), put_y_value(7, 3), put_y_value(3, 4), call(/(
			'hanoi:hanoi', 5), 8), put_y_value(6, 0), put_y_value(
		    7, 1), put_y_value(4, 2), put_y_value(5, 3), put_y_value(1,
		    4), call(/('hanoi:hanoi', 5), 6), put_structure(/(mv, 2), 0
		), unify_y_local_value(4), unify_y_local_value(5), put_list(1),
		unify_x_value(
		    0), unify_nil, put_y_unsafe_value(3, 0), put_y_value(0, 2),
		call(
		    /('hanoi:append', 3), 3), put_y_unsafe_value(0, 0),
		put_y_unsafe_value(
		    1, 1), put_y_value(2, 2), deallocate, execute(/(
			'hanoi:append', 3))]
	) =>
	(
	    Size = [0, 0, 15, 6, 9, 5]
	) # "Applying to hanoi/5".

wamcode_clause((_H :- B), WamCode, [[]|WamCodeLits]) :-
	!,
	wamcode_clause(B, WamCode, WamCodeLits).
wamcode_clause((L, B), WamCode0, [WamCodeLit|WamCodeLits]) :-
	!,
	wamcode_literal(L, WamCode0, WamCode, WamCodeLit),
	wamcode_clause(B, WamCode, WamCodeLits).
wamcode_clause(B, WamCode0, [WamCodeLit]) :-
	wamcode_literal(B, WamCode0, WamCode, WamCodeLit0),
	append(WamCodeLit0, WamCode, WamCodeLit).

wamcode_literal(_L, WamCode0, _WamCode, _WamCodeLit) :-
	var(WamCode0),
	!.
wamcode_literal(L, [ifshallow|WamCode0], WamCode, WamCodeLit) :-
	!,
	opt_wamcode(WamCode0, 1, WamCode1),
	wamcode_literal(L, WamCode1, WamCode, WamCodeLit).
wamcode_literal(L, WamCode0, WamCode, WamCodeLit) :-
	functor(L, F, N),
	(
	    ( CallLit = call(F/N, _)
	    ; CallLit = execute(F/N)
	    ),
	    append(WamCodeLit0, [CallLit|WamCode], WamCode0) ->
	    append(WamCodeLit0, [CallLit], WamCodeLit)
	; % it there are not a call, we suppose is a builtin
	    WamCode = WamCode0,
	    WamCodeLit = []
	).

opt_wamcode(WamCode0, Select, WamCode) :-
	append(SelWamCode1, [else|WamCode1],  WamCode0),
	append(SelWamCode0, [endif|WamCode2], WamCode1),
	(
	    Select == 1 ->
	    SelWamCode = SelWamCode1
	; SelWamCode = SelWamCode0
	),
	append(SelWamCode, WamCode2, WamCode),
	!.

wamcode_size(B, WamCode, Sizes) :-
	wamcode_clause(B, WamCode, WamCodeLit),
	lengths(WamCodeLit, Sizes).

lengths([],     []).
lengths([L|Ls], [S|Ss]) :-
	length(L, S),
	lengths(Ls, Ss).
