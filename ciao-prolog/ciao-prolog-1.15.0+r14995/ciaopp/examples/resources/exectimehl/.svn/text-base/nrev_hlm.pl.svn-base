:- module(nrev_hlm, [nrev/2], [assertions, regtypes, nativeprops,
		ciaopp(examples(resources(exectimehl)))]).

:- export(short_int_list/1).
:- regtype short_int_list/1.

short_int_list([]).
short_int_list([A|B]) :-
	int(A),
	short_int_list(B).

:- entry nrev/2 : short_int_list * var.

nrev([],    []).
nrev([B|A], C) :-
	nrev(A, D),
	append(D, [B], C).

append([],    A, A).
append([B|A], D, [B|C]) :-
	append(A, D, C).
