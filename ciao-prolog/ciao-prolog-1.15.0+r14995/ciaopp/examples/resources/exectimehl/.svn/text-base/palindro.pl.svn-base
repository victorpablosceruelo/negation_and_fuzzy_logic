:- module(palindro, [palindrome/2], [assertions, regtypes,
		ciaopp(examples(resources(exectimehl)))]).

%:- entry palindrome/2 : list(gnd) * var.
:- entry palindrome/2 : int_list * var.

:- export(int_list/1).
:- regtype int_list/1.

int_list([]).
int_list([A|L]) :-
	int(A),
	int_list(L).

palindrome([],         []).
palindrome([First|L1], L2) :-
	palindrome(L1, Ls2),
	palindrome(L1, Lg2),
	append(Ls2, [First|Lg2], L2).

append([],    L,  L).
append([H|L], L1, [H|R]) :-
	append(L, L1, R).
