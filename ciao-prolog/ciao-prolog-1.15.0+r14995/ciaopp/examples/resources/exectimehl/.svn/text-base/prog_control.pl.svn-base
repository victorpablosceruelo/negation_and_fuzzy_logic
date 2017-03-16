:- module(_, [list1/1, listn/2, listt/2, listr/1], [assertions, regtypes,
		nativeprops, ciaopp(examples(resources(exectimehl)))]).

:- regtype
	int_list/1.
int_list([]).
int_list([A|B]) :-
	int(A),
	int_list(B).
:- entry
	list1(_) :int_list.
list1([]).
list1([_|A]) :-
	list1(A).
:- entry
	listr(_) :int_list.
listr([]).
listr([_|A]) :-
	listr(A),
	dummy,
	dummy,
	dummy,
	dummy.
dummy.
:- entry
	listn(_, _) :int_list * var.
listn([],    []).
listn([A|B], [A|C]) :-
	listn(B, C).
:- entry
	listt(_, _) :int_list * var.
listt([],    []).
listt([A|B], [f(A, A, A, A, A, A, A, A, A, A)|C]) :-
	listt(B, C).
