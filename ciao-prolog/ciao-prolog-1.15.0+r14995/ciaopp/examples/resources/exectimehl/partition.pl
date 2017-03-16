:- module(partition, [partition/4], [assertions, regtypes,
		nativeprops, predefres(res_all)]).

:- entry partition(A, B, C, D) : list(num) * num * var * var.

:- true pred partition(A, B, C, D) : ( mshare([[C], [D]]),
	    var(C), var(D), ground([A, B]) ) => ground([A, B, C, D]).

partition([],    _, [],        []).
partition([E|R], C, [E|Left1], Right) :-
	E < C, !,
	partition(R, C, Left1, Right).
partition([E|R], C, Left, [E|Right1]) :-
	E >= C,
	partition(R, C, Left, Right1).
