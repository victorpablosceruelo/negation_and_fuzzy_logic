:- module(_, [reverse/2], [assertions, regtypes, nativeprops,
        predefres(res_ticks)]).

:- doc(author, "Edison Mera").
:- doc(author, "Teresa Trigo").

:- entry reverse(X, Y) : (var(Y), list(X, int)).

 :- check pred reverse(A, B)
     : (list(A, int), var(B))
     => (list(A, int), list(B, int))
     + (cost(ub, ticks, 10 * length(A) - 20)).

reverse(X, Y) :-
    reverse2(X, [], Y).

:- trust comp reverse2(A, B, C) + (size_metric(B, void)).

reverse2([],    Y,  Y).
reverse2([X|L], Y0, Y) :-
    reverse2(L, [X|Y0], Y).
