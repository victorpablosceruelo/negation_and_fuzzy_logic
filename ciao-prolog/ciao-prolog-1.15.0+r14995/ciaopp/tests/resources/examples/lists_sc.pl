:- module(_, _, [assertions, ciaopp(tests(resources)), predefres(res_steps)]).

:- doc(author, "Edison Mera").

:- doc(module, "This example tests the existence of strong
	connected components that have bidirectional dependencies, in
	this example, between mylist/1 and mylist2/1.").

:- entry mylist/1 : gnd.

mylist([]).
mylist([_|Xs]) :-
	mylist2(Xs).

mylist2(X) :- mylist(X).

:- entry myappend/3: list(gnd) * list(gnd) * var.

myappend([],     X, X).
myappend([X|Xs], Y, [X|Zs]) :-
	myappend(Xs, Y, Zs).
