:- module(copy_term, [copy_term/2, copy_term2/2], [corec]).

/* copy_term/2 */

copy_term(A, B):-
	copy_term_(A, B, _).

:- corecursion copy_term_(===, =, _) with !.

copy_term_(A, B, Dic):-
	var(A), !,
	dic_lookup(A, B, Dic).
copy_term_(A, A, _) :-
	atomic(A), !.
copy_term_(A, B, Dic):-
	A =.. [F|AArgs],
	copy_term_args(AArgs, BArgs, Dic),
	B =.. [F|BArgs].

copy_term_args([], [], _Dic).
copy_term_args([A|AArgs], [B|BArgs], Dic):-
	copy_term_(A, B, Dic),
	copy_term_args(AArgs, BArgs, Dic).

dic_lookup(A, B, Dic) :-
	var(Dic), !, 
	Dic = [A-B|_].
dic_lookup(A, B, [X-Y|T]):-
	(
	    A == X, !,
	    B = Y
	;
	    dic_lookup(A, B, T)
	).



copy_term2(A, B):-
	copy_term2_(A, B, true).

:- corecursion copy_term2_(===, =, Cont) with (!, call(Cont)).

copy_term2_(A, _, Cont):-
	var(A), !,
	call(Cont).
copy_term2_(A, A, Cont) :-
	atomic(A), !, 
	call(Cont).
copy_term2_(A, B, Cont):-
	A =.. [F|AArgs],
	copy_term2_args(AArgs, BArgs, Cont),
	B =.. [F|BArgs].

copy_term2_args([], [], Cont):- call(Cont).
copy_term2_args([A|AArgs], [B|BArgs], Cont):-
	copy_term2_(A, B, copy_term2_args(AArgs, BArgs, Cont)).
