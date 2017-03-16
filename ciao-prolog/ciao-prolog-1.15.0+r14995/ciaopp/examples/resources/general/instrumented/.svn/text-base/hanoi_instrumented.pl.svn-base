:- module(_, [hanoi/4], [assertions, nativeprops, regtypes, resdefs]).

:- load_resource_module(hanoi_res).
:- resource movements.
:- head_cost(ub, movements, head_movements).
:- literal_cost(ub, movements, lit_movements).

:- entry hanoi(A, B, C, D) : num * elem * elem * elem.
:- trust comp hanoi(N, A, B, C)
	+ ( size_metric(A, void(A)), size_metric(B, void(B)), size_metric(C,
		void(C)) ).

hanoi(1, A, _, C) :- move_disks(A, C), !.
hanoi(N, A, B, C) :-
	N1 is N - 1,
	hanoi(N1, A, C, B),
	hanoi(N1, B, A, C),
	move_disks(A, C).

:- impl_defined([move_disks/2]).
:- trust pred move_disks(X, Y) : (elem(X), elem(Y))=>(elem(X), elem(Y))
	+ cost(ub, movements, 1).

move_disks(_, _) :- display('+1').

:- export(elem/1).
:- regtype elem/1.
elem(a).
elem(b).
elem(c).
