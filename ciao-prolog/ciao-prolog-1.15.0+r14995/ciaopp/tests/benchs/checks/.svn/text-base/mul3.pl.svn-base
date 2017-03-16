:-module(mul3, [r/0], [assertions, regtypes]).

% assertion checked with pred_ctchecks set to new_succ or new_all_succ

:- check success go(A,B):ta(A) => ta(B).  


r:-
	go(a,_X),
	go(b,_Y).

go(a,a).
go(b,b).



:- regtype ta/1.

ta(a).

:- regtype tb/1.

tb(b).

