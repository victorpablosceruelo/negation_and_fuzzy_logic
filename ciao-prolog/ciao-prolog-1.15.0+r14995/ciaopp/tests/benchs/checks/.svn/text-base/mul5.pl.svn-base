:-module(mul5, [r/0], [assertions, regtypes, nativeprops]).

:-check success p(A,B): ta(A) => ta(B). % checked with pred_ctchecks =  new_succ or new_all_succ
:-check success p(A,B) => ta(B).        % false

:-comp p/2 + terminates.

r:-
	p(a,_X),
	p(b,_Y).

p(a,a).
p(b,b).



:- regtype ta/1.

ta(a).

:- regtype tb/1.

tb(b).

