:- module(unifctx_bug, [], [assertions, contextcore, fsyntax, regtypes, expander]).

% bug: A and B are incorrectly unified 

:- export(main/0).
main :-
	A <- a,
        B <- b,
	f do ( ( A <- aa ; B <- bb ) ).

:- control (f do code) {
  '' :- cond.
  cond :- code.
}.




