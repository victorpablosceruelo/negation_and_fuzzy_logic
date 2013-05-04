:- module(colp_rt, ['$colp1'/3, '$colp2'/3]).

:- use_module(library(odd), [setarg/3]).

:- initialization(set_prolog_flag(check_cycles, on)).

'$colp1'(I, Mem, X):-
%	message(['$colp1'(I, Mem, X)]),
	arg(I, Mem, T), member(X, T).
'$colp2'(I, Mem, X):-
%	message(['$colp2'(I, Mem, X)]),
	arg(I, Mem, T), setarg(I, Mem, [X|T]).

