% Incomplete tests for fsyntax
% (Jose F. Morales)

:- module(f, [main/0], [fsyntax]).
:- fun_eval(arith(true)).

main :-
	N = 10,
	a(N, Ma),
	b(N, Mb),
	c(N, Mc),
	message(['Testing functional notation and escaping with N=', N]),
	message(['This should say: ~(fib(N)) ~(fib(N)) ~(fib(N)) ~(fib(N)) 55 ^(Var) Var']),
	message(['And it says:     ', Ma, ' ', Mb, ' ', Mc, ' ', ^(~fib(N)), ' ', ~fib(N), ' ', ^X, ' ', X]),
	write_fib0(10),
	write_fib(10).

a(N, ^(~fib(N))).

b(N) := ^(~fib(N)).

c(N, X) :- X = ^(~fib(N)).


fib(0) := 0.
fib(1) := 1.
fib(N) := ~fib(N-1) + ~fib(N-2) :- integer(N), N > 1.

% [da(,(integer(_146496),,(>(_146496,1),,(is(_227392,-(_146496,1)),,(fib(_227392,_227268),,(is(_227308,-(_146496,2)),,(fib(_227308,_227272),is(_227280,+(_227268,_227272)))))))))]
% [dg(,(is(_227720,integer(_146496)),,(_227720,,(>(_146496,1),,(is(_227392,-(_146496,1)),,(fib(_227392,_227268),,(is(_227308,-(_146496,2)),,(fib(_227308,_227272),is(_227280,+(_227268,_227272))))))))))]


write_fib0(N):-
	fib(N,N1),
        message(['The ',N,'. Fibonacci number is: ',N1,'.']).
write_fib(N):-
        message(['The ',N,'. Fibonacci number is: ',~fib(N),'.']).

:- export(tailcall/1).
% This should generate a tail call
% Check with: ciaodump --module compile__emu f
tailcall := M :-
	M = ~tailcall_2. % syntax expand may leave spurious true that are removed
tailcall := M :-
	M = ~tailcall_2, true. % optimcomp removes this last true

tailcall_2 := 3.