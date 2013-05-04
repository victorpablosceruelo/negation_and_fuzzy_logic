:- module(arithm_opers,
	[
	    max/3,
	    min/3,
	    pow/3,
	    logar/3,
	    factorial/2,
	    log2/2
	], [assertions]).
% Define aritmetic operations by using CIAO (iso) builtins.
% These were defined in math* but now are removed.

 %% :- pred pow(+Base, +Exponent, -Res)
 %% 
 %% # "Exponentiation. @var{Res} is @var{Base} raised to @var{Exponent}.". 

pow(X, Y, Z):- Z is X**Y.

 %% :- pred log2(+Expr, -Log)
 %% 
 %% # "Logarithm. @var{Log} is the logarithm in base 2 of the arithmetic expression @var{Expr}.". 

log2(Expr, Log):- Log is log(Expr)/log(2).


 %% :- pred logar(+Base, +Expr, -Log)
 %% 
 %% # "Logarithm. @var{Log} is the logarithm in base @var{Base} of the arithmetic expression @var{Expr}.". 

logar(Base, Expr, Log):- Log is log(Expr)/log(Base).

factorial(0, 1):-!. 
factorial(N, F):-
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is F1 * N.
%
%  Performing the arithmetic maximum function.
%
max(N,M,N) :-
	N >= M,
	!.
max(N,M,M) :-
	N < M.

%
%  Performing the arithmetic minimum function.
%
min(N,M,N) :-
	N < M.
min(N,M,M) :-
	N >= M.
