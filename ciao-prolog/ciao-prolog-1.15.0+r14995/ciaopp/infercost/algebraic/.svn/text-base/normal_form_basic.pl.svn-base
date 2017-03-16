:- module(normal_form_basic, [variable/1, userfunc/1], [assertions]).


%
%  Test if a term is a variable.
%
variable($(_)).
variable($(_,_)).


%
%  Test if a term is a user-defined function.
%
userfunc(X) :-
	functor(X,F,N),
	(F,N) \== ('$',1), (F,N) \== ('$',2), (F,N) \== ('+',2),
	(F,N) \== ('-',2), (F,N) \== ('*',2), (F,N) \== ('/',2),
	(F,N) \== ('-',1), (F,N) \== (exp,2), (F,N) \== (log,2),
	(F,N) \== (fact,1),(F,N) \== (max,2), (F,N) \== (min,2),
	(F,N) \== (arg,2), (F,N) \== (arity,1),(F,N) \== (head,1),
	(F,N) \== (tail,1),(F,N) \== (sum,4), (F,N) \== (prod,4).
