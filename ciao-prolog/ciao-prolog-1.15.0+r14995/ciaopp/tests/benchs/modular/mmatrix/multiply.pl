:- module(multiply,[multiply/3],[]).

:- use_module(vmul, [vmul/3]).

multiply([],_,[]).
multiply([V0|Rest], V1, [Result|Others]):-  
	multiply(Rest, V1, Others),
	vmul(V0,V1,Result).

