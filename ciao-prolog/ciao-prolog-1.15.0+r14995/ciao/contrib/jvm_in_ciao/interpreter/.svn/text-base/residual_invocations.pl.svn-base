:- module(residual_invocations,[res_invoke/1],[assertions]).

:- doc(title,"Support for letting residual invocations in ISSD").

:- doc(author, "M. Zamalloa").

:- doc(module,"This module provides basic support for the generation of residual 
	invocations when specializing the JVM interpreter in ISSD (Incremental
	and structure sensitive decompilation) mode.").

:- pred res_invoke(Call) #"This will be the predicate which will remain in the 
	residual code".
:- trust comp res_invoke/1 + memo.
:- trust comp res_invoke/1 + (bind_ins,sideff(free)).
res_invoke(Call) :-
	call(Call).
