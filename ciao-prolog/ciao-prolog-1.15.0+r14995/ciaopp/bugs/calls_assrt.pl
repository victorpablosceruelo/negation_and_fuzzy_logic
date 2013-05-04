% Calls assertions with type names in argument positions raise error
% messages.  When these calls assertions are located in library modules,
% the error message is written to the .ast file, corrupting it and
% making it useless.
%
% How to reproduce it:  In a top-level shell, load CiaoPP and do:
%
% ?- module(calls_assrt).
%
:- module(_, _, [assertions]).

%:- check calls intercept(callable,term,callable).
:- check calls intercept(X, Y, Z) : (callable(X), term(Y), callable(Z)).

intercept(_, _, _).
