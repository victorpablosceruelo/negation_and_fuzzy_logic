:- module(arithpreds, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Arithmetic Predicates").
:- doc(author, "Jose F. Morales").

:- doc(bug, "Incomplete version of library(arithpreds) for pl2js").
:- doc(bug, "This library exports 'fun_eval' propeties. Is that what we want?").

% Arithmetic
:- export((+)/3).
%:- pred (+)/3 + unfold.
:- fun_eval((+)/2).
A + B := ~'$+'(A, B).

:- export((-)/3).
%:- pred (-)/3 + unfold.
:- fun_eval((-)/2).
A - B := ~'$-'(A, B).

:- export((-)/2).
%:- pred (-)/2 + unfold.
:- fun_eval((-)/1).
- A := ~'$-'(A).

:- export((*)/3).
%:- pred (*)/3 + unfold.
:- fun_eval((*)/2).
A * B := ~'$*'(A, B).

:- export((/)/3).
%:- pred (/)/3 + unfold.
:- fun_eval((/)/2).
A / B := ~'$/'(A, B).

:- export((//)/3).
%:- pred (//)/3 + unfold.
:- fun_eval((//)/2).
A // B := ~'$//'(A, B).

:- export((mod)/3).
%:- pred (mod)/3 + unfold.
:- fun_eval((mod)/2).
A mod B := ~'$mod'(A, B).

:- export((<<)/3).
%:- pred (<<)/3 + unfold.
:- fun_eval((<<)/2).
A << B := ~'$<<'(A, B).

:- export((>>)/3).
%:- pred (>>)/3 + unfold.
:- fun_eval((>>)/2).
A >> B := ~'$>>'(A, B).

:- export((cos)/2).
%:- pred (cos)/2 + unfold.
:- fun_eval((cos)/1).
cos(A) := ~'$cos'(A).

:- export((sin)/2).
%:- pred (sin)/2 + unfold.
:- fun_eval((sin)/1).
sin(A) := ~'$sin'(A).
