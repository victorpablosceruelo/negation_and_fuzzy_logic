:- module(test, [foo/1, sign/2], [assertions, regtypes, fsyntax]).

% Analyze with eterms+shfr+nfg

%:- pred foo(X) : ground(X), number(X).
%:- entry foo/1 : int.
:- entry foo/1 : num.
foo(X) :- X < 0.
%foo(X) :- X = 0.
foo(X) :- X >= 0.
 
%:- pred sign(X, Y) : ground(X), number(X), var(Y).
%:- entry sign/2 : int * var.
:- entry sign/2 : num * var.
sign(X, Y) :- X < 0, Y = -1.
%sign(X, Y) :- X = 0, Y = 0.
sign(X, Y) :- X >= 0, Y = 1.

:- entry bar/1 : num.
bar(X) :- X < 0.
bar(X) :- X > 0.
bar(X) :- X =:= 0.

% TODO: Cover test does not work with regular types, just numbers?
%
% :- module(test, [foo/1], [assertions, regtypes, fsyntax]).
% 
% :- regtype bool/1.
% bool := true | false.
% 
% foo(X) :- bool(X), answer(X).
% 
% :- trust pred answer(X) : bool(X).
% answer(true).

