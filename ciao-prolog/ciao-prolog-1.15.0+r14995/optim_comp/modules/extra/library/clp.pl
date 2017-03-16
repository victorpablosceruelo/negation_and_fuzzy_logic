
:- op(700, xfx, [(.=.),(.<>.),(.<.),(.=<.),(.>.),(.>=.)]).

:- true comp ( A .=. B ) + native(A.=.B).
:- '$props'(.=. /2, [impnat=intrinsic]).
:- true comp ( A .>. B ) + native(A.>.B).
:- '$props'(.>. /2, [impnat=intrinsic]).
:- true comp ( A .<. B ) + native(A.<.B).
:- '$props'(.<. /2, [impnat=intrinsic]).
:- true comp ( A .>=. B ) + native(A.>=.B).
:- '$props'(.>=. /2, [impnat=intrinsic]).
:- true comp ( A .=<. B ) + native(A.=<.B).
:- '$props'(.=<. /2, [impnat=intrinsic]).
% Nop! :- true comp ( A .<>. B ) + native(A.<>.B).
:- '$props'(.<>. /2, [impnat=intrinsic]).
