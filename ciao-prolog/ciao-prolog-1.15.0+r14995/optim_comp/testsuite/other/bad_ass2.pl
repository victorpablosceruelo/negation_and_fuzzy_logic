:- module(_, _, [assertions, basicmodes, regtypes]).

:- calls jj(X) : a(X).

:- use_module(.(bad_ass2_b)).

:- prop a/1.
a(_).

jj(_).

:- pred f(out(X)) :: b.
:- pred f(out(X)) :: c.

f(3).

:- pred g(+) :: int.

g(3).

:- regtype col/1.
col(a).
col(b).
