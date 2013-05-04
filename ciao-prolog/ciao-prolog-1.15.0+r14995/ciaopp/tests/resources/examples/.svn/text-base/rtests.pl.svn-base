:- module(rtests, _, [assertions, basicmodes, regtypes,
		ciaopp(tests(resources))]).

:- doc(author, "Edison Mera").

:- doc(module, "This module test the trust_default declarations
	that let us to configure default values for the resources when
	no analysis or trusted cost is available.").

:- resource res_1.

:- trust_default +(cost(ub, res_1, 0)).

:- resource res_2.

:- trust_default +(cost(ub, res_2, inf)).

:- resource res_3.

:- literal_cost(ub, res_1, 0).

:- literal_cost(ub, res_2, 0).

:- literal_cost(ub, res_3, 0).

:- entry p(+X, +Y).

:- trust pred p(+X, +Y) + (head_cost(ub, res_2, 1)).
:- trust pred p(+X, +Y) + (head_cost(ub, res_3, 1)).

p(X, Y) :- q(X), r(Y), s(X).

:- trust comp q(+X) + (cost(ub, res_1, 6)).
:- trust comp q(+X) + (cost(ub, res_2, 6)).
:- trust comp q(+X) + (cost(ub, res_3, 6)).

q(_).

:- trust comp r(+X) + (cost(ub, res_1, 5)).
:- trust comp r(+X) + (cost(ub, res_2, 5)).
:- trust comp r(+X) + (cost(ub, res_3, 5)).

r(_).

% :- trust comp s(+X) + (cost(ub, res_1, inf)).
% :- trust comp s(+X) + (cost(ub, res_2, 5)).
:- trust comp s(+X) + (cost(ub, res_3, 5), not_fails, is_det, relations(inf)).

% s(_).

:- impl_defined([s/1]).
