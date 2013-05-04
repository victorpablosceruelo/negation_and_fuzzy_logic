:- module(append, [append/3], [assertions, nativeprops, regtypes %]).
 		, ciaopp(tests(resources)), predefres(res_all)]).

% :- use_package(ciaopp(tests(resources))).
% :- use_package(predefres(res_all)).

:- redefining(append/3).

:- entry append(Xs, Ys, Zs) : (list(Xs, num), list(Ys, num), var(Zs)).

append([],     Y,  Y).
append([X|Xs], Ys, [X|Zs]) :-
	append(Xs, Ys, Zs).
