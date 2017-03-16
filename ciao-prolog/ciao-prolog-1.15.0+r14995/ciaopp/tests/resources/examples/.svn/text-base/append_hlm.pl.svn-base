:- module(_, [append/3], [assertions, nativeprops, regtypes,
		ciaopp(tests(resources)),
		res_exectime_hlm(auto(res_exectime_hlm_63))]).

:- doc(author, "Edison Mera").

:- doc(module, "This program appends two lists.").

:- include(resources(saved_results(platform))).

:- entry append(Xs, Ys, Zs) : (list(Xs, num), list(Ys, num), var(Zs)).

append([],     Y,  Y).
append([X|Xs], Ys, [X|Zs]) :-
	append(Xs, Ys, Zs).
