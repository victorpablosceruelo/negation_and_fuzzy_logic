:- module(_, [append/3], [assertions, nativeprops, regtypes,
		ciaopp(examples(resources(exectimell)))]).

:- doc(author, "Edison Mera").

:- doc(module, "This program appends two lists.").

:- entry append(Xs, Ys, Zs) : ( list(Xs, num), list(Ys, num),
	    var(Zs) ).

append([],     Y,  Y).
append([X|Xs], Ys, [X|Zs]) :-
	append(Xs, Ys, Zs).
