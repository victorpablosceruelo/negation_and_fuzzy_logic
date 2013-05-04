:- module(_,[upper/2,upper_all/2],_).

upper(Lower,Upper) :-
	var(Upper),
	ground(Lower),!,
	(Lower>=0'a , Lower=<0'z ->
	    Upper is Lower + (0'X-0'x);
	    Upper = Lower
	).
upper(Lower,Upper) :-
	var(Lower),
	ground(Upper),!,
	(Upper>=0'A , Upper=<0'Z ->
	    Lower is Upper - (0'X-0'x);
	    Lower = Upper
	).

upper(Lower,Upper) :-
	ground(Lower),
	ground(Upper),!,
	Lower is Upper - (0'X-0'x).

upper_all([],[]).
upper_all([H|R],[UH|UR]):-
	upper(H,UH),
	upper_all(R,UR).