:- module(_, _, [dcg, fsyntax]).

:- use_module(library(format)).
:- use_module(library(aggregates)).

t(ciao,queens11,1,780,7157).
t(ciao,crypt,1000,1809,10632).
t(ciao,primes,10000,1270,6398).
t(ciao,tak,100,1140,5434).
t(ciao,deriv,10000,150,9596).
t(ciao,poly,100,539,13531).
t(ciao,qsort,10000,580,6972).
t(ciao,exp,10,601,6453).
t(ciao,fib,1000,350,5323).
t(ciao,knights,1,740,7801).
t(ctoc,queens11,1,600,22432).
t(ctoc,crypt,1000,1390,69860).
t(ctoc,primes,10000,1220,23368).
t(ctoc,tak,100,1170,15732).
t(ctoc,deriv,10000,320,31632).
t(ctoc,poly,100,580,81444).
t(ctoc,qsort,10000,510,71788).
t(ctoc,exp,10,560,20536).
t(ctoc,fib,1000,450,11852).
t(ctoc,knights,1,670,28496).
t(ctoce,queens11,1,1100,19588).
t(ctoce,crypt,1000,2840,49208).
t(ctoce,primes,10000,2270,19476).
t(ctoce,tak,100,2240,17028).
t(ctoce,deriv,10000,620,26392).
t(ctoce,poly,100,1450,48008).
t(ctoce,qsort,10000,1280,53480).
t(ctoce,exp,10,580,19736).
t(ctoce,fib,1000,480,13688).
t(ctoce,knights,1,1590,23356).
t(ctocop,queens11,1,260,19776).
t(ctocop,crypt,1000,970,71588).
t(ctocop,primes,10000,890,18000).
t(ctocop,tak,100,700,16120).
t(ctocop,deriv,10000,290,29548).
t(ctocop,poly,100,480,69660).
t(ctocop,qsort,10000,430,58824).
t(ctocop,exp,10,570,20808).
t(ctocop,fib,1000,420,11868).
t(ctocop,knights,1,610,28388).
t(ctocope,queens11,1,930,19976).
t(ctocope,crypt,1000,2450,50264).
t(ctocope,primes,10000,2030,17796).
t(ctocope,tak,100,1430,17484).
t(ctocope,deriv,10000,580,25544).
t(ctocope,poly,100,1170,43888).
t(ctocope,qsort,10000,1180,48056).
t(ctocope,exp,10,590,20552).
t(ctocope,fib,1000,500,13964).
t(ctocope,knights,1,1510,23568).

compare_time(A, B, Xs-Av) :-
	findall(X, compare_time_2(A, B, X), Xs),
	average(Xs, Av).

compare_time_2(A, B, P-Ta-Tb-T) :-
	t(A, P, _, Ta, _),
	t(B, P, _, Tb, _),
	T is ((Ta / Tb) - 1) * 100.

compare_size(A, B, Xs-Av) :-
	findall(X, compare_size_2(A, B, X), Xs),
	average(Xs, Av).

compare_size_2(A, B, P-Sa-Sb-S) :-
	t(A, P, _, _, Sa),
	t(B, P, _, _, Sb),
	S is (Sb / Sa).

average(Xs, Av) :- average_2(Xs, 0, 0, Av).

average_2([], C, S, Av) :- Av is S / C.
average_2([_-X|Xs], C, S, Av) :- C1 is C + 1, S1 is S + X, average_2(Xs, C1, S1, Av).
