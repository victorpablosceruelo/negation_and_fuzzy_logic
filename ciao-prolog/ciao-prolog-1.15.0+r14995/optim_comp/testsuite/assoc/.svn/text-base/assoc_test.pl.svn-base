:- module(assoc_test, [], []).

% Tests for several associative list implementations.
%
% Jose F. Morales

% alg: assoc_yap, data: rnd(5000,10000), time: 21.731
% alg: assoc_ciao, data: rnd(5000,10000), time: 25.978
% alg: dict, data: rnd(5000,10000), time: 14.365
% alg: assoc_yap, data: inc(5000), time: 16.848
% alg: assoc_ciao, data: inc(5000), time: 21.158
% alg: dict, data: inc(5000), time: 1651.754
% alg: assoc_yap, data: dec(5000), time: 17.358
% alg: assoc_ciao, data: dec(5000), time: 20.146
% alg: dict, data: dec(5000), time: 1626.252

:- use_module(library(assoc_yap)).
:- use_module(library(assoc_ciao)).
:- use_module(library(dict)).
:- use_module(library(prolog_sys)).

:- export(main/0).
main :-
	( ( D = rnd(5000,10000)
	  ; D = inc(5000)
	  ; D = dec(5000)
	  ),
	  do(assoc_yap, D),
	  do(assoc_ciao, D),
	  do(dict, D),
	  fail
	; true
	).

test(assoc_yap,D) :- gen_data(D, L), assoc_yap:list_to_assoc(L, _A).
test(assoc_ciao,D) :- gen_data(D, L), assoc_ciao:list_to_assoc(L, _A).
test(dict,D) :- gen_data(D, L), list_to_dic(L, _, _A).

% Generation of test data
gen_data(rnd(N,M), D) :- rndlst(N, M, D).
gen_data(inc(N), D) :- inclst(N, D).
gen_data(dec(N), D) :- declst(N, D).

% ---------------------------------------------------------------------------

% A generator of random K-V lists modulo N
rndlst(I, N, Ks) :- rndlst_(I, 1, 1, N, Ks).

rndlst_(0, _, _, _N, []) :- !.
rndlst_(I, Z, W, N, [K-_|Ks]) :-
	Z1 is 36969 * (Z /\ 65535) + (Z >> 16),
	W1 is 18000 * (W /\ 65535) + (W >> 16),
	R is (Z1 << 16) + W1, % 32-bit result
	%
	K is (R mod N),
        I1 is I - 1,
	rndlst_(I1, Z1, W1, N, Ks).

% List of increasing numbers
inclst(I, Ks) :- inclst_(I, 0, Ks).

inclst_(0, _N, []) :- !.
inclst_(I, N, [N-_|Ks]) :-
        I1 is I - 1,
        N1 is N + 1,
	inclst_(I1, N1, Ks).

% List of decreasing numbers
declst(0, []) :- !.
declst(I, [I-_|Ks]) :-
        I1 is I - 1,
	declst(I1, Ks).

% ---------------------------------------------------------------------------

% List to dic 
list_to_dic([],Assoc,Assoc).
list_to_dic([K-V|Rest],Assoc,Assoc2) :-
%	dic_lookup(Assoc,K,V),
	dic_replace(Assoc,K,V,Assoc1),
	list_to_dic(Rest,Assoc1,Assoc2).

% ---------------------------------------------------------------------------

'$cputime'(X) :- statistics(runtime, [X|_]).

do(Alg,D) :-
	'$cputime'(T1),
	test(Alg,D),
	'$cputime'(T2),
	Time is T2-T1,
	display('alg: '), display(Alg), display(', '),
	display('data: '), display(D), display(', '),
	display('time: '), display(Time), nl.
