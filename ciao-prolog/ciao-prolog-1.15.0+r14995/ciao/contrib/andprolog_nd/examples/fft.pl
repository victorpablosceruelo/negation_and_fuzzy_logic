:- module(fft,
	[
	    seq/2,
	    par/2,
	    par_nondet/2,
	    data/1
	],
	[andprolog_nd]).

:- push_prolog_flag(multi_arity_warnings, off).

:- use_module(library(lists)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gc(20).

data(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

seq(0,true) :- fft_seq.
par(0,true) :- gc(GC), fft_par_gc(GC).
par_nondet(0,true) :- gc(GC), fft_par_nondet_gc(GC).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Versions without granularity control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fft_seq :- 	
        K is 9,
	N is 1<<K,
	numlist(1, N, Raw),
	mark,
	fwd_fft_seq(Raw, _),
	stopins.

fft_par :- 	
        K is 9,
	N is 1<<K,
	numlist(1, N, Raw),
	mark,
	fwd_fft_par(Raw, _),
	stopins.

mark.
stopins.

/*	F[k] = sum[j=0..N-1] exp(2.pi.i.j.k/N) f[j]

	     = sum[j=0..N/2-1] exp(2.pi.i.k.(2j)/N f[2j]
	     + sum[j=0..N/2-1] exp(2.pi.i.k.(2j+1)/N f[2j+1]

	     = sum[j=0..N/2-1] exp(2.pi.i.j.k/(N/2)) f[2j]
	     + W^k
	     * sum[j=0..N/2-1] exp(2.pi.i.j.k/(N/2)) f[2j+1]

	F(k;L) = f(k;E) + exp(2.pi.i.k/length(L))f(k;O)
		where evens_and_odds(L, E, O).
*/

fwd_fft_seq(Raw, FFT) :-
	length(Raw, N),
	fft_seq(N, Raw, FFT, fwd).

fwd_fft_par(Raw, FFT) :-
	length(Raw, N),
	fft_par(N, Raw, FFT, fwd).

inv_fft_seq(FFT, Raw) :-
	length(FFT, N),
	fft_seq(N, FFT, Mid, inv),
	scale(Mid, N, Raw).

inv_fft_par(FFT, Raw) :-
	length(FFT, N),
	fft_par(N, FFT, Mid, inv),
	scale(Mid, N, Raw).

fft_seq(1, [X], [C], _) :- !,
	complex_val(X, C).
fft_seq(N, Raw, FFT, Dir) :-		
	n_cos_sin(N, Cos, Sin),
	pack_w(Dir, Cos, Sin, W),
	M is N>>1,
	evens_and_odds(Raw, E, O),
	fft_seq(M, E, Ef, Dir),
	fft_seq(M, O, Of, Dir),
	fft_seq(Ef, Of, W, (1.0,0.0), Z, FFT, FF2),
	fft_seq(Ef, Of, W, Z, _, FF2, []). 

fft_par(1, [X], [C], _) :- !,
	complex_val(X, C).
fft_par(N, Raw, FFT, Dir) :-
	n_cos_sin(N, Cos, Sin),
	pack_w(Dir, Cos, Sin, W),
	M is N>>1,
	evens_and_odds(Raw, E, O),
	fft_par(M, E, Ef, Dir) '&!'
	fft_par(M, O, Of, Dir),
	fft_par(Ef, Of, W, (1.0,0.0), Z, FFT, FF2),
	fft_par(Ef, Of, W, Z, _, FF2, []). 

fft_par_nondet(1, [X], [C], _) :- !,
	complex_val(X, C).
fft_par_nondet(N, Raw, FFT, Dir) :-		
	n_cos_sin(N, Cos, Sin),
	pack_w(Dir, Cos, Sin, W),
	M is N>>1,
	evens_and_odds(Raw, E, O),
	fft_par_nondet(M, E, Ef, Dir) &
	fft_par_nondet(M, O, Of, Dir),
	fft_par_nondet(Ef, Of, W, (1.0,0.0), Z, FFT, FF2),
	fft_par_nondet(Ef, Of, W, Z, _, FF2, []). 

pack_w(fwd, C, S, (C,S)).
pack_w(inv, C, S, (C,Z)) :- Z is -S.

fft_seq([], [], _, Z, Z, F, F).
fft_seq([E|Es], [O|Os], W, Z0, Z, [F|Fs], Fl) :-
	complex_mul(Z0, O, Zt),
	complex_add(Zt, E, F),
	complex_mul(Z0, W, Z1),
	fft_seq(Es, Os, W, Z1, Z, Fs, Fl).

fft_par([], [], _, Z, Z, F, F).
fft_par([E|Es], [O|Os], W, Z0, Z, [F|Fs], Fl) :-
	(
	    complex_mul(Z0, O, Zt),
	    complex_add(Zt, E, F)
	) '&!'
        (
	    complex_mul(Z0, W, Z1),
	    fft_par(Es, Os, W, Z1, Z, Fs, Fl)
        ).

fft_par_nondet([], [], _, Z, Z, F, F).
fft_par_nondet([E|Es], [O|Os], W, Z0, Z, [F|Fs], Fl) :-
	(
	    complex_mul(Z0, O, Zt),
	    complex_add(Zt, E, F)
	) &
        (
	    complex_mul(Z0, W, Z1),
	    fft_par(Es, Os, W, Z1, Z, Fs, Fl)
        ).

evens_and_odds([], [], []).
evens_and_odds([E,O|EOs], [E|Es], [O|Os]) :-
	evens_and_odds(EOs, Es, Os).

scale([], _, []).
scale([(Ra,Ia)|Xs], Scale, [(Rs,Is)|Ys]) :-
	Rs is Ra/Scale,
	Is is Ia/Scale,
	scale(Xs, Scale, Ys).

complex_val((Ra,Ia), (Rs,Is)) :- !,
	Rs is Ra*1.0,
	Is is Ia*1.0.
complex_val(Ra, (Rs,0.0)) :-
	Rs is Ra*1.0.

complex_add((Ra,Ia), (Rb,Ib), (Rs,Is)) :-
	Rs is Ra+Rb,
	Is is Ia+Ib.

complex_mul((Ra,Ia), (Rb,Ib), (Rs,Is)) :-
	Rs is Ra*Rb-Ia*Ib,
	Is is Ra*Ib+Rb*Ia.

complex_exp(Ang, (Rs,Is)) :-
	cos(Ang, Rs),
	sin(Ang, Is).


%   n_cos_sin(N, C, S) is true when N is 2^K for K=1..23,
%   C is cos(2.pi/N), and S is sin(2.pi/N).

n_cos_sin(        2, -1.00000000,  0.00000000).
n_cos_sin(        4,  0.00000000,  1.00000000).
n_cos_sin(        8,  0.707106781,  0.707106781).
n_cos_sin(       16,  0.923879533,  0.382683432).
n_cos_sin(       32,  0.980785280,  0.195090322).
n_cos_sin(       64,  0.995184727,  0.0980171403).
n_cos_sin(      128,  0.998795456,  0.0490676743).
n_cos_sin(      256,  0.999698819,  0.0245412285).
n_cos_sin(      512,  0.999924702,  0.0122715383).
n_cos_sin(     1024,  0.999981175,  0.00613588465).
n_cos_sin(     2048,  0.999995294,  0.00306795676).
n_cos_sin(     4096,  0.999998823,  0.00153398019).
n_cos_sin(     8192,  0.999999706,  0.000766990319).
n_cos_sin(    16384,  0.999999926,  0.000383495188).
n_cos_sin(    32768,  0.999999982,  0.000191747597).
n_cos_sin(    65536,  0.999999995,  0.0000958737991).
n_cos_sin(   131072,  0.999999999,  0.0000479368996).
n_cos_sin(   262144,  1.00000000,   0.0000239684498).
n_cos_sin(   524288,  1.00000000,   0.0000119842249).
n_cos_sin(  1048576,  1.00000000,   0.00000599211245).
n_cos_sin(  2097152,  1.00000000,   0.00000299605623).
n_cos_sin(  4194304,  1.00000000,   0.00000149802811).
n_cos_sin(  8388608,  1.00000000,   0.000000749014057).

data(0, [0,0,0,0]).
data(1, [1,0,0,0]).
data(2, [0,1,0,0]).
data(8, [1,2,3,4,3,2,1,0]).
data(9, [1,2,1,2,1,2,1,2]).

numlist(I, N, []) :-
	I > N, !.
numlist(I, N, [I|L]) :-
	I =< N, J is I+1,
	numlist(J, N, L).


sin(X, Sin):- Sin is sin(X).
cos(X, Cos):- Cos is cos(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fft_par_gc(G):- 	
        K is 9,
	N is 1<<K,
	numlist(1, N, Raw),
	mark,
	fwd_fft_par_gc(Raw, G, _),
	stopins.

fwd_fft_par_gc(Raw, G, FFT) :-
	length(Raw, N),
	fft_par_gc(N, Raw, G, FFT, fwd).

inv_fft_par_gc(FFT, G, Raw) :-
	length(FFT, N),
	fft_par_gc(N, FFT, G, Mid, inv),
	scale(Mid, N, Raw).

fft_par_gc(1, [X], _G, [C], _) :- !,
	complex_val(X, C).
fft_par_gc(N, Raw, G, FFT, Dir) :-
	n_cos_sin(N, Cos, Sin),
	pack_w(Dir, Cos, Sin, W),
	M is N>>1,
	evens_and_odds(Raw, E, O),
	(
	    N > G ->
	    fft_par_gc(M, E, G, Ef, Dir) '&!'
	    fft_par_gc(M, O, G, Of, Dir),
	    fft_par_gc(Ef, Of, G, W, (1.0,0.0), Z, FFT, FF2),
	    fft_par_gc(Ef, Of, G, W, Z, _, FF2, [])
	;
	    fft_seq(M, E, Ef, Dir),
	    fft_seq(M, O, Of, Dir),
	    fft_seq(Ef, Of, W, (1.0,0.0), Z, FFT, FF2),
	    fft_seq(Ef, Of, W, Z, _, FF2, [])
	).

fft_par_gc([], [], _G, _, Z, Z, F, F).
fft_par_gc([E|Es], [O|Os], G, W, Z0, Z, [F|Fs], Fl) :-
	(
	    complex_mul(Z0, O, Zt),
	    complex_add(Zt, E, F)
	),
        (
	    complex_mul(Z0, W, Z1),
	    fft_par_gc(Es, Os, G, W, Z1, Z, Fs, Fl)
        ).

fft_par_nondet_gc(G):- 	
        K is 9,
	N is 1<<K,
	numlist(1, N, Raw),
	mark,
	fwd_fft_par_nondet_gc(Raw, G, _),
	stopins.

fwd_fft_par_nondet_gc(Raw, G, FFT) :-
	length(Raw, N),
	fft_par_nondet_gc(N, Raw, G, FFT, fwd).

inv_fft_par_nondet_gc(FFT, G, Raw) :-
	length(FFT, N),
	fft_par_nondet_gc(N, FFT, G, Mid, inv),
	scale(Mid, N, Raw).

fft_par_nondet_gc(1, [X], _G, [C], _) :- !,
	complex_val(X, C).
fft_par_nondet_gc(N, Raw, G, FFT, Dir) :-
	n_cos_sin(N, Cos, Sin),
	pack_w(Dir, Cos, Sin, W),
	M is N>>1,
	evens_and_odds(Raw, E, O),
	(
	    N > G ->
	    fft_par_nondet_gc(M, E, G, Ef, Dir) &
	    fft_par_nondet_gc(M, O, G, Of, Dir),
	    fft_par_nondet_gc(Ef, Of, G, W, (1.0,0.0), Z, FFT, FF2),
	    fft_par_nondet_gc(Ef, Of, G, W, Z, _, FF2, [])
	;
	    fft_seq(M, E, Ef, Dir),
	    fft_seq(M, O, Of, Dir),
	    fft_seq(Ef, Of, W, (1.0,0.0), Z, FFT, FF2),
	    fft_seq(Ef, Of, W, Z, _, FF2, [])
	).

fft_par_nondet_gc([], [], _G, _, Z, Z, F, F).
fft_par_nondet_gc([E|Es], [O|Os], G, W, Z0, Z, [F|Fs], Fl) :-
	(
	    complex_mul(Z0, O, Zt),
	    complex_add(Zt, E, F)
	),
        (
	    complex_mul(Z0, W, Z1),
	    fft_par_nondet_gc(Es, Os, G, W, Z1, Z, Fs, Fl)
        ).

