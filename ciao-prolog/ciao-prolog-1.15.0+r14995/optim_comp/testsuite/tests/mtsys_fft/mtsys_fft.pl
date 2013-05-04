#include "../mtsys_common.pl"

% Fast Fourier transform in Prolog
%
% /* Originally written by Richard O'keefe (June 28, 1988). */
% Adapted to this benchmark framework by Jose Morales.

% TODO: segmentation fault when 'semidet' is uncommented!
#if defined(CIAO3)
:- include(engine(spec_arithmetic)).
#endif

:- use_module(library(lists), [length/2]).

benchmark_data(fft, 1, _Data).
benchmark(_Data, none) :-
	test_fft.

test_fft :-
 	K is 14,
	N is 1<<K,
	numlist(1, N, Raw),
	fwd_fft(Raw, _).

/*	F[k] = sum[j=0..N-1] exp(2.pi.i.j.k/N) f[j]

	     = sum[j=0..N/2-1] exp(2.pi.i.k.(2j)/N f[2j]
	     + sum[j=0..N/2-1] exp(2.pi.i.k.(2j+1)/N f[2j+1]

	     = sum[j=0..N/2-1] exp(2.pi.i.j.k/(N/2)) f[2j]
	     + W^k
	     * sum[j=0..N/2-1] exp(2.pi.i.j.k/(N/2)) f[2j+1]

	F(k;L) = f(k;E) + exp(2.pi.i.k/length(L))f(k;O)
		where evens_and_odds(L, E, O).
*/

fwd_fft(Raw, FFT) :-
	length(Raw, N),
#if defined(CIAO3)
	'$trust_type'(N, number),
#endif
	fft(N, Raw, FFT, fwd).

inv_fft(FFT, Raw) :-
	length(FFT, N),
#if defined(CIAO3)
	'$trust_type'(N, number),
#endif
	fft(N, FFT, Mid, inv),
	scale(Mid, N, Raw).

#if (OPT_MASK & OPT_IMP)
% :- '$props'(fft/4, [imp=semidet]).
#endif
fft(1, [X], [C], _) :- !,
	complex_val(X, C).
fft(N, Raw, FFT, Dir) :-		
	n_cos_sin(N, Cos, Sin),
	pack_w(Dir, Cos, Sin, W),
	M is N>>1,
#if defined(CIAO3)
	'$trust_type'(M, number),
#endif
	evens_and_odds(Raw, E, O),
	fft(M, E, Ef, Dir),
	fft(M, O, Of, Dir),
	fft_2(Ef, Of, W, (1.0,0.0), Z, FFT, FF2),
#if defined(CIAO3)
        % TODO: a difference-list analysis is required to infeer that FFT is a list of pairs of floats!
	fft_2(Ef, Of, W, Z, _, FF2, []),
	'$trust_type'(FFT, reclist(str(','/2, [float,float]))). 
#else
	fft_2(Ef, Of, W, Z, _, FF2, []). 
#endif


#if (OPT_MASK & OPT_IMP)
% :- '$props'(pack_w/4, [imp=semidet]).
% :- '$trust_entry'(pack_w/4, sht, [atom(fwd), any, any, any]).
% :- '$props'(pack_w/4, [sht_usermemo = shtdef([atom(fwd), any, any, any], [any,any,any,any])]).
% TODO: if check events is wrong, this property makes the program fail with a segfault
% :- '$props'(pack_w/4, [indexed=false]).
#endif
pack_w(fwd, C, S, (C,S)).
pack_w(inv, C, S, (C,Z)) :- Z is -S.

#if (OPT_MASK & OPT_IMP)
% :- '$props'(fft_2/7, [imp=semidet]).
% :- '$trust_entry'(fft_2/7, sht, [nonvar, any, any, any, any, any, any]).
% :- '$props'(fft_2/7, [sht_usermemo = shtdef([nonvar, any, any, any, any, any, any], [any,any,any,any,any,any,any])]).
#endif
fft_2([], [], _, Z, Z, F, F).
fft_2([E|Es], [O|Os], W, Z0, Z, [F|Fs], Fl) :-
	complex_mul(Z0, O, Zt),
	complex_add(Zt, E, F),
	complex_mul(Z0, W, Z1),
	fft_2(Es, Os, W, Z1, Z, Fs, Fl).

#if (OPT_MASK & OPT_IMP)
% :- '$props'(evens_and_odds/3, [imp=semidet]).
% :- '$props'(evens_and_odds/3, [sht_usermemo = shtdef([nonvar, any, any], [any,any,any])]).
#endif
evens_and_odds([], [], []).
evens_and_odds([E,O|EOs], [E|Es], [O|Os]) :-
	evens_and_odds(EOs, Es, Os).

#if (OPT_MASK & OPT_IMP)
% :- '$props'(scale/3, [imp=semidet]).
#endif
scale([], _, []).
scale([(Ra,Ia)|Xs], Scale, [(Rs,Is)|Ys]) :-
	Rs is Ra/Scale,
	Is is Ia/Scale,
	scale(Xs, Scale, Ys).

#if (OPT_MASK & OPT_IMP)
% :- '$props'(complex_val/2, [imp=semidet]).
#endif
complex_val((Ra,Ia), (Rs,Is)) :- !,
	Rs is Ra*1.0,
	Is is Ia*1.0.
complex_val(Ra, (Rs,0.0)) :-
	Rs is Ra*1.0.

#if (OPT_MASK & OPT_IMP)
% :- '$props'(complex_add/3, [imp=semidet]).
#endif
complex_add((Ra,Ia), (Rb,Ib), (Rs,Is)) :-
	Rs is Ra+Rb,
	Is is Ia+Ib.

#if (OPT_MASK & OPT_IMP)
% :- '$props'(complex_mul/3, [imp=semidet]).
#endif
complex_mul((Ra,Ia), (Rb,Ib), (Rs,Is)) :-
	Rs is Ra*Rb-Ia*Ib,
	Is is Ra*Ib+Rb*Ia.

#if (OPT_MASK & OPT_IMP)
% :- '$props'(complex_exp/2, [imp=semidet]).
#endif
complex_exp(Ang, (Rs,Is)) :-
	Rs is cos(Ang),
	Is is sin(Ang).

%   n_cos_sin(N, C, S) is true when N is 2^K for K=1..23,
%   C is cos(2.pi/N), and S is sin(2.pi/N).

#if (OPT_MASK & OPT_IMP)
% :- '$props'(n_cos_sin/3, [imp=semidet]).
#endif
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

numlist(I, N, []) :-
	I > N, !.
numlist(I, N, [I|L]) :-
	I =< N, J is I+1,
	numlist(J, N, L).


