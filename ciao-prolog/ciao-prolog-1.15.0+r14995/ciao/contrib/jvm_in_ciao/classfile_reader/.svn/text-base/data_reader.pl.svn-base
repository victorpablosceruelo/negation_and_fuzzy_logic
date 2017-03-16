:- module(data_reader,[read_u1/2,read_u2/2,read_u4/2,read_u8/2,
	               read_s1/2,read_s2/2,read_s4/2,read_utf8/2,skip/2,
		       read_long/2,read_float/2,read_double/2]).

read_u1(Fin,U1) :-
	get_code(Fin,U1).

read_u2(FIn,U2) :-
	get_code(FIn,B1),
	get_code(FIn,B2),
	U2 is ((B1 << 8) + B2).

read_u4(FIn,U4) :-
	get_code(FIn,B1),
	get_code(FIn,B2),
	get_code(FIn,B3),
	get_code(FIn,B4),
	U4 is (B1 << 24 + B2 << 16 + B3 << 8 + B4).

read_u8(FIn,U8) :-
	read_u4(FIn,HBytes),
	read_u4(FIn,LBytes),
	U8 is (HBytes << 32 + LBytes).

read_s1(FIn,S1) :-
	read_u1(FIn,U1),
	sign_ext_u1(U1,S1).

read_s2(FIn,S2) :-
	read_u2(FIn,U2),
	sign_ext_u2(U2,S2).

read_s4(FIn,S4) :-
	read_u4(FIn,U4),
	sign_ext_u4(U4,S4).  

read_long(FIn,S8) :-
	read_u8(FIn,U8),
	sign_ext_u8(U8,S8). 

sign_ext_u1(Bits,Bits) :-
	(Bits >> 7) =:= 0.

sign_ext_u1(Bits,S1) :-
	(Bits >> 7) =:= 1,
	S1 is (Bits - 256).

sign_ext_u2(Bits,Bits) :-
	(Bits >> 15) =:= 0.

sign_ext_u2(Bits,S2) :-
	(Bits >> 15) =:= 1,
	S2 is (Bits - 65536).  

sign_ext_u4(Bits,Bits) :-
	(Bits >> 31) =:= 0.

sign_ext_u4(Bits,S4) :-
	(Bits >> 31) =:= 1,
	S4 is (Bits - 4294967296).

sign_ext_u8(Bits,Bits) :-
	(Bits >> 63) =:= 0.
sign_ext_u8(Bits,S8) :-
	(Bits >> 63) =:= 1,
	S8 is (Bits - 18446744073709551616).
%sign_ext_u2(Bits,S2) :- (Bits >> 15) =:= 1,S2 is (Bits \/ 0xffffff00).

:- push_prolog_flag(multi_arity_warnings,off).
read_utf8(FIn,Utf) :-
	read_u2(FIn,L),
	read_utf8(FIn,[],Utf,L).

read_utf8(_,Ac,Utf,0) :-
	!,
	reverse(Ac,Utf).

read_utf8(FIn,Acu,Utf,N) :-
	get_code(FIn,C),
	NewN is N - 1,
	read_utf8(FIn,[C|Acu],Utf,NewN).
:- pop_prolog_flag(multi_arity_warnings).

skip(_,0) :- !.
skip(FIn,N) :-
	get_code(FIn,_),
	M is N - 1,
	skip(FIn,M).

read_double(FIn,2) :-
	skip(FIn,8).
%	read_u4(FIn,_),
%	read_u4(FIn,_).
read_float(FIn,1) :- 
	read_u4(FIn,_U4),!.
read_float(FIn,F) :-
	read_u4(FIn,U4),
	%F is (float) U4,
	%write(F),
	extract_sign(U4,S),
	write(S),nl,
	extract_exponent(U4,E),
	write(E),nl,
	extract_mantissa(U4,E,M),
	write(M),nl,
	F = float(s(S),m(M),e(E)).
	%F is S * (M ** (E-150)).

/*
test_float :-
	open('floatneg',read,FIn),
	read_float(FIn,F),
	write(F),
	close(FIn).
*/

extract_sign(Bits,1) :-
	(Bits >> 31) =:= 0.

extract_sign(Bits,-1) :-
	(Bits >> 31) =:= 1.

extract_exponent(Bits,E) :-
	E is ((Bits >> 23) /\ 0xff).

extract_mantissa(Bits,0,M) :-
	!,
	M is ((Bits /\ 0x7fffff) << 1).

extract_mantissa(Bits,_,M) :-
	M is ((Bits /\ 0x7fffff) \/ 0x800000).

%Tratamiento de errores!! 
%En Java hacen: if (ch1 | ch2) < 0 throw new EOFException...
