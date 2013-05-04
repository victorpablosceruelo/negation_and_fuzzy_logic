:-module(binary_operations,_,[assertions,nortchecks,regtypes,basicmodes,nativeprops]).

:-use_module(library(lists), [length/2, append/3]).
:-use_module(library(jvm_in_ciao(interpreter(extended_lists))), [init/3]).


:- prop inf/2.
inf(A,B):- 
	A<B.

:- calls my_number_codes/3 : term*term*term.
:- trust success my_number_codes(+_1,+_2,?_3) : nnegint*int*term => list(_3,bit).
:- trust success my_number_codes(+_1,+_2,?_3) : (int(_1),inf(_1,0),int(_2),term(_3)) => (_3=[_|Bin],list(Bin,bit)).
my_number_codes(Int,2,Bin):-
	number_codes(Int,2,Bin),
	binary(Bin). %% check
my_number_codes(Int,2,[45|Bin]):-
	number_codes(Int,2,[45|Bin]),
	binary(Bin). %% check

:- pred removeLeadingZero(in(Bin1),?Bin2) : binary*term => binary*binary + eval. 
removeLeadingZero(Bin,Bin):-
	Bin=[49|_].
removeLeadingZero([48],[48]).
removeLeadingZero([48|Bin1],Bin):-
	Bin1=[_|_],
	removeLeadingZero(Bin1,Bin).

:- regtype bit/1.
bit(48). % codes 0
bit(49). % codes 1

:- regtype binary/1.
binary(Bin):-
	list(Bin,bit).

:- entry integer_to_binary(in(A),in(B),?C) : int*int*term.
:- pred integer_to_binary(in(A),in(B),?C) : int*int*term => int*int*binary.
:- trust comp integer_to_binary(in(A),in(B),?C) : int*int*term + eval.
integer_to_binary(Int,Bits,Bin):-
	Int < 0,
	Int2 is (-Int)-1,
	my_number_codes(Int2,2,Bin1),
	binary(Bin1), %% check
	complements(Bin1,Bits,Bin).
integer_to_binary(Int,Bits,Bin):-
	Int >= 0,
	my_number_codes(Int,2,Bin1),
	Bin2=[48|Bin1],
	binary(Bin2), %% check
	signed_extension(Bin2,Bits,Bin).

% :- entry complements/3 : binary*integer*term.
% :- pred complements/3 : binary*integer*term => binary*integer*binary + eval.
:- entry complements(in(A),in(B),?C) : binary*int*term.
:- pred complements(in(A),in(B),?C) : binary*int*term => binary*int*binary.
:- trust comp complements(in(A),in(B),?C) : binary*int*term + eval.
complements([],0,[]).
complements([48|LSB],N,[49|RLSB]):-
	length([_|LSB],N),
	N1 is N-1,
	integer(N1), %% check
	complements(LSB,N1,RLSB).
complements([49|LSB],N,[48|RLSB]):-
	length([_|LSB],N),
	N1 is N-1,
	integer(N1), %% check
	complements(LSB,N1,RLSB).
complements(UB,N,[49|RLSB]):-
	length(UB,UBLength),
	UBLength < N,
	N1 is N-1,
	integer(N1), %% check
	complements(UB,N1,RLSB).
complements(UB,N,SB):-
	integer(N),
	length(UB,UBLength),
	UBLength > N,
	UB=[_|LSB],
	complements(LSB,N,SB).

% :- entry signed_extension/3 : binary*integer*term.
% :- pred signed_extension/3 : binary*integer*term => binary*integer*binary + eval.
:- entry signed_extension(in(Bin1),in(NbBits),?Bin2) : binary*int*term.
:- pred signed_extension(in(Bin1),in(NbBits),?Bin2) : 
	binary*int*term => binary*int*binary.
:- trust success signed_extension(in(Bin1),in(NbBits),?Bin2) : 
	binary*int*term => binary(Bin2).
:- trust comp signed_extension(in(Bin1),in(NbBits),?Bin2) : 
	binary*int*term + eval.
signed_extension(L,N,L):-
	length(L,N),
	binary(L).
signed_extension([B|L],N,[B|RL]):-
	length([_|L],Nb),
	Nb < N,
	N1 is N-1,
	integer(N1),
	signed_extension([B|L],N1,RL),
	binary([B|RL]).
signed_extension(L1,N,L2):-
	length(L1,Nb),
	Nb > N,
	length(L2,N),
	append(_,L2,L1),
	binary(L2).

:- entry signed_interpretation(in(Binary),in(NbBits),?Int) : 
	binary*int*term.
:- pred signed_interpretation(in(Binary),in(NbBits),?Int) : 
	binary*int*term
     => binary*int*int.
:- trust comp signed_interpretation(in(Binary),in(NbBits),?Int) : 
	binary*int*term + eval.
signed_interpretation(Bin1,Nb,Int):-
	signed_extension(Bin1,Nb,Bin2),
	Bin2=[48|_],
	removeLeadingZero(Bin2,Bin3),
	my_number_codes(Int,2,Bin3),
	integer(Int). %% check
signed_interpretation(Bin1,Nb,Int):-
	signed_extension(Bin1,Nb,Bin2),
	Bin2=[49|_],
	binary(Bin2), %% check
	complements(Bin2,Nb,Bin3),
	my_number_codes(Int1,2,Bin3),
	Int is -(Int1+1),
	integer(Int).

:- entry bitewiseOr(+Bin1,+Bin2,?BinRes) : binary*binary*term.
:- pred bitewiseOr(+Bin1,+Bin2,?BinRes) : binary*binary*term => binary*binary*binary.
:- trust comp bitewiseOr(+Bin1,+Bin2,?BinRes) : binary*binary*term + eval.
bitewiseOr([],[],[]).
bitewiseOr([48|Bin1],[48|Bin2],[48|BinR]):-
	bitewiseOr(Bin1,Bin2,BinR).
bitewiseOr([49|Bin1],[48|Bin2],[49|BinR]):-
	bitewiseOr(Bin1,Bin2,BinR).
bitewiseOr([48|Bin1],[49|Bin2],[49|BinR]):-
	bitewiseOr(Bin1,Bin2,BinR).
bitewiseOr([49|Bin1],[49|Bin2],[49|BinR]):-
	bitewiseOr(Bin1,Bin2,BinR).

:- entry bitewiseAnd(+Bin1,+Bin2,?BinRes) : binary*binary*term.
:- pred bitewiseAnd(in(Bin1),in(Bin2),go(BinRes)) : binary*binary*term => binary*binary*binary.
:- trust comp bitewiseAnd(+Bin1,+Bin2,?BinRes) : binary*binary*term + eval.
bitewiseAnd([],[],[]).
bitewiseAnd([48|Bin1],[48|Bin2],[48|BinR]):-
	bitewiseAnd(Bin1,Bin2,BinR).
bitewiseAnd([49|Bin1],[48|Bin2],[48|BinR]):-
	bitewiseAnd(Bin1,Bin2,BinR).
bitewiseAnd([48|Bin1],[49|Bin2],[48|BinR]):-
	bitewiseAnd(Bin1,Bin2,BinR).
bitewiseAnd([49|Bin1],[49|Bin2],[49|BinR]):-
	bitewiseAnd(Bin1,Bin2,BinR).

:- entry bitewiseXOr(+Bin1,+Bin2,?BinRes) : binary*binary*term.
:- pred bitewiseXOr(+Bin1,+Bin2,go(BinRes)) : binary*binary*term => binary*binary*binary.
:- trust comp bitewiseXOr(+Bin1,+Bin2,?BinRes) : binary*binary*term + eval.
bitewiseXOr([],[],[]).
bitewiseXOr([48|Bin1],[48|Bin2],[48|BinR]):-
	bitewiseXOr(Bin1,Bin2,BinR).
bitewiseXOr([49|Bin1],[48|Bin2],[49|BinR]):-
	bitewiseXOr(Bin1,Bin2,BinR).
bitewiseXOr([48|Bin1],[49|Bin2],[49|BinR]):-
	bitewiseXOr(Bin1,Bin2,BinR).
bitewiseXOr([49|Bin1],[49|Bin2],[48|BinR]):-
	bitewiseXOr(Bin1,Bin2,BinR).


% conversions
:- entry conversion(in(NbBits),in(Int),?Binary) : int*int*term.
:- pred conversion(in(NbBits),in(Int),go(Binary)) : int*int*term => int*int*int.
:- success conversion(NbBits,Int,Binary)
     => conversion2(NbBits,Int,Binary).
:- trust comp conversion(in(NbBits),in(Int),?Binary) : int*int*term + eval.
conversion(Bits,I,B):-
	I>=0,
	B1 is I /\ truncate(2**Bits-1),
	B1 > truncate(2**(Bits-1)-1),
	B is truncate(-(2**Bits)+B1),
	integer(B). %% check
%	check((conversion2(Bits,I,B))).
conversion(Bits,I,B):-
	I>=0,
	B is I /\ truncate(2**Bits-1),
	B =< truncate(2**(Bits-1))-1,
	integer(B). %% check
%	check((conversion2(Bits,I,B))).
conversion(Bits,I,B):-
	I<0,
	B1 is -I /\ truncate(2**Bits-1),
	B1 > truncate(2**(Bits-1)),
	B is truncate(2**Bits)-B1,
	integer(B). %% check
%	check((conversion2(Bits,I,B))).
conversion(Bits,I,B):-
	I<0,
	B1 is -I /\ truncate(2**Bits-1),
	B1 =< truncate(2**(Bits-1)),
	B is truncate(-B1),
	integer(B). %% check
%	check((conversion2(Bits,I,B))).

:- prop conversion2/3.
:- pred conversion2(in(NbBits),in(Int),in(Binary)) : 
	int*int*int => int*int*int.
:- trust comp conversion2(in(NbBits),in(Int),?Binary) : int*int*term + eval.
conversion2(Bits,I,B):-
	integer_to_binary(I,Bits,Bin),signed_interpretation(Bin,Bits,B).

:- trust comp bin_i2bool(in(Int),?Byte) : int*term + eval.
:- trust comp bin_i2bool(I,B) : var(I) + memo.
:- trust comp bin_i2bool/2 + (bind_ins,sideff(free)).
bin_i2bool(I,B):-
	B is I mod 2.

:- entry bin_i2b(in(Int),?Byte) : int*term.
:- pred bin_i2b(in(Int),go(Byte)) : int*term => int*int.
:- trust comp bin_i2b(in(Int),?Byte) : int*term + eval.
:- trust comp bin_i2b(I,B) : var(I) + memo.
:- trust comp bin_i2b/2 + (bind_ins,sideff(free)).
bin_i2b(Int,Byte):-
	conversion(8,Int,Byte).

:- entry bin_i2s(in(Int),?Short) : int*term.
:- pred bin_i2s(in(Int),go(Short)) : int*term => int*int.
:- trust comp bin_i2s(in(Int),?Short) : int*term + eval.
:- trust comp bin_i2s(I,B) : var(I) + memo.
:- trust comp bin_i2s/2 + (bind_ins,sideff(free)).
bin_i2s(Int,Short):-
	conversion(16,Int,Short).

:- entry bin_l2i(in(Long),?Int) : int*term.
:- pred bin_l2i(in(Long),?Int) : int*term => int*int.
:- trust comp bin_l2i(in(Long),?Int) : int*term + (sideff(free),eval).
:- trust comp bin_l2i(I,B) : var(I) + memo.
:- trust comp bin_l2i/2 + (bind_ins,sideff(free)).
bin_l2i(Long,Int):-
	Long1 is Long, % because l2i is used in checks
	conversion(32,Long1,Int).

% operations on int

:- entry bin_addInt(in(Int1),in(Int2),?Int) : int*int*term.
:- pred bin_addInt(in(Int1),in(Int2),?Int) : int*int*term => int*int*int.
:- trust comp bin_addInt(in(Int1),in(Int2),?Int) : int*int*term + (sideff(free),eval,not_fails).

bin_addInt(I1,I2,R):-
	R is I1+I2,
	integer(R), %% check
	R >= -(2**31),
	R < 2**31.
%	check((bin_l2i(I1+I2,R))).
bin_addInt(I1,I2,R):-
	R1 is I1+I2,
	R1 < -truncate(2**31),
	R is I1+I2+truncate(2**32),
	integer(R). %% check
%	check((bin_l2i(I1+I2,R))).
bin_addInt(I1,I2,R):-
	R1 is I1+I2,
	R1 >= 2**31,
	R is I1+I2-truncate(2**32),
	integer(R). %% check
%	check((bin_l2i(I1+I2,R))).

:- entry bin_subInt(in(Int1),in(Int2),?Int) : int*int*term.
:- pred bin_subInt(in(Int1),in(Int2),?Int) : int*int*term => int*int*int.
:- trust comp bin_subInt(in(Int1),in(Int2),?Int) : int*int*term + (sideff(free),eval).
bin_subInt(I1,I2,R):-
	R is I1-I2,
	integer(R), %% check
	R >= -(2**31),
	R < 2**31.
%	check((bin_l2i(I1-I2,R))).
bin_subInt(I1,I2,R):-
	R1 is I1-I2,
	R1 < -(2**31),
	R is (I1-I2)+truncate(2**32),
	integer(R). %% check
%	check((bin_l2i(I1-I2,R))).
bin_subInt(I1,I2,R):-
	R1 is I1-I2,
	R1 >= 2**31,
	R is (I1-I2)-truncate(2**32),
	integer(R). %% check
%	check((bin_l2i(I1-I2,R))).

% comment from the JVM specification of Sun :
%  An int division rounds towards 0; that is, the quotient produced for int values
% in n/d is an int value q whose magnitude is as large as possible while satisfying 
% |d.q| =< |n|.
% Moreover, q is positive when |n|>=|d| and n and d have the same sign, but q is negative when
% |n| >= |d| and n and d have opposite signs. 
%  There is one special case that does not satisfy this rule: if the dividend is the
% negative integer of largest possible magnitude for the int type, and the divisor 
% is -1, then overflow occurs, and the result is equal to the dividend. Despite the 
% overflow, no exception is thrown in this case. 

:- entry bin_divInt(in(Int1),in(Int2),?Int) : int*int*term.
:- pred bin_divInt(in(Int1),in(Int2),?Int) : int*int*term => int*int*int.
:- trust comp bin_divInt(in(Int1),in(Int2),?Int) : int*int*term + (sideff(free),eval).
bin_divInt(-2147483648,-1,-2147483648).
bin_divInt(I1,I2,R):-
	\+ (I1 = -2147483648, I2 = -1),
	R is I1 // I2.
%	check(abs(I2*R) =< abs(I1)).
% divInt(I1,I2,0):-
% 	abs(I2) > abs(I1).
% divInt(I1,I2,R):-
% 	abs(I1) >= abs(I2),
% 	I1 * I2 > 0, % same sign
% 	I1b is I1 - I2,
% 	integer(I1b), %% check
% 	divInt(I1b,I2,Rb),
% 	R is 1 + Rb,
% 	integer(R). %% check
% divInt(I1,I2,R):-
% 	abs(I1) >= abs(I2),
% 	I1 * I2 < 0, % different signs,
% 	I1b is I1 + I2,
% 	integer(I1b), %% check
% 	divInt(I1b,I2,Rb),
% 	R is -(1+Rb),
% 	integer(R). %% check

:- entry bin_mulInt(in(Int1),in(Int2),?Int) : int*int*term.
:- pred bin_mulInt(in(Int1),in(Int2),?Int) : int*int*term => int*int*int.
:- trust comp bin_mulInt(in(Int1),in(Int2),?Int) : int*int*term + (sideff(free),eval).
:- trust comp bin_mulInt/3 + sideff(free).
bin_mulInt(I1,I2,R):-
	Mul is truncate(I1*I2),
	integer(Mul),
	bin_l2i(Mul,R).

:- entry bin_remInt(in(Int1),in(Int2),?Int) : int*int*term.
:- pred bin_remInt(in(Int1),in(Int2),?Int) : int*int*term => int*int*int.
:- trust comp bin_remInt(in(Int1),in(Int2),?Int) : int*int*term + (sideff(free),eval).
bin_remInt(I1,I2,R):-
	bin_divInt(I1,I2,Div),
	bin_mulInt(Div,I2,Mul),
	bin_subInt(I1,Mul,R).

:- entry bin_shlInt(in(Int1),in(Int2),?Int) : int*int*term.
:- pred bin_shlInt(in(Int1),in(Int2),?Int) : int*int*term => int*int*int.

:- trust comp bin_shlInt(A,B,C) : (int(A),int(B)) + memo. 
:- trust comp bin_shlInt(A,B,C) : var(A) + memo. 
:- trust comp bin_shlInt(A,B,C) : var(B) + memo. 
:- trust comp bin_shlInt/3 + (sideff(free),bind_ins).
bin_shlInt(I1,I2,R):-
%	integer(I1),integer(I2),
	integer_to_binary(I1,32,I1b),
	length(NL,I2),
	extended_lists:init(NL,48,I2),
%	binary(NL), %% check
	append(I1b,NL,R1),
%	binary(I1b),binary(NL),
%	binary(R1), %% check
	signed_interpretation(R1,32,R).

:- entry bin_shrInt(in(Int1),in(Int1),?Int) : int*int*term.
:- pred bin_shrInt(in(Int1),in(Int1),?Int) : int*int*term => int*int*int.
:- trust comp bin_shrInt(A,B,C) : (int(A),int(B)) + memo. 
:- trust comp bin_shrInt(A,B,C) : var(A) + memo. 
:- trust comp bin_shrInt(A,B,C) : var(B) + memo. 
:- trust comp bin_shrInt/3 + (sideff(free),bind_ins).
bin_shrInt(I1,I2,R):-
% 	integer(I1),integer(I2),
	integer_to_binary(I1,32,I1b),
% 	binary(I1b),
	length(NL,I2),
	append(R1,NL,I1b),
% 	binary(R1),binary(NL),binary(I1b),
	signed_interpretation(R1,32,R).

:- entry bin_ushrInt(in(Int1),in(Int2),?Int) : int*int*term.
:- pred bin_ushrInt(in(Int1),in(Int2),?Int) : int*int*term => int*int*int.
:- trust comp bin_ushrInt(A,B,C) : (int(A),int(B)) + memo. 
:- trust comp bin_ushrInt(A,B,C) : var(A) + memo. 
:- trust comp bin_ushrInt(A,B,C) : var(B) + memo. 
:- trust comp bin_ushrInt/3 + (sideff(free),bind_ins).
bin_ushrInt(I1,I2,R):-
	integer_to_binary(I1,32,I1b),
	length(NL,I2),
	append(R1,NL,I1b),
	length(NL2,I2),
	extended_lists:init(NL2,48,I2),
	append(NL2,R1,R2),
	signed_interpretation(R2,32,R).

:- entry bin_orInt(in(Int1),in(Int2),?Int) : int*int*term.
:- pred bin_orInt(in(Int1),in(Int2),?Int) : int*int*term => int*int*int.
:- trust comp bin_orInt(A,B,C) : (int(A),int(B)) + memo. 
:- trust comp bin_orInt(A,B,C) : var(A) + memo. 
:- trust comp bin_orInt(A,B,C) : var(B) + memo. 
:- trust comp bin_orInt/3 + (sideff(free),bind_ins).
bin_orInt(I1,I2,R):-
	integer_to_binary(I1,32,Bin1),
	integer_to_binary(I2,32,Bin2),
	bitewiseOr(Bin1,Bin2,R1),
	signed_interpretation(R1,32,R).

:- entry bin_andInt(in(Int1),in(Int2),?Int) : int*int*term.
:- pred bin_andInt(in(Int1),in(Int2),?Int) : int*int*term => int*int*int.
:- trust comp bin_andInt(A,B,C) : (int(A),int(B)) + memo. 
:- trust comp bin_andInt(A,B,C) : var(A) + memo. 
:- trust comp bin_andInt(A,B,C) : var(B) + memo. 
:- trust comp bin_andInt/3 + (sideff(free),bind_ins).
bin_andInt(I1,I2,R):-
	integer_to_binary(I1,32,Bin1),
	integer_to_binary(I2,32,Bin2),
	bitewiseAnd(Bin1,Bin2,R1),
	signed_interpretation(R1,32,R).

:- entry bin_xorInt(in(Int1),in(Int2),?Int) : int*int*term.
:- pred bin_xorInt(in(Int1),in(Int2),?Int) : int*int*term => int*int*int.
:- trust comp bin_xorInt(in(Int1),in(Int2),?Int) : int*int*term + eval.
:- trust comp bin_xorInt(I1,I2,R) : var(I1) + memo.
:- trust comp bin_xorInt(I1,I2,R) : var(I2) + memo.
:- trust comp bin_xorInt/3 + (bind_ins,sideff(free)).
bin_xorInt(I1,I2,R):-
	integer_to_binary(I1,32,Bin1),
	integer_to_binary(I2,32,Bin2),
	bitewiseXOr(Bin1,Bin2,R1),
	signed_interpretation(R1,32,R).

:- entry bin_negInt(in(Int1),go(Int)) : int*term.
:- pred bin_negInt(in(Int1),go(Int)) : int*term => int*int.
bin_negInt(-2147483648,-2147483648).
	% -2147483648 = -2**31, it is not changed by the neg operation because of the representation
	% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ineg 
bin_negInt(I1,R):-
	I1 =\= -2147483648,
	R is -I1,
	integer(R).
