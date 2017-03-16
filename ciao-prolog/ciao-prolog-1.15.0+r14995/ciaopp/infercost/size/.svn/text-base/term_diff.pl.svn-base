:- module(term_diff, [general_term_diff/7], [assertions]).

%
%  term_diff.pl			Nai-Wei Lin			November, 1991
%
%  This file contains the procedures for performing the diff functions.
%

% Modified by Edison Mera 2006-07-06
% - all predicates tested

:- use_module(infercost(top(utility)), 
	[
	    minimum/3,
	    addition/3,
	    noncompound/1,
	    sub/3,
	    member/2,
	    compound/1
	]).
:- use_module(infercost(size(clause)), [clause_term_measure/6]).

%
%  Compute the size difference between a term and its predecessors.
%

:- test general_term_diff(A,B,C,D,E,F,G):(A=[st((\+)/1,H,[+,+],[?,?],I,[1],J,inf,[0],K),st(is/2,L,[-,+],[int,int],M,[1],[],inf,[calc],N),st(= /2,O,[?,?],[?,?],P,[1],[],inf,[0],Q),st(functor/3,R,[+,-,-],[size,size,int],S,[1],[],inf,[prof],T),st(arg/3,U,[+,+,-],[int,size,size],V,[1],[],inf,[prof],W),st(functor1/3,X,[-,+,+],[size,size,int],Y,[1],[],inf,[0],Z),st(arg/4,A1,[+,+,+,-],[int,size,size,size],B1,[1],[],inf,[0],C1),st(== /2,D1,[+,+],[?,?],E1,[1],F1,inf,[0],G1),st(\== /2,H1,[+,+],[?,?],I1,[1],J1,inf,[0],K1),st(=:= /2,L1,[+,+],[int,int],M1,[1],N1,inf,[calc],O1),st(=\= /2,P1,[+,+],[int,int],Q1,[1],R1,inf,[calc],S1),st(< /2,T1,[+,+],[int,int],U1,[1],V1,inf,[calc],W1),st(> /2,X1,[+,+],[int,int],Y1,[1],Z1,inf,[calc],A2),st(=< /2,B2,[+,+],[int,int],C2,[1],D2,inf,[calc],E2),st(>= /2,F2,[+,+],[int,int],G2,[1],H2,inf,[calc],I2),st(atomic/1,J2,[+],[void],K2,[1],L2,inf,[prof],M2),st('lists:nth'/3,N2,[+,+,-],[int,length,void],O2,[1],P2,inf,[1],Q2),st(atom/1,R2,[+],[void],S2,[1],T2,inf,[prof],U2),st(number/1,V2,[+],[void],W2,[1],X2,inf,[prof],Y2),st(integer/1,Z2,[+],[void],A3,[1],B3,inf,[prof],C3),st(float/1,D3,[+],[void],E3,[1],F3,inf,[prof],G3),st(nonvar/1,H3,[+],[void],I3,[1],J3,inf,[prof],K3),st(write/1,L3,[+],[void],M3,[1],N3,inf,[0],O3),st(tab/1,P3,[+],[void],Q3,[1],R3,inf,[0],S3),st(nl/0,T3,[],[],U3,[1],V3,1,[0],W3),st(fail/0,X3,[],[],Y3,[0],Z3,0,[0],A4),st(true/0,B4,[],[],C4,[1],D4,1,[prof],E4),st(!/0,F4,[],[],G4,[1],H4,1,[prof],I4),st(findall/3,J4,[-,+,-],[void,void,length],K4,[1],L4,inf,[0],M4)|N4],B=[st('peano:pnumvalue'/2,['peano:pnumvalue'(0,0):'peano:pnumvalue/2/1',('peano:pnumvalue'(s(O4),P4):-'peano:pnumvalue'(O4,Q4),P4 is Q4+1):'peano:pnumvalue/2/2'|R4],[+,-],[size,int],[[1],[2]],S4,T4,U4,V4,W4),st('peano:padd'/3,['peano:padd'(0,X4,X4):'peano:padd/3/1',('peano:padd'(s(Y4),Z4,s(A5)):-'peano:padd'(Y4,Z4,A5)):'peano:padd/3/2'|B5],[+,+,-],[size,size,size],C5,D5,E5,F5,G5,H5),st('peano:pmul'/3,['peano:pmul'(0,I5,0):'peano:pmul/3/1',('peano:pmul'(s(J5),K5,L5):-'peano:pmul'(J5,K5,M5),'peano:padd'(K5,M5,L5)):'peano:pmul/3/2'|N5],[+,+,-],[size,size,size],O5,P5,Q5,R5,S5,T5),st('peano:ppow'/3,['peano:ppow'(0,U5,s(0)):'peano:ppow/3/1',('peano:ppow'(s(V5),W5,X5):-'peano:ppow'(V5,W5,Y5),'peano:pmul'(Y5,V5,X5)):'peano:ppow/3/2'|Z5],[+,+,-],[size,size,size],A6,B6,C6,D6,E6,F6)|G6],C=int,D=('peano:pnumvalue'(s(O4),P4):-'peano:pnumvalue'(O4,Q4),P4 is Q4+1),E=[$(2,1)],F=P4)=>(G= $(2,1)).

:- test general_term_diff(A,B,C,D,E,F,G):(A=[st((\+)/1,H,[+,+],[?,?],I,[1],J,inf,[0],K),st(is/2,L,[-,+],[int,int],M,[1],[],inf,[calc],N),st(= /2,O,[?,?],[?,?],P,[1],[],inf,[0],Q),st(functor/3,R,[+,-,-],[size,size,int],S,[1],[],inf,[prof],T),st(arg/3,U,[+,+,-],[int,size,size],V,[1],[],inf,[prof],W),st(functor1/3,X,[-,+,+],[size,size,int],Y,[1],[],inf,[0],Z),st(arg/4,A1,[+,+,+,-],[int,size,size,size],B1,[1],[],inf,[0],C1),st(== /2,D1,[+,+],[?,?],E1,[1],F1,inf,[0],G1),st(\== /2,H1,[+,+],[?,?],I1,[1],J1,inf,[0],K1),st(=:= /2,L1,[+,+],[int,int],M1,[1],N1,inf,[calc],O1),st(=\= /2,P1,[+,+],[int,int],Q1,[1],R1,inf,[calc],S1),st(< /2,T1,[+,+],[int,int],U1,[1],V1,inf,[calc],W1),st(> /2,X1,[+,+],[int,int],Y1,[1],Z1,inf,[calc],A2),st(=< /2,B2,[+,+],[int,int],C2,[1],D2,inf,[calc],E2),st(>= /2,F2,[+,+],[int,int],G2,[1],H2,inf,[calc],I2),st(atomic/1,J2,[+],[void],K2,[1],L2,inf,[prof],M2),st('lists:nth'/3,N2,[+,+,-],[int,length,void],O2,[1],P2,inf,[1],Q2),st(atom/1,R2,[+],[void],S2,[1],T2,inf,[prof],U2),st(number/1,V2,[+],[void],W2,[1],X2,inf,[prof],Y2),st(integer/1,Z2,[+],[void],A3,[1],B3,inf,[prof],C3),st(float/1,D3,[+],[void],E3,[1],F3,inf,[prof],G3),st(nonvar/1,H3,[+],[void],I3,[1],J3,inf,[prof],K3),st(write/1,L3,[+],[void],M3,[1],N3,inf,[0],O3),st(tab/1,P3,[+],[void],Q3,[1],R3,inf,[0],S3),st(nl/0,T3,[],[],U3,[1],V3,1,[0],W3),st(fail/0,X3,[],[],Y3,[0],Z3,0,[0],A4),st(true/0,B4,[],[],C4,[1],D4,1,[prof],E4),st(!/0,F4,[],[],G4,[1],H4,1,[prof],I4),st(findall/3,J4,[-,+,-],[void,void,length],K4,[1],L4,inf,[0],M4)|N4],B=[st('append:append'/3,['append:append'([],O4,O4):'append:append/3/1',('append:append'([P4|Q4],R4,[P4|S4]):-'append:append'(Q4,R4,S4)):'append:append/3/2'|T4],[+,+,-],[length,length,length],[[1],[2]],U4,V4,W4,X4,Y4)|Z4],C=length,D='append:append'([],O4,O4),E=[$(0,2)],F=O4)=>(G= $(0,2)).

general_term_diff(BT, ST, Measure, Clause, PList, Term, Size) :-
	general_term_diff_(PList, BT, ST, Measure, Clause, Term, Size).

general_term_diff_([],         _BT, _ST, _Measure, _Clause, _Term, bot).
general_term_diff_([Pos|PList], BT,  ST,  Measure,  Clause,  Term, Size) :-
	clause_term_measure(BT, ST, Clause, Pos, PosTerm, PosMeasure),
	term_diff(PosMeasure, Measure, Pos, PosTerm, Term, Size1),
	general_term_diff_(PList, BT, ST, Measure, Clause, Term, Size2),
	minimum(Size1, Size2, Size).

%
%  Compute the size difference between two terms.
%

:- test term_diff(A,B,C,D,E,F):(A=length,B=length,C=0,D=[G|H],E=H)=>(F= -1).
:- export(term_diff/6).

term_diff(M1, M2,_Pos,_Term1,_Term2,bot) :-
	M1 \== (?),
	M2 \== (?),
	M1 \== M2,
	!.
term_diff((?),(?),_Pos,_Term1,_Term2,bot) :-
	!.
term_diff((?), M2, Pos, Term1, Term2,Size) :-
	M2 \== (?),
	!,
	term_diff(M2,Pos,Term1,Term2,Size).
term_diff(M1,(?),Pos,Term1,Term2,Size) :-
	M1 \== (?),
	!,
	term_diff(M1,Pos,Term1,Term2,Size).
term_diff(M1,M2,Pos,Term1,Term2,Size) :-
	M1 == M2,
	M1 \== (?),
	!,
	term_diff(M1,Pos,Term1,Term2,Size).

:- push_prolog_flag(multi_arity_warnings, off).

:- test term_diff(A,B,C,D,E):(A=length,B=5,C=[F|G],D=G)=>(E=4).
:- export(term_diff/5).

term_diff(int,Pos,Term1,Term2,Size) :-
	term_diff_int(Term1,Term2,Size1),
	addition(Pos,Size1,Size).
term_diff(length,Pos,Term1,Term2,Size) :-
	term_diff_length(Term1,Term2,Size1),
	addition(Pos,Size1,Size).
term_diff(depth(ChildList),Pos,Term1,Term2,Size) :-
	term_diff_depth(ChildList,Term1,Term2,Size1),
	addition(Pos,Size1,Size).
term_diff(size,Pos,Term1,Term2,Size) :-
	term_diff_size(Pos,Term1,Term2,Size).

%
%  Compute the size difference between two terms under the measure int.
%

:- test term_diff_int(A,B,C):true=>(A=D,B=E,C=bot).
:- test term_diff_int(A,B,C):(A=B)=>(C=0).
:- export(term_diff_int/3).

term_diff_int(Term1,Term2,0) :-
	Term1 == Term2,
	!.
term_diff_int(Term1,Term2,bot) :-
	Term1 \== Term2.

%
%  Compute the size difference between two terms under the measure length.
%

:- test term_diff_length(A,B,C):(A=[_|E],B=E)=>(C= -1).
:- export(term_diff_length/3).

term_diff_length(Term1,Term2,0) :-
	Term1 == Term2,
	!.
term_diff_length(Term1,Term2,bot) :-
	Term1 \== Term2,
	noncompound(Term1),
	!.
term_diff_length(Term1,Term2,Size) :-
	Term1 \== Term2,
	Term1 = [_|TList],
	!,
	term_diff_length(TList,Term2,Size1),
	sub(Size1,1,Size).

%
%  Compute the size difference between two terms under the measure depth.
%

:- test term_diff_depth(A,B,C,D):(A=[1,2],B=f(E,F),C=E)=>(D= -1).
:- export(term_diff_depth/4).

term_diff_depth(_,Term1,Term2,0) :-
	Term1 == Term2,
	!.
term_diff_depth(_,Term1,Term2,bot) :-
	Term1 \== Term2,
	noncompound(Term1),
	!.
term_diff_depth(ChildList,Term1,Term2,Size) :-
	Term1 \== Term2,
	compound(Term1),
	functor(Term1,_,N),
	term_diff_depth(N,ChildList,Term1,Term2,Size1),
	sub(Size1,1,Size).

:- test term_diff_depth(A,B,C,D,E):(A=2,B=[1,2],C=f(F,G),D=F)=>(E=0).
:- export(term_diff_depth/5).

term_diff_depth(0,_,_,_,bot) :-
	!.
term_diff_depth(N,ChildList,Term1,Term2,Size) :-
	N > 0,
	N1 is N-1,
	(
	    utility:member(ChildList,N) ->
	    (
		arg(N,Term1,Arg),
		term_diff_depth(ChildList,Arg,Term2,SizeN),
		term_diff_depth(N1,ChildList,Term1,Term2,SizeN1),
		minimum(SizeN,SizeN1,Size)
	    )
	;
	    term_diff_depth(N1,ChildList,Term1,Term2,Size)
	).

%
%  Compute the size difference between two terms under the measure size.
%

:- test term_diff_size(A,B,C,D):(A=0,B=s(E),C=E)=>(D=arg(0,1)).
:- export(term_diff_size/4).

term_diff_size(Pos,Term1,Term2,Pos) :-
	Term1 == Term2,
	!.
term_diff_size(Pos,[Head|_],Term,head(Pos)) :-
	Head == Term,
	!.
term_diff_size(Pos,[_|Tail],Term,tail(Pos)) :-
	Tail == Term,
	!.
term_diff_size(_,Term1,Term2,bot) :-
	Term1 \== Term2,
	noncompound(Term1),
	!.
term_diff_size(Pos,Term1,Term2,Size) :-
	Term1 \== Term2,
	compound(Term1),
	functor(Term1,_,N),
	term_diff_size(N,Pos,Term1,Term2,Size).

:- test term_diff_size(A,B,C,D,E):(A=1,B=1,C=s(F),D=F)=>(E=arg(1,1)).
:- export(term_diff_size/5).

term_diff_size(0,_,_,_,bot) :-
	!.
term_diff_size(N,Pos,Term1,Term2,Size) :-
	N > 0,
	N1 is N-1,
	arg(N,Term1,Arg),
	(
	    Arg == Term2 ->
	    Size = arg(Pos,N)
	;
	    term_diff_size(N1,Pos,Term1,Term2,Size)
	).

:- pop_prolog_flag(multi_arity_warnings).

