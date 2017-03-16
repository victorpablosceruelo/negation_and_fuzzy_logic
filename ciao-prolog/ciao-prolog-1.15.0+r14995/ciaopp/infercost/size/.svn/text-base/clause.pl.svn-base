:- module(clause,
	[
	    ith_clause_literal/3,
	    number_of_literals/3,
            ith_body_literal/3,
	    clause_term_measure/6
	], [assertions]).

%
%  clause.pl			Nai-Wei Lin			December, 1991
%
%  This file contains the procedures for finding information about a clause
%  or a literal in a clause.
%

:- use_module(infercost(init(symtable)), [literal_property/5]).
:- use_module(infercost(init(initsystem_basic)), [clause_type/2]).
:- use_module(infercost(top(utility)), 
	[
	    ith_list_element/3,
	    nonsequence/1
	]).
:- use_module(infercost(dependency(position)), [pos_litnum/2, pos_argnum/2]).
:- use_module(infercost(init(builtin)), 
	[second_order_predicate_pred_arg/2]).


%
%  Get the term and its measure at an argument position of a clause.
%
:-test clause_term_measure(A,B,C,D,E,F):(A=[st((\+)/1,G,[+,+],[?,?],H,[1],I,inf,[0],J),st(is/2,K,[-,+],[int,int],L,[1],[],inf,[calc],M),st(= /2,N,[?,?],[?,?],O,[1],[],inf,[0],P),st(functor/3,Q,[+,-,-],[size,size,int],R,[1],[],inf,[prof],S),st(arg/3,T,[+,+,-],[int,size,size],U,[1],[],inf,[prof],V),st(functor1/3,W,[-,+,+],[size,size,int],X,[1],[],inf,[0],Y),st(arg/4,Z,[+,+,+,-],[int,size,size,size],A1,[1],[],inf,[0],B1),st(== /2,C1,[+,+],[?,?],D1,[1],E1,inf,[0],F1),st(\== /2,G1,[+,+],[?,?],H1,[1],I1,inf,[0],J1),st(=:= /2,K1,[+,+],[int,int],L1,[1],M1,inf,[calc],N1),st(=\= /2,O1,[+,+],[int,int],P1,[1],Q1,inf,[calc],R1),st(< /2,S1,[+,+],[int,int],T1,[1],U1,inf,[calc],V1),st(> /2,W1,[+,+],[int,int],X1,[1],Y1,inf,[calc],Z1),st(=< /2,A2,[+,+],[int,int],B2,[1],C2,inf,[calc],D2),st(>= /2,E2,[+,+],[int,int],F2,[1],G2,inf,[calc],H2),st(atomic/1,I2,[+],[void],J2,[1],K2,inf,[prof],L2),st('lists:nth'/3,M2,[+,+,-],[int,length,void],N2,[1],O2,inf,[1],P2),st(atom/1,Q2,[+],[void],R2,[1],S2,inf,[prof],T2),st(number/1,U2,[+],[void],V2,[1],W2,inf,[prof],X2),st(integer/1,Y2,[+],[void],Z2,[1],A3,inf,[prof],B3),st(float/1,C3,[+],[void],D3,[1],E3,inf,[prof],F3),st(nonvar/1,G3,[+],[void],H3,[1],I3,inf,[prof],J3),st(write/1,K3,[+],[void],L3,[1],M3,inf,[0],N3),st(tab/1,O3,[+],[void],P3,[1],Q3,inf,[0],R3),st(nl/0,S3,[],[],T3,[1],U3,1,[0],V3),st(fail/0,W3,[],[],X3,[0],Y3,0,[0],Z3),st(true/0,A4,[],[],B4,[1],C4,1,[prof],D4),st(!/0,E4,[],[],F4,[1],G4,1,[prof],H4),st(findall/3,I4,[-,+,-],[void,void,length],J4,[1],K4,inf,[0],L4)|M4],B=[st('append:append'/3,['append:append'([],N4,N4):'append:append/3/1',('append:append'([O4|P4],Q4,[O4|R4]):-'append:append'(P4,Q4,R4)):'append:append/3/2'|S4],[+,+,-],[length,length,length],[[1],[2]],T4,U4,V4,W4,X4)|Y4],C='append:append'([],N4,N4),D= $(0,2),E=N4)=>(F=length).

clause_term_measure(BT,ST,Clause,Pos,Term,Measure) :-
	pos_litnum(Pos,LitNum),
	%write(LitNum),nl,
	(LitNum > 0 ->
		(arg(2,Clause,Body),
		 number_of_literals(Body,1,Num),
		 (LitNum > Num ->
			NewNum is LitNum-Num;	% assume depth-one single pred
			NewNum = LitNum));
		NewNum = LitNum),
	ith_clause_literal(NewNum,Clause,Lit),
	%write(Lit),nl,
	((LitNum > 0, LitNum > Num) ->
		second_order_predicate_pred_arg(Lit,NewLit);
		NewLit = Lit),
	pos_argnum(Pos,ArgNum),
	arg(ArgNum,NewLit,Term),
	functor(NewLit,F,A),
	literal_property(BT,ST,F/A,measure,MeasureList),
	ith_list_element(ArgNum,MeasureList,Measure).
	
/*
clause_term_measure(_,ST,Clause,Pos,Term,Measure) :-
	pos_litnum(Pos,0),
	pos_argnum(Pos,N),
	head_term_measure(ST,Clause,N,Term,Measure).
clause_term_measure(BT,ST,Clause,Pos,Term,Measure) :-
	pos_litnum(Pos,M),
	M > 0,
	pos_argnum(Pos,N),
	body_term_measure(BT,ST,Clause,M,N,Term,Measure).

%
%  Get the term and its measure at an argument position of the head.
%
head_term_measure(ST,Clause,N,Term,Measure) :-
	clause_type(Clause,Type),
	(Type =:= 2 ->
		arg(1,Clause,Head);
		Clause = Head),
	arg(N,Head,Term),
	functor(Head,F,A),
	find_symbol_field(ST,F/A,measure,MeasureList),
	ith_list_element(N,MeasureList,Measure).

%
%  Get the term and its measure at an argument position of a body literal.
%
body_term_measure(BT,ST,Clause,M,N,Term,Measure) :-
	arg(2,Clause,Body),
	literal_term_measure(BT,ST,Body,1,M,N,Term,Measure).

%
%  Get the term and its measure at an argument position of a literal.
%
literal_term_measure(BT,ST,(Lit,_),M,M,N,Term,Measure) :-
	arg(N,Lit,Term),
	functor(Lit,F,A),
	find_symbol_field(BT,F/A,measure,MeasureList),
	(var(MeasureList) ->
		find_symbol_field(ST,F/A,measure,MeasureList);
		true),
	ith_list_element(N,MeasureList,Measure).
literal_term_measure(BT,ST,Body,M,M,N,Term,Measure) :-
	nonsequence(Body),
	arg(N,Body,Term),
	functor(Body,F,A),
	find_symbol_field(BT,F/A,measure,MeasureList),
	(var(MeasureList) ->
		find_symbol_field(ST,F/A,measure,MeasureList);
		true),
	ith_list_element(N,MeasureList,Measure).
literal_term_measure(BT,ST,(_,Body),I,M,N,Term,Measure) :-
	I < M,
	I1 is I+1,
	literal_term_measure(BT,ST,Body,I1,M,N,Term,Measure).
literal_term_measure(_,_,Body,I,M,_,_,_) :-
	I > M,
	nonsequence(Body),
        %% Commented out by PLG 3 Aug 99 (ICLP99 demo). 
	%% error_message('illegal argument position').
        %% Added by PLG 3 Aug 99 (ICLP99 demo). 
        error_message(arg1, _, '').
*/
%
%  Get the ith literal in the clause.
%
ith_clause_literal(I,Clause,Lit) :-
	clause_type(Clause,Type),
	ith_clause_literal_(Type,I,Clause,Lit).

ith_clause_literal_(2,0,Clause,Lit) :-
	!,
	arg(1,Clause,Lit).
ith_clause_literal_(2,I,Clause,Lit) :-
	I > 0,
	!,
	arg(2,Clause,Body),
	ith_body_literal(I,Body,Lit).
ith_clause_literal_(3,0,Clause,Clause).

%
%  Get the ith literal in the body.
%
ith_body_literal(1,(Lit,_),Lit) :-
	!.
ith_body_literal(1,Lit,Lit) :-
	!,
	nonsequence(Lit).
ith_body_literal(LitNum,(_,Body),Lit) :-
	LitNum > 1,
	LitNum1 is LitNum-1,
	ith_body_literal(LitNum1,Body,Lit).

%
number_of_literals(Lit,Num,Num) :-
	nonsequence(Lit),
	!.
number_of_literals((_,Body),N1,Num) :-
	N2 is N1+1,
	number_of_literals(Body,N2,Num).
