:- module(gen_c_expre, [trans_c/7], [assertions]).  

:- doc(author,"Pedro L@'{o}pez").  

:- use_module(library(lists), [append/3]). 

:- use_module(name_basic, [temp_name/2]). 
:- use_module(g_utility, [ins_without_dup/2]). 


trans_c(Exp,_,_,TVT,[],TVT,Exp):-
	number(Exp).
trans_c(Exp,AV,_,TVT,[],TVT,ExpName):-
	p_var(Exp,AV,ExpName).
trans_c(Exp,AV,IT,TVT,IL,NTVT,TExp):-
	simple_func(Exp),
	functor(Exp,F,N),
	functor(TExp,F,N),
        trans_c_simple(N,Exp,AV,IT,TVT,IL,NTVT,TExp).
trans_c( sum(Ind,Low,Upp,Exp),AV,IT,TVT,IL,NTVT,Result):- 
	index_name(Ind,IndName),
        ins_without_dup(IT,IndName),
	trans_c(Low,AV,IT,TVT,IL1,TVT1,TLow),
	trans_c(Upp,AV,IT,TVT1,IL2,TVT2,NUpp),
        trans_upp(NUpp,TVT2,IL3,TVT3,TUpp),
        TVT4 is TVT3 + 1,
	temp_name(TVT3,Result),
	trans_c(Exp,AV,IT,TVT4,IL4,NTVT,TExp),
	IL5 = [ sum(IndName,TLow,TUpp,Result,TExp,IL4) ],
        append(IL3,IL5,IL6),
	append(IL2,IL6,IL7),
	append(IL1,IL7,IL).
trans_c( prod(Ind,Low,Upp,Exp),AV,IT,TVT,IL,NTVT,Result):- 
	index_name(Ind,IndName),
	ins_without_dup(IT,IndName),
	trans_c(Low,AV,IT,TVT,IL1,TVT1,TLow),
	trans_c(Upp,AV,IT,TVT1,IL2,TVT2,NUpp),
        trans_upp(NUpp,TVT2,IL3,TVT3,TUpp),
        TVT4 is TVT3 + 1,
        temp_name(TVT3,Result),
	trans_c(Exp,AV,IT,TVT4,IL4,NTVT,TExp),
	IL5 = [ prod(IndName,TLow,TUpp,Result,TExp,IL4) ],
        append(IL3,IL5,IL6),
	append(IL2,IL6,IL7),
	append(IL1,IL7,IL).
trans_c(fact(Exp),AV,IT,TVT,IL,NTVT,Result):- 
	TVT1 is TVT + 1,
        trans_c(Exp,AV,IT,TVT1,IL1,TVT2,NExp),
	trans_upp(NExp,TVT2,IL2,TVT3,TExp),
	NTVT is TVT3 + 1,
	temp_name(TVT3,Result),
	temp_name(TVT,TVTName),
	IL3 = [fact(TVTName,Result,TExp) ],
        append(IL2,IL3,IL4),
	append(IL1,IL4,IL).
	
trans_c_simple(0,_,_,_,TVT,[],TVT,_).
trans_c_simple(N,Exp,AV,IT,TVT,IL,NTVT,TExp):-
	N>0,
	arg(N,Exp,Arg),
	trans_c(Arg,AV,IT,TVT,IL1,TVT1,TArg),
	arg(N,TExp,TArg),
	N1 is N-1,
	trans_c_simple(N1,Exp,AV,IT,TVT1,IL2,NTVT,TExp),
        append(IL1,IL2,IL).

	
simple_func(Exp):-
	functor(Exp,F,N),
        N>0,
	(F,N) \== ('$',1),
	(F,N) \== ('$',2),
	(F,N) \== (fact,1),
        (F,N) \== (sum,4),
	(F,N) \== (prod,4).

trans_upp(Exp,TV,IL,NTV,NExp):-
	(
	    need_evaluation(Exp)->
	    (
		temp_name(TV,NExp),
		IL = [assig(NExp,Exp)],
		NTV is TV + 1
	    )
	;
	    (
		IL = [],
		NTV = TV,
		NExp = Exp
	    )
	).

need_evaluation(Exp):-
	functor(Exp,_,N),
        N>0.

p_var($(I),_,I):- 
	atom(I).
p_var($(N),AV,Name):- 
	number(N),
	atom_concat('head_',N,Name),
	ins_without_dup(AV,N).
p_var($(0,N),AV,Name):- 
	number(N),
	atom_concat('head_',N,Name),
	ins_without_dup(AV,N).
p_var($(N,M),AV,Name):-
	number(N),
	N =\= 0,
	number(M),
	name(N,LitNum),
	name(M,ArgNum),
	append("_",ArgNum,ArgName),
	append(LitNum,ArgName,LitName),
	append("body_",LitName,Lname),
	name(Name,Lname),
	ins_without_dup(AV,$(N,M)).

% same as concat
% create_name(Prefix,Sufix,Name):-
%   name(Sufix,L),append(Prefix,L,Z),name(Name,Z). 


index_name($(I),I):-
	atom(I),
	!.
index_name(I,I).

