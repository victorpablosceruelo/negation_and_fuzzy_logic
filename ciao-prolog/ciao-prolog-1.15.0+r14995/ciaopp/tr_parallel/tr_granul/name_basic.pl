:- module(name_basic,
	[
	    temp_name/2, 
	    convert_name/2, 
	    create_func_name/5, 
	    create_dyn_pred_name/2 
	],
	[assertions]). 

:- doc(author,"Pedro L@'{o}pez").  

:- use_module(library(lists), [length/2, append/3]). 

:- use_module(infercost(init), [find_symbol_entry/3]). 

%
% Creates a name for a function that evaluates sizes.
%

create_func_name(ST,FNum,Arity,NFNum,FunName):- 
	atom_concat('f_',FNum,Name),
	find_symbol_entry(ST,Name/Arity,Entry),
	(
	    var(Entry) ->
	    (
		FunName = Name,
		NFNum is FNum + 1
	    )
	;
	    (
		NFN is FNum + 1,
		create_func_name(ST,NFN,Arity,NFNum,FunName)
	    )
       ).

temp_name(TV,TName):-
	atom_concat('tmp_', TV, TName). 

convert_name(N,Name):- 
        number(N),
        atom_concat('head_',N,Name).
convert_name($(N,M),Name):-
        number(N),
        N =\= 0,
        number(M),
        name(N,LitNum),
        name(M,ArgNum),
	append("_",ArgNum,ArgName),
        append(LitNum,ArgName,LitName),
        append("body_",LitName,Lname),
        name(Name,Lname).

create_dyn_pred_name(Label, TrPred/NA):-
	Label = label(Pred/A, InSizes,OutSizes),
	length(InSizes, NumIn),
	length(OutSizes, NumOut),
	NA is NumIn + NumOut + A,
	create_code_list(OutSizes, CLOut),
	(
	    InSizes \== [] ->
	    create_code_list(InSizes, CLIn),     
	    append("_i", CLIn, L1)
	;
	    L1 = "_"
	),
	append(L1,"o",L2),
	append(L2, CLOut, L3),
	name(Pred, PredL),
	append(PredL, L3, L4),
	name(TrPred, L4). 

create_code_list([], []).
create_code_list([E|L], [EL|S1]):-
	name(E, [EL]),  
	create_code_list(L,S1).


