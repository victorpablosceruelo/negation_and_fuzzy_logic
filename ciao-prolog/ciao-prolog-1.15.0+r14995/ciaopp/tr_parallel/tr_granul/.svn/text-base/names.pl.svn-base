:- module(names, [create_names/9], [assertions, api(ciaopp_api)]). 

:- doc(author,"Pedro L@'{o}pez").  

:- doc(module,"Rename predicates that need, either supply sizes or
                   do test.").

% ciao library:
:- use_module(library(lists), [length/2]). 

% ciaopp library:
:- use_module(infercost(init), [find_symbol_entry/3]). 
:- use_module(infercost(gran(gran_table)), [find_gran_field/4]).  

% Own library:

:- use_module(gen_expre, [gen_expresion/6]). 

:- use_module(names_table, [insert_name_field/4]). 

%
% Warning!:Procedure "create_names" is dependent of the structure of GT.
%

create_names(GT,_,_,_,_,_,_,_,_):- 
	var(GT).
create_names(GT,ST,PE,GranNames,SeqNames,FNum,NFNum,NT,FN):-
	nonvar(GT),
	GT = [st(Pred/Arity,_,_,_,Type,_,_)|GTList],
	(
	    Type \== sequential ->
            (
		find_symbol_entry(ST,Pred/Arity, 
		          st(Pred/Arity,_,ModeList,MeaList,_,_,_,_,_,_)),
		create_arglist(1,ModeList,MeaList,ArgList), 
		insert_name_field(NT,Pred/Arity,arg_map,ArgList),
		length(ArgList,NumInputArg),
		GArity is Arity + NumInputArg,
		create_gran_name(ST,Pred,GArity,GranNames,Gname), 
		insert_name_field(NT,Pred/Arity,gran_name,Gname), 
		(
		    Pred/Arity == PE ->
		    (
			create_seq_name(ST,Pred,Arity,SeqNames,Sname),
			insert_name_field(NT,Pred/Arity,seq_name,Sname),
			NewSeqNames = [Sname|SeqNames]
		    ) 
		;
		    NewSeqNames = SeqNames
		),
		find_gran_field(GT,Pred/Arity,sizes,ASizeRel),
		create_func_names(ST,FNum,NFN1,FN,ASizeRel,FuncNames),%
		insert_name_field(NT,Pred/Arity,actual_sizes,FuncNames),
		(
		    Type == do_test -> 
                  % Decomment to create C file. PLG OCT 97
	            ( true
                  % find_symbol_field(ST,Pred/Arity,time,TFun),
                  % TFun = [TimeFunc],
                  % Decomment to create C file. PLG OCT 97
                  % create_test_function(ST,NFN1,NFN,FN,TimeFunc,HTimeFunc),
                  % insert_name_field(NT,Pred/Arity,test,HTimeFunc)
                    )
                ;
		    NFN = NFN1
		),
		NewGranNames = [Gname|GranNames]
	    )
	;
	    (NewSeqNames = SeqNames,NewGranNames = GranNames,NFN = FNum)
	),
	create_names(GTList,ST,PE,NewGranNames,NewSeqNames,NFN,NFNum,NT,FN).

 %% % Not used at momment. Do not delete!. Can be used to create a C file. 
 %% % with the cost functions. 
 %% % The call is commented out in create_names/9.
 %% create_test_function(ST,FNum,NFNum,FN,TimeFunc,HTimeFunc):-
 %% 	gen_c_expresion(ST,TimeFunc,FNum,NFNum,FN,HTimeFunc).


create_gran_name(ST,Pred,Arity,Newnames,Name):-
	get_mod_pred(Pred,Mod,Pred1),
	!,
	atom_concat('g_',Pred1,NewPred1), 
	atom_concat(Mod,':',NewMod1),
	atom_concat(NewMod1,NewPred1,NewPred),
	find_symbol_entry(ST,NewPred/Arity,Entry),
	(
	    (var(Entry),\+member(NewPred/Arity, Newnames)) -> 
	     Name = NewPred/Arity
	;
	    create_gran_name(ST,NewPred,Arity,Newnames,Name)
	).
create_gran_name(ST,Pred,Arity,Newnames,Name):-
	atom_concat('g_',Pred,NewPred), 
	find_symbol_entry(ST,NewPred/Arity,Entry),
	(
	    (var(Entry),\+member(NewPred/Arity, Newnames)) -> 
	     Name = NewPred/Arity
	;
	    create_gran_name(ST,NewPred,Arity,Newnames,Name)
	).

create_seq_name(ST,Pred,Arity,Newnames,Name):-
	get_mod_pred(Pred,Mod,Pred1),
	!,
	atom_concat('s_',Pred1,NewPred1), 
	atom_concat(Mod,':',NewMod1),
	atom_concat(NewMod1,NewPred1,NewPred),
	find_symbol_entry(ST,NewPred/Arity,Entry),
	(
	    (var(Entry),\+member(NewPred/Arity, Newnames)) ->
	     Name = NewPred/Arity
	;
	    create_seq_name(ST,NewPred,Arity,Newnames,Name)
	).
create_seq_name(ST,Pred,Arity,Newnames,Name):-
	atom_concat('s_',Pred,NewPred), 
	find_symbol_entry(ST,NewPred/Arity,Entry),
	(
	    (var(Entry),\+member(NewPred/Arity, Newnames)) ->
	     Name = NewPred/Arity
	;
	    create_seq_name(ST,NewPred,Arity,Newnames,Name)
	).

%% create_arglist(_,[],[]).
%% 
%% create_arglist(ArgNum,[+|ModeList],[ ArgNum | AList ] ):-
%% 	NewArgNum is ArgNum + 1,
%% 	create_arglist(NewArgNum,ModeList, AList ).
%% 
%% create_arglist(ArgNum,[-|ModeList],AList):-
%% 	NewArgNum is ArgNum + 1,
%% 	create_arglist(NewArgNum,ModeList, AList ).
%%


% Creates a list with the input arguments numbers whose measure is not
% void. Has the format [(e, 1), ..., (e, N), (h, 1), ..., (h, M)].
% where (e, i) represents that the size of the argument number i will be
% supplied by an extra argument of the annotated head.  (h, i) means
% that the size of the argument number i will be supplied by the
% argument number i itself, and no extra argument is needed for the
% annotated head (in that case, the measure of the argument number i is
% supposed to be int), which means that before calling a version of a
% predicate that performs granularity control only is neccessary to
% supply the sizes of arguments of type (e, i).
 

create_arglist(_,[],[],[]):-!. 
create_arglist(ArgNum,[+|ModeList], [Mea|MeaList], [InputArg|AList]):-
	Mea \== void,!,
        NewArgNum is ArgNum + 1,
        (Mea == int -> InputArg = (h,ArgNum)
                    ;  InputArg = (e,ArgNum)),
	create_arglist(NewArgNum, ModeList, MeaList, AList).
create_arglist(ArgNum, [_|ModeList], [_|MeaList], AList):-
	NewArgNum is ArgNum + 1,
	create_arglist(NewArgNum, ModeList, MeaList, AList).


create_func_names(_,FNum,FNum,_,[],[]). 
create_func_names(ST,FNum,NFNum,FN,[ASizeRel|Alist],[FuncNames|FList]):-
	create_func_name_clause(ST,FNum,NFN,FN,ASizeRel,FuncNames),
	create_func_names(ST,NFN,NFNum,FN,Alist,FList).

create_func_name_clause(_,FNum,FNum,_,[],[]). 
create_func_name_clause(ST,FNum,NFNum,FN,[ASize|AList],[NSize|FList]):-
        ASize = iasize(Key,Size),
        gen_expresion(ST,Size,FNum,NFN,FN,FSize),
	NSize = iasize(Key,FSize),
        create_func_name_clause(ST,NFN,NFNum,FN,AList,FList).



