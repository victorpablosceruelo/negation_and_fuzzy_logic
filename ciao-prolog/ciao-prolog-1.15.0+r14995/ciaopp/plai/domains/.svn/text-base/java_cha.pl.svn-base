:- module(_,
	_,
% 	 [java_cha_call_to_entry/8,
% 	  java_cha_exit_to_prime/7,
% 	  java_cha_project/3,
% 	  java_cha_extend/4,
% 	  java_cha_compute_lub/2,
% 	  java_cha_glb/3,
% 	  java_cha_less_or_equal/2,
% 	  java_cha_compute_lub_el/3,
% 	  java_cha_sort/2,
% 	  java_cha_special_builtin/4,
% 	  java_cha_success_builtin/5,      
% 	  java_cha_call_to_success_fact/8,
% 	  java_cha_input_interface/4,      
% 	  java_cha_input_user_interface/3, 
% 	  java_cha_asub_to_native/3,
% 	  java_cha_unknown_call/4,
% 	  java_cha_unknown_entry/3,
%         java_string_list_to_prolog_term_list/2
%         ],
	 [assertions,regtypes,basicmodes]).

:- use_module(library(messages), 
        [debug_message/2,
	 error_message/2]).
:- use_module(library(sort)).
:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(sets), 
	[ord_intersection/3,
	 ord_union/3,
	 ord_member/2,
	 ord_subset/2]).
:- use_module(library(lists), [append/3]).
:- use_module(plai(java_aux), 
	[
	 blt/4, 
	 special_builtin/2,
	 object/2,
	 variable/2,
	 null/2,
	 atom/2,
	 basic_type/1,
	 compose_array_type/2
        ]).
:- use_module(domain(java_nullity), 
	[java_nullity_peel_equations/3,
	 change_values_insert/4,
	 change_values/4,
	 var_value/3,
	 java_nullity_remove/3,
	 ord_inters_diff/4
        ]).
:- use_module(ilciao(java_interface), 
	[
	 java_create/2,   
	 java_invoke/2
	]).
:- use_module(library(read_from_string), [read_from_string_atmvars/2]).

:- doc(title,"Simple Class Hierarchy Analysis for Java programs").
:- doc(author, "Jorge Navas").
:- doc(author, "Mario Mendez").

%------------------------------------------------------------------------%
% This module implements the abstract operations of a simple class       |
% hierarchy analysis for the PLAI framework of abstract interpretation.  |
% An abstract substitution is a list of Var/Cone(C) elements, where      |
% Cone(C) is the set of all subclasses of the class C, including C. C is |
% the class defined for Var.                                             |
%------------------------------------------------------------------------%

java_cha_init_abstract_domain.


%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT PROJECTION                               |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_cha_project(+,+,-)                                                |
% java_cha_project(ASub,Vars,Proj)                                       |
%------------------------------------------------------------------------%

java_cha_project('$bottom',_,'$bottom'):- 
	!.
java_cha_project(ASub,Vars,Proj) :-
	project_aux(Vars,ASub,Proj),
	!.
java_cha_project(ASub,Vars,_Proj) :-
	error_message("java_cha_project(~q,~q)
	  \n",[ASub, Vars]),
	throw(abstract_operation_error).

project_aux([],_,[]):- !.
project_aux(_,[],[]):- !.
project_aux([Head1|Tail1],[Head2/Val|Tail2],Proj) :-
	compare(Order,Head1,Head2),
	project_aux_(Order,Head1,Tail1,Head2/Val,Tail2,Proj).
project_aux_(=,_,Tail1,HeadVal,Tail2,[HeadVal|Proj]) :-
	project_aux(Tail1,Tail2,Proj).
project_aux_(<,_Head1,_Tail1,_,[],[]) :- !.
project_aux_(>,_Head1,_Tail1,_,[],[]) :- !.
project_aux_(>,Head1,Tail1,_,[Head2/Val|Tail2],Proj) :-
	compare(Order,Head1,Head2),
	project_aux_(Order,Head1,Tail1,Head2/Val,Tail2,Proj).

%------------------------------------------------------------------------%
%                       ABSTRACT Call To Entry                           |
%------------------------------------------------------------------------%
% java_cha_call_to_entry(+,+,+,+,+,+,-,-)                                |          
% java_cha_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo)          |
%------------------------------------------------------------------------%          
% Since it assumed only type-safe programs, it suffices a renaming       |
% of the abstract substitution from Sv to Hv variables. If an argument   |
% of Sg is not variable, its corresponding in Head will taken from its   |
% signature. Note that local variables are not added. They will be added |
% on demand.                                                             |
%------------------------------------------------------------------------%

java_cha_call_to_entry(_Sv,_Sg,_Hv,_Head,_Fv,'$bottom','$bottom',_ExtraInfo):-!.
java_cha_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
        change_values_insert(Hv,Proj,ASub,_Empty),
	java_nullity_peel_equations(Head,Sg,Eqs),    
	types_from_signature(Head,Hv,Types),
%	Types = [],
	rename_call2entry(Eqs,Types,ASub,ASub0),
%	java_cha_project(ASub0,Hv,Entry),  
	java_nullity_remove(Sv,ASub0,Entry),
	ExtraInfo = Fv,
	!.
java_cha_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,_Entry,_ExtraInfo):-
	error_message("java_cha_call_to_entry(~q,~q,~q,~q,~q,~q)
	  \n",[Sv,Sg,Hv,Head,Fv,Proj]),
	throw(abstract_operation_error).

rename_call2entry(Eqs,Types,ASub,ASub1):-
	rename_call2entry_(Eqs,Types,ASub,ASub0),
	( ASub == ASub0 -> ASub1 = ASub ;
	  rename_call2entry(Eqs,Types,ASub0,ASub1)).
rename_call2entry_([],[],ASub,ASub).
rename_call2entry_([(X,_Y)|Eqs],[X0/TypeX|Types],ASub,ASub2):-
	X == X0,!,
	change_values_insert([X],ASub,ASub1,TypeX),
        rename_call2entry_(Eqs,Types,ASub1,ASub2).
rename_call2entry_([_|Eqs],Types,ASub,ASub1):-
	rename_call2entry_(Eqs,Types,ASub,ASub1).


var_value_(ASub,X,Y,_TypeX,ASub0):-
	var_value(ASub,Y,K_Y),
	!,
	change_values_insert([X],ASub,ASub0,K_Y).
var_value_(ASub,X,_Y,TypeX,ASub0):-
	!,
	change_values_insert([X],ASub,ASub0,TypeX).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT Exit To Prime                            |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_cha_exit_to_prime(+,+,+,+,+,-,-)                                  |
% java_cha_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime)             |
%------------------------------------------------------------------------%
% Similar to call2entry. A simple renaming from Hv to Sg variables is    |
% performed. Note that this is correct (although not efficient) because  |
% of the use of shadow variables.                                        |
%------------------------------------------------------------------------%

java_cha_exit_to_prime(_Sg,_Hv,_Head,_Sv,'$bottom',_Flag,'$bottom') :- !.
java_cha_exit_to_prime(Sg,Hv,Head,_Sv,Exit,Fv,Prime):-
	java_nullity_peel_equations(Sg,Head,Eqs),    
	rename_exit2prime(Eqs,Exit,Prime0),
	append(Hv,Fv,HvFv_u),
	sort(HvFv_u,HvFv),
	java_nullity_remove(HvFv,Prime0,Prime),
	!.
%	java_cha_project(Prime0,Sv,Prime).
java_cha_exit_to_prime(Sg,Hv,Head,Sv,Exit,_Fv,_Prime):-
	error_message("java_cha_exit_to_prime(~q,~q,~q,~q,~q)
	  \n",[Sg,Hv,Head,Sv,Exit]),
	throw(abstract_operation_error).

rename_exit2prime(Eqs,ASub,ASub1):-
	rename_exit2prime_(Eqs,ASub,ASub0),
	( ASub == ASub0 -> ASub1 = ASub ;
	  rename_exit2prime(Eqs,ASub0,ASub1)).
rename_exit2prime_([],ASub,ASub).
rename_exit2prime_([(X,Y)|Eqs],ASub,ASub2):-
        rename_exit2prime0(X,Y,ASub,ASub1),
        rename_exit2prime_(Eqs,ASub1,ASub2).

rename_exit2prime0(X,Y,ASub,ASub0):-
	var(X),var(Y),!, 
	var_value(ASub,Y,Val),
	change_values_insert([X],ASub,ASub0,Val).
rename_exit2prime0(X,Y,ASub,ASub0):-
	var(X),basic_type(Y),!,  % Y could be void
	change_values_insert([X],ASub,ASub0,Y).
rename_exit2prime0(X,_Y,ASub,ASub):-
	atm(X),!.
rename_exit2prime0(X,_Y,ASub,ASub):-
	number(X),!.

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT Extend                                   |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_cha_extend(+,+,+,-)                                               |
% java_cha_extend(Prime,Sv,Call,Succ)                                    |
%------------------------------------------------------------------------%
% Succ is computed updating the values of Call with those in Prime. Again|
% this is correct because of the use of shadow variables.                |
%------------------------------------------------------------------------%
java_cha_extend('$bottom',_Sv,_Call,'$bottom'):- 
	!.
java_cha_extend(_Prime,[],Call,Succ):- 
	!,
	Call = Succ.
java_cha_extend(Prime,Sv,Call,Succ):- 
	update_Call_insert(Call,Prime,Sv,Succ),
	!.
java_cha_extend(Prime,Sv,Call,_Succ):- 
	error_message("java_cha_extend(~q,~q,~q)
	  \n",[Prime, Sv, Call]),
	throw(abstract_operation_error).

% update_Call([],_,[]).
% update_Call(Call,[],Call).
% update_Call([X/ValueX|Call],[Y/ValueY|Prime],Succ):-
% 	compare(Order,X,Y),
% 	update_Call_(Order,X/ValueX,Call,Y/ValueY,Prime,Succ).

% update_Call_(=,_,Call,ElemP,Prime,[ElemP|Succ]):-
% 	update_Call(Call,Prime,Succ).
% update_Call_(<,ElemC,Call,ElemP,Prime,[ElemC|Succ]):-
% 	update_Call(Call,[ElemP|Prime],Succ).
% update_Call_(>,ElemC,Call,_ElemP,Prime,[ElemC|Succ]):-
% 	update_Call(Call,Prime,Succ).

update_Call_insert([],Prime,Sv,Succ):-
        insert_if_member(Prime,Sv,Succ).
update_Call_insert(Call,[],_Sv,Call).
update_Call_insert([X/ValueX|Call],[Y/ValueY|Prime],Sv,Succ):-
	compare(Order,X,Y),
	update_Call_(Order,X/ValueX,Call,Y/ValueY,Prime,Sv,Succ).

update_Call_(=,_,Call,ElemP,Prime,Sv,[ElemP|Succ]):-
	update_Call_insert(Call,Prime,Sv,Succ).
update_Call_(<,ElemC,Call,ElemP,Prime,Sv,[ElemC|Succ]):-
	update_Call_insert(Call,[ElemP|Prime],Sv,Succ).
update_Call_(>,ElemC,Call,ElemP/ValP,Prime,Sv,Succ):-
	( ord_member(ElemP,Sv) ->
	  Succ = [ElemP/ValP,ElemC|Succ0]
        ;
	  Succ = [ElemC|Succ0]  
        ),
	update_Call_insert(Call,Prime,Sv,Succ0).

insert_if_member([],_Sv,[]):-!.
insert_if_member([X/V|Xs],Sv,[X/V|Acc]):-
	ord_member(X,Sv),!,
	insert_if_member(Xs,Sv,Acc).
insert_if_member([_|Xs],Sv,Acc):-
	!,
	insert_if_member(Xs,Sv,Acc).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT LUB                                      |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_cha_compute_lub(+,-)                                              |
% java_cha_compute_lub(ListASub,Lub)                                     |
%------------------------------------------------------------------------%
java_cha_compute_lub([X],X):- !.
java_cha_compute_lub([ASub1,ASub2|Xs],Lub):-
	java_cha_compute_lub_el(ASub1,ASub2,ASubLub),
	java_cha_compute_lub([ASubLub|Xs],Lub).

java_cha_compute_lub_el('$bottom',ASub,ASub):- !.
java_cha_compute_lub_el(ASub,'$bottom',ASub):- !.

java_cha_compute_lub_el(ASub1,ASub2,Lub):- 
	compute_lub_java_cha(ASub1,ASub2,Lub).

compute_lub_java_cha(ASub1,ASub2,Lub):- 
	ASub1 == ASub2, !,
	Lub = ASub1.
compute_lub_java_cha([Xv|ASub1],[Yv|ASub2],Lub):- 
	Xv == Yv, !,
	Lub = [Xv|Lub_Cont],
	compute_lub_java_cha(ASub1,ASub2,Lub_Cont).
compute_lub_java_cha([X/K1|ASub1],[X/K2|ASub2],[X/K|Lub_Cont]):-
	ord_union(K1,K2,K),
	compute_lub_java_cha(ASub1,ASub2,Lub_Cont).

%------------------------------------------------------------------------%
%                         HANDLING BUILTINS                              |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_cha_special_builtin(+,+,-,-)                                      |
% java_cha_special_builtin(SgKey,Sg,Type,Condvars)                       |
%------------------------------------------------------------------------%
java_cha_special_builtin(SgKey,Sg,insert_if_new,[L/K_L,R/K_R])       :- 
	blt(ge,SgKey,Sg,[L,K_L,R,K_R]).
java_cha_special_builtin(SgKey,Sg,insert_if_new,[L/K_L,R/K_R])       :- 
	blt(gt,SgKey,Sg,[L,K_L,R,K_R]).
java_cha_special_builtin(SgKey,Sg,insert_if_new,[L/K_L,R/K_R])       :- 
	blt(lt,SgKey,Sg,[L,K_L,R,K_R]).
java_cha_special_builtin(SgKey,Sg,insert_if_new,[L/K_L,R/K_R])       :- 
	blt(le,SgKey,Sg,[L,K_L,R,K_R]).
java_cha_special_builtin(SgKey,Sg,insert_if_new,[L/K_L,R/K_R])       :- 
	blt(eq,SgKey,Sg,[L,K_L,R,K_R]).
java_cha_special_builtin(SgKey,Sg,insert_if_new,[L/K_L,R/K_R])       :- 
	blt(ne,SgKey,Sg,[L,K_L,R,K_R]).
java_cha_special_builtin(SgKey,Sg,insert_if_new,[N/K_R,O/K_O,R/K_R]) :- 
	blt(stf,SgKey,Sg,[N,O,K_O,_,_,R,K_R]).
java_cha_special_builtin(SgKey,Sg,insert_if_new,[N/K_N,Ind/K_Ind,R/K_R]) :- 
	blt(sta,SgKey,Sg,[N,Ind,K_Ind,R,K_R]),
	compose_array_type(K_R,K_N).
java_cha_special_builtin(SgKey,Sg,insert_if_new,[X/K_X,Y/K_Y,Z/K_Z]) :- 
	blt(add,SgKey,Sg,[X,K_X,Y,K_Y,Z,K_Z]).
java_cha_special_builtin(SgKey,Sg,insert_if_new,[X/K_X,Y/K_Y,Z/K_Z]) :- 
	blt(sub,SgKey,Sg,[X,K_X,Y,K_Y,Z,K_Z]).
java_cha_special_builtin(SgKey,Sg,insert_if_new,[X/K_X,Y/K_Y,Z/K_Z]) :- 
	blt(div,SgKey,Sg,[X,K_X,Y,K_Y,Z,K_Z]).
java_cha_special_builtin(SgKey,Sg,insert_if_new,[X/K_X,Y/K_Y,Z/K_Z]) :- 
	blt(rem,SgKey,Sg,[X,K_X,Y,K_Y,Z,K_Z]).
java_cha_special_builtin(SgKey,Sg,insert_if_new,[X/K_X,Y/K_Y,Z/K_Z]) :- 
	blt(mul,SgKey,Sg,[X,K_X,Y,K_Y,Z,K_Z]).
java_cha_special_builtin(SgKey,Sg,new,[O,K_O])                       :- 
	blt(new,SgKey,Sg,[O,K_O]).
java_cha_special_builtin(SgKey,Sg,new,[O,K_O])                       :- 
	blt(newa,SgKey,Sg,[O,K_O,_Dim]).
java_cha_special_builtin(SgKey,Sg,asg_field,[L,K_F])                 :- 
	blt(gtf,SgKey,Sg,[L,_,_,_,_,K_F]).
java_cha_special_builtin(SgKey,Sg,asg_field,[L,K_L])                 :- 
	blt(gta,SgKey,Sg,[L,K_L,_Array,_Ind,_K_Ind]).
java_cha_special_builtin(SgKey,Sg,asg,[L,K_L,R,K_R])                 :- 
	blt(asg,SgKey,Sg,[L,K_L,R,K_R]).
java_cha_special_builtin(SgKey,_,unchanged,_)                        :- 
	blt(unk,SgKey,_,_).
% Special methods handled as builtins:
java_cha_special_builtin(SgKey, _, unchanged, _):-
	special_builtin(object_init,SgKey),!.
java_cha_special_builtin('true/0',_, unchanged, _).
java_cha_special_builtin(SgKey,_,_,_):-
	debug_message("Be cautious, some builtins are not supported yet, ~q ?",[SgKey]),
	!,fail.


%------------------------------------------------------------------------%
% java_cha_success_builtin(+,+,+,+,-)                                    |
% java_cha_success_builtin(Type,Sv_u,Condv,Call,Succ)                    |
%------------------------------------------------------------------------%
java_cha_success_builtin(unchanged,_,_,Succ,Succ):-!.
java_cha_success_builtin(Type,Sv_u,Condv,Call,Succ):- !,
	java_cha_success_builtin_(Type,Sv_u,Condv,Call,Succ_u),
	java_cha_sort(Succ_u,Succ).

java_cha_success_builtin_(_,_Sv_u,_Condv,'$bottom','$bottom'):- !.
java_cha_success_builtin_(insert_if_new,_Sv_u,Condvars,Call,Succ):- 
	change_types_insert(Condvars,Call,Succ).
java_cha_success_builtin_(new,_Sv_u,[X,K],Call,Succ):- !,
	change_values_insert([X],Call,Succ,[K]).
java_cha_success_builtin_(asg_field,_Sv_u,[L,K_F],Call,Succ):- 
	asg_field_success_builtin([L,K_F],Call,Succ).
java_cha_success_builtin_(asg,_Sv_u,[L,_K_L,R,K_R],Call,Succ):- 
	asg_success_builtin([L,_K_L,R,K_R],Call,Succ).

asg_field_success_builtin([L,K_F],Call,Succ):-
        % IMPORTANT NOTE: This operation could incorrect if the same field is 
        % defined in different classes.
	cone(K_F,Val),
 	change_values_insert([L],Call,Succ,Val).
asg_field_success_builtin([L,K_F],_Call,_Succ):-
	error_message("gtf(~q, of class ~q) failed \n",[L,K_F]),!,fail.

%------------------------------------------------------------------------%
% asg_success_builtin(E1,E2)                                             |
%------------------------------------------------------------------------%
% Possible cases:                                                        |
% object   := object                                                     |
% object   := null                                                       |
% object   := atom (if object is String)                                 |
% variable := variable                                                   |
% variable := atom                                                       |
%------------------------------------------------------------------------%
asg_success_builtin([L,K_L,R,K_R],Call,Succ):- 
	object(L,K_L), object(R,K_R), !,		
	insert_val0_if_undefined(Call,L,R,K_R,Succ).
asg_success_builtin([L,K_L,R,K_R],Call,Succ):- 
	object(L,K_L), atom(R,K_R), !,
	insert_val_if_undefined(Call,L,K_R,Succ).
asg_success_builtin([L,K_L,R,K_R],Call,Succ):- 
	object(L,K_L), null(R,K_R), !,
	cone(K_L,Val),
	change_values_insert([L],Call,Succ,Val).
asg_success_builtin([L,K_L,R,K_R],Call,Succ):- 
	variable(L,K_L), variable(R,K_R), !,
	insert_val_if_undefined(Call,L,K_R,Succ).	
asg_success_builtin([L,K_L,R,K_R],Call,Succ):- 
	variable(L,K_L), atom(R,K_R), !,	
	insert_val_if_undefined(Call,L,K_R,Succ).
asg_success_builtin_([L,K_L,R,K_R],_,_):-
	error_message("asg(~q,~q,~q,~q) failed \n",[L,K_L,R,K_R]),!,fail.

check_var_value(_,Call,X,Val):-
	var_value(Call,X,Val),!.
check_var_value(N,_,X,_):-
	error_message("~q - Variable ~q not defined \n",[N,X]),!,fail.

insert_val_if_undefined(Call,X,_,Call):-
	var_value(Call,X,_),!.
insert_val_if_undefined(Call,X,K,Succ):-
	cone(K,Val),!,
	change_values_insert([X],Call,Succ,Val).

insert_val0_if_undefined(Call,X,Y,_K_Y,Succ):-
	var_value(Call,Y,Val),!,
	change_values_insert([X],Call,Succ,Val).
insert_val0_if_undefined(Call,X,_Y,K_Y,Succ):- !,
	cone(K_Y,Val),
	change_values_insert([X],Call,Succ,Val).


%------------------------------------------------------------------------%
%                      ABSTRACT Call to Success Fact                     %
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% Specialized version of call_to_entry + exit_to_prime + extend for facts%
%------------------------------------------------------------------------%
java_cha_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ) :-
	types_from_signature(Head, Hv, Temp1),
	ord_union(Proj,Temp1,Temp2),
 % exit_to_prime
	java_cha_exit_to_prime(Sg,Hv,Head,Sv,Temp2,[],Prime),
% extend
	java_cha_extend(Prime,Hv,Call,Succ),!.
java_cha_call_to_success_fact(_Sg,_Hv,_Head,_Sv,_Call,_Proj, '$bottom','$bottom').	

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT SORT
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_cha_sort(+,-)                                                     |
% java_cha_sort(Asub,Asub_s)                                             |
%------------------------------------------------------------------------%
java_cha_sort('$bottom','$bottom'):- !.
java_cha_sort(Asub,Asub_s):- sort(Asub,Asub_s).

java_cha_sort_list_of_lists('$bottom','$bottom'):-!.
java_cha_sort_list_of_lists([],[]).
java_cha_sort_list_of_lists([L|Ls],[L_s|Ls_s]):-
	java_cha_sort(L,L_s),
	java_cha_sort_list_of_lists(Ls,Ls_s).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT ASUB TO NATIVE
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_cha_asub_to_native(+,+,-)                                         |
% java_cha_asub_to_native(ASub,Qv,ASub_user)                             |
%------------------------------------------------------------------------%
java_cha_asub_to_native('$bottom',_Qv,_ASub_user):- !, fail.
java_cha_asub_to_native(ASub,_Qv,ASub_user):-
	ASub_user = [types(ASub)].


%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT UNKNOWN ENTRY                            |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_cha_unknown_entry(+,+,-)                                          |
% java_cha_unknown_entry(Sg,Qv,Call)                                     |
%------------------------------------------------------------------------%
java_cha_unknown_entry(Sg,Qv,Call):-
	types_from_signature(Sg,Qv,Call).
	
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT UNKNOWN CALL
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_cha_unknown_call(+,+,+,-)                                         |   
% java_cha_unknown_call(Sg,Call,Vars,Succ)                               |
% It's used only for dealing with external calls.                        |
%------------------------------------------------------------------------%
java_cha_unknown_call(_Sg,'$bottom',_Vars,'$bottom').
java_cha_unknown_call(Sg,Call,Sv,Succ):-	
 	ord_inters_diff(Call,Sv,_Int,Disj),
 	functor(Sg,F,A),
 	functor(Head,F,A),
	varset(Head,Hv),
 	Head =.. [_|VarsDef],
 	types_from_signature(Head,VarsDef,Exit),
 	java_cha_exit_to_prime(Sg,Hv,Head,Sv,Exit,[],Prime),
        ord_union(Prime,Disj,Succ).


%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT LESS OR EQUAL
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_cha_less_or_equal(+,+)                                            |
% java_cha_less_or_equal(ASub0,ASub1)                                    |
%------------------------------------------------------------------------%
java_cha_less_or_equal('$bottom',_).
java_cha_less_or_equal(ASub0,ASub1):-
	less_or_equal_(ASub0,ASub1).

less_or_equal_([],[]).
less_or_equal_([X/Value0|Rest0],[X/Value1|Rest1]):-
	less_or_equal_el(Value0,Value1),
	less_or_equal_(Rest0,Rest1).

%less_or_equal_el(_,top):-!.
%less_or_equal_el('$basic_type','$basic_type').
less_or_equal_el(Value0,Value1):-
	Value0 == Value1,!.
less_or_equal_el(Value0,Value1):-
	ord_subset(Value0,Value1),!.
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT GLB
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_cha_glb(+,+,-)                                                    |
% java_cha_glb(ASub0,ASub1,Glb)                                          |
%------------------------------------------------------------------------%
java_cha_glb(ASub0,ASub1,Glb):-
	ASub0 == ASub1,!,
	Glb = ASub1.
java_cha_glb(ASub0,ASub1,Glb):-
	glb_(ASub0,ASub1,Glb),!.
java_cha_glb(_,_,'$bottom').

glb_([],[],[]).
glb_([Xv|ASub0],[Yv|ASub1],[Xv|Glb]):-
	Xv == Yv,!,
	glb_(ASub0,ASub1,Glb).

% glb_([X/top|ASub0],[X/Value|ASub1],[X/Value|Glb]):-
% 	!,
% 	glb_(ASub0,ASub1,Glb).
% glb_([X/Value|ASub0],[X/top|ASub1],[X/Value|Glb]):-
% 	!,
% 	glb_(ASub0,ASub1,Glb).
glb_([X/ValueX|ASub0],[X/ValueY|ASub1],[Glb|Glbs]):-
	!,
	ord_intersection(ValueX,ValueY,Value),
	( Value=[], ValueX\==[], ValueY\==[] -> 
	  Glb = '$bottom' ; 
	  Glb=[X/Value] ),
	glb_(ASub0,ASub1,Glbs).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT INPUT USER INTERFACE                     |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_cha_input_user_interface(+,+,-)                                   |    
% java_cha_input_user_interface(InputUser,Qv,ASub)                       |
%------------------------------------------------------------------------%
java_cha_input_user_interface(ASub,_Qv,ASub).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT INPUT INTERFACE                          |
%------------------------------------------------------------------------%
% java_cha_input_interface(+,?,+,+)                                      |
% java_cha_input_interface(Prop,Kind,Struct0,Struct1)                    |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
java_cha_input_interface(Info,perfect,_ASub0,ASub1):-
	Info = tau(ASub1).

% java_cha_input_interface(_,_,_,_):-
%    error_message("The input_user_interface operation is not defined yet"),!.

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%            Intermediate Functions                                      |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%

%------------------------------------------------------------------------%
% types_from_signature(+Sg,+Vars,-ASub)                                  |
%------------------------------------------------------------------------%
% ASub is an abstract substitution which contains Vars. The abstract     |
% state is produced by using the signature of SgKey.                     |
%------------------------------------------------------------------------%
types_from_signature(Sg, Vars, ASub):-
	functor(Sg,F,_),
	java_create('soot.ciao.CiaoInterface',CiaoInterface),       
	java_invoke(CiaoInterface,getFormalParamsTypes(F,TypesList)),
	!,
        java_string_list_to_prolog_term_list(TypesList, Types),
	each_cone(Types, Vars, ASub).


each_cone([],[],[]).
each_cone([void|Ks],Vars,Zs):-
	each_cone(Ks,Vars,Zs).
each_cone([K|Ks],[Var|Vars],[Var/Pow_K|Zs]):-
	cone(K,Pow_K),
	each_cone(Ks,Vars,Zs).

cone(K,[K]).


java_string_list_to_prolog_term_list(Java_List, Prolog_List):-
	java_invoke(Java_List,size(Size)),
	java_string_list_to_prolog_term_list_(Java_List, 0, Size, Prolog_List).

java_string_list_to_prolog_term_list_(_Java_List, Pos, Size, []):-
	Pos >= Size,
	!.
java_string_list_to_prolog_term_list_(Java_List, Pos, Size, [Term|R]):-
	java_invoke(Java_List,get(Pos, String)),
	read_from_string_atmvars(String, Term),
	Pos1 is Pos + 1,
	java_string_list_to_prolog_term_list_(Java_List, Pos1, Size, R).
%------------------------------------------------------------------------%
% change_types(+Types,+Call,-Succ)                                       |
%------------------------------------------------------------------------%
% Types is a list of Var/K (K is not expanded). If Var is not in Call,   |
% then Succ is the union between Var/K' and Call. (K' is the expansion of| 
% K). Otherwise, Succ = Call                                             |
%------------------------------------------------------------------------%
change_types([],Succ,Succ):-!.
change_types([Var/_K|Xs],Call,Succ):-
	( var(Var), var_value(Call,Var,_Value)),!,
	change_types(Xs,Call,Succ).
change_types([Var/K|Xs],Call,Succ):-
	var(Var),!,
	cone(K,Ks),
	change_values(Var,Call,Succ0,Ks),
	change_types(Xs,Succ0,Succ).
change_types([_|Xs],Call,Succ):-
	!,
	change_types(Xs,Call,Succ).

%------------------------------------------------------------------------%
% change_types_insert(+Types,+Call,-Succ)                                |
%------------------------------------------------------------------------%
% Types is a list of Var/K (K is not expanded). If Var is not in Call,   |
% then Succ is the union between Var/K' and Call. (K' is the expansion of| 
% K). Otherwise, it substitutes the old value of Var in Call by K'       |
%------------------------------------------------------------------------%
change_types_insert([],Succ,Succ):-!.
change_types_insert([Var/K|Xs],Call,Succ):-
	var(Var),!,
	cone(K,Ks),
	change_values_insert([Var],Call,Succ0,Ks),
	change_types_insert(Xs,Succ0,Succ).
change_types_insert([_|Xs],Call,Succ):-!,
	change_types_insert(Xs,Call,Succ).

	
