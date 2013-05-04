:- module(_,
	 [java_nullity_call_to_entry/8,
	  java_nullity_exit_to_prime/7,
	  java_nullity_project/3,
	  java_nullity_extend/4,
	  java_nullity_compute_lub/2,
	  java_nullity_glb/3,
	  java_nullity_less_or_equal/2,
	  java_nullity_compute_lub_el/3,
	  java_nullity_sort/2,
	  java_nullity_special_builtin/4,
	  java_nullity_success_builtin/5,      
	  java_nullity_call_to_success_fact/8,
	  java_nullity_input_interface/4,      % not defined yet
	  java_nullity_input_user_interface/3, % not defined yet
	  java_nullity_asub_to_native/3,
	  java_nullity_unknown_call/4,
	  java_nullity_unknown_entry/3,
	  % intermediate operations
	  java_nullity_peel_equations/3,
	  project_aux/3,
	  create_values/3,
	  change_values_insert/4,
	  change_values/4,
	  member_value/3,
	  values_equal/3,
	  var_value/3,
	  update_Call/3,
	  ord_inters_diff/4,
	  java_nullity_remove/3
         ],
	 [assertions,regtypes,basicmodes]).

:- use_module(library(messages), [debug_message/2, error_message/2]).
:- use_module(library(sort)).
:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(sets), [merge/3, ord_subtract/3]).
:- use_module(library(lists), [append/3]).
:- use_module(ilciao(java_interface), 
	[
	 java_create/2,   
	 java_invoke/2
	]).
:- use_module(domain(java_cha), 
	[java_string_list_to_prolog_term_list/2]).
:- use_module(plai(java_aux), 
	[
	 blt/4,
	 special_builtin/2,
	 basic_type/1,
	 object/2,
	 variable/2,
	 null/2,
	 atom/2
        ]).

:- doc(title,"Simple nullity abstract domain for Java programs").
:- doc(author, "Jorge Navas").

%----------------------------------------------------------------------%
% This module implements the abstract operations of a simple nullity   |
% domain for the PLAI framework of abstract interpretation.  An        |
% abstract substitution is a list of Var/Mode elements, where Var is a |
% variable and Mode is 'any', 'n' or 'nn'.                             |
%----------------------------------------------------------------------%
% The abstract domain lattice is:                                      |
%                               any                                    |
%                               /  \                                   |
%                              /    \                                  |
%                    (null)   n     nn  (not null)                     |
%                             \     /                                  |
%                              \   /                                   |
%                              bottom                                  |
%----------------------------------------------------------------------%

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT PROJECTION                               |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_nullity_project(+,+,-)                                               |
% java_nullity_project(ASub,Vars,Proj)                                      |
% Proj is the result of                                                  |
% eliminating from ASub all X/Value such that X not in Vars              |
%------------------------------------------------------------------------%

java_nullity_project('$bottom',_,'$bottom'):- !.
java_nullity_project(ASub,Vars,Proj) :- 
	project_aux(Vars,ASub,Proj).

%------------------------------------------------------------------------%
% project_aux(+,+,-)                                                     %
% project_aux(Vars,ListValues,Proj)                                      %
% Eliminates from each list in the second argument any variable/Value    %
% such that the variable is not an element of the first argument         %
%------------------------------------------------------------------------%

project_aux([],_,[]):- !.
project_aux(_,[],[]):- !.
project_aux([Head1|Tail1],[Head2/Val|Tail2],Proj) :-
	compare(Order,Head1,Head2),
	project_aux_(Order,Head1,Tail1,Head2/Val,Tail2,Proj).
project_aux_(=,_,Tail1,HeadVal,Tail2,[HeadVal|Proj]) :-
	project_aux(Tail1,Tail2,Proj).
project_aux_(>,Head1,Tail1,_,[Head2/Val|Tail2],Proj) :-
	compare(Order,Head1,Head2),
	project_aux_(Order,Head1,Tail1,Head2/Val,Tail2,Proj).

java_nullity_remove([],Proj,Proj):- !.
java_nullity_remove(_,[],[]):- !.
java_nullity_remove([Head1|Tail1],[Head2/Val|Tail2],Proj) :-
	compare(Order,Head1,Head2),
	java_nullity_remove_(Order,Head1,Tail1,Head2/Val,Tail2,Proj).
java_nullity_remove_(=,_,Tail1,_,Tail2,Proj) :-
	java_nullity_remove(Tail1,Tail2,Proj).
java_nullity_remove_(>,Head1,Tail1,Head2/Val,[NHead2/NVal2|Tail2],[Head2/Val|Proj]) :-
	compare(Order,Head1,NHead2),
	java_nullity_remove_(Order,Head1,Tail1,NHead2/NVal2,Tail2,Proj).
java_nullity_remove_(<,_,Tail1,Head2/Val,Tail2,Proj) :-
	java_nullity_remove(Tail1,[Head2/Val|Tail2],Proj).

%------------------------------------------------------------------------%
%                       ABSTRACT Call To Entry                           |
%------------------------------------------------------------------------%
% java_nullity_call_to_entry(+,+,+,+,+,+,-,-)                               |     
% java_nullity_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo)         |
%------------------------------------------------------------------------%
java_nullity_call_to_entry(_Sv,_Sg,_Hv,_Head,_Fv,'$bottom','$bottom',_ExtraInfo).
java_nullity_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
        change_values_insert(Hv,Proj,ASub,_Empty),
	java_nullity_peel_equations(Head,Sg,Eqs),    
	rename_call2entry(Eqs,ASub,ASub0),
%	java_nullity_project(ASub0,Hv,Entry0),
	java_nullity_remove(Sv,ASub0,Entry0),
        change_values_insert(Fv,Entry0,Entry,n),
        ExtraInfo = Fv,!.

rename_call2entry(Eqs,ASub,ASub1):-
	rename_call2entry_(Eqs,ASub,ASub0),
	( ASub == ASub0 -> ASub1 = ASub ;
	  rename_call2entry(Eqs,ASub0,ASub1)).
rename_call2entry_([],ASub,ASub).
rename_call2entry_([(X,Y)|Eqs],ASub,ASub2):-
        rename_call2entry0(X,Y,ASub,ASub1),
        rename_call2entry_(Eqs,ASub1,ASub2).
rename_call2entry0(X,Y,ASub,ASub0):-
	var(Y),!,
	var_value(ASub,Y,Val),
	change_values_insert([X],ASub,ASub0,Val).
rename_call2entry0(X,_Y,ASub,ASub0):-
	!,
	change_values_insert([X],ASub,ASub0,n).	
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT Exit To Prime                            |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_nullity_exit_to_prime(+,+,+,+,+,-,-)                                 |
% java_nullity_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime)            |
%------------------------------------------------------------------------%
java_nullity_exit_to_prime(_Sg,_Hv,_Head,_Sv,'$bottom',_Flag,'$bottom') :- !.
java_nullity_exit_to_prime(Sg,Hv,Head,_Sv,Exit,Fv,Prime):-
	java_nullity_peel_equations(Sg,Head,Eqs),    
	rename_exit2prime(Eqs,Exit,Prime0),
	append(Hv,Fv,HvFv_u),
	sort(HvFv_u,HvFv),
	java_nullity_remove(HvFv,Prime0,Prime).	
%	java_nullity_project(Prime0,Sv,Prime).

rename_exit2prime(Eqs,ASub,ASub1):-
	rename_exit2prime_(Eqs,ASub,ASub0),
	( ASub == ASub0 -> ASub1 = ASub ;
	  rename_exit2prime(Eqs,ASub0,ASub1)).
rename_exit2prime_([],ASub,ASub).
rename_exit2prime_([(X,Y)|Eqs],ASub,ASub2):-
        rename_exit2prime0(X,Y,ASub,ASub1),
        rename_exit2prime_(Eqs,ASub1,ASub2).
rename_exit2prime0(X,Y,ASub,ASub0):-
	var(X),
	basic_type(Y),!,
	change_values_insert([X],ASub,ASub0,n).
rename_exit2prime0(X,Y,ASub,ASub0):-
	var(X),
	var(Y),!,
	var_value(ASub,Y,Val),
	change_values_insert([X],ASub,ASub0,Val).
rename_exit2prime0(_X,Y,ASub,ASub):-
	var(Y),!.
rename_exit2prime0(_X,Y,ASub,ASub):-
	atm(Y),!.
				 	
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT Extend                                   |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_nullity_extend(+,+,+,-)                                           |
% java_nullity_extend(Prime,Sv,Call,Succ)                                |
% If Prime = bottom, Succ = bottom. If Sv = [], Call = Succ.             |
% Otherwise, Succ is computed updating the values of Call with those in  |
% Prime                                                                  |
%------------------------------------------------------------------------%

java_nullity_extend('$bottom',_Sv,_Call,'$bottom'):- !.
java_nullity_extend(_Prime,[],Call,Succ):- !,Call = Succ.
java_nullity_extend(Prime,_Sv,Call,Succ):- 
	update_Call(Call,Prime,Succ).

update_Call([],_,[]).
update_Call(Call,[],Call).
update_Call([X/ValueX|Call],[Y/ValueY|Prime],Succ):-
	compare(Order,X,Y),
	update_Call_(Order,X/ValueX,Call,Y/ValueY,Prime,Succ).

update_Call_(=,_,Call,ElemP,Prime,[ElemP|Succ]):-
	update_Call(Call,Prime,Succ).
update_Call_(<,ElemC,Call,ElemP,Prime,[ElemC|Succ]):-
	update_Call(Call,[ElemP|Prime],Succ).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT LUB                                      |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_nullity_compute_lub(+,-)                                          |
% java_nullity_compute_lub(ListASub,Lub)                                 |
% It computes the lub of a set of Asub. For each two abstract            |
% substitutions ASub1 and ASub2 in ListASub, obtaining the lub is just   |
% foreach X/Value1 in ASub1 and X/Value2 in ASub2:                       |
%    - if Value1 == Value2, X/Value1 in Lub                              |
%    - otherwise, X/any in Lub                                           |
%------------------------------------------------------------------------%
java_nullity_compute_lub([X],X):- !.
java_nullity_compute_lub([ASub1,ASub2|Xs],Lub):-
	java_nullity_compute_lub_el(ASub1,ASub2,ASubLub),
	java_nullity_compute_lub([ASubLub|Xs],Lub).

java_nullity_compute_lub_el('$bottom',ASub,ASub):- !.
java_nullity_compute_lub_el(ASub,'$bottom',ASub):- !.
java_nullity_compute_lub_el(ASub1,ASub2,Lub):- 
	compute_lub_java_nullity(ASub1,ASub2,Lub).

compute_lub_java_nullity([],ASub2,ASub2):- !.
compute_lub_java_nullity(ASub1,[],ASub1):- !.
compute_lub_java_nullity(ASub1,ASub2,Lub):- 
	ASub1 == ASub2, !,
	Lub = ASub1.
compute_lub_java_nullity([Xv|ASub1],[Yv|ASub2],Lub):- 
	Xv == Yv, !,
	Lub = [Xv|Lub_Cont],
	compute_lub_java_nullity(ASub1,ASub2,Lub_Cont).
compute_lub_java_nullity([X/_|ASub1],[X/_|ASub2],[X/any|Lub_Cont]):-
	compute_lub_java_nullity(ASub1,ASub2,Lub_Cont).

%------------------------------------------------------------------------%
%                         HANDLING BUILTINS                              |
%------------------------------------------------------------------------%

%------------------------------------------------------------------------%
% java_nullity_special_builtin(+,+,-,-)                                  |
% java_nullity_special_builtin(SgKey,Sg,Type,Condvars)                   |
% It divides builtins into groups determined by the flag returned in the |
% second argument + some special handling for some builtins.             |
%------------------------------------------------------------------------%
java_nullity_special_builtin(SgKey,_,unchanged,_)       :- 
	blt(ge,SgKey,_,_).
java_nullity_special_builtin(SgKey,_,unchanged,_)       :- 
	blt(gt,SgKey,_,_).
java_nullity_special_builtin(SgKey,_,unchanged,_)       :- 
	blt(lt,SgKey,_,_).
java_nullity_special_builtin(SgKey,_,unchanged,_)       :- 
	blt(le,SgKey,_,_).
java_nullity_special_builtin(SgKey,_,unchanged,_)       :- 
	blt(stf,SgKey,_,_).
java_nullity_special_builtin(SgKey,Sg,new,[O])          :- 
	blt(sta,SgKey,Sg,[O,_,_,_,_]).
java_nullity_special_builtin(SgKey,_,unchanged,_)       :- 
	blt(unk,SgKey,_,_).
java_nullity_special_builtin(SgKey,Sg,null,[X,Y,Z])     :- 
	blt(add,SgKey,Sg,[X,_,Y,_,Z,_]).
java_nullity_special_builtin(SgKey,Sg,null,[X,Y,Z])     :- 
	blt(sub,SgKey,Sg,[X,_,Y,_,Z,_]).
java_nullity_special_builtin(SgKey,Sg,null,[X,Y,Z])     :- 
	blt(mul,SgKey,Sg,[X,_,Y,_,Z,_]).
java_nullity_special_builtin(SgKey,Sg,null,[X,Y,Z])     :- 
	blt(div,SgKey,Sg,[X,_,Y,_,Z,_]).
java_nullity_special_builtin(SgKey,Sg,null,[X,Y,Z])     :- 
	blt(rem,SgKey,Sg,[X,_,Y,_,Z,_]).
java_nullity_special_builtin(SgKey,Sg,new,[O])          :- 
	blt(new,SgKey,Sg,[O,_]).
java_nullity_special_builtin(SgKey,Sg,new,[A])          :- 
	blt(newa,SgKey,Sg,[A,_,_]).
java_nullity_special_builtin(SgKey,Sg,any,[L])          :- 
	blt(gtf,SgKey,Sg,[L,_,_,_,_,_]).
java_nullity_special_builtin(SgKey,Sg,any,[L])          :- 
	blt(gta,SgKey,Sg,[L,_,_,_,_]).
java_nullity_special_builtin(SgKey,Sg,asg,[L,K_L,R,K_R]):- 
	blt(asg,SgKey,Sg,[L,K_L,R,K_R]).
java_nullity_special_builtin(SgKey,Sg,eq,[L,K_L,R,K_R]) :- 
	blt(eq,SgKey,Sg,[L,K_L,R,K_R]).
java_nullity_special_builtin(SgKey,Sg,ne,[L,K_L,R,K_R]) :- 
	blt(ne,SgKey,Sg,[L,K_L,R,K_R]).
% Special methods handled as builtins:
java_nullity_special_builtin(SgKey,_,unchanged,_):-
	special_builtin(object_init,SgKey),!.
java_nullity_special_builtin('true/0',_,unchanged,_).
java_nullity_special_builtin(SgKey,_,_,_):-
	debug_message("Be careful, some builtins are not supported yet, ~q ?",[SgKey]),
	!,fail.

%------------------------------------------------------------------------%
% java_nullity_success_builtin(+,+,+,+,-)                                |
% java_nullity_success_builtin(Type,Sv_u,Condv,Call,Succ)                |
%------------------------------------------------------------------------%
java_nullity_success_builtin(_,_Sv_u,_Condv,'$bottom','$bottom'):- !.
java_nullity_success_builtin(new,_Sv_u,Condv,Call,Succ):- !,
	change_values_insert(Condv,Call,Succ,nn).
java_nullity_success_builtin(null,_Sv_u,Condv,Call,Succ):- !,
	make_null(Condv,Call,Succ).
java_nullity_success_builtin(asg,_Sv_u,[L,K_L,R,K_R],Call,Succ):-
	asg_success_builtin(L,K_L,R,K_R,Call,Succ).
java_nullity_success_builtin(eq,_Sv_u,[L,K_L,R,K_R],Call,Succ):-
	eq_success_builtin(L,K_L,R,K_R,Call,Succ).
java_nullity_success_builtin(ne,_Sv_u,[L,K_L,R,K_R],Call,Succ):-
	ne_success_builtin(L,K_L,R,K_R,Call,Succ).
java_nullity_success_builtin(any,_Sv_u,Condv,Call,Succ):- !,
	change_values_insert(Condv,Call,Succ,any).
java_nullity_success_builtin(unchanged,_,_,Succ,Succ):-!.

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
asg_success_builtin(L,K_L,R,K_R,Call,Succ):-
    object(L,K_L), object(R,K_R), !,   
    var_value(Call,R,Val),
    change_values_insert([L],Call,Succ,Val).
asg_success_builtin(L,K_L,R,K_R,Call,Succ):-
    object(L,K_L), atom(R,K_R), !,   
    change_values_insert([L],Call,Succ,n).
asg_success_builtin(L,K_L,R,K_R,Call,Succ):-
    object(L,K_L), null(R,K_R),!,   
    change_values_insert([L],Call,Succ,n).
asg_success_builtin(L,K_L,R,K_R,Call,Succ):-
    variable(L,K_L), variable(R,K_R), !,
    change_values_insert([L],Call,Succ,n).
asg_success_builtin(L,K_L,R,K_R,Call,Succ):-
    variable(L,K_L), atom(R,K_R), !,
    change_values_insert([L],Call,Succ,n).
asg_success_builtin(L,K_L,R,K_R,_,_):-
    error_message("asg(~q,~q,~q,~q) failed \n",[L,K_L,R,K_R]),!,fail.

%------------------------------------------------------------------------%
% eq_success_builtin(E1,E2)                                              |
%------------------------------------------------------------------------%
% Possible cases:                                                        |
% object == object                                                       |
% object == null or null == object                                       |
% variable == variable                                                   |
% variable == atom or atom == variable                                   |
% atom == atom                                                           |
% null == null                                                           |
%------------------------------------------------------------------------%
eq_success_builtin(L,K_L,R,K_R,Call,Succ):-
    object(L,K_L), object(R,K_R), !,
    var_value(Call,L,Val_L),
    var_value(Call,R,Val_R),
    ( Val_L = n ->
      ( Val_R = nn ->
	Succ ='$bottom'
      ; change_values_insert([R],Call,Succ,n) )
    ; ( Val_L = nn ->
	( Val_R = n ->
	  Succ = '$bottom'
	; change_values_insert([R],Call,Succ,nn) )
      ; % Val_L = any
	change_values_insert([L],Call,Succ,Val_R) ) ).
eq_success_builtin(L,K_L,R,K_R,Call,Succ):-
    object(L,K_L), null(R,K_R), !,
    var_value(Call,L,Val_L),
    ( Val_L = nn ->
      Succ = '$bottom'
    ; change_values_insert([L],Call,Succ,n)).
eq_success_builtin(L,K_L,R,K_R,Call,Succ):-
    object(R,K_R), null(L,K_L), !,
    var_value(Call,R,Val_R),
    ( Val_R = nn ->
      Succ = '$bottom'
    ; change_values_insert([R],Call,Succ,n) ).
eq_success_builtin(L,K_L,R,K_R,Call,Succ):-
    variable(L,K_L), variable(R,K_R), 
    var_value(Call,L,n),
    var_value(Call,R,n), !,
    Succ = Call.
eq_success_builtin(L,K_L,R,K_R,Call,Succ):-
    variable(L,K_L), atom(R,K_R), 
    change_values_insert([L],Call,Succ,n).
eq_success_builtin(L,K_L,R,K_R,Call,Succ):-
    atom(L,K_L), variable(R,K_R), 
    var_value(Call,R,n), !,
    Succ = Call.
eq_success_builtin(L,K_L,R,K_R,Call,Succ):-    
    null(L,K_L), null(R,K_R), !,
    Call = Succ.
eq_success_builtin(L,K_L,R,K_R,Call,Succ):-        
    atom(L,K_L), atom(R,K_R), !,
    ( L == R ->
      Call = Succ
    ; Succ = '$bottom' ).
eq_success_builtin(L,K_L,R,K_R,_,_):-            
    error_message("~q of class ~q == ~q of class ~q failed",
                  [L,K_L,R,K_R]),!,fail.

%------------------------------------------------------------------------%
% ne_success_builtin(E1,E2)                                              |
%------------------------------------------------------------------------%
% Possible cases:                                                        |
% object \== object                                                      |
% object \== null or null \== object                                     |
% variable \== variable                                                  |
% variable \== atom or atom \== variable                                 |
% atom \== atom                                                          |
% null \== null                                                          |
%------------------------------------------------------------------------%
ne_success_builtin(L,K_L,R,K_R,Call,Succ):-
    object(L,K_L), object(R,K_R), !,
    var_value(Call,L,Val_L),
    var_value(Call,R,Val_R),
    ( ( Val_L = n , Val_R = n) ->
      Succ = '$bottom'
    ; Succ = Call ).
ne_success_builtin(L,K_L,R,K_R,Call,Succ):-
    object(L,K_L), null(R,K_R),!,
    var_value(Call,L,Val_L),
    ( Val_L = n ->
      Succ = '$bottom'
    ; Succ = Call).      
ne_success_builtin(L,K_L,R,K_R,Call,Succ):-
    object(R,K_R), null(L,K_L), !,
    var_value(Call,R,Val_R),
    ( Val_R = n ->
      Succ = '$bottom'
    ; Succ = Call).      
ne_success_builtin(L,K_L,R,K_R,Call,Succ):-
    variable(L,K_L), variable(R,K_R), 
    var_value(Call,L,n),
    var_value(Call,R,n), !,
    Succ = Call.
ne_success_builtin(L,K_L,R,K_R,Call,Succ):-
    variable(L,K_L), atom(R,K_R), 
    change_values_insert([L],Call,Succ,n).
ne_success_builtin(L,K_L,R,K_R,Call,Succ):-
    atom(L,K_L), variable(R,K_R),
    var_value(Call,R,n), !,
    Succ = Call.
ne_success_builtin(L,K_L,R,K_R,_Call,Succ):-    
    null(L,K_L), null(R,K_R), !,
    Succ = '$bottom'.
ne_success_builtin(L,K_L,R,K_R,Call,Succ):-        
    atom(L,K_L), atom(R,K_R), !,
    ( L \== R ->
      Call = Succ
    ; Succ = '$bottom' ).
ne_success_builtin(L,K_L,R,K_R,_,_):-            
    error_message("~q of class ~q \== ~q of class ~q failed",
                  [L,K_L,R,K_R]),!,fail.


make_null([],Succ,Succ).
make_null([X|Xs],Call,Succ):-
	var(X),
	change_values_insert([X],Call,Succ0,n),
	make_null(Xs,Succ0,Succ).
make_null([_|Xs],Call,Succ):-
	make_null(Xs,Call,Succ).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                   ABSTRACT Call to Success Fact                        %
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% Specialized version of call_to_entry + exit_to_prime + extend for facts%
%-------------------------------------------------------------------------

java_nullity_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
        change_values_insert(Hv,Proj,Temp1,any),
	java_nullity_exit_to_prime(Sg,Hv,Head,Sv,Temp1,[],Prime),
	java_nullity_extend(Prime,Hv,Call,Succ).
java_nullity_call_to_success_fact(_Sg,_Hv,_Head,_Sv,_Call,_Proj,'$bottom','$bottom').

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT SORT
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_nullity_sort(+,-)                                                 |
% java_nullity_sort(Asub,Asub_s)                                         |
% it sorts the set of X/Value in Asub obtaining Asub_s.                  |
%------------------------------------------------------------------------%
java_nullity_sort('$bottom','$bottom'):- !.
java_nullity_sort(Asub,Asub_s):- sort(Asub,Asub_s).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT ASUB TO NATIVE
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_nullity_asub_to_native(+,+,-)                                     |
% java_nullity_asub_to_native(ASub,Qv,ASub_user)                         |
% The user friendly format consists in extracting the ground variables   |
% and the nonground variables                                            |
%------------------------------------------------------------------------%
java_nullity_asub_to_native('$bottom',_Qv,_ASub_user):- !, fail.
java_nullity_asub_to_native(Abs,_Qv,ASub_user):-
	member_value(Abs,Nv,n),
	member_value(Abs,NNv,nn),
	member_value(Abs,Av,any),
	( Nv=[] -> ASub_user=ASub_user0 ; ASub_user=[null(Nv)|ASub_user0] ),
	( NNv=[] -> ASub_user0=ASub_user1 ; ASub_user0=[not_null(NNv)|ASub_user1] ),
	( Av=[] -> ASub_user1=[] ; ASub_user1=[any(Av)] ).

%------------------------------------------------------------------------%
% java_nullity_output_interface(+,-)                                     |
% java_nullity_output_interface(ASub,Output)                             |
% The readible format still close to the internal formal is identical    |
%------------------------------------------------------------------------% 
% java_nullity_output_interface(Output,Output).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT UNKNOWN ENTRY
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_nullity_unknown_entry(+,+,-)                                      |
% java_nullity_unknown_entry(Sg,Qv,Call)                                 |
% The top value is  X/any forall X in the set of variables               |
%------------------------------------------------------------------------%
java_nullity_unknown_entry(Sg,Qv,Call):-
	nullity_from_signature(Sg,Vals),
	each_create_values(Qv,Vals,Call).

each_create_values([],[],[]).
each_create_values([Qv|Qvs],[Val|Vals],[Qv/Val|Call]):-
	each_create_values(Qvs,Vals,Call).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT UNKNOWN CALL
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_nullity_unknown_call(+,+,+,-)                                     | 
% java_nullity_unknown_call(Sg,Call,Vars,Succ)                           |
% Gives the "top" value for the variables involved in a                  |
% literal whose definition is not present, and adds this top value to    |
% Call.                                                                  |
%------------------------------------------------------------------------%
% NOTE: Java semantics assures that only the ret argument may change its |
% abstract state after this operations. This holds only for nullity and  |
% some other abstract domains.                                           |
%------------------------------------------------------------------------%

java_nullity_unknown_call(_Sg,'$bottom',_Vars,'$bottom').
java_nullity_unknown_call(Sg,Call,_Sv,Succ):-
	nullity_from_signature(Sg,Vals),
	( Vals = [] ->
	  Call = Succ
        ;
	  arg(1,Sg,Ret),  
	  Vals = [Val|_],
	  change_values_insert([Ret],Call,Succ,[Val])
	).    
	
% This version assumes that all arguments can change their state after 
% calling the external method. However, for nullity ONLY the ret argument can
% change.
%  	ord_inters_diff(Call,Sv,_Int,Disj),
%  	functor(Sg,F,A),
%  	functor(Head,F,A),
% 	varset(Head,Hv),
%  	nullity_from_signature(Head,Vals),
% 	create_asub_from_lists(Hv,Vals,Exit),
%  	java_nullity_exit_to_prime(Sg,Hv,Head,Sv,Exit,[],Prime),
%         ord_union(Prime,Disj,Succ).

/*
create_asub_from_lists([],[],[]).
create_asub_from_lists([V|Vs],[Val|Vals],[V/Val|Rs]):-
	create_asub_from_lists(Vs,Vals,Rs).
*/

nullity_from_signature(Sg,Vals):-
	functor(Sg,F,_),
	java_create('soot.ciao.CiaoInterface',CiaoInterface),       
	java_invoke(CiaoInterface,getFormalParamsTypes(F,TypesList)),
	!,
        java_string_list_to_prolog_term_list(TypesList, Types),
	resolve_nullity(Types,Vals).
	
resolve_nullity([],[]).
resolve_nullity([X|Xs],[n|Vals]):-
	basic_type(X),!,
	resolve_nullity(Xs,Vals).
resolve_nullity([_|Xs],[any|Vals]):-
	!,
	resolve_nullity(Xs,Vals).

ord_inters_diff([],_Xs,[],[]):-!.
ord_inters_diff(Ys,[],[],Ys):-!.
ord_inters_diff([X/Val|Xs],[Y|Ys],[X/Val|Int],Diff):-
	X == Y,!,
	ord_inters_diff(Xs,Ys,Int,Diff).
ord_inters_diff([X/Val|Xs],[Y|Ys],Int,[X/Val|Diff]):-
	!,
	ord_inters_diff(Xs,[Y|Ys],Int,Diff).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT LESS OR EQUAL
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_nullity_less_or_equal(+,+)                                           |
% java_nullity_less_or_equal(ASub0,ASub1)                                   |
% Succeeds if ASub1 is more general or equal to ASub0                    |
%------------------------------------------------------------------------%
java_nullity_less_or_equal('$bottom',_).
java_nullity_less_or_equal(ASub0,ASub1):-
	java_nullity_less_or_equal_(ASub0,ASub1).

java_nullity_less_or_equal_([],[]).
java_nullity_less_or_equal_([X/Value0|Rest0],[X/Value1|Rest1]):-
	java_nullity_less_or_equal_el(Value0,Value1),
	java_nullity_less_or_equal_(Rest0,Rest1).

java_nullity_less_or_equal_el(n,n).
java_nullity_less_or_equal_el(n,any).
java_nullity_less_or_equal_el(nn,nn).
java_nullity_less_or_equal_el(nn,any).
java_nullity_less_or_equal_el(any,any).
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT GLB
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_nullity_glb(+,+,-)                                                |
% java_nullity_glb(ASub0,ASub1,Glb)                                      |
%------------------------------------------------------------------------%
java_nullity_glb(ASub0,ASub1,Glb):-
	ASub0 == ASub1,!,
	Glb = ASub1.
java_nullity_glb(ASub0,ASub1,Glb):-
	java_nullity_glb_(ASub0,ASub1,Glb),!.
java_nullity_glb(_,_,'$bottom').

java_nullity_glb_([],[],[]).
java_nullity_glb_([Xv|ASub0],[Yv|ASub1],[Xv|Glb]):-
	Xv == Yv,!,
	java_nullity_glb_(ASub0,ASub1,Glb).
java_nullity_glb_([X/any|ASub0],[X/Value|ASub1],[X/Value|Glb]):-
	!,
	java_nullity_glb_(ASub0,ASub1,Glb).
java_nullity_glb_([X/Value|ASub0],[X/any|ASub1],[X/Value|Glb]):-
	!,
	java_nullity_glb_(ASub0,ASub1,Glb).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT INPUT USER INTERFACE
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% java_nullity_input_user_interface(+,+,-)                               |
% java_nullity_input_user_interface(InputUser,Qv,ASub)                   |
% Obtaining the abstract substitution for null from the user supplied    |
%------------------------------------------------------------------------%
java_nullity_input_user_interface((N_u,NN_u),Qv,ASub):-
	may_be_var(N_u,N),
	may_be_var(NN_u,NN),
	merge(N,NN,Infv),
	ord_subtract(Qv,Infv,AnyV),
	create_values(N,Temp1,n),
	change_values_insert(NN,Temp1,Temp2,nn),
	change_values_insert(AnyV,Temp2,ASub,any).

% java_nullity_input_user_interface(_,_Qv,_ASub):-
%    error_message("The input_user_interface operation is not defined yet"),!.

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT INPUT INTERFACE                          |
%------------------------------------------------------------------------%
% java_nullity_input_interface(+,?,+,+)                                  |
% java_nullity_input_interface(Prop,Kind,Struct0,Struct1)                |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
java_nullity_input_interface(null(X),perfect,(N0,NN),(N,NN)):-
	varset(X,Vs),
	myappend(N0,Vs,N).
% java_nullity_input_interface(free(X),approx,Struct0,Struct):-
% 	gr_input_interface(not_ground(X),_Any,Struct0,Struct).
java_nullity_input_interface(not_null(X),perfect,(N,NN0),(N,NN)):-
	varset(X,Vs),
	myappend(Vs,NN0,NN).

% java_nullity_input_interface(_,_,_,_):-
%    error_message("The input_user_interface operation is not defined yet"),!.

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                        Intermediate Functions                          |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%

%------------------------------------------------------------------------%
% create_values(+,-,+)                                                   |
% create_values(Vars,Asub,Value)                                         |
% Forall X in Vars, X/Value is in Asub                                   |
%------------------------------------------------------------------------%
create_values([],[],_Value):-
	!.
create_values([X|Xs],New,Value):-
	nonvar(X),
	!,
	create_values(Xs,New,Value).
create_values([X|Xs],[X/Value|New],Value):-
	!,
	create_values(Xs,New,Value).

%------------------------------------------------------------------------%
% change_values_insert(+,+,-,+)                                          |
% change_values_insert(Vars,Fr,NewFr,Value)                              |
% Forall X in Vars, if exists X/V in Fr it is changed to X/Value,        |
% otherwise X/Value is added to Fr. Both Ordered                         |
%------------------------------------------------------------------------%
change_values_insert([],Fr,Fr,_):- 
	!.
change_values_insert(Vars,[],NewFr,Value):- 
	!,
	create_values(Vars,NewFr,Value).
change_values_insert([X|Xs],Fr,NewFr,Value):- 
	nonvar(X),
	!,
	change_values_insert(Xs,Fr,NewFr,Value).
change_values_insert([X|Xs],[Y/Val|Fr],NewFr,Value):- 
	!,
	compare(D,Y,X),
	change_values_insert_(D,Y/Val,Fr,X,Xs,NewFr,Value).

change_values_insert_(=,_,Fr,X,Xs,[X/Value|NewFr],Value):-
	change_values_insert(Xs,Fr,NewFr,Value).
change_values_insert_(>,Elem,Fr,X,[],NewFr,Value):- !,
	NewFr = [X/Value,Elem|Fr].
change_values_insert_(>,Elem,Fr,X,Xs,[X/Value|NewFr],Value):- 
	change_values_insert(Xs,[Elem|Fr],NewFr,Value).
change_values_insert_(<,Elem,[],X,Xs,NewFr,Value):- !,
	NewFr = [Elem,X/Value|RestFr],
	create_values(Xs,RestFr,Value).
change_values_insert_(<,Elem,Fr,X,Xs,[Elem|NewFr],Value):-
	change_values_insert([X|Xs],Fr,NewFr,Value).

myappend(Vs,V0,V):-
	var(Vs), !,
	V=V0.
myappend(Vs,V0,V):-
	merge(Vs,V0,V).

may_be_var(X,X):- ( X=[] ; true ), !.

%------------------------------------------------------------------------%
% change_values(+,+,-,+)                                                 |
% change_values(Vars,Fr,NewFr,Value)                                     |
% As change_values_insert/4 but it does not insert.                      |
%------------------------------------------------------------------------%
change_values([],Fr,Fr,_):- 
	!.
change_values(_Vars,[],[],_Value):- 
	!.
change_values([X|Xs],Fr,NewFr,Value):- 
	nonvar(X),
	!,
	change_values(Xs,Fr,NewFr,Value).
change_values([X|Xs],[Y/Val|Fr],NewFr,Value):- 
	!,
	compare(D,Y,X),
	change_values_(D,Y/Val,Fr,X,Xs,NewFr,Value).

change_values_(=,_,Fr,X,Xs,[X/Value|NewFr],Value):-
	change_values(Xs,Fr,NewFr,Value).
change_values_(>,Elem,Fr,_X,[],[Elem|Fr],_Value):- !.
change_values_(>,Elem,Fr,_X,Xs,NewFr,Value):- 
	change_values(Xs,[Elem|Fr],NewFr,Value).
change_values_(<,Elem,Fr,X,Xs,[Elem|NewFr],Value):-
	change_values([X|Xs],Fr,NewFr,Value).

%------------------------------------------------------------------------%
% member_value(+,-,+)                                                    |
% member_value(Abs,Vars,Value)                                           |
% %----------------------------------------------------------------------%

member_value([],[],_).
member_value([X/V|Rest],[X|RestV],Value):-
	V==Value,!,
	member_value(Rest,RestV,Value).
member_value([_|Rest],RestV,Value):-
	member_value(Rest,RestV,Value).

%------------------------------------------------------------------------%
% values_equal(+,+,+)                                                    |
% values_equal(Vars,Fr,Value)                                            |
% Satisfied if the values of all variables in Vars is equal to Value     |
%------------------------------------------------------------------------%

values_equal([],_,_).
values_equal([X|Xs],[Y/V|Ys],Value):-
	compare(D,X,Y),
	values_equal_(D,X,Xs,V,Ys,Value).

values_equal_(=,_X,Xs,Value,Ys,Value):-
	values_equal(Xs,Ys,Value).
values_equal_(>,X,Xs,_,[Y/V|Ys],Value):-
	compare(D,X,Y),
	values_equal_(D,X,Xs,V,Ys,Value).

%------------------------------------------------------------------------%
% var_value(+,+,-)                                                       |
% var_value(Fr,X,Value)                                                  |
% It obtains in the third argument the value of X (n, nn or any)         |
%------------------------------------------------------------------------%

var_value([Y/V|More],X,Value):-
	compare(D,X,Y),
	var_value_(D,V,More,X,Value).

var_value_(=,Value,_More,_X,Value).
var_value_(>,_Elem,[Y/V|More],X,Value):-
	compare(D,X,Y),
	var_value_(D,V,More,X,Value).

%------------------------------------------------------------------------%
% peel_equations(+,+,-)                                                  |
% peel_equations(Term1,Term2,Eqs)                                        |
% Eqs is a list of pairs VarX-VarY, VarX-atmY / atmY-VarX, or atmX-atmX  |
%------------------------------------------------------------------------%

java_nullity_peel_equations(Term1,Term2,Equs) :-
	sh_peel(Term1,Term2,Temp-[]),!,
	sort(Temp,Equs). 

sh_peel(Term1,Term2,Binds) :- 
	Term1 == Term2, !,
	Binds = X-X.  

sh_peel(Term1,Term2,Binds) :-
	var(Term1),!,
	valid_value(Term2),
	Binds = [(Term1,Term2)|X]-X.
sh_peel(Term1,Term2,Binds) :-
	var(Term2),!,
	valid_value(Term1),
	Binds = [(Term1,Term2)|X]-X.
sh_peel(Term1,Term2,Binds) :-
	functor(Term1,F,N),
	functor(Term2,F,N),
	sh_peel_args(Term1,Term2,0,N,Binds).

valid_value(Term):- var(Term),!.
valid_value(Term):- atm(Term),!.
valid_value(Term):- number(Term),!.

sh_peel_args(_,_,N1,N,Binds) :-
	N1 = N, !,
	Binds = X-X.
sh_peel_args(Term1,Term2,N1,N,Binds) :-
	N2 is N1 + 1,
	arg(N2,Term1,A1),
	arg(N2,Term2,A2),
	sh_peel(A1,A2,Bind1),
	sh_peel_args(Term1,Term2,N2,N,Bind2),
	append_dl(Bind1,Bind2,Binds).

append_dl(X-Y,Y-Z,X-Z).

