:- module(nfplai,
	[ nf_call_to_entry/8,
	  nf_exit_to_prime/7,
	  nf_project/3,
	  nf_extend/4,
	  nf_widen/3,
	  nf_widencall/3,
	  nf_compute_lub/2,
	  nf_compute_clauses_lub/3,
	  nf_glb/3,
	  nf_less_or_equal/2,
	  nf_identical_abstract/2,
	  nf_sort/2,
	  nf_call_to_success_fact/8,
	  nf_split_combined_domain/3,
	  nf_special_builtin/4,
	  nf_success_builtin/5,
	%  nf_call_to_success_builtin/6,
	  nf_input_interface/4,
	  nf_input_user_interface/3,
	  nf_asub_to_native/5,
	  nf_unknown_call/3,
	  nf_unknown_entry/2,
	  nf_empty_entry/2,
          nf_statistics/1
	],
	[ assertions,regtypes,basicmodes
	]).

:- use_module(domain(eterms)).
:- use_module(domain(share)).
:- use_module(domain(nfabs)).

:- use_module(library(idlists), [memberchk/2]).
:- use_module(library(lists), [append/3]).
:- use_module(library(sets), [ord_subtract/3]).
:- use_module(library(sort), [sort/2]).

% Solved: 
%:- doc(bug,"1. Some asubs carry $bottom within the nf/3 representation.").
% was because of builtins; solution: the if-then-elses in split_back

%------------------------------------------------------------------------%

%% asub('$bottom','$bottom',_Modes,_NonF):- !.
%% asub('$bottom',_Types,'$bottom',_NonF):- !.
%% asub('$bottom',_Types,_Modes,'$bottom'):- !.
asub(nf(Types,Modes,NonF),Types,Modes,NonF).

%------------------------------------------------------------------------%
% nf_call_to_entry(+,+,+,+,+,+,-,-)                                      %
% nf_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo)                %
%------------------------------------------------------------------------%

nf_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	asub(Proj,PTypes,PModes,PNonF),
	shfr_call_to_entry(Sv,Sg,Hv,Head,Fv,PModes,EModes,ExtraInfoModes),
	eterms_call_to_entry(Sg,Hv,Head,Fv,PTypes,ETypes,ExtraInfoTypes),
	( ETypes = '$bottom'
	-> Entry = '$bottom'
	 ; nfabs:nf_call_to_entry(Sv,Sg,Hv,Head,Fv,PNonF,ENonF,_Extra),
	   shfr_obtain(free,Sv,PModes,FVars),
	   ord_subtract(Sv,FVars,InVars),
	   asub(Entry,ETypes,EModes,ENonF)
	),
	asub(ExtraInfo,ExtraInfoTypes,ExtraInfoModes,InVars).

%------------------------------------------------------------------------%
% nf_exit_to_prime(+,+,+,+,+,-,-)                                        %
% nf_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime)                   %
%------------------------------------------------------------------------%

nf_exit_to_prime(_Sg,_Hv,_Head,_Sv,'$bottom',_ExtraInfo,'$bottom'):- !.
nf_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	asub(Exit,ETypes,EModes,ENonF),
	asub(ExtraInfo,ExtraInfoTypes,ExtraInfoModes,ExtraInfoNonF),
	shfr_exit_to_prime(Sg,Hv,Head,Sv,EModes,ExtraInfoModes,PModes),
	eterms_exit_to_prime(Sg,Hv,Head,Sv,ETypes,ExtraInfoTypes,PTypes),
	( PTypes = '$bottom'
	-> Prime = '$bottom'
	 ; nfabs:nf_exit_to_prime(Sg,Hv,Head,Sv,ENonF,ExtraInfoNonF,PNonF),
	   ( PNonF = '$bottom'
	   -> Prime = '$bottom'
	    ; asub(Prime,PTypes,PModes,PNonF) )
	).

%------------------------------------------------------------------------%
% nf_project(+,+,-)                                                      %
% nf_project(ASub,Vars,Proj)                                             %
%------------------------------------------------------------------------%

nf_project('$bottom',_Vars,'$bottom'):- !.
nf_project(ASub,Vars,Proj):-
	asub(ASub,ATypes,AModes,ANonF),
	shfr_project(AModes,Vars,PModes),
	eterms_project(Vars,ATypes,PTypes),
	nfabs:nf_project(ANonF,Vars,PNonF),
	asub(Proj,PTypes,PModes,PNonF).

%------------------------------------------------------------------------%
% nf_extend(+,+,+,-)                                                     %
% nf_extend(Prime,Sv,Call,Succ)                                          %
%------------------------------------------------------------------------%

nf_extend('$bottom',_Sv,_Call,'$bottom'):- !.
nf_extend(Prime,Sv,Call,Succ):-
	asub(Prime,PTypes,PModes,PNonF),
	asub(Call,CTypes,CModes,CNonF),
	shfr_extend(PModes,Sv,CModes,SModes),
	eterms_extend(PTypes,Sv,CTypes,STypes),
	nfabs:nf_extend(PNonF,Sv,CNonF,SNonF),
	asub(Succ,STypes,SModes,SNonF).

%------------------------------------------------------------------------%
% nf_widen(+,+,-)                                                        %
% nf_widen(ASub1,ASub2,ASub)                                             %
%------------------------------------------------------------------------%

nf_widen('$bottom',ASub1,ASub):- !, ASub=ASub1.
nf_widen(ASub0,'$bottom',ASub):- !, ASub=ASub0.
nf_widen(ASub0,ASub1,ASub):-
	asub(ASub0,ATypes0,AModes0,ANonF0),
	asub(ASub1,ATypes1,AModes1,ANonF1),
	shfr_compute_lub([AModes0,AModes1],AModes),
	eterms_widen(ATypes0,ATypes1,ATypes),
	nfabs:nf_compute_lub([ANonF0,ANonF1],ANonF),
	asub(ASub,ATypes,AModes,ANonF).

%------------------------------------------------------------------------%
% nf_widencall(+,+,-)                                                    %
% nf_widencall(ASub1,ASub2,ASub)                                         %
%------------------------------------------------------------------------%

nf_widencall('$bottom',ASub1,ASub):- !, ASub=ASub1.
nf_widencall(ASub0,'$bottom',ASub):- !, ASub=ASub0.
nf_widencall(ASub0,ASub1,ASub):-
	asub(ASub0,ATypes0,_AModes0,_ANonF0),
	asub(ASub1,ATypes1,AModes1,ANonF1),
	eterms_widencall(ATypes0,ATypes1,ATypes),
	asub(ASub,ATypes,AModes1,ANonF1).

%------------------------------------------------------------------------%
% nf_compute_lub(+,-)                                                    %
% nf_compute_lub(ListASub,Lub)                                           %
%------------------------------------------------------------------------%

nf_compute_lub(ListASub,Lub):-
	split(ListASub,LTypes,LModes,LNonF),
	shfr_compute_lub(LModes,LubModes),
	eterms_compute_lub(LTypes,LubTypes),
	nfabs:nf_compute_lub(LNonF,LubNonF),
	asub(Lub,LubTypes,LubModes,LubNonF).

split([],[],[],[]).
split([ASub|ListASub],[ATypes|LTypes],[AModes|LModes],[ANonF|LNonF]):-
	asub(ASub,ATypes,AModes,ANonF),
	split(ListASub,LTypes,LModes,LNonF).

nf_split_combined_domain(ListASub,[LTypes,LModes,LNonF],[eterms,shfr,nf]):-
	( var(LTypes)
	-> split(ListASub,LTypes,LModes,_LNonF),
	   LNonF=ListASub
	 ; split_back(ListASub,LTypes,LModes,LNonF)
	).

split_back([],[],[],[]).
split_back([ASub|ListASub],[ATypes|LTypes],[AModes|LModes],[ASubNonF|LNonF]):-
	( ATypes == '$bottom' -> ASub = '$bottom'
	; AModes == '$bottom' -> ASub = '$bottom'
	; asub(ASub,ATypes,AModes,ANonF),
	  asub(ASubNonF,_ATypes,_AModes,ANonF)
	),
	split_back(ListASub,LTypes,LModes,LNonF).

%------------------------------------------------------------------------%
% nf_compute_clauses_lub(+,-)                                            %
% nf_compute_clauses_lub(ListASub,Lub)                                   %
%------------------------------------------------------------------------%

nf_compute_clauses_lub(['$bottom'],_Proj,[]):- !.
nf_compute_clauses_lub([ASub],Proj,[Lub]):-
	asub(ASub,ATypes,AModes,ANonFList),
	asub(Proj,PTypes,PModes,_PNonFList),
	compute_modetypes(PTypes,PModes,_Head,ModeTypes),
	nf_compute_covering(ModeTypes,ANonFList,LubNonF),
	asub(Lub,ATypes,AModes,LubNonF).

compute_modetypes(Types,Modes,Head,MTypes):-
	% shfr_obtain(free,_Vars,Modes,FVars), % PLG
        shfr_obtain(free,Modes,FVars), % Added PLG
	sort(Types,Types_s),
	compute_modetypes0(Types_s,FVars,Vars,ModeTypes),
	Head =.. [p|Vars],
	MTypes =.. [p|ModeTypes].

compute_modetypes0([],_FVars,[],[]).
compute_modetypes0([Var:(_,T)|Types],FVars,[Var|Vars],[M:T|ModeTypes]):-
	get_mode(Var,FVars,M),
	compute_modetypes0(Types,FVars,Vars,ModeTypes).

get_mode(Var,FVars,M):-
	memberchk(Var,FVars), !,
	M = out.
get_mode(_Var,_GVars,in).

%------------------------------------------------------------------------%
% nf_glb(+,+,-)                                                          %
% nf_glb(ASub0,ASub1,Glb)                                                %
%------------------------------------------------------------------------%

nf_glb('$bottom',_ASub1,'$bottom'):- !.
nf_glb(_ASub0,'$bottom','$bottom'):- !.
nf_glb(ASub0,ASub1,Glb):-
	asub(ASub0,ATypes0,AModes0,ANonF0),
	asub(ASub1,ATypes1,AModes1,ANonF1),
	shfr_glb(AModes0,AModes1,GModes),
	eterms_glb(ATypes0,ATypes1,GTypes),
	nfabs:nf_glb(ANonF0,ANonF1,GNonF),
	asub(Glb,GTypes,GModes,GNonF).

%------------------------------------------------------------------------%
% nf_less_or_equal(+,+)                                                  %
% nf_less_or_equal(ASub0,ASub1)                                          %
%------------------------------------------------------------------------%

nf_less_or_equal('$bottom','$bottom'):- !.
nf_less_or_equal(ASub0,ASub1):-
	asub(ASub0,ATypes0,AModes0,ANonF0),
	asub(ASub1,ATypes1,AModes1,ANonF1),
	shfr_less_or_equal(AModes0,AModes1),
	eterms_less_or_equal(ATypes0,ATypes1),
	nfabs:nf_less_or_equal(ANonF0,ANonF1).

%------------------------------------------------------------------------%
% nf_identical_abstract(+,+)                                             %
% nf_identical_abstract(ASub1,ASub2)                                     %
%------------------------------------------------------------------------%

nf_identical_abstract('$bottom','$bottom'):- !.
nf_identical_abstract(ASub0,ASub1):-
	asub(ASub0,ATypes0,AModes0,ANonF0),
	asub(ASub1,ATypes1,AModes1,ANonF1),
	AModes0 == AModes1,
	eterms_identical_abstract(ATypes0,ATypes1),
	nfabs:nf_identical_abstract(ANonF0,ANonF1).

%------------------------------------------------------------------------%
% nf_sort(+,-)                                                           %
% nf_sort(ASub0,ASub1)                                                   %
%------------------------------------------------------------------------%

nf_sort('$bottom','$bottom'):- !.
nf_sort(ASub0,ASub1):-
	asub(ASub0,ATypes0,AModes0,ANonF0),
	shfr_sort(AModes0,AModes1),
	eterms_sort(ATypes0,ATypes1),
	nfabs:nf_sort(ANonF0,ANonF1),
	asub(ASub1,ATypes1,AModes1,ANonF1).

%------------------------------------------------------------------------%
% nf_call_to_success_fact(+,+,+,+,+,+,-,-)                               %
% nf_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ)            %
%-------------------------------------------------------------------------

nf_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	asub(Call,CTypes,CModes,CNonF),
	asub(Proj,PTypes,PModes,PNonF),
	shfr_call_to_success_fact(Sg,Hv,Head,Sv,CModes,PModes,RModes,SModes),
	eterms_call_to_success_fact(Sg,Hv,Head,Sv,CTypes,PTypes,RTypes,STypes),
	nfabs:nf_call_to_success_fact(Sg,Hv,Head,Sv,CNonF,PNonF,RNonF,SNonF),
	asub(Prime,RTypes,RModes,RNonF),
	asub(Succ,STypes,SModes,SNonF).


%-------------------------------------------------------------------------
% nf_special_builtin(+,+,-,-)                                            |
% nf_special_builtin(SgKey,Sg,Type,Condvars)                             |
%-------------------------------------------------------------------------

nf_special_builtin(SgKey,Sg,SgKey,Sg):-
	nfabs:nf_special_builtin(SgKey).

%-------------------------------------------------------------------------
% nf_success_builtin(+,+,+,+,-)                                          |
% nf_success_builtin(Type,Sv_u,Condv,Call,Succ)                          |
%-------------------------------------------------------------------------

nf_success_builtin(Type,_Sv_u,Sg,Call,Succ):-
	asub(Call,Types,Modes,CallNonF),
	nfabs:nf_success_builtin(Type,Modes,Sg,CallNonF,SuccNonF),
	asub(Succ,Types,Modes,SuccNonF).

%-------------------------------------------------------------------------
% nf_call_to_success_builtin(+,+,+,+,+,-)                                %
% nf_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ)                 %
%-------------------------------------------------------------------------
% Not used

%------------------------------------------------------------------------%
% nf_input_interface(+,+,+,-)                                            %
% nf_input_interface(InputUser,Kind,StructI,StructO)                     %
%------------------------------------------------------------------------%

nf_input_interface(InputUser,Kind,StructI,StructO):-
	( nonvar(Kind)
	-> KModes=Kind, KTypes=Kind, KNonF=Kind
	; true ),
	asub(StructI,ITypes,IModes,INonF),
	shfr_input_interface_(InputUser,KModes,IModes,OModes),
	eterms_input_interface_(InputUser,KTypes,ITypes,OTypes),
	nf_input_interface_(InputUser,KNonF,INonF,ONonF),
	asub(StructO,OTypes,OModes,ONonF).

shfr_input_interface_(InputUser,Kind,IModes,OModes):-
	shfr_input_interface(InputUser,Kind,IModes,OModes), !.
shfr_input_interface_(_InputUser,_Kind,IModes,IModes).

eterms_input_interface_(InputUser,Kind,ITypes,OTypes):-
	eterms_input_interface(InputUser,Kind,ITypes,OTypes), !.
eterms_input_interface_(_InputUser,_Kind,ITypes,ITypes).

nf_input_interface_(InputUser,Kind,INonF,ONonF):-
	nfabs:nf_input_interface(InputUser,Kind,INonF,ONonF), !.
nf_input_interface_(_InputUser,_Kind,INonF,INonF).

%------------------------------------------------------------------------%
% nf_input_user_interface(+,+,-)                                         %
% nf_input_user_interface(InputUser,Qv,ASub)                             %
%------------------------------------------------------------------------%

nf_input_user_interface(Struct,Qv,ASub):-
	asub(Struct,Types,Modes,NonF),
	shfr_input_user_interface(Modes,Qv,AModes),
	eterms_input_user_interface(Types,Qv,ATypes),
	nfabs:nf_input_user_interface(NonF,Qv,ANonF),
	asub(ASub,ATypes,AModes,ANonF).

%------------------------------------------------------------------------%
% nf_asub_to_native(+,+,+,-,-)                                           %
% nf_asub_to_native(ASub,Qv,Flag,Stat,Comp)                              %
%------------------------------------------------------------------------%
% Qv should be the goal for comp-props!!!!!

nf_asub_to_native(ASub,Qv,Flag,Props,CompProps):-
	asub(ASub,ATypes,AModes,ANonF),
	shfr_asub_to_native(AModes,Qv,Props1),
	eterms_asub_to_native(ATypes,Flag,Props2),
	nfabs:nf_asub_to_native(ANonF,Qv,CompProps),
	append(Props1,Props2,Props).

%------------------------------------------------------------------------%
% nf_unknown_call(+,+,-)                                                 %
% nf_unknown_call(Vars,Call,Succ)                                        %
%------------------------------------------------------------------------%

nf_unknown_call(Vars,Call,Succ):-
	asub(Call,CTypes,CModes,CNonF),
	shfr_unknown_call(CModes,Vars,SModes),
	eterms_unknown_call(CTypes,Vars,STypes),
	nfabs:nf_unknown_call(Vars,CNonF,SNonF),
	asub(Succ,STypes,SModes,SNonF).

%------------------------------------------------------------------------%
% nf_unknown_entry(+,-)                                                  %
% nf_unknown_entry(Vars,Entry)                                           %
%------------------------------------------------------------------------%

nf_unknown_entry(Vars,Entry):-
	shfr_unknown_entry(Vars,EModes),
	eterms_unknown_entry(Vars,ETypes),
	nfabs:nf_unknown_entry(Vars,ENonF),
	asub(Entry,ETypes,EModes,ENonF).

%------------------------------------------------------------------------%
% nf_empty_entry(+,-)                                                    %
% nf_empty_entry(Vars,Entry)                                             %
%------------------------------------------------------------------------%

nf_empty_entry(Vars,Entry):-
	shfr_empty_entry(Vars,EModes),
	eterms_empty_entry(Vars,ETypes),
	nfabs:nf_empty_entry(Vars,ENonF),
	asub(Entry,ETypes,EModes,ENonF).

%-----------------------------------------------------------------------

nf_statistics(Info):- nfabs:nf_statistics(Info).
