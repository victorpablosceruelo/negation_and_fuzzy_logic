:- module(detplai,
	[ det_call_to_entry/8,
	  det_exit_to_prime/7,
	  det_project/3,
	  det_extend/4,
	  det_widen/3,
	  det_widencall/3,
	  det_compute_lub/2,
	  det_compute_clauses_lub/3,
	  det_glb/3,
	  det_less_or_equal/2,
	  det_identical_abstract/2,
	  det_sort/2,
	  det_call_to_success_fact/8,
	  det_split_combined_domain/3,
	  det_special_builtin/4,
	  det_success_builtin/5,
	%  det_call_to_success_builtin/6,
	  det_input_interface/4,
	  det_input_user_interface/3,
	  det_asub_to_native/5,
	  det_unknown_call/3,
	  det_unknown_entry/2,
	  det_empty_entry/2,
          det_statistics/1,
          det_obtain/4
	],
	[ assertions,regtypes,basicmodes
	]).

:- use_module(domain(eterms)).
:- use_module(domain(share)).
:- use_module(domain(detabs)).

:- use_module(library(idlists), [memberchk/2]).
:- use_module(library(lists), [append/3]).
% :- use_module(library(sets), [ord_subtract/3]). % Commented out. Aug 24, 2012. Not used anymore -PLG 
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
% det_call_to_entry(+,+,+,+,+,+,-,-)                                      %
% det_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo)                %
%------------------------------------------------------------------------%

det_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	asub(Proj,PTypes,PModes,PNonF),
	shfr_call_to_entry(Sv,Sg,Hv,Head,Fv,PModes,EModes,ExtraInfoModes),
	eterms_call_to_entry(Sg,Hv,Head,Fv,PTypes,ETypes,ExtraInfoTypes),
	( ETypes = '$bottom'
	-> Entry = '$bottom'
	 ; detabs:det_call_to_entry(Sv,Sg,Hv,Head,Fv,PNonF,ENonF,_Extra),
           shfr_obtain(ground,Sv,PModes,InVars), % Added. Aug 24, 2012 -PLG 
	   % shfr_obtain(free,Sv,PModes,FVars),  % Commented out. Aug 24, 2012. Not a safe asumption. -PLG 
	   % ord_subtract(Sv,FVars,InVars),      % Commented out. Aug 24, 2012 -PLG 
	   asub(Entry,ETypes,EModes,ENonF)
	),
	asub(ExtraInfo,ExtraInfoTypes,ExtraInfoModes,InVars).

%------------------------------------------------------------------------%
% det_exit_to_prime(+,+,+,+,+,-,-)                                        %
% det_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime)                   %
%------------------------------------------------------------------------%

det_exit_to_prime(_Sg,_Hv,_Head,_Sv,'$bottom',_ExtraInfo,'$bottom'):- !.
det_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	asub(Exit,ETypes,EModes,ENonF),
	asub(ExtraInfo,ExtraInfoTypes,ExtraInfoModes,ExtraInfoNonF),
	shfr_exit_to_prime(Sg,Hv,Head,Sv,EModes,ExtraInfoModes,PModes),
	eterms_exit_to_prime(Sg,Hv,Head,Sv,ETypes,ExtraInfoTypes,PTypes),
	( PTypes = '$bottom'
	-> Prime = '$bottom'
	 ; detabs:det_exit_to_prime(Sg,Hv,Head,Sv,ENonF,ExtraInfoNonF,PNonF),
	   ( PNonF = '$bottom'
	   -> Prime = '$bottom'
	    ; asub(Prime,PTypes,PModes,PNonF) )
	).

%------------------------------------------------------------------------%
% det_project(+,+,-)                                                      %
% det_project(ASub,Vars,Proj)                                             %
%------------------------------------------------------------------------%

det_project('$bottom',_Vars,'$bottom'):- !.
det_project(ASub,Vars,Proj):-
	asub(ASub,ATypes,AModes,ANonF),
	shfr_project(AModes,Vars,PModes),
	eterms_project(Vars,ATypes,PTypes),
	detabs:det_project(ANonF,Vars,PNonF),
	asub(Proj,PTypes,PModes,PNonF).

%------------------------------------------------------------------------%
% det_extend(+,+,+,-)                                                     %
% det_extend(Prime,Sv,Call,Succ)                                          %
%------------------------------------------------------------------------%

det_extend('$bottom',_Sv,_Call,'$bottom'):- !.
det_extend(Prime,Sv,Call,Succ):-
	asub(Prime,PTypes,PModes,PNonF),
	asub(Call,CTypes,CModes,CNonF),
	shfr_extend(PModes,Sv,CModes,SModes),
	eterms_extend(PTypes,Sv,CTypes,STypes),
	detabs:det_extend(PNonF,Sv,CNonF,SNonF),
	asub(Succ,STypes,SModes,SNonF).

%------------------------------------------------------------------------%
% det_widen(+,+,-)                                                        %
% det_widen(ASub1,ASub2,ASub)                                             %
%------------------------------------------------------------------------%

det_widen('$bottom',ASub1,ASub):- !, ASub=ASub1.
det_widen(ASub0,'$bottom',ASub):- !, ASub=ASub0.
det_widen(ASub0,ASub1,ASub):-
	asub(ASub0,ATypes0,AModes0,ANonF0),
	asub(ASub1,ATypes1,AModes1,ANonF1),
	shfr_compute_lub([AModes0,AModes1],AModes),
	eterms_widen(ATypes0,ATypes1,ATypes),
	detabs:det_compute_lub([ANonF0,ANonF1],ANonF),
	asub(ASub,ATypes,AModes,ANonF).

%------------------------------------------------------------------------%
% det_widencall(+,+,-)                                                    %
% det_widencall(ASub1,ASub2,ASub)                                         %
%------------------------------------------------------------------------%

det_widencall('$bottom',ASub1,ASub):- !, ASub=ASub1.
det_widencall(ASub0,'$bottom',ASub):- !, ASub=ASub0.
det_widencall(ASub0,ASub1,ASub):-
	asub(ASub0,ATypes0,_AModes0,_ANonF0),
	asub(ASub1,ATypes1,AModes1,ANonF1),
	eterms_widencall(ATypes0,ATypes1,ATypes),
	asub(ASub,ATypes,AModes1,ANonF1).

%------------------------------------------------------------------------%
% det_compute_lub(+,-)                                                    %
% det_compute_lub(ListASub,Lub)                                           %
%------------------------------------------------------------------------%

det_compute_lub(ListASub,Lub):-
	split(ListASub,LTypes,LModes,LNonF),
	shfr_compute_lub(LModes,LubModes),
	eterms_compute_lub(LTypes,LubTypes),
	detabs:det_compute_lub(LNonF,LubNonF),
	asub(Lub,LubTypes,LubModes,LubNonF).

split([],[],[],[]).
split([ASub|ListASub],OutATypes,OutAModes,OutANonF):-
        (ASub == '$bottom' 
              -> OutATypes = LTypes, 
                 OutAModes = LModes, 
                 OutANonF  = LNonF
              ;  asub(ASub,ATypes,AModes,ANonF),
                 OutATypes = [ATypes|LTypes], 
                 OutAModes = [AModes|LModes], 
                 OutANonF  = [ANonF|LNonF]),
	split(ListASub,LTypes,LModes,LNonF).

det_split_combined_domain(ListASub,[LTypes,LModes,LNonF],[eterms,shfr,det]):-
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
% det_compute_clauses_lub(+,-)                                           %
% det_compute_clauses_lub(ListASub,Lub)                                  %
%------------------------------------------------------------------------%

det_compute_clauses_lub(['$bottom'],_Proj,[]):- !.
det_compute_clauses_lub([ASub],Proj,[Lub]):-
	asub(ASub,ATypes,AModes,ANonFList),
	asub(Proj,PTypes,PModes,_PNonFList),
	compute_modetypes(PTypes,PModes,_Head,ModeTypes),
	det_compute_covering(ModeTypes,ANonFList,LubNonF),
	asub(Lub,ATypes,AModes,LubNonF).

compute_modetypes(Types,Modes,Head,MTypes):-
        shfr_obtain(ground,Modes,FVars), % Added. Aug 24, 2012 -PLG
        % shfr_obtain(free,Modes,FVars), % Commented out. Aug 24, 2012. Not a safe asumption -PLG. 
	sort(Types,Types_s),
	compute_modetypes0(Types_s,FVars,Vars,ModeTypes),
	Head =.. [p|Vars],
	MTypes =.. [p|ModeTypes].

compute_modetypes0([],_FVars,[],[]).
compute_modetypes0([Var:(_,T)|Types],FVars,[Var|Vars],[M:T|ModeTypes]):-
	get_mode(Var,FVars,M),
	compute_modetypes0(Types,FVars,Vars,ModeTypes).

get_mode(Var,FVars,M):-          % Added. Aug 24, 2012 -PLG
	memberchk(Var,FVars), !,
	M = in.
get_mode(_Var,_GVars,out).

% get_mode(Var,FVars,M):-       % Commented out. Aug 24, 2012. Not a safe asumption -PLG. 
% 	memberchk(Var,FVars), !,
% 	M = out.
% get_mode(_Var,_GVars,in).

%------------------------------------------------------------------------%
% det_glb(+,+,-)                                                         %
% det_glb(ASub0,ASub1,Glb)                                               %
%------------------------------------------------------------------------%

det_glb('$bottom',_ASub1,'$bottom'):- !.
det_glb(_ASub0,'$bottom','$bottom'):- !.
det_glb(ASub0,ASub1,Glb):-
	asub(ASub0,ATypes0,AModes0,ANonF0),
	asub(ASub1,ATypes1,AModes1,ANonF1),
	shfr_glb(AModes0,AModes1,GModes),
	eterms_glb(ATypes0,ATypes1,GTypes),
	detabs:det_glb(ANonF0,ANonF1,GNonF),
	asub(Glb,GTypes,GModes,GNonF).

%------------------------------------------------------------------------%
% det_less_or_equal(+,+)                                                  %
% det_less_or_equal(ASub0,ASub1)                                          %
%------------------------------------------------------------------------%

det_less_or_equal('$bottom','$bottom'):- !.
det_less_or_equal(ASub0,ASub1):-
	asub(ASub0,ATypes0,AModes0,ANonF0),
	asub(ASub1,ATypes1,AModes1,ANonF1),
	shfr_less_or_equal(AModes0,AModes1),
	eterms_less_or_equal(ATypes0,ATypes1),
	detabs:det_less_or_equal(ANonF0,ANonF1).

%------------------------------------------------------------------------%
% det_identical_abstract(+,+)                                             %
% det_identical_abstract(ASub1,ASub2)                                     %
%------------------------------------------------------------------------%

det_identical_abstract('$bottom','$bottom'):- !.
det_identical_abstract(ASub0,ASub1):-
	asub(ASub0,ATypes0,AModes0,ANonF0),
	asub(ASub1,ATypes1,AModes1,ANonF1),
	AModes0 == AModes1,
	eterms_identical_abstract(ATypes0,ATypes1),
	detabs:det_identical_abstract(ANonF0,ANonF1).

%------------------------------------------------------------------------%
% det_sort(+,-)                                                           %
% det_sort(ASub0,ASub1)                                                   %
%------------------------------------------------------------------------%

det_sort('$bottom','$bottom'):- !.
det_sort(ASub0,ASub1):-
	asub(ASub0,ATypes0,AModes0,ANonF0),
	shfr_sort(AModes0,AModes1),
	eterms_sort(ATypes0,ATypes1),
	detabs:det_sort(ANonF0,ANonF1),
	asub(ASub1,ATypes1,AModes1,ANonF1).

%------------------------------------------------------------------------%
% det_call_to_success_fact(+,+,+,+,+,+,-,-)                               %
% det_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ)            %
%-------------------------------------------------------------------------

det_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	asub(Call,CTypes,CModes,CNonF),
	asub(Proj,PTypes,PModes,PNonF),
	shfr_call_to_success_fact(Sg,Hv,Head,Sv,CModes,PModes,RModes,SModes),
	eterms_call_to_success_fact(Sg,Hv,Head,Sv,CTypes,PTypes,RTypes,STypes),
	detabs:det_call_to_success_fact(Sg,Hv,Head,Sv,CNonF,PNonF,RNonF,SNonF),
	asub(Prime,RTypes,RModes,RNonF),
	asub(Succ,STypes,SModes,SNonF).


%-------------------------------------------------------------------------
% det_special_builtin(+,+,-,-)                                            |
% det_special_builtin(SgKey,Sg,Type,Condvars)                             |
%-------------------------------------------------------------------------

det_special_builtin(SgKey,Sg,SgKey,Sg):-
	detabs:det_special_builtin(SgKey).

%-------------------------------------------------------------------------
% det_success_builtin(+,+,+,+,-)                                          |
% det_success_builtin(Type,Sv_u,Condv,Call,Succ)                          |
%-------------------------------------------------------------------------

det_success_builtin(Type,_Sv_u,Sg,Call,Succ):-
	asub(Call,Types,Modes,CallNonF),
	detabs:det_success_builtin(Type,Modes,Sg,CallNonF,SuccNonF),
	asub(Succ,Types,Modes,SuccNonF).

%-------------------------------------------------------------------------
% det_call_to_success_builtin(+,+,+,+,+,-)                                %
% det_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ)                 %
%-------------------------------------------------------------------------
% Not used

%------------------------------------------------------------------------%
% det_input_interface(+,+,+,-)                                            %
% det_input_interface(InputUser,Kind,StructI,StructO)                     %
%------------------------------------------------------------------------%

det_input_interface(InputUser,Kind,StructI,StructO):-
	( nonvar(Kind)
	-> KModes=Kind, KTypes=Kind, KNonF=Kind
	; true ),
	asub(StructI,ITypes,IModes,INonF),
	shfr_input_interface_(InputUser,KModes,IModes,OModes),
	eterms_input_interface_(InputUser,KTypes,ITypes,OTypes),
	det_input_interface_(InputUser,KNonF,INonF,ONonF),
	asub(StructO,OTypes,OModes,ONonF).

shfr_input_interface_(InputUser,Kind,IModes,OModes):-
	shfr_input_interface(InputUser,Kind,IModes,OModes), !.
shfr_input_interface_(_InputUser,_Kind,IModes,IModes).

eterms_input_interface_(InputUser,Kind,ITypes,OTypes):-
	eterms_input_interface(InputUser,Kind,ITypes,OTypes), !.
eterms_input_interface_(_InputUser,_Kind,ITypes,ITypes).

det_input_interface_(InputUser,Kind,INonF,ONonF):-
	detabs:det_input_interface(InputUser,Kind,INonF,ONonF), !.
det_input_interface_(_InputUser,_Kind,INonF,INonF).

%------------------------------------------------------------------------%
% det_input_user_interface(+,+,-)                                         %
% det_input_user_interface(InputUser,Qv,ASub)                             %
%------------------------------------------------------------------------%

det_input_user_interface(Struct,Qv,ASub):-
	asub(Struct,Types,Modes,NonF),
	shfr_input_user_interface(Modes,Qv,AModes),
	eterms_input_user_interface(Types,Qv,ATypes),
	detabs:det_input_user_interface(NonF,Qv,ANonF),
	asub(ASub,ATypes,AModes,ANonF).

%------------------------------------------------------------------------%
% det_asub_to_native(+,+,+,-,-)                                           %
% det_asub_to_native(ASub,Qv,Flag,Stat,Comp)                              %
%------------------------------------------------------------------------%
% Qv should be the goal for comp-props!!!!!

det_asub_to_native(ASub,Qv,Flag,Props,CompProps):-
	asub(ASub,ATypes,AModes,ANonF),
	shfr_asub_to_native(AModes,Qv,Props1),
	eterms_asub_to_native(ATypes,Flag,Props2),
	detabs:det_asub_to_native(ANonF,Qv,CompProps),
	append(Props1,Props2,Props).

%------------------------------------------------------------------------%
% det_unknown_call(+,+,-)                                                 %
% det_unknown_call(Vars,Call,Succ)                                        %
%------------------------------------------------------------------------%

det_unknown_call(Vars,Call,Succ):-
	asub(Call,CTypes,CModes,CNonF),
	shfr_unknown_call(CModes,Vars,SModes),
	eterms_unknown_call(CTypes,Vars,STypes),
	detabs:det_unknown_call(Vars,CNonF,SNonF),
	asub(Succ,STypes,SModes,SNonF).

%------------------------------------------------------------------------%
% det_unknown_entry(+,-)                                                  %
% det_unknown_entry(Vars,Entry)                                           %
%------------------------------------------------------------------------%

det_unknown_entry(Vars,Entry):-
	shfr_unknown_entry(Vars,EModes),
	eterms_unknown_entry(Vars,ETypes), 
	detabs:det_unknown_entry(Vars,ENonF),
	asub(Entry,ETypes,EModes,ENonF).

%------------------------------------------------------------------------%
% det_empty_entry(+,-)                                                    %
% det_empty_entry(Vars,Entry)                                             %
%------------------------------------------------------------------------%

det_empty_entry(Vars,Entry):-
	shfr_empty_entry(Vars,EModes),
	eterms_empty_entry(Vars,ETypes),
	detabs:det_empty_entry(Vars,ENonF),
	asub(Entry,ETypes,EModes,ENonF).

%-----------------------------------------------------------------------

det_statistics(Info):- detabs:det_statistics(Info).

%-----------------------------------------------------------------------

det_obtain(Prop,Vars,ASub,Info):- detabs:det_obtain(Prop,Vars,ASub,Info).
