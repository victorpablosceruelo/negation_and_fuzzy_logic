/*             Copyright (C)2004-2005 UNM-CLIP				*/

:- doc(author,"Jorge Navas").

%------------------------------------------------------------------------%
%                      CLIQUE-Sharing+Def domain                         %   
%------------------------------------------------------------------------%
% This file contains the domain dependent abstract functions for the     |
% clique-sharing domain combined with the definiteness abstract domain.  |
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                                                                        |
%        programmer: J. Navas                                            |
%                                                                        |
%------------------------------------------------------------------------%
% The meaning of the variables are defined in sharing_clique.pl and      |
% def.pl                                                                 |
%------------------------------------------------------------------------% 
% This domain is represented by a pair (SH,D) where SH is the original   |
% clique-sharing domain and D is the definiteness domain.                |
% The combination of the clique-sharing domain with Def is the simplest  |
% possible. For any operation of the analysis, abstract amgu in          |
% particular, the Def component is evaluated first. All sharing groups   |
% containing at least one variable that is definitely ground according   |
% to the resulting Def formula are removed from the sharing component.   | 
% For the clique groups the intersection between the clique groups and   |
% those ground variables are removed.                                    |
%------------------------------------------------------------------------%

:- doc(bug,"1. In case of success multivariance the predicate
           eliminate_equivalent/2 must be redefined.").
:- doc(bug,"2. The following builtins: ==../2, ==/2 and copy_term/2 
	   are not defined for the domain def").
%------------------------------------------------------------------------%
% share_clique_def_call_to_entry(+,+,+,+,+,+,-,?)                        |
%------------------------------------------------------------------------%
share_clique_def_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,(BothEntry,ExtraInfo)):-
	Proj = (SH_Proj,Def_Proj),
	def_call_to_entry(Sg,Hv,Head,Def_Proj,Def_Entry,BothEntry),
	share_clique_def_compose((SH_Proj,Def_Entry),NewSH_Proj),
	share_clique_call_to_entry(Sv,Sg,Hv,Head,Fv,NewSH_Proj,(Cl_Entry,Sh_Entry),
	                           ExtraInfo),
	Entry = ((Cl_Entry,Sh_Entry),Def_Entry),!.
share_clique_def_call_to_entry(_,_,_,_,_,_,'$bottom',_).

%------------------------------------------------------------------------%
% share_clique_def_exit_to_prime(+,+,+,+,+,-,-)                          |
%------------------------------------------------------------------------%
share_clique_def_exit_to_prime(_,_,_,_,'$bottom',_,'$bottom'):-!.
share_clique_def_exit_to_prime(Sg,Hv,Head,Sv,Exit,(BothEntry,ExtraInfo),Prime):-
	Exit = (SH_Exit,Def_Exit),
	def_exit_to_prime(Def_Exit,BothEntry,Hv,Sv,Head,Sg,Def_Prime),
	share_clique_def_compose((SH_Exit,Def_Prime),NewSH_Exit),
	share_clique_exit_to_prime(Sg,Hv,Head,Sv,NewSH_Exit,ExtraInfo,(Cl_Pr,Sh_Pr)),
	Prime = ((Cl_Pr,Sh_Pr),Def_Prime),!.
share_clique_def_exit_to_prime(_,_,_,_,_,_,'$bottom').

%------------------------------------------------------------------------%
% share_clique_def_extend(+,+,+,-)                                       |
% share_clique_def_extend(Prime,Sv,Call,Succ)                            |
%------------------------------------------------------------------------%
share_clique_def_extend('$bottom',_Hv,_Call,'$bottom').
share_clique_def_extend((SH_Prime,Def_Prime),Sv,(SH_Call,Def_Call),Succ):-
	def_extend(Def_Prime,Def_Call,Def_Succ),
	share_clique_def_compose((SH_Prime,Def_Succ),NewSH_Prime),
	share_clique_def_compose((SH_Call,Def_Succ),NewSH_Call),
        share_clique_extend(NewSH_Prime,Sv,NewSH_Call,(Cl_Succ,Sh_Succ)),
	Succ = ((Cl_Succ,Sh_Succ),Def_Succ),!.
share_clique_def_extend(_Prime,_Sv,_Call,'$bottom').

%------------------------------------------------------------------------%
% share_clique_def_project(+,+,-)                                        |
% share_clique_def_project(Vars,ASub,Proj)                               |
%------------------------------------------------------------------------%
share_clique_def_project(_,'$bottom','$bottom'):- !.
share_clique_def_project(Vars,(SH_ASub,Def_ASub),Proj) :-
	def_project(Def_ASub,Vars,Def_Proj),
	share_clique_project(Vars,SH_ASub,SH_Proj),
	Proj = (SH_Proj,Def_Proj).

%------------------------------------------------------------------------%
% share_clique_def_sort(+,-)                                             |
% share_clique_def_sort(Asub,Asub_s)                                     |
%------------------------------------------------------------------------%

share_clique_def_sort('$bottom','$bottom'):- !.
share_clique_def_sort((SH_ASub,Def_ASub),ASub_s ):-
	def_sort(Def_ASub,Def_ASub_s),
	share_clique_sort(SH_ASub,SH_ASub_s),
	ASub_s = (SH_ASub_s,Def_ASub_s).


%------------------------------------------------------------------------%
% share_clique_def_glb(+,+,-)                                            |
% share_clique_def_glb(ASub0,ASub1,Lub)                                  |
%------------------------------------------------------------------------%

share_clique_def_glb((SH_ASub0,Def_ASub0),(SH_ASub1,Def_ASub1),Glb):- 
	def_glb(Def_ASub0,Def_ASub1,Def_glb),
	share_clique_def_compose((SH_ASub0,Def_glb),NewSH_ASub0),
	share_clique_def_compose((SH_ASub1,Def_glb),NewSH_ASub1),
	share_clique_glb(NewSH_ASub0,NewSH_ASub1,SH_Glb),
	Glb = (SH_Glb,Def_glb).


%------------------------------------------------------------------------%
% share_clique_def_identical_abstract(+,+)                               |
% share_clique_def_identical_abstract(ASub0,ASub1)                       |
%------------------------------------------------------------------------%
share_clique_def_identical_abstract('$bottom','$bottom'):-!.
share_clique_def_identical_abstract((SH0,_),(SH1,_)):-!,
	share_clique_identical_abstract(SH0,SH1).

%------------------------------------------------------------------------%
% share_clique_def_eliminate_equivalent(+,-)                             |
% share_clique_def_eliminate_equivalent(TmpLSucc,LSucc)                  |
%------------------------------------------------------------------------%

share_clique_def_eliminate_equivalent(TmpLSucc,Succ):-
	sort(TmpLSucc,Succ).

%------------------------------------------------------------------------%
% share_clique_def_less_or_equal(+,+)                                    |
% share_clique_def_less_or_equal(ASub0,ASub1)                            |
%------------------------------------------------------------------------%

share_clique_def_less_or_equal('$bottom',_ASub):- !.
share_clique_def_less_or_equal((SH0,_),(SH1,_)):-!,
	share_clique_less_or_equal(SH0,SH1).
		
%------------------------------------------------------------------------%
%                      ABSTRACT Call to Success Fact                     %
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% Specialized version of call_to_entry + exit_to_prime + extend for facts%
%------------------------------------------------------------------------%

share_clique_def_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	Call = (SH_Call,Def_Call),
	Proj = (_,Def_Proj),
	def_call_to_success_fact(Def_Proj,Hv,Head,Sv,Sg,Def_Call,Def_Prime,
	                         Def_Succ),
	share_clique_def_compose((SH_Call,Def_Succ),NewSH_Call),
	share_clique_call_to_success_fact(Sg,Hv,Head,Sv,NewSH_Call,_Proj,
                                          (Cl_Prime,Sh_Prime),(Cl_Succ,Sh_Succ)),
	Prime = ((Cl_Prime,Sh_Prime),Def_Prime),
	Succ = ((Cl_Succ,Sh_Succ),Def_Succ),!.
share_clique_def_call_to_success_fact(_Sg,_Hv,_Head,_Sv,_Call,_Proj,'$bottom','$bottom').


%------------------------------------------------------------------------%
% share_clique_def_compute_lub(+,-)                                      |
% share_clique_def_compute_lub(ListASub,Lub)                             |
%------------------------------------------------------------------------%

share_clique_def_compute_lub([ASub1,ASub2|Rest],Lub) :-
	share_clique_def_lub_cl(ASub1,ASub2,ASub3),
	share_clique_def_compute_lub([ASub3|Rest],Lub).
share_clique_def_compute_lub([ASub],ASub).

share_clique_def_lub_cl('$bottom',ASub,ASub):-!.
share_clique_def_lub_cl(ASub,'$bottom',ASub):-!.
share_clique_def_lub_cl((SH_ASub1,Def_ASub1),(SH_ASub2,Def_ASub2),ASub3):-
	def_compute_lub_el(Def_ASub1,Def_ASub2,Def_ASub3),
	share_clique_def_compose((SH_ASub1,Def_ASub3),NewSH_ASub1),
	share_clique_def_compose((SH_ASub2,Def_ASub3),NewSH_ASub2),
	share_clique_lub_cl(NewSH_ASub1,NewSH_ASub2,SH_ASub3),
	ASub3 = (SH_ASub3,Def_ASub3).
%------------------------------------------------------------------------%
% share_clique_def_input_user_interface(+,+,-)                           |
% share_clique_def_input_user_interface(InputUser,Qv,ASub)               |
%------------------------------------------------------------------------%

share_clique_def_input_user_interface((Gv,Sh,Cl,I),Qv,Call):-
	share_clique_input_user_interface((Gv,Sh,Cl,I),Qv,SH_Call),
	may_be_var(Gv,Gv0),
	Def_Call = a(Gv0,[]),
	Call = (SH_Call,Def_Call).

%------------------------------------------------------------------------%
% share_clique_def_asub_to_native(+,+,-)                                 |
% share_clique_def_asub_to_native(ASub,Qv,ASub_user)                     |
%------------------------------------------------------------------------%

share_clique_def_asub_to_native('$bottom',_Qv,_ASub_user):- !, fail.
share_clique_def_asub_to_native(((Cl,Sh),a(G,_SS)),Qv,Info):-
 	ord_union(Sh,Cl,All),
	projected_gvars(All,Qv,Gv),	
	if_not_nil(Cl,clique(Cl),Info,Info0),
	if_not_nil(Sh,sharing(Sh),Info0,Info1),
	if_not_nil(Gv,ground(Gv),Info1,[]),
        ( Gv == G -> true
        ; warning_message("The set of ground variables are different")).
	
%------------------------------------------------------------------------%
% share_clique_def_unknown_call(+,+,-)                                   |
% share_clique_def_unknown_call(Call,Vars,Succ)                          |
% Note that def does not define this operation.                          |
%------------------------------------------------------------------------%

share_clique_def_unknown_call('$bottom',_Vars,'$bottom').
share_clique_def_unknown_call((SH_Call,Def_Call),Vars,Succ):-	
	share_clique_unknown_call(SH_Call,Vars,SH_Succ),
	Succ = (SH_Succ,Def_Call).

%------------------------------------------------------------------------%
% share_clique_def_empty_entry(+,-)                                      |
% share_clique_def_empty_entry(Vars,Entry)                               |
%------------------------------------------------------------------------%

share_clique_def_empty_entry(Vars,Entry):-
	def_unknown_entry(Vars,Def_Entry),
	share_clique_empty_entry(Vars,SH_Entry),
	Entry = (SH_Entry,Def_Entry).

%------------------------------------------------------------------------%
% share_clique_def_unknown_entry(+,-)                                    |
% share_clique_def_unknown_entry(Qv,Call)                                |
%------------------------------------------------------------------------%

share_clique_def_unknown_entry(Qv,((Qv,[]),a([],[]))).

%------------------------------------------------------------------------%
%                         HANDLING BUILTINS                              |
%------------------------------------------------------------------------%
% share_clique_def_special_builtin(+,+,-,-)                              |
% share_clique_def_special_builtin(SgKey,Sg,Type,Condvars)               |
%------------------------------------------------------------------------%

share_clique_def_special_builtin(SgKey,Sg,Type,Condvars):-
	share_clique_special_builtin(SgKey,Sg,SH_Type,SH_Condvars),!,
	( def_special_builtin(SgKey,Sg,Def_Type,Def_Condvars) ->
   	  Type = (SH_Type,Def_Type),
	  Condvars = (SH_Condvars,Def_Condvars)
        ;
	  warning_message("The builtin ~w is not defined in def",
	                  [SgKey]),    
	  Type = (SH_Type,not_defined),
	  Condvars = (SH_Condvars,_)
        ).

%------------------------------------------------------------------------%
% share_clique_def_compose(+,-)                                                           
% share_clique_def_compose(((Cl,Sh),a(G,SS)),((NewCl,NewSh),a(NewG,NewSS)))     
%------------------------------------------------------------------------%
% The clique-sharing and def domains are combined in the simplest        | 
% possible way and this is as follows:                                   |
% - All sharing groups containing at least one variable that is          |
%   definitely ground according to the resulting def formula are removed |
%   from the sharing component.                                          |
% - The intersection between every clique group and the definite ground  |
%   variables are removed too.                                           |
% Note that for any abstract function, the def component is evaluated    |
% first.                                                                 |
%------------------------------------------------------------------------%

share_clique_def_compose(((Cl,Sh),a(G,_)),(NewCl,NewSh)):-
	irrel_w(G,(Cl,Sh),(NewCl,NewSh)),!.
share_clique_def_compose(_,'$bottom'):-!.


