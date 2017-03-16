% Implementation
%
% Use of a logic framework forces differences respect to the paper
%
% (1) All logic variables must be given an abstract value at all program points 
%   (1.1) in call_to_entry local variables and Res are declared to be null objects  
%   (1.2) is incorrect to project over each Res after assignment [we keep it]
%   (1.3) for reusing Res we have to project it our *before* expression eval,
%         and then assign it new values 


:- module(_
	,_
	,[]).

% by using this module we import sharing freeness
:- use_module(domain(share)).

% keep an eye on this module, interesting predicates
:- use_module(domain(s_grshfr), [
	                         change_values_insert/4,
				 member_value_freeness/3,
				 var_value/3
				 ]).
:- use_module(domain(sondergaard), [
	    	                 son_to_share/4
		                 ]).

:- use_module(domain(oo_types)).

% rest of libraries in use
:- use_module(library(sort)).
:- use_module(library(lists)).
:- use_module(library(sets)).
:- use_module(library(terms_vars)).
:- use_module(library(messages)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               Configuration    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Turn this to on/off for using dynamic types
% You can always use the CiaoPP flag oo_types_dyn_info
use_dynamic_types(on).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                Initialization: load of Metainfo
% Assumption: the metainfo for file.pl is stored in file_mi.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
oo_shnltau_init_abstract_domain:-
	%use_dynamic_types(Mode),
	%set_pp_flag(oo_types_dyn_info,Mode),
	oo_types_init_abstract_domain.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                Empty/Unknown entry
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% In Java all the public methods have to have an entry (about types).
oo_shnltau_unknown_entry(_Qv,'$bottom').	


% BEGIN TODO
% semantics of this ???
% END TODO
oo_shnltau_empty_entry(Qv,Call):-
	oo_shnltau_unknown_entry(Qv,Call).


% External calls have to be represented through java_external_call
oo_shnltau_unknown_call(_Call,_Vars,Succ):-
	Succ = '$bottom'.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              Lattice Preds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% shfr_project has the 2 first params swapped which respect to the convention
oo_shnltau_project('$bottom',_Vars,'$bottom'):-
	!.
oo_shnltau_project(ASub,Vars,Proj):-
	split(ASub,ShNl,Tau,Mod),
	shfr_project(ShNl,Vars,ShNl_Proj),
	oo_types_project(Tau,Vars,Tau_Proj),
	ord_intersection(Mod,Vars,Mod_Proj),
	split(Proj,ShNl_Proj,Tau_Proj,Mod_Proj).

oo_shnltau_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	split(Proj,ShNl_Proj,Tau_Proj),
	% Sh
 	shfr_call_to_entry(Sv,Sg,Hv,Head,[],
 	                   ShNl_Proj,ShNl_Entry1,ShNl_ExtraInfo),
	ShNl_Entry1 = (Sh,Nl),
	% Nl
	change_values_insert(Fv,Nl,New_Nl,g),
	ShNl_Entry =  (Sh,New_Nl),
	% Types
	oo_types_call_to_entry(Sv,Sg,Hv,Head,Fv,
	                   Tau_Proj,Tau_Entry,Tau_ExtraInfo),
	Mod = [],		   
	split(Entry,ShNl_Entry,Tau_Entry,Mod),
	split(ExtraInfo,ShNl_ExtraInfo,Tau_ExtraInfo).




oo_shnltau_exit_to_prime(_Sg,_Hv,_Head,_Sv,'$bottom',_ExtraInfo,'$bottom'):-
	!.
oo_shnltau_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	split(Exit,ShNl_Exit,Tau_Exit,Mod_Exit),
	split(ExtraInfo,ShNl_ExtraInfo, Tau_ExtraInfo),
	shfr_exit_to_prime(Sg,Hv,Head,Sv,
	                   ShNl_Exit,ShNl_ExtraInfo,ShNl_Prime),
	oo_types_exit_to_prime(Sg,Hv,Head,Sv,
	                   Tau_Exit,Tau_ExtraInfo,Tau_Prime),	   
	% Tau_ExtraInfo contains binds (Var1,Var2). 
        % We use then for renaming back (abmgu)   
	mod_exit_to_prime(Mod_Exit, Tau_ExtraInfo,Mod_Prime),	   
	split(Prime,ShNl_Prime,Tau_Prime,Mod_Prime).

mod_exit_to_prime([],_,[]).
mod_exit_to_prime([Var_Exit|RE],Binds,[Var_Prime|RP]):-
	member((X,Y),Binds),
	Y == Var_Exit,
	!,
	X  = Var_Prime,
	mod_exit_to_prime(RE,Binds,RP).
mod_exit_to_prime([_|RE],Binds,RP):-
	mod_exit_to_prime(RE,Binds,RP).



oo_shnltau_extend('$bottom',_Sv,_Call,'$bottom'):-
	!.
oo_shnltau_extend(Prime,Sv,Call,Succ):-
	split(Call,ShNl_Call,Tau_Call,Mod_Call), 
	split(Prime,ShNl_Prime,Tau_Prime,Mod_Prime),
	ShNl_Call =  (_Sh_Call,Nl_Call),
	ShNl_Prime = (Sh_Prime,Nl_Prime),
	% Sh
        project_out_sh(ShNl_Call,Mod_Prime,(Sh1,_)),	
	% BEGIN CHANGE ME
	% TOO COMPLICATED
	% work-around for vars that are null on Call but not in Prime
	find_null_vars(Nl_Call,Nullv),
	list_to_list_of_lists(Nullv,NullVars1),
	list_to_list_of_lists(Mod_Prime,NullVars2),
	ord_union(NullVars1,NullVars2,NullVars),
	ord_union(Sh1,NullVars,Sh2),	
	share_extend(Sh_Prime,Sv,Sh2,Sh3),
	prune(NullVars,Sh3,Sh_Prime,Sh_Succ),
	% END CHANGE ME
	% Nl
        project_out_nl(ShNl_Call,Sv,(_,Nl1)),
	ord_union(Nl1,Nl_Prime,Nl_Succ),
 	ShNl_Succ = (Sh_Succ,Nl_Succ),
	% Types
        oo_types_extend(Tau_Prime,Sv,Tau_Call,Tau_Succ),
	% Mod
	ord_union(Mod_Call,Mod_Prime,Mod_Succ),
	split(Succ,ShNl_Succ,Tau_Succ,Mod_Succ).




% if [V] is in Sh but V was null on Sh_Call, erase it from Sh
% except if Sh_Prime contains it
prune([],Sh_Succ,_,Sh_Succ).
prune([NullVar|T],Sh,Sh_Prime,Sh_Succ):-
	(ord_member(NullVar,Sh) ->
	       (ord_member(NullVar,Sh_Prime) ->
		     Sh1 = Sh
	             ;
		     ord_delete(Sh,NullVar,Sh1)
	       )
	       ;
	       Sh1 = Sh
	),
	prune(T,Sh1,Sh_Prime,Sh_Succ).


oo_shnltau_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	split(Call,ShNl_Call,Tau_Call,Mod_Call),
	split(Proj,ShNl_Proj,Tau_Proj,Mod_Proj),
	shfr_call_to_success_fact(Sg,Hv,Head,Sv,
	                          ShNl_Call,ShNl_Proj,ShNl_Prime,ShNl_Succ),
	oo_types_call_to_success_fact(Sg,Hv,Head,Sv,
	                          Tau_Call,Tau_Proj,Tau_Prime,Tau_Succ),
	split(Prime,ShNl_Prime,Tau_Prime,Mod_Proj),
	split(Succ, ShNl_Succ, Tau_Succ,Mod_Call).


oo_shnltau_compute_lub(ListASub,Lub):-
	pairlist2listpairs(ListASub,ShNl,Tau,Mod),
	shfr_compute_lub(ShNl,ShNl_Lub),
	(ShNl_Lub == '$bottom' ->
	     Lub = '$bottom'
	     ;
	     oo_types_compute_lub(Tau,Tau_Lub),
	     (Tau_Lub == '$bottom' ->
	          Lub = '$bottom'
                  ;
		  mod_compute_lub(Mod,Mod_Lub),
   	          split(Lub,ShNl_Lub,Tau_Lub,Mod_Lub)
             )
	).

pairlist2listpairs([],[],[],[]).
pairlist2listpairs(['$bottom'|T],['$bottom'|T1],['$bottom'|T2],T3):-
	pairlist2listpairs(T,T1,T2,T3).
pairlist2listpairs([(H1,H2,H3)|T],[H1|T1],[H2|T2],[H3|T3]):-
	pairlist2listpairs(T,T1,T2,T3).

mod_compute_lub([],[]).
mod_compute_lub([H],H).
mod_compute_lub([E1,E2|T],Lub):-
	ord_intersection(E1,E2,I),
	mod_compute_lub([I|T],Lub).


oo_shnltau_lub(ASub0,ASub1,Lub):-
	split(ASub0,ShNl0,Tau0,Mod1),
	split(ASub1,ShNl1,Tau1,Mod2),
	shfr_compute_lub_el(ShNl0,ShNl1,Lub0),
	oo_types_compute_lub_el(Tau0,Tau1,Lub1),
	ord_intersection(Mod1,Mod2,Lub3),
	split(Lub,Lub0,Lub1,Lub3).
	
%%% DOME!!!!!!!!!!!!!!!!
oo_shnltau_glb(ASub0,ASub1,Glb):-
	split(ASub0,ShNl0,Tau0),
	split(ASub1,ShNl1,Tau1),
	shfr_glb(ShNl0,ShNl1,Glb0),
	oo_types_glb(Tau0,Tau1,Glb1),
	split(Glb,Glb0,Glb1).

oo_shnltau_less_or_equal(ASub0,ASub1):-       
	split(ASub0,ShNl0,Tau0),
	split(ASub1,ShNl1,Tau1),
	shfr_less_or_equal(ShNl0,ShNl1),
	oo_types_less_or_equal(Tau0,Tau1).

oo_shnltau_sort(ASub,ASub_s):-
	split(ASub,ShNl,Tau,Mod),
	shfr_sort(ShNl,ShNl_s),
	oo_types_sort(Tau,Tau_s),
	split(ASub_s,ShNl_s,Tau_s,Mod).


% assume the format of the assert is a list where props are mixed
oo_shnltau_input_user_interface(ASub,_Qv,ASub).


% Assumption
% Info provided by the user only refers to types
% Info referred to Nl or Sh are automatically inferred by the analysis
oo_shnltau_input_interface(Info,Kind,A,B):-
	oo_shnltau_init_abstract_domain,
 	oo_types_input_interface(Info,Kind,A,Tau), 
	nl_from_types(Tau,Nl,UnkOrNnull),
	powerset_sh(UnkOrNnull,Sh),
	Mod  = [],
	split(B,(Sh,Nl),Tau,Mod).


% BEGIN CORRECT ME
% THIS SHOULD BE CONSTRAINED BY REACHABILITY
% like a 'constrained' powerset
% is also used in more places -----> move to the end of file
powerset_sh(Objects,Pow):-
	share_unknown_entry(Objects,Pow).
% END CORRECT ME


% Assumptions:
% This is always present in the first position
% Out  "                     "  last   "
nl_from_types([This/_|R],Nl,UnkOrNnull1):-
	change_values_insert([This],[],Nl1,f),	
	nl_from_types_(R,[],[],Null,UnkOrNnull),
	change_values_insert(Null,Nl1,Nl2,g),		
	change_values_insert(UnkOrNnull,Nl2,Nl,nf),
	insert(UnkOrNnull,This,UnkOrNnull1).

nl_from_types_([Out/_],Null,UnkOrNnull,NewNull,UnkOrNnull):-
	insert(Null,Out,NewNull).
nl_from_types_([X/Type|R],Null,UnkOrNnull,NewNull,NewUnkOrNnull):-
	oo_types_atomic_type(Type),
	!,
	insert(Null,X,Null1),
	nl_from_types_(R,Null1,UnkOrNnull,NewNull,NewUnkOrNnull).
nl_from_types_([X/_Type|R],Null,UnkOrNnull,NewNull,NewUnkOrNnull):-
	insert(UnkOrNnull,X,UnkOrNnull1),
	nl_from_types_(R,Null,UnkOrNnull1,NewNull,NewUnkOrNnull).



oo_shnltau_asub_to_native(ASub,Qv,Info):- 
	% removal of temporal vars
	type_component(ASub,T),
	find_any_vars(T,AnyVars),
	oo_shnltau_project_out(ASub,AnyVars,ASub1),
	% Sh
	split(ASub1,(Sh,Nl),Tau,Mod),
	Info1 = [mshare(Sh)],
	%Nl
	find_null_vars(Nl,Null),
	add_if_not_empty(Info1,Null,null,Tmp1),
	find_notnull_vars(Nl,NotNull),
	add_if_not_empty(Tmp1,NotNull,notnull,Info2),
        % Tau
	oo_types_asub_to_native(Tau,Qv,Info3),
	append(Info2,Info3,Info4),
	% Mod
	add_if_not_empty(Info4,Mod,mod,Info).


add_if_not_empty(L,[],_,L):-
	!.
add_if_not_empty(L1,L2,F,L):-
	Prop =.. [F,L2],
	append(L1,[Prop],L).



oo_shnltau_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ):-
	split(Call,ShNl_Call,Tau_Call,Mod),
	split(Proj,ShNl_Proj,Tau_Proj),
	shfr_call_to_success_builtin(SgKey,Sg,Sv,
	                             ShNl_Call,ShNl_Proj,ShNl_Succ),
	oo_types_call_to_success_builtin(SgKey,Sg,Sv,
	                             Tau_Call,Tau_Proj,Tau_Succ),
	split(Succ,ShNl_Succ,Tau_Succ,Mod).			     




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%    BUILTINS    %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

oo_shnltau_special_builtin('java_bytecode:java_skip/0',
                           'java_bytecode:java_skip',
	                   skip,nothing).

oo_shnltau_special_builtin('java_bytecode:java_decl/2',
                           'java_bytecode:java_decl'(C,V),
	                   declaration,Condv):-
	Condv = (C,V).
oo_shnltau_special_builtin('java_bytecode:java_param/2',
                           'java_bytecode:java_param'(C,V),
                            parameter,Condv):-
	Condv = (C,V).

oo_shnltau_special_builtin('java_bytecode:java_meth_decl/3',
                           'java_bytecode:java_meth_decl'(M,C,T),
                            method_declaration,Condv):-
	Condv = (M,C,T).


oo_shnltau_special_builtin('java_bytecode:java_null/2',
                  	'java_bytecode:java_null'(C,Res),
			null,Condv):-
        Condv = (C,Res).
oo_shnltau_special_builtin('java_bytecode:java_new/2' ,
                  	'java_bytecode:java_new'(C,Res),
			new,Condv):-
        Condv = (C,Res).
oo_shnltau_special_builtin('java_bytecode:java_get/2',
                  	'java_bytecode:java_get'(V,Res),
			get,Condv):-
        Condv = (V,Res).
oo_shnltau_special_builtin('java_bytecode:java_getfield/3',
	                'java_bytecode:java_getfield'(V,Field,Res),
			 getfield,Condv):-
        Condv = (V,Field,Res).

oo_shnltau_special_builtin('java_bytecode:java_set/2',
                  	'java_bytecode:java_set'(V,Res),
			set,Condv):-
        Condv = (V,Res).
oo_shnltau_special_builtin('java_bytecode:java_setfield/3',
	                'java_bytecode:java_setfield'(V,Field,Res),
			setfield,Condv):-
        Condv = (V,Field,Res).

oo_shnltau_special_builtin('==/2',==(X,Y),eq,Condv):-
	Condv = (X,Y).	
oo_shnltau_special_builtin('\\==/2','\\=='(X,Y),neq,Condv):-
	Condv = (X,Y).	

% Builtins not affecting the abstract state can be ignored
oo_shnltau_special_builtin('</2',_,unchanged,_Condv).
oo_shnltau_special_builtin('>=/2',_,unchanged,_Condv).


%%%%%%%%%%%%%%%%%%%%%%%%%%  Error mechanism  %%%%%%%%%%%%%%%%%%%%%%%%%%

% Invariant:unprocessed builtin can only be the result of an error
% any other builtin must be declared explicitely
oo_shnltau_success_builtin(Builtin,Sv,Condv,Call,Succ):-
	oo_shnltau_success_builtin_(Builtin,Sv,Condv,Call,Succ),
	!.
oo_shnltau_success_builtin(Builtin,_Sv,Condv,Call,'$bottom'):-
	error_message("Builtin ~w failed {Condv = ~w, Call = ~w}",[Builtin,Condv,Call]),
        throw(domain_error(oo_shnltau,builtin(Builtin))).




%%%%%%%%%%%%%%%%%%%%%%%%%%  Expressions %%%%%%%%%%%%%%%%%%%%%%%%%%

% null
oo_shnltau_success_builtin_(null,Sv,Condv,Call,Succ):-
	!,
	Condv = (_K,Res),
	oo_shnltau_project_out(Call,[Res],Call1), % because of (1.3)
	split(Call1,(Sh_Call,Nl_Call),Tau_Call,Mod),
	% ShNl	
        change_values_insert([Res],Nl_Call,Nl_Succ,g),
	% Types
        oo_types_success_builtin(null,Sv,Condv,Tau_Call,Tau_Succ),
	split(Succ,(Sh_Call,Nl_Succ),Tau_Succ,Mod).

% new k
oo_shnltau_success_builtin_(new,Sv,Condv,Call,Succ):-
	!,
	Condv = (_K,Res),
	oo_shnltau_project_out(Call,[Res],Call1), % because of (1.3)
	split(Call1,(Sh_Call,Nl_Call),Tau_Call,Mod),
	% ShNl	
	insert(Sh_Call,[Res],Sh_Succ),
	change_values_insert([Res],Nl_Call,Nl_Succ,f),
	% Types
        oo_types_success_builtin(new,Sv,Condv,Tau_Call,Tau_Succ),
	split(Succ,(Sh_Succ,Nl_Succ),Tau_Succ,Mod).

% v
oo_shnltau_success_builtin_(get,Sv,Condv,Call,Succ):-
	!,
	Condv = (V,Res),
	oo_shnltau_project_out(Call,[Res],Call1), % because of (1.3)
	split(Call1,(Sh_Call,Nl_Call),Tau_Call,Mod),
	% Sh	
	(nl(V,Nl_Call,g) ->
	    Sh_Succ = Sh_Call,
	    Nl1 = Nl_Call   
	    ;
	    uplus((Sh_Call,Nl_Call),(Res,V),(Sh_Succ,Nl1))	    
	),
	% Nl
	nl(V,Nl1,V_Mode),
	change_values_insert([Res],Nl1,Nl_Succ,V_Mode),
	% Types
        oo_types_success_builtin(get,Sv,Condv,Tau_Call,Tau_Succ),
	split(Succ,(Sh_Succ,Nl_Succ),Tau_Succ,Mod).


% i
% MISSING: java_get(3,Res) + java_set(V,Res). Does it make sense??? 


% v.f
oo_shnltau_success_builtin_(getfield,Sv,Condv,Call,Succ):-
	!,
	Condv = (V,Field,Res),
	oo_shnltau_project_out(Call,[Res],Call1), % because of (1.3)
	split(Call1,(Sh_Call,Nl_Call),Tau_Call,Mod),
	(nl(V,Nl_Call,g) ->
	     Succ = '$bottom'
	     ;
	     var_value(Tau_Call,V,VK),
	     oo_types_field_type(VK,Field,K),
	     (oo_types_non_atomic_type(K) ->                
	        uplus((Sh_Call,Nl_Call),(Res,V),(Sh_Succ,Nl1)),      
 		change_values_insert([Res],Nl1,Nl_Succ,nf)
	        ;
	        Sh_Succ = Sh_Call,
		change_values_insert([Res],Nl_Call,Nl_Succ,g)
	     ),
             % Types
     	     oo_types_success_builtin(getfield,Sv,Condv,Tau_Call,Tau_Succ),
  	     split(Succ,(Sh_Succ,Nl_Succ),Tau_Succ,Mod)
	).



%%%%%%%%%%%%%%%%%%%%%%%%  Commands    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% v:t 
oo_shnltau_success_builtin_(declaration,Sv,Condv,Call,Succ):-
	!,
	Condv = (_K,Res),
	oo_shnltau_project_out(Call,[Res],Call1), % because of (1.3)
	split(Call1,(Sh_Call,Nl_Call),Tau_Call,Mod),
	% Nl
        change_values_insert([Res],Nl_Call,Nl_Succ,g),
	% Types
        oo_types_success_builtin(declaration,Sv,Condv,Tau_Call,Tau_Succ),
	split(Succ,(Sh_Call,Nl_Succ),Tau_Succ,Mod).


% v:=Res
oo_shnltau_success_builtin_(set,Sv,Condv,Call,Succ):-
	!,
	Condv = (V,Res),
	split(Call,ShNl_Call,Tau_Call,Mod_Call),
	% Sh
        uplus(ShNl_Call,(V,Res),ShNl1),
        % Nl
	ShNl1 = (Sh_Succ,Nl1),
        nl(Res,Nl1,Mode),
	change_values_insert([V],Nl1,Nl_Succ,Mode),		
	% Types
	oo_types_success_builtin(set,Sv,Condv,Tau_Call,Tau_Succ),
	% Mod
	%%%%%%%%% We can add cases here to improve precision
        %%%%%%%%% Namely v =v and v = (k)v do not change Mod
        var_value(Tau_Call,V,VK),
	(oo_types_atomic_type(VK) ->
	     Mod_Succ = Mod_Call
	     ;
      	     insert(Mod_Call,V,Mod_Succ)
	),
	split(Succ,(Sh_Succ,Nl_Succ),Tau_Succ,Mod_Succ).

% v.f:=Res
oo_shnltau_success_builtin_(setfield,Sv,Condv,Call,Succ):-
	!,
	Condv = (V,Field,Res),
	split(Call,ShNl_Call,Tau_Call,Mod_Call),
	(nl(V,ShNl_Call,g) ->
	     Succ = '$bottom'
	     ;
	     var_value(Tau_Call,V,VK),
	     oo_types_field_type(VK,Field,K),
	     % Sh
	     (oo_types_atomic_type(K) ->
		ShNl_Succ = ShNl_Call 
	        ;
		ShNl_Call = (Sh_Call,Nl_Call), 
		sh_add_and_close(Sh_Call,(Res,V),Sh_Succ),
		ShNl_Succ=  (Sh_Succ,Nl_Call)
	     ),
  	     % Types
	     oo_types_success_builtin(setfield,Sv,Condv,Tau_Call,Tau_Succ),
	     % Mod
	     (member(K,VK) ->
	        insert(Mod_Call,V,Mod_Succ)
	        ;
		Mod_Succ = Mod_Call
	     ),
	     split(Succ,ShNl_Succ,Tau_Succ,Mod_Succ)
	).


% throw Res
% TODO

% return Res
% solved through set(Out,Res) and unification
% by assumption on the language no statement follows


% if (v = w) then comm
% TODO

% if (v != w) then comm
% TODO

% if (v == null) then comm
oo_shnltau_success_builtin_(eq,Sv,Condv,Call,Succ):-
	Condv = (V,W),
	var(V),
	W == null,
	!,
	split(Call,ShNl_Call,Tau_Call,Mod_Call),
        (nl(V,ShNl_Call,f) ->
	      Succ = '$bottom'
	      ;
     	      (nl(V,ShNl_Call,g) ->
            	   Succ = Call
	           ;
		   % Sh
		   project_out_shnl(ShNl_Call,[V],ShNl_Call1),
		   ShNl_Call1 = (Sh,Nl_Call1),
		   % Nl
	           change_values_insert([V],Nl_Call1,Nl_Succ,g),
	           ShNl_Succ = (Sh,Nl_Succ),
	           % Mod
	           insert(Mod_Call,V,Mod_Succ),
 	           % Types
    	           oo_types_success_builtin(eq,Sv,Condv,Tau_Call,Tau_Succ),
		   split(Succ,ShNl_Succ,Tau_Succ,Mod_Succ)
	      )
	).
% if (v == i) then comm
oo_shnltau_success_builtin_(eq,_Sv,Condv,Call,Succ):-
	Condv = (V,W),
	var(V),
	int(W),
	!,
	Succ = Call.
	


% if (v != null) then comm
oo_shnltau_success_builtin_(neq,Sv,Condv,Call,Succ):-
	Condv = (V,W),
	var(V),
	W == null,
	!,
	split(Call,ShNl_Call,Tau_Call,Mod_Call),
	(nl(V,ShNl_Call,g) ->
	       Succ = '$bottom'
	       ;
	       (nl(V,ShNl_Call,f) ->
		   Succ = Call
	           ;
                   % Nl
		   ShNl_Call = (Sh_Call,Nl_Call),
	           change_values_insert([V],Nl_Call,Nl_Succ,f),
		   ShNl_Succ = (Sh_Call,Nl_Succ),
 	           % Mod

		   % BEGIN CHECK ME
		   % IS IT CORRECT?
 	           %insert(Mod_Call,V,Mod_Succ),
		   % END ME
		   Mod_Succ = Mod_Call,
		   
	           % Types
 	           oo_types_success_builtin(neq,Sv,Condv,Tau_Call,Tau_Succ),
		   split(Succ,ShNl_Succ,Tau_Succ,Mod_Succ)
	      )
	).
% if (v != i) then comm
oo_shnltau_success_builtin_(neq,_Sv,Condv,Call,Succ):-
	Condv = (V,W),
	var(V),
	int(W),
	!,
	Succ = Call.


% skip
oo_shnltau_success_builtin_(skip,_Sv,_Condv,Call,Call).




%%%%%%%%%%%%%%%%%%%%  Metacommands    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

oo_shnltau_success_builtin_(method_declaration,Sv,Condv,Call,Succ):-
	split(Call,ShNl_Call,Tau_Call,Mod),
	oo_types_success_builtin(method_declaration,Sv,Condv,Tau_Call,Tau_Succ),
	split(Succ,ShNl_Call,Tau_Succ,Mod).


% 'unchanged' commands that do not affect sharing semantics
oo_shnltau_success_builtin_(unchanged,_Sv,_Condv,Call,Succ):-
	!,
	Succ = Call.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%   Auxiliary Preds %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- push_prolog_flag(multi_arity_warnings,off).
split('$bottom','$bottom',_):-!.
split('$bottom',_,'$bottom'):-!.
split((A,B,_),A,B).

split('$bottom','$bottom',_,_):-!.
split('$bottom',_,'$bottom',_):-!.
split((A,B,C),A,B,C).
:- pop_prolog_flag(multi_arity_warnings).


% remove a set of variables from ASub
oo_shnltau_project_out(ASub,Vars,Proj):-
        split(ASub,(_Sh,Nl),_Tau,_Mod),
	varset(Nl,AllVars),
	ord_subtract(AllVars,Vars,RestVars),
	oo_shnltau_project(ASub,RestVars,Proj).
	
% remove a set of variables from ShNl
project_out_shnl(ASub,Vars,Proj):-
	ASub = (_Sh,Nl),
	varset(Nl,AllVars),
	ord_subtract(AllVars,Vars,Vars1),
	shfr_project(ASub,Vars1,Proj).

% remove a set of variables just from the sharing, keeping Nl 
project_out_sh(ASub,Vars,Proj):-
	ASub = (_Sh,Nl),
	!,
	project_out_shnl(ASub,Vars,(Sh_Proj,_)),
	Proj = (Sh_Proj,Nl).

% remove a set of variables just from Nl, keeping Sh
project_out_nl(ASub,Vars,Proj):-
	ASub = (Sh,_Nl),
	!,
	project_out_shnl(ASub,Vars,(_,Nl_Proj)),
	Proj = (Sh,Nl_Proj).

	
% return the mode of a given variable
nl(Var,ShNl,Mode):-
	ShNl = (_Sh,Nl),
	!,
	nl(Var,Nl,Mode).
nl(Var,Nl,Mode):-
	var_value(Nl,Var,Mode).

% inserts a pair in Sh in sorted order 
insert_sorted(Sh,[Var1,Var2],New_Sh):-
	sort_pair(Var1,Var2,(V1,V2)),
	insert(Sh,[V1,V2],New_Sh).

% sort a pair of variables
sort_pair(Var1,Var2,(Var1,Var2)):-
	Var1 @=< Var2,!.
sort_pair(Var1,Var2,(Var2,Var1)):-
	Var1 @> Var2,!.
	

% find all variables verifying some particular characteristic
find_null_vars(Nl,Null):-
	member_value_freeness(Nl,Null,g).
find_notnull_vars(Nl,NotNull):-	
	member_value_freeness(Nl,NotNull,f).


% get the type component in ASub
type_component(ASub,Tau):-
	split(ASub,_,Tau,_).


% from oo_son to oo_share
% BEGIN CORRECT ME
% the use of linearity is correct or should we ignore it?
oo_son_to_oo_shnltau(ASub,Vars,New_ASub):-
	ASub = ((Gr,PairSh),Tau),
	son_to_share((Gr,PairSh),Vars,Sh,Linear),
	change_values_insert(Gr,[],Nl1,g),
	change_values_insert(Vars,Nl1,Nl2,ng),
	change_values_insert(Linear,Nl2,Nl,f),
	New_ASub = ((Sh,Nl),Tau,Vars).
% END CORRECT ME


%-------------------------------------------------------------------------
% simulate ShNl unification when doing X = Y
% implies project out X
% and add (transitively) the relation [X,Y] depending on Nl
% 
% assume that the pair is VariableAssigned = NewValue
%-------------------------------------------------------------------------
uplus(ShNl,[],ShNl).
uplus(ShNl,[Pair|RP],NewShNl):-
	!,
	uplus(ShNl,Pair,ShNl1),
	uplus(ShNl1,RP,NewShNl).
uplus(ShNl,Pair,NewShNl):-
	Pair = (V1,_V2),!,
	project_out_shnl(ShNl,[V1],ShNl1),
	ShNl1 = (Sh1,Nl1),
	sh_add_and_close(Sh1,Pair,Sh2),
	NewShNl = (Sh2,Nl1).

% add a pair to Sh and apply closure conversion
sh_add_and_close(Sh,Pair,NewSh):-
	Pair = (X,Y),
	varset(Pair,Sv),
	% BEGIN CHANGE ME
	% TOO COMPLICATED
	(member_sh(X,Sh) ->
	     Sh1 = Sh
	     ;
	     insert(Sh,[X],Sh1)
	),
	share_project(Sv,Sh1,Proj),
	share_call_to_success_builtin('=/2','='(X,Y),Sv,Sh1,Proj,Sh2),
	(ord_member([Y],Sh) ->
	     NewSh = Sh2
	     ;
	     ord_delete(Sh2,Sv,NewSh)
	).
	% END CHANGE ME





member_sh(_V,[]):-
	!,
	fail.
member_sh(V,[H|_T]):-
	ord_member(V,H),
	!.
member_sh(V,[_|T]):-
	member_sh(V,T).


%-------------------------------------------------------------------------
% Find pairs (p_i,v_i); If p_i is atomic then skip
%-------------------------------------------------------------------------

unify_call(Term1,Term2,SBinds) :- 
	functor(Term1,F,N),
	functor(Term2,F,N),
	unify_call_args(0,N,Term1,Term2,Binds,[]),
	sort(Binds,SBinds).

unify_call_args(N,N,_,_,Binds,Rest) :- !,
	Binds = Rest.
unify_call_args(N1,N,Term1,Term2,Binds,Rest) :-
	N2 is N1 + 1,
	arg(N2,Term1,A1),
	arg(N2,Term2,A2),
	unify_par(A1,A2,Binds,Rest1),
	unify_call_args(N2,N,Term1,Term2,Rest1,Rest).

unify_par(Term1,Term2,Binds,Rest) :-
	var(Term1),
	var(Term2),!,
	Binds = [(Term1,Term2)|Rest].
unify_par(_Term1,_Term2,Rest,Rest).
