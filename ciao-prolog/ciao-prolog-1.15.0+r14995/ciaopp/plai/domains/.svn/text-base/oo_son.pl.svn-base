
% Implementation of the Pair-Sharing domain for Object Oriented Programs
% based on Secci & Spoto's paper on SAS'05
% "Pair-Sharing Analysis of Object-Oriented Programs"
%
% Since in the article they only use declared types make sure of
% use_dynamic_types(off)
%
% Information about declared types is necessary for method calls
% Changes from the paper are based in either
% (1) presence of atomic types
% (2) All logic variables must be given an abstract value at all program points 
%   (2.1) in call_to_entry local variables and Res are declared to be null objects  
%   (2.2) is incorrect to project over each Res after assignment [we keep it]
%   (2.3) for reusing Res we have to project it our *before* expression eval,
%         and then assign it new values 


:- module(_
	,_
	,[]).

:- use_module(domain(sondergaard), 
	[
	  % Interprocedural predicates
	  son_project/3,      
	  son_call_to_entry/6,
	  son_extend/4,       
	  son_exit_to_prime/7,
	  son_call_to_success_fact/8, 
	  son_unknown_entry/2,

	  % Lattice predicates
	  son_compute_lub/2,  
	  son_lub/3,  
	  son_glb/3,        
	  son_less_or_equal/2,
	  son_sort/2,         

	  % builtin interpretations
	  son_call_to_success_builtin/6,
	  son_success_builtin/5,

	  % I/O predicates
	  son_asub_to_native/3

 	]).
:- use_module(domain(s_grshfr), 
	 [
	  var_value/3
	 ]).


:- use_module(domain(oo_types)).
:- use_module(cafelito(cafelito_semantics)). 


% load of metainfo on demand

% rest of libraries in use
:- use_module(library(lists)).
:- use_module(library(sort)).
:- use_module(library(sets)).
:- use_module(library(terms_vars)).
:- use_module(library(messages)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               Configuration    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Turn this to on/off for using dynamic types
% You can always use the CiaoPP flag oo_types_dyn_info
use_dynamic_types(off).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                Initialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
oo_son_init_abstract_domain:-
% 	use_dynamic_types(Mode),
% 	set_pp_flag(oo_types_dyn_info,Mode),
	oo_types_init_abstract_domain.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                Empty/Unknown entry
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% In Java all the public methods have to have an entry (about types).
oo_son_unknown_entry(_Qv,Call):-
	Call = '$bottom'.


% BEGIN TODO
% semantics of this ???
% END TODO
oo_son_empty_entry(Qv,Call):-
	oo_son_unknown_entry(Qv,Call).


% External calls have to be represented through java_external_call
oo_son_unknown_call(_Call,_Vars,Succ):-
	Succ = '$bottom'.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        Interprocedural control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

oo_son_project(ASub,Vars,Proj):-
	split(ASub,NullSh,Tau),
	son_project(Vars,NullSh,NullSh_Proj),
	oo_types_project(Tau,Vars,Tau_Proj),
	split(Proj,NullSh_Proj,Tau_Proj).


oo_son_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	split(Proj,NullSh_Proj,Tau_Proj),
	% NullSh
	son_call_to_entry(Hv,Sg,Head,NullSh_Proj,NullSh_Entry,NullSh_ExtraInfo),
	NullSh_Entry = (Null_Tmp1,Sh_Entry),
	ord_union(Null_Tmp1,Fv,Null_Entry),
	% Tau
	oo_types_call_to_entry(Sv,Sg,Hv,Head,Fv,
	                   Tau_Proj,Tau_Entry,Tau_ExtraInfo),
			   
	split(Entry,(Null_Entry,Sh_Entry),Tau_Entry),
	split(ExtraInfo,NullSh_ExtraInfo,Tau_ExtraInfo).


oo_son_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	split(Exit,NullSh_Exit,Tau_Exit),
	split(ExtraInfo,NullSh_ExtraInfo, Tau_ExtraInfo),
	son_exit_to_prime(Sg,Hv,Head,Sv,NullSh_Exit,NullSh_ExtraInfo,NullSh_Prime),
	oo_types_exit_to_prime(Sg,Hv,Head,Sv,
	                   Tau_Exit,Tau_ExtraInfo,Tau_Prime),	   
	split(Prime,NullSh_Prime,Tau_Prime).

oo_son_extend(Prime,Sv,Call,Succ):-
	split(Call,NullSh_Call,Tau_Call), 
	split(Prime,NullSh_Prime,Tau_Prime),
	% BEGIN CHANGE ME
	% TOO COMPLICATED
	% work-around for vars that are null on Call but not in Prime
	% like in oo_shnltau
	NullSh_Call = (Null_Call,Sh_Call),
	list_to_list_of_lists(Null_Call,NullVars),
	ord_union(NullVars,Sh_Call,Fake_Sh_Call),
	Fake_NullSh_Call = ([],Fake_Sh_Call),
	son_extend(NullSh_Prime,Sv,Fake_NullSh_Call,NullSh_Tmp1),
	prune(NullVars,NullSh_Tmp1,NullSh_Prime,NullSh_Succ),
	% END CHANGE ME
        oo_types_extend(Tau_Prime,Sv,Tau_Call,Tau_Succ),
	split(Succ,NullSh_Succ,Tau_Succ).

prune([],Sh_Succ,_,Sh_Succ).
prune([NullVar|T],NullSh,NullSh_Prime,NullSh_Succ):-
	NullSh = (Null,Sh),
	NullSh_Prime = (_Null_Prime,Sh_Prime),
	(ord_member(NullVar,Sh) ->
	       (ord_member(NullVar,Sh_Prime) ->
		     NullSh_1 = NullSh 
	             ;
		     ord_delete(Sh,NullVar,Sh1),
		     ord_union(Null,NullVar,Null1),
		     NullSh_1 = (Null1,Sh1)
	       )
	       ;
	       NullSh_1 = NullSh
	),
	prune(T,NullSh_1,NullSh_Prime,NullSh_Succ).


oo_son_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	split(Call,NullSh_Call,Tau_Call),
	split(Proj,NullSh_Proj,Tau_Proj),
	son_call_to_success_fact(Sg,Hv,Head,Sv,NullSh_Call,NullSh_Proj,NullSh_Prime,NullSh_Succ),
	oo_types_call_to_success_fact(Sg,Hv,Head,Sv,
	                          Tau_Call,Tau_Proj,Tau_Prime,Tau_Succ),
	split(Prime,NullSh_Prime,Tau_Prime),
	split(Succ, NullSh_Succ, Tau_Succ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        Lattice predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

oo_son_compute_lub(ListASub,Lub):-
	pairlist2listpairs(ListASub,NullSh,Tau),	
	son_compute_lub(NullSh,NullSh_Lub), 
	oo_types_compute_lub(Tau,Tau_Lub),
	split(Lub,NullSh_Lub,Tau_Lub).
	     
pairlist2listpairs([],[],[]).
pairlist2listpairs(['$bottom'|T],['$bottom'|T1],['$bottom'|T2]):-
	pairlist2listpairs(T,T1,T2).
pairlist2listpairs([(H1,H2)|T],[H1|T1],[H2|T2]):-
	pairlist2listpairs(T,T1,T2).

oo_son_lub(ASub0,ASub1,Lub):-
	split(ASub0,NullSh0,Tau0),
	split(ASub1,NullSh1,Tau1),
	son_lub(NullSh0,NullSh1,NullSh_Lub),
	oo_types_compute_lub_el(Tau0,Tau1,Tau_Lub),
	split(Lub,NullSh_Lub,Tau_Lub).


oo_son_glb(ASub0,ASub1,Glb):-
	split(ASub0,NullSh0,Tau0),
	split(ASub1,NullSh1,Tau1),
	son_glb(NullSh0,NullSh1,NullSh_Glb),
	oo_types_glb(Tau0,Tau1,Tau_Glb),
	split(Glb,NullSh_Glb,Tau_Glb).


oo_son_less_or_equal(ASub0,ASub1):-
	split(ASub0,NullSh0,Tau0),
	split(ASub1,NullSh1,Tau1),
	son_less_or_equal(NullSh0,NullSh1),
	oo_types_less_or_equal(Tau0,Tau1).


oo_son_sort(ASub,ASub_s):-
	split(ASub,NullSh,Tau),
	son_sort(NullSh,NullSh_s),
	oo_types_sort(Tau,Tau_s),
	split(ASub_s,NullSh_s,Tau_s).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Interface Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

oo_son_input_user_interface(ASub,_Qv,ASub).

oo_son_input_interface(Info,Kind,A,B):-
	oo_son_init_abstract_domain,
 	oo_types_input_interface(Info,Kind,A,Tau), 
	nl_from_types(Tau,Null,UnkOrNnull),
	powerset_sh(UnkOrNnull,NullSh),
	NullSh = ([],Sh),
	split(Tmp,(Null,Sh),Tau),
	apply_reach_constraints(Tmp,B).



% BEGIN CORRECT ME
% Shared with oo_shnltau, move to common file
nl_from_types([This/_|R],Null,UnkOrNnull):-
	nl_from_types_(R,[],[This],Null,UnkOrNnull).

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
% END CORRECT ME


powerset_sh(Objects,Pow):-
	son_unknown_entry(Objects,Pow).


% BEGIN CORRECT ME
% CORRECT BYTECODE SEMANTICS
% ignore Linearity
oo_son_identical_abstract(ASub1,ASub2):-
	split(ASub1,(Null,Sh1),Tau),
	split(ASub2,(Null,Sh2),Tau),
	varset(Sh1,Vars1),
	varset(Sh2,Vars2),
	ord_union(Vars1,Vars2,Vars),
	list_to_list_of_lists(Vars,LVars),
	ord_union(LVars,Sh1,NSh1),
	ord_union(LVars,Sh2,NSh2),
	NSh1 == NSh2.
% END CORRECT ME

oo_son_asub_to_native(ASub,Qv,ASub_user):- 
	% removal of temporal vars
	type_component(ASub,T),
	find_any_vars(T,AnyVars),
	oo_son_project_out(ASub,AnyVars,ASub1),
	% rely on son domain for output
	split(ASub1,NullSh,Tau),
	son_asub_to_native(NullSh,Qv,Info1),
	adapt_to_native(Info1,Info2),
	oo_types_asub_to_native(Tau,Qv,Info3),
	append(Info2,Info3,ASub_user).

% change ground functor to nullity and place last, keep the rest
adapt_to_native([],[]).
adapt_to_native([Gr|R],Native):-
	Gr = ground(NullVars),
	!,
	Null = null(NullVars),
	adapt_to_native(R,NR),
	append(NR,[Null],Native).
adapt_to_native([W|R],[W|NR]):-
	adapt_to_native(R,NR).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          Builtins (this is the important part!)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

oo_son_special_builtin('java_bytecode:java_skip/0',
                           'java_bytecode:java_skip',
	                   skip,nothing).

oo_son_special_builtin('java_bytecode:java_decl/2',
                           'java_bytecode:java_decl'(C,V),
	                   declaration,Condv):-
	Condv = (C,V).
oo_son_special_builtin('java_bytecode:java_param/2',
                           'java_bytecode:java_param'(C,V),
                            parameter,Condv):-
	Condv = (C,V).

oo_son_special_builtin('java_bytecode:java_meth_decl/3',
                           'java_bytecode:java_meth_decl'(M,C,T),
                            method_declaration,Condv):-
	Condv = (M,C,T).


oo_son_special_builtin('java_bytecode:java_null/2',
                  	'java_bytecode:java_null'(C,Res),
			null,Condv):-
        Condv = (C,Res).
oo_son_special_builtin('java_bytecode:java_new/2' ,
                  	'java_bytecode:java_new'(C,Res),
			new,Condv):-
        Condv = (C,Res).
oo_son_special_builtin('java_bytecode:java_get/2',
                  	'java_bytecode:java_get'(V,Res),
			get,Condv):-
        Condv = (V,Res).
oo_son_special_builtin('java_bytecode:java_getfield/3',
	                'java_bytecode:java_getfield'(V,Field,Res),
			 getfield,Condv):-
        Condv = (V,Field,Res).

oo_son_special_builtin('java_bytecode:java_set/2',
                  	'java_bytecode:java_set'(V,Res),
			set,Condv):-
        Condv = (V,Res).
oo_son_special_builtin('java_bytecode:java_setfield/3',
	                'java_bytecode:java_setfield'(V,Field,Res),
			setfield,Condv):-
        Condv = (V,Field,Res).

oo_son_special_builtin('==/2',==(X,Y),eq,Condv):-
	Condv = (X,Y).	
oo_son_special_builtin('\\==/2','\\=='(X,Y),neq,Condv):-
	Condv = (X,Y).	


% Builtins not affecting the abstract state can be ignored
oo_son_special_builtin('</2',_,unchanged,_Condv).
oo_son_special_builtin('>=/2',_,unchanged,_Condv).


%%%%%%%%%%%%%%%%%%%%%%%%%%  Error mechanism  %%%%%%%%%%%%%%%%%%%%%%%%%%

% Invariant:unprocessed builtin can only be the result of an error
% any other builtin must be declared explicitely
oo_son_success_builtin(Builtin,Sv,Condv,Call,Succ):-
	oo_son_success_builtin_(Builtin,Sv,Condv,Call,Succ),
	!.
oo_son_success_builtin(Builtin,_Sv,Condv,Call,'$bottom'):-
	error_message("Builtin ~w failed {Condv = ~w, Call = ~w}",[Builtin,Condv,Call]),
        throw(domain_error(oo_son,builtin(Builtin))).



%%%%%%%%%%%%%%%%%%%%%%%%%%  Expressions %%%%%%%%%%%%%%%%%%%%%%%%%%

% null
oo_son_success_builtin_(null,Sv,Condv,Call,Succ):-
	!,
	Condv = (_K,Res), 
	oo_son_project_out(Call,[Res],Call1),  % 2.3
	split(Call1,(Null,Sh),Tau_Call),
	insert(Null,Res,Null1),% theoretically necessary; redundant with (2)
	oo_types_success_builtin(null,Sv,Condv,Tau_Call,Tau_Succ),
	split(Succ,(Null1,Sh),Tau_Succ).


% new k
oo_son_success_builtin_(new,Sv,Condv,Call,Succ):-
	!,
	Condv = (_K,Res),
	oo_son_project_out(Call,[Res],Call1), % 2.3
	split(Call1,(Null_Succ,Sh),Tau_Call),
	insert(Sh,[Res],Sh_Succ),
	oo_types_success_builtin(new,Sv,Condv,Tau_Call,Tau_Succ),
	split(Succ,(Null_Succ,Sh_Succ),Tau_Succ).

% v
oo_son_success_builtin_(get,Sv,Condv,Call,Succ):-
	!,
	Condv = (V,Res),
	oo_son_project_out(Call,[Res],Call1), % 2.3
	split(Call1,(Null_Call,Sh_Call),Tau_Call),
	(null(V,Call1) ->
	   insert(Null_Call,Res,Null_Succ),
	   Sh_Succ = Sh_Call
	   ;
	   Null_Succ = Null_Call, 
	   replace_var_in_sh(Sh_Call,V,Res,Sh_Succ1),
	   ord_union(Sh_Call,Sh_Succ1,Sh_Succ2),
	   insert_sorted(Sh_Succ2,[V,Res],Sh_Succ)
	),
        oo_types_success_builtin(get,Sv,Condv,Tau_Call,Tau_Succ),
	split(Succ,(Null_Succ,Sh_Succ),Tau_Succ).


% i
% MISSING: java_get(3,Res) + java_set(V,Res). Does it make sense??? 


% v.f
oo_son_success_builtin_(getfield,Sv,Condv,Call,Succ):-
	!,
	Condv = (V,Field,Res),
	oo_son_project_out(Call,[Res],Call1), % 2.3
	(null(V,Call1) ->
	   Succ = '$bottom' 
	   ;
	   split(Call1,(Null_Call,Sh_Call),Tau_Call),
	   var_value(Tau_Call,V,VK),
	   oo_types_field_type(VK,Field,K),
	   % check introduced because of (1)
	   (oo_types_atomic_type(K) ->
	        insert(Null_Call,Res,Null_Succ), % th. necessary; redundant with (2)
		Sh_Succ = Sh_Call
	        ;
	        Null_Succ = Null_Call,
		% BEGIN OPTIMIZE ME (1)
		% It would be optimal if every pair is added only when 
	        % reach(W) intersection reach(Res) is not empty
	        replace_var_in_sh(Sh_Call,V,Res,Sh_Succ1),
		% END OPTIMIZE ME
	        ord_union(Sh_Call,Sh_Succ1,Sh_Succ2),
	        insert_sorted(Sh_Succ2,[V,Res],Sh_Succ)
	   ),
     	   oo_types_success_builtin(getfield,Sv,Condv,Tau_Call,Tau_Succ),
	   split(Tmp,(Null_Succ,Sh_Succ),Tau_Succ),
	   % BEGIN OPTIMIZE ME (2)
	   % Deletable after writing the first optimization
	   apply_reach_constraints(Tmp,Succ)
	   % END OPTIMIZE ME
	).



%%%%%%%%%%%%%%%%%%%%%%%%  Commands    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
% v:t 
oo_son_success_builtin_(declaration,Sv,Condv,Call,Succ):-
	!,
	Condv = (_K,Res),
	oo_son_project_out(Call,[Res],Call1), % 2.3
	split(Call1,(Null_Call,Sh_Call),Tau_Call),
	insert(Null_Call,Res,Null_Succ),
        oo_types_success_builtin(declaration,Sv,Condv,Tau_Call,Tau_Succ),
	split(Succ,(Null_Succ,Sh_Call),Tau_Succ).


% v:=Res
oo_son_success_builtin_(set,Sv,Condv,Call,Succ):-
	!,
	Condv = (V,Res),
	split(Call,NullSh_Call,Tau_Call),
	NullSh_Call = (Null_Call,Sh_Call),
	% (3) forces keeping Res
	(null(Res,Call) ->
	     insert(Null_Call,V,Null_Succ),
	     Sh_Succ = Sh_Call
	     ; 
	     remove_var_from_nullsh(NullSh_Call,V,NullSh1),
	     NullSh1 = (Null_Succ,Sh_Tmp1),
	     replace_var_in_sh(Sh_Tmp1,Res,V,Sh_Tmp2),
	     ord_union(Sh_Tmp1,Sh_Tmp2,Sh_Tmp3),
	     insert_sorted(Sh_Tmp3,[V,Res],Sh_Succ)
	),     
	oo_types_success_builtin(set,Sv,Condv,Tau_Call,Tau_Succ),
	split(Succ,(Null_Succ,Sh_Succ),Tau_Succ).



% v.f:=Res
oo_son_success_builtin_(setfield,Sv,Condv,Call,Succ):-
	!,
	Condv = (V,_Field,Res),
	(null(V,Call) ->
	    Succ = '$bottom'
	    ;
	    split(Call,NullSh_Call,Tau_Call),
	    % we keep Res in either Null or Sh by (3)
	    (null(Res,Call) ->
	        Succ = Call
	        ;
		NullSh_Call = (Null_Call,Sh_Call),
		% BEGIN OPTIMIZE ME (1)
		% It would be optimal if transitive closure
		% takes Reachability into account
		transitive_closure(Sh_Call,[V,Res],Sh_Succ),
		% END OPTIMIZE ME (1)
		NullSh_Succ = (Null_Call,Sh_Succ),
	        oo_types_success_builtin(setfield,Sv,Condv,Tau_Call,Tau_Succ),
		split(Tmp,NullSh_Succ,Tau_Succ),
		% BEGIN OPTIMIZE ME (2)
		% delete after coding optim (1)
		apply_reach_constraints(Tmp,Succ)
		% END OPTIMIZE ME (2)
	    )
	).
	    

% throw Res
% TODO

% return Res
% solved through set(Out,Res) and unification
% by assumption on the language no statement follows

% if (v = w) then comm
% impossible to determine in the domain

% if (v != w) then comm
% TODO : if there is no pair (V,W) then proceed


% if (v = null) then comm
oo_son_success_builtin_(eq,Sv,Condv,Call,Succ):-
	Condv = (V,W),
	var(V),
	W == null,
	!,
	(null(V,Call) ->
		Succ = Call
	        ;
	        split(Call,(Null_Call,Sh_Call),Tau_Call),		
		insert(Null_Call,V,Null_Succ),
	        remove_var_from_sh(Sh_Call,V,Sh_Succ),
		oo_types_success_builtin(eq,Sv,Condv,Tau_Call,Tau_Succ),
	        split(Succ,(Null_Succ,Sh_Succ),Tau_Succ)
	).
% if (v == i) then comm
oo_son_success_builtin_(eq,_Sv,Condv,Call,Succ):-
	Condv = (V,W),
	var(V),
	int(W),
	!,
	Succ = Call.


% if (v != null) then comm
oo_son_success_builtin_(neq,Sv,Condv,Call,Succ):-
	Condv = (V,W),
	var(V),
	W == null,
	!,
	(null(V,Call) ->
		Succ = '$bottom'
	        ;
	        split(Call,NullSh_Call,Tau_Call),		
		oo_types_success_builtin(neq,Sv,Condv,Tau_Call,Tau_Succ),
	        split(Succ,NullSh_Call,Tau_Succ)
	).
% if (v != i) then comm
oo_son_success_builtin_(neq,_Sv,Condv,Call,Succ):-
	Condv = (V,W),
	var(V),
	int(W),
	!,
	Succ = Call.


oo_son_success_builtin_(skip,_Sv,_Condv,Call,Call).


%%%%%%%%%%%%%%%%%%%%  Metacommands    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

oo_son_success_builtin_(method_declaration,Sv,Condv,Call,Succ):-
	!,
	split(Call,NullSh_Call,Tau_Call),
	oo_types_success_builtin(method_declaration,Sv,Condv,Tau_Call,Tau_Succ),
	split(Succ,NullSh_Call,Tau_Succ).

oo_son_success_builtin_(unchanged,_Sv,_Condv,Call,Succ):-
	!,
	Succ = Call.

oo_son_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ):-
	split(Call,NullSh_Call,Tau_Call),
	split(Proj,NullSh_Proj,Tau_Proj),
	son_call_to_success_builtin(SgKey,Sg,Sv,
	                             NullSh_Call,NullSh_Proj,NullSh_Succ),
	oo_types_call_to_success_builtin(SgKey,Sg,Sv,
	                             Tau_Call,Tau_Proj,Tau_Succ),
	split(Succ,NullSh_Succ,Tau_Succ).			     


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%              Auxiliary predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split('$bottom','$bottom',_):-!.
split('$bottom',_,'$bottom'):-!.
split((A,B),A,B).

type_component(ASub,Tau):-
	split(ASub,_,Tau).


% inserts a pair in Sh in sorted order 
insert_sorted(Sh,[Var1,Var2],New_Sh):-
	Var1 @=< Var2,!,
	insert(Sh,[Var1,Var2],New_Sh).
insert_sorted(Sh,[Var1,Var2],New_Sh):-
	Var1 @> Var2,
	insert(Sh,[Var2,Var1],New_Sh).


% tell whether a variable is null attending to Null
null(Var,ASub):-
	ASub = ((Null,_Sh),_Tau),
	!,
	ord_member(Var,Null).
null(Var,NullSh):-
	NullSh = (Null,_Sh),
	ord_member(Var,Null).


% remove a set of variables from ASub
oo_son_project_out(ASub,Vars,Proj):-
        split(ASub,_,Tau),
	varset(Tau,AllVars),
	ord_subtract(AllVars,Vars,RestVars),
	oo_son_project(ASub,RestVars,Proj).


% remove a variable from the sharing set
% if it's in the ground part, just remove it there
% if it's in the sharing, do not remove aliases as it 
% is an conservative approximation 
remove_var_from_nullsh(NullSh,Var,New_NullSh):-
	NullSh = (Null,Sh),
	(null(Var,NullSh) ->
	     ord_delete(Null,Var,New_Null),
	     New_NullSh = (New_Null,Sh)
             ;
	     remove_var_from_sh(Sh,Var,New_Sh),
	     New_NullSh = (Null,New_Sh)
        ).

% remove a variable from Sh
% Aliases are preserved to keep approximation correctly
remove_var_from_sh([],_,[]).
remove_var_from_sh([[V]|R],Var,New_Sh):-
	(V == Var ->
		New_Sh = RSh
                ;
	        New_Sh = [[V]|RSh]
	),
	remove_var_from_sh(R,Var,RSh).
remove_var_from_sh([[V1,V2]|R],Var,New_Sh):-
	(V1 == Var ->
		   New_Sh = RSh
	           ;
               	   (V2 == Var ->
		            New_Sh = RSh
		            ;
		            New_Sh = [[V1,V2]|RSh]  
		   )
	),
	remove_var_from_sh(R,Var,RSh).


% replace a variable by another just in the sharing and nullity sets
replace_var_in_nullsh(NullSh,Var,New_Var,New_NullSh):-
	NullSh = (Null,Sh),  
	(null(Var,NullSh)->
	    ord_delete(Null,Var,Null1),
	    insert(Null1,New_Var,New_Null),
	    New_Sh = Sh
	    ;
	    New_Null = Null,
	    replace_var_in_sh(Sh,Var,New_Var,New_Sh)
	),
	New_NullSh = (New_Null,New_Sh).

% replace a variable by another just in the sharing set 
replace_var_in_sh(Sh,Var,New_Var,New_Sh):-
	replace_var_in_sh_(Sh,Var,New_Var,Sh_Unsorted),
	son_sort(([],Sh_Unsorted),([],Sh_Sorted)),
	New_Sh = Sh_Sorted.

replace_var_in_sh_([],_,_,[]).
replace_var_in_sh_([[V]|RSh],Var,New_Var,New_Sh):-
	replace_var_in_sh_(RSh,Var,New_Var,RNew_Sh),
	(V == Var ->
	     New_Sh = [[New_Var]|RNew_Sh]
	     ;
	     New_Sh = [[V]|RNew_Sh]
	).
replace_var_in_sh_([[V1,V2]|RSh],Var,New_Var,New_Sh):-
	replace_var_in_sh_(RSh,Var,New_Var,RNew_Sh),
	(V1 == Var ->
	     New_Sh = [[New_Var,V2]|RNew_Sh]
	     ;
	     (V2 == Var ->
	            New_Sh = [[V1,New_Var]|RNew_Sh]
	            ;
	            New_Sh = [[V1,V2]|RNew_Sh]
	     )
	).


% Only for sharing: add a pair X = Y and close
% Precondition: X,Y are not null 

% BEGIN DELETE ME
% BRUTE-FORCE DEBUG
% delete after checking equality
transitive_closure(Sh,[X,Y],ShStar):-	
	transitive_closure1(Sh,[X,Y],ShStar1),
%	transitive_closure2(Sh,[X,Y],ShStar2),
%	ShStar1 = ShStar2,
	ShStar  = ShStar1.
transitive_closure1(Sh,[X,Y],ShStar):-
	Call = ([],Sh),
	son_success_builtin('=/2',_,p(X,Y),Call,Succ),
	Succ = ([],ShStar).
% END DELETE ME

transitive_closure2(Sh,[X,Y],ShStar):-
	vars_sharing_with(Sh,X,Sh_X),
	insert(Sh_X,X,Sh1),
	vars_sharing_with(Sh,Y,Sh_Y),
	insert(Sh_Y,Y,Sh2),
	ord_subtract(Sh2,Sh1,Sh3),
	cross_product([Sh1,Sh3],NewPairs),
	ord_union(Sh,NewPairs,Star1),
	insert(Star1,[X],Star2),
	sort(Star2,ShStar).


% retrieve for variable V all variables W s.t. [V,W]\in Sh
vars_sharing_with([],_,[]).
vars_sharing_with([[_V]|RSh],Var,Sh_Var):-
	vars_sharing_with(RSh,Var,Sh_Var).
vars_sharing_with([[V1,V2]|RSh],Var,Sh_Var):-
	vars_sharing_with(RSh,Var,RSh_Var),
	(V1 == Var ->
             insert(RSh_Var,V2,Sh_Var)
	     ;
	     (V2 == Var ->
	            insert(RSh_Var,V1,Sh_Var)
	            ;
	            Sh_Var = RSh_Var
	     )
	).



% eliminate those pairs that are impossible by type definition
% For example, v/string and  w/integer never share (reach same object) 
apply_reach_constraints(ASub,New_ASub):-
	split(ASub,(Gr,Sh),Tau),
	varset(Sh,Vars),
	gather_reach(Vars,Tau,VarsReach),
	filter_sh_by_reach(Sh,VarsReach,New_Sh),
	split(New_ASub,(Gr,New_Sh),Tau).

% get the reachability for every var in advance saves a lot of time
gather_reach([],_Tau,[]).
gather_reach([V|RV],Tau,[V/ReachV|Reach]):-
	var_value(Tau,V,Type),
	oo_types_reachability(Type,ReachV),
	gather_reach(RV,Tau,Reach).

% if reach(V1) intersection reach(V2) is empty 
% or just atomic types, eliminate pair
filter_sh_by_reach([],_VarsReach,[]).
filter_sh_by_reach([[V]|RSh],VarsReach,[[V]|RNSh]):-
	filter_sh_by_reach(RSh,VarsReach,RNSh).
filter_sh_by_reach([[V1,V2]|RSh],VarsReach,RNSh):-
	var_value(VarsReach,V1,R1),
	var_value(VarsReach,V2,R2),
	atomic_types(Atomic),
	ord_intersection(R1,R2,Tmp),
	ord_subtract(Tmp,Atomic,Common),
	filter_sh_by_reach(RSh,VarsReach,RecReach),
	(Common = [] ->
	     RNSh = RecReach
	     ;
	     RNSh = [[V1,V2]|RecReach]
	).