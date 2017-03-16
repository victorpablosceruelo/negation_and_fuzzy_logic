:- module(_
	,_
	,[]).


:- use_module(domain(gr), [gr_sort/2]).
:- use_module(domain(s_grshfr), [
	                   change_values_insert/4,
			   member_value_freeness/3,
			   var_value/3
			   ]).

:- use_module(cafelito(cafelito_semantics)). 

:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(library(messages)).

% load of metainfo on demand
:- use_module(library(compiler), [use_module/1]).
:- use_module(program(itf_db), [curr_file/2]).
:- use_module(library(dynamic), [asserta/1, retractall/1]).

:- use_module(library(sets)).
:- use_module(library(sort), [sort/2]).

:- discontiguous current_pp_flag/2.

:- multifile assertall_metainfo/1.

:- dynamic already_loaded/1.
:- dynamic subclass/2.

%%%%%%%%%%%%%%%%%%%%%%%%%% Initialization %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.
issue_debug_messages( oo_types ).


% Load of metainformation relative to the file analyzed
oo_types_init_abstract_domain:-
        curr_file(File,_),
	already_loaded(File),
	!.
oo_types_init_abstract_domain:-
	 (current_pp_flag(oo_types_dyn_info,off) ->
	      debug_message("Type analysis using only declared types")
	      ;
	      debug_message("Type analysis using dynamic types")
	 ),
	 curr_file(File,Module),
	 atom_concat(FileNoExt,'.pl',File),
	 atom_concat(FileNoExt,'_mi.pl',FileMetaInfo),
	 atom_concat(Module,'_mi.pl',ModuleMetaInfo),
	 (use_module(FileMetaInfo) ->
	    debug_message("Loaded metainformation file ~w ",[ModuleMetaInfo])
	    ;
	    error_message("Metainformation file ~w does not exit",[FileMetaInfo]),
	    throw(domain_error(absent_metainfo(FileMetaInfo)))
	),
	retractall_metainfo,
	load_metainfo(Module),
	retractall(already_loaded(_)),
	asserta(already_loaded(File)).

% load metainfo from both java_semantics and module_mi by fail
load_metainfo(Module):-
	assertall_metainfo(Module),
	fail.
load_metainfo(_Module).

%%%%%%%%%%%%%%%%%%% Empty/Unknown Entry %%%%%%%%%%%%%

oo_types_unknown_entry(_Qv,'$bottom').
	

oo_types_empty_entry(Qv,Call):-
	oo_types_unknown_entry(Qv,Call).

% External calls have to be represented through java_external_call
oo_types_unknown_call(_Call,_Vars,Succ):-
	Succ = '$bottom'.


%%%%%%%%%%%%%%%%%% Control Preds %%%%%%%%%%%%%%%%%%%%%%

oo_types_project('$bottom',_,Proj):- 
	!,
	Proj = '$bottom'.
oo_types_project(ASub,Vars,Proj) :- 
	project_aux(Vars,ASub,Proj).

project_aux([],_,Proj):- !,
	Proj = [].
project_aux(_,[],Proj):- !,
	Proj = [].
project_aux([Head1|Tail1],[Head2/Val|Tail2],Proj) :-
	compare(Order,Head1,Head2),
	project_aux_(Order,Head1,Tail1,Head2/Val,Tail2,Proj).

project_aux_(=,_,Tail1,HeadVal,Tail2,[HeadVal|Proj]) :-
	project_aux(Tail1,Tail2,Proj).
project_aux_(>,Head1,Tail1,_,[Head2/Val|Tail2],Proj) :-
	compare(Order,Head1,Head2),
	project_aux_(Order,Head1,Tail1,Head2/Val,Tail2,Proj).




oo_types_call_to_entry(_Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	oo_types_simplify_equations(Sg,Head,Binds),
	change_values_insert(Hv,Proj,Temp1,[any]),
	abs_gset_params(Temp1,Binds,Temp2),
	change_values_insert(Fv,Temp2,Temp3,[any]),
	merge(Hv,Fv,HvFv),
	oo_types_project(Temp3,HvFv,Entry),
	ExtraInfo = Binds,
	!.



oo_types_exit_to_prime(_Sg,_Hv,_Head,_Sv,'$bottom',_Flag,Prime) :- 
	!,
	Prime = '$bottom'.
oo_types_exit_to_prime(_Sg,Hv,_Head,Sv,Exit,ExtraInfo,Prime):-
	ExtraInfo = Binds,
 	oo_types_project(Exit,Hv,BPrime),
	abs_gupdate_params(BPrime,Binds,TempPrime),
	oo_types_project(TempPrime,Sv,Prime),
	!.



oo_types_extend('$bottom',_Sv,_Call,Succ):- !,
	Succ = '$bottom'.
oo_types_extend(_Prime,[],Call,Succ):- !,
	Call = Succ.
oo_types_extend(Prime,_Sv,Call,Succ):-
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



oo_types_call_to_success_fact(_Sg,_Hv,_Head,_Sv,_Call,_Proj,'$bottom','$bottom').
oo_types_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	oo_types_simplify_equations(Sg,Head,Binds),!,
	change_values_insert(Hv,Proj,Temp1,any),
	abs_gset_params(Temp1,Binds,Temp2),
	oo_types_project(Temp2,Sv,Prime),
	oo_types_extend(Prime,Sv,Call,Succ).




%%%%%%%%%%%%%%%%%%% Lattice Preds %%%%%%%%%%%%%%%%%%%%%

oo_types_compute_lub([X],X):- !.
oo_types_compute_lub([ASub1,ASub2|Xs],Lub):-
	oo_types_compute_lub_el(ASub1,ASub2,ASubLub),
	oo_types_compute_lub([ASubLub|Xs],Lub).


oo_types_compute_lub_el('$bottom',ASub,ASub):- !.
oo_types_compute_lub_el(ASub,'$bottom',ASub):- !.
oo_types_compute_lub_el(ASub1,ASub2,Lub):- 
	compute_lub_oo_types(ASub1,ASub2,Lub).


compute_lub_oo_types(ASub1,ASub2,Lub):- 
	ASub1 == ASub2, !,
	Lub = ASub1.
compute_lub_oo_types([Xv|ASub1],[Yv|ASub2],Lub):- 
	Xv == Yv, !,
	Lub = [Xv|Lub_Cont],
	compute_lub_oo_types(ASub1,ASub2,Lub_Cont).
compute_lub_oo_types([X/C1|ASub1],[X/C2|ASub2],[X/SC|Lub_Cont]):-
	least_ancestor(C1,C2,SC),
	compute_lub_oo_types(ASub1,ASub2,Lub_Cont).


oo_types_glb(ASub0,ASub1,Glb):-
	ASub0 == ASub1,!,
	Glb = ASub1.
oo_types_glb(ASub0,ASub1,Glb):-
	oo_types_glb_(ASub0,ASub1,Glb),!.
oo_types_glb(_,_,'$bottom').

oo_types_glb_([],[],[]).
oo_types_glb_([Xv|ASub0],[Yv|ASub1],[Xv|Glb]):-
	Xv == Yv,!,
	oo_types_glb_(ASub0,ASub1,Glb).
oo_types_glb_([X/C1|ASub0],[X/C2|ASub1],[X/Sub|Glb]):-
	(ancestor_eq(C1,C2) ->
	    Sub = C2
	    ;
	   (ancestor_eq(C2,C1) ->
	       Sub = C1
	       ;
	       Sub = '$bottom'
	   )
	),
	oo_types_glb_(ASub0,ASub1,Glb).

% it's assumed the two abstract subst. are defined on the same vars
oo_types_less_or_equal('$bottom',_).
oo_types_less_or_equal(ASub0,ASub1):-
	oo_types_less_or_equal_(ASub0,ASub1).

oo_types_less_or_equal_([],[]).
oo_types_less_or_equal_([X/C|Rest0],[X/C|Rest1]):-
	!,
	oo_types_less_or_equal_(Rest0,Rest1).
oo_types_less_or_equal_([X/C0|Rest0],[X/C1|Rest1]):-
	ancestor_eq(C1,C0),
	oo_types_less_or_equal_(Rest0,Rest1).


oo_types_sort(ASub,ASub_s):-
	gr_sort(ASub,ASub_s).



%%%%%%%%%%%%%%%%%%% Interface Preds %%%%%%%%%%%%%%%%%%%%%

oo_types_input_user_interface(ASub,_Qv,ASub).

% Assumption
% Assertions contain just the root type; here we add the whole hierchachy
oo_types_input_interface(Info,perfect,_ASub0,ASub1):-
	oo_types_init_abstract_domain,
	Info = tau(Types),
	expand_types(Types,ETypes),
	ASub1 = ETypes.

expand_types([],[]).
expand_types([V/T|R],[V/ET|ER]):-
	T = [Type],
	powerset(Type,ET),
	expand_types(R,ER).

oo_types_asub_to_native(ASub,_Qv,ASub_user):-
	ASub_user = [tau(ASub)].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%    BUILTINS    %%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Handles those builtins for which computing Proj is easier than Succ 
oo_types_call_to_success_builtin(none,_Sg,_Sv,_Call,_Proj,_Succ).



oo_types_special_builtin('java_bytecode:java_decl/2',
                           'java_bytecode:java_decl'(C,V),
	                   declaration,Condv):-
	Condv = (C,V).
oo_types_special_builtin('java_bytecode:java_null/2',
                  	'java_bytecode:java_null'(C,Res),
			null,Condv):-
        Condv = (C,Res).
oo_types_special_builtin('java_bytecode:java_new/2' ,
                  	'java_bytecode:java_new'(C,Res),
			new,Condv):-
        Condv = (C,Res).
oo_types_special_builtin('java_bytecode:java_get/2',
                  	'java_bytecode:java_get'(V,Res),
			get,Condv):-
        Condv = (V,Res).
oo_types_special_builtin('java_bytecode:java_getfield/3',
	                'java_bytecode:java_getfield'(V,Field,Res),
			 getfield,Condv):-
        Condv = (V,Field,Res).


oo_types_special_builtin('java_bytecode:java_skip/0',
                           'java_bytecode:java_skip',
	                   skip,nothing).


oo_types_special_builtin('java_bytecode:java_meth_decl/3',
                           'java_bytecode:java_meth_decl'(M,C,T),
                            method_declaration,Condv):-
	Condv = (M,C,T).

oo_types_special_builtin('java_bytecode:java_set/2',
                  	'java_bytecode:java_set'(V,Res),
			set,Condv):-
        Condv = (V,Res).
oo_types_special_builtin('java_bytecode:java_setfield/3',
	                'java_bytecode:java_setfield'(V,_,Res),
			setfield,Condv):-
        Condv = (V,Res).

oo_types_special_builtin('==/2',==(X,Y),eq,Condv):-
	Condv = (X,Y).	
oo_types_special_builtin('\\==/2','\\=='(X,Y),neq,Condv):-
	Condv = (X,Y).	

% Builtins not affecting the abstract state can be ignored
oo_types_special_builtin('</2',_,unchanged,_Condv).
oo_types_special_builtin('>=/2',_,unchanged,_Condv).


%%%%%%%%%%%%%%%%%%%%%%%%%%  Error mechanism  %%%%%%%%%%%%%%%%%%%%%%%%%%

% Invariant:unprocessed builtin can only be the result of an error
% any other builtin must be declared explicitely
oo_types_success_builtin(Builtin,Sv,Condv,Call,Succ):-
	oo_types_success_builtin_(Builtin,Sv,Condv,Call,Succ),
	!.
oo_types_success_builtin(Builtin,_Sv,Condv,Call,'$bottom'):-
	error_message("Builtin ~w failed {Condv = ~w, Call = ~w}",[Builtin,Condv,Call]),
        throw(domain_error(oo_types,builtin(Builtin))).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Expressions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% null
oo_types_success_builtin_(null,_Sv,Condv,Call,Succ):-
	Condv = (K,Res),
	powerset(K,Pow),
	change_values_insert([Res],Call,Succ,Pow).

% new k
oo_types_success_builtin_(new,_Sv,Condv,Call,Succ):-
	!,
	Condv = (K,Res),
	change_values_insert([Res],Call,Succ,[K]).

% v
oo_types_success_builtin_(get,_Sv,Condv,Call,Succ):-
	!,
	Condv = (V,Res),
	var_value(Call,V,K),
	change_values_insert([Res],Call,Succ,K).

% i
% MISSING, although is exactly as 'v' inserting [int] instead of K


% v.f
% Important: assume no overloading of fields
oo_types_success_builtin_(getfield,_Sv,Condv,Call,Succ):-
	!,
	Condv = (V,Field,Res),
	var_value(Call,V,C1),
	oo_types_field_type(C1,Field,K),
	change_values_insert([Res],Call,Succ,K).


%%%%%%%%%%%%%%%%%%%%%%%%  Commands    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% v:t; used in both dynamic and static cases 
oo_types_success_builtin_(declaration,_Sv,Condv,Call,Succ):-
	!,
	Condv = (Type,Res),
	powerset(Type,Pow),
	change_values_insert([Res],Call,Succ,Pow).

% v:= Res
oo_types_success_builtin_(set,_Sv,Condv,Call,Succ):-
	use_dynamic_types,
	!,
	Condv = (V,Res),
	var_value(Call,Res,ResK),
	var_value(Call,V,VK),
	% prevent v:=null from assign incorrect type information
	(ancestor_eq(ResK,VK) ->
	    Succ = Call	    
	    ;
	    change_values_insert([V],Call,Succ,ResK)  
	).   
oo_types_success_builtin_(set,_Sv,_Condv,Call,Succ):-
	Succ = Call.


% v.f:= Res
oo_types_success_builtin_(setfield,_Sv,_Condv,Call,Call):-
	!.


% throw Res
% TODO

% return Res
% implicit in set(Out,Res)

% if (v = w) then comm
% TODO

% if (v != w) then comm
% TODO

% if (v = null) then comm
% if (v = i)    then comm
oo_types_success_builtin_(eq,_Sv,_Condv,Tau_Call,Tau_Call):-
	!.

% if (v != null) then comm
% if (v != i)    then comm
oo_types_success_builtin_(neq,_Sv,_Condv,Tau_Call,Tau_Call):-
	!.
	      
% skip
oo_types_success_builtin_(skip,_Sv,_Condv,Call,Call):-
	!.


%%%%%%%%%%%%%%%%%%%%  Metacommands    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% in which class the method is defined
% useful to increase precision through type information
oo_types_success_builtin_(method_declaration,_Sv,Condv,Call,Succ):-
	!,
	Condv = (Meth_Name,K,This),
	% the method is declared in K and subclasses (if not redefined)
	descendants(K,Tmp1),
	filter_if_redefine(Tmp1,Meth_Name,Des),
	insert(Des,K,KDes),
	% 'This' is therefore a subset of its actual value  
	var_value(Call,This,ThisK),
	ord_intersection(KDes,ThisK,PossibleK),
	(PossibleK \== [] ->
	    change_values_insert([This],Call,Succ,PossibleK)
	    ;
	    Succ = '$bottom'
        ).

oo_types_success_builtin_(unchanged,_Sv_u,_Condv,Call,Succ):-
	Succ = Call.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%   Auxiliary Preds %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

use_dynamic_types:-
	current_pp_flag(oo_types_dyn_info,on).

% gather descendants of a class (or list of classes) using the info
powerset(Type,Pow):-
	type_and_descendants(Type,Pow).


% gather all variables with type 'any' (thus they are temporal)
find_any_vars(Tau,AnyVars):-
	member_value_freeness(Tau,AnyVars,[any]).


% for two types, return the least common ancestor
least_ancestor(L1,L2,LA):-
	ord_union(L1,L2,LA).


% for lists, check if L1 is ancestor (or equal) of L2
ancestor_eq([any],_):-
	!.
ancestor_eq(L1,L2):-
	ord_subset(L2,L1),
	!.
ancestor_eq(L1,L2):-
	ord_subset(L1,L2),
	!,
	fail.
ancestor_eq(L1,L2):-
	map_ancestor_eq(L2,L1).

map_ancestor_eq([],_).
map_ancestor_eq([K|RK],Anc):-
	member(AK,Anc),
	ancestor_or_same(AK,K),
	!,
	map_ancestor_eq(RK,Anc).



% filter out from list those classes redefining Meth_Name
filter_if_redefine([],_,[]).
filter_if_redefine([C|RC],Meth_Name,Res):-
	filter_if_redefine(RC,Meth_Name,RRes),
	(defined(C,Meth_Name) ->
	    Res = RRes   
	    ;
	    Res = [C|RRes]
	).



% atomic variables have associated typesets of just 1 element
% there is no possibility (semantics) for something like [int,a,b]
oo_types_atomic_type(TypeList):-
	TypeList = [AtomicType],
	atomic_type(AtomicType).
	
oo_types_non_atomic_type(TypeList):-
	oo_types_atomic_type(TypeList),
	!,
	fail.
oo_types_non_atomic_type(_).


% Assumption
% the type of a field is the same of all the TypeSet (assume no overloading)
% Otherwise we are returning a random type for the field
oo_types_field_type(ListK,FieldName,FieldType):-
	member(K,ListK),
	field_type(K,FieldName,FieldType),
	!.
field_type(K,FieldName,FieldType):-
	(field(K,FieldName,Type) ->
	        FieldType = [Type]
	        ;    
	        ancestor(AK,K),
		field_type(AK,FieldName,FieldType)
	).


oo_types_reachability([],[]).
oo_types_reachability([T|RT],Reach):-
	reachability(T,R1),
	oo_types_reachability(RT,R2),
	ord_union(R1,R2,Reach).	
	



%%%%%%%%%%%%%%% Abstract unification %%%%%%%%%%%%%%%%%%%%%%%


% AT THIS MOMENT PEEL + ABS UNIFY WORK FOR PARAMETERS ONLY,
% ALTHOUGH THE EFFECT SHOULD BE THE SAME AS FOR THE 'SET' OPERATION

% only unifications possible btw variable and parameter are
%         a) both are var
%         b) variable is a ct, parameter is a var (in Java is always this way)
oo_types_simplify_equations(Term1,Term2,Binds):-
	oo_types_free_peel(Term1,Term2,Temp,[]),
	sort(Temp,Binds).

oo_types_free_peel(Term1,Term2,Binds,Rest) :-
	var(Term1),
	var(Term2),
	!,
	Binds = [(Term1,Term2)|Rest].
oo_types_free_peel(_Term1,Term2,Binds,Rest) :-
	var(Term2), 
	!,
	Binds = Rest.
oo_types_free_peel(Term1,Term2,Binds,Rest) :-
	Term1 == Term2, !,
	Binds = Rest.
oo_types_free_peel(Term1,Term2,Binds,Rest) :- 
	functor(Term1,F,N),
	functor(Term2,F,N),
	oo_types_free_peel_args(0,N,Term1,Term2,Binds,Rest).
	
oo_types_free_peel_args(N,N,_,_,Binds,Rest) :- !,
	Binds = Rest.
oo_types_free_peel_args(N1,N,Term1,Term2,Binds,Rest) :-
	N2 is N1 + 1,
	arg(N2,Term1,A1),
	arg(N2,Term2,A2),
	oo_types_free_peel(A1,A2,Binds,Rest1),
	oo_types_free_peel_args(N2,N,Term1,Term2,Rest1,Rest).



% on unification of X/C1 and Param/C2, Param gets the type of 
% corresponding variable on the call. Later java_decl/2 
% instructions could refine the type
abs_gset_params(Proj,Binds,NewProj):-
	ab_set_params(Binds,Proj,NewProj).

ab_set_params([],Proj,Proj).
ab_set_params([(X,Y)|Binds],Proj,Proj1):-
	var_value(Proj,X,C1),
	change_values_insert([Y],Proj,NewProj,C1),
	ab_set_params(Binds,NewProj,Proj1).


% on backwards unification of X/C1 and Param/C2, X gets
% the new type, independently of the old value
abs_gupdate_params(Proj,Binds,NewProj):-
	ab_update_params(Binds,Proj,NewProj).

ab_update_params([],Proj,Proj).
ab_update_params([(X,Y)|Binds],Proj,Proj1):-
	var_value(Proj,Y,C2),
	change_values_insert([X],Proj,NewProj,C2),
	ab_update_params(Binds,NewProj,Proj1).







