:- module(polyhedron,_,[]).
:- use_module(library(sets)).
:- use_module(library(lists)).
:- use_module(library(write)).
:- use_module(library(sort)).
:- use_module(library(terms_vars), [varset/2]).

% uncomment this line (and comment the next one)
% if you don't have PPL installed in your computer

:- use_module(ppl_ciao_dummy).
%:- use_module('/usr/local/src/ppl-0.7/interfaces/Prolog/Ciao/ppl_ciao').


% STARTED Fri 2005/07/01 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO
%
% Vars => To list again
% Pre/postconditions are not tested, why?
% movee dim2var to a API-friendly version
% Process ALL builtins
% Handling of bottom is semantically wrong (no transformation!)
% Test  Glb / Less or Equal
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%-------------------------------------------------------------------------
%-------------------------------------------------------------------------
%                      ABSTRACT SORT
%-------------------------------------------------------------------------
%-------------------------------------------------------------------------
polyhedron_sort(ASub1,ASub1):-
	ppl_initialize,
	%polyhedron_sort_(ASub1,ASub2),
	ppl_finalize.

% the real sort only takes place in particular scenarios; it must be
% called explicitely
polyhedron_sort_(ASub,ASub_Sorted):-
	ASub = (Poly,Vars),
	sort_ll(Vars,Sorted_Vars),
	ppl_Polyhedron_get_minimized_constraints(Poly,Cons_Sys),
	dim2var(Cons_Sys,Vars,Renum_Cons_Sys),
	dim2var(Renum_Cons_Sys,Sorted_Vars,Cons_Sys2),
	ppl_Polyhedron_space_dimension(Poly,Dims),
	ppl_new_Polyhedron_from_space_dimension(nnc,Dims,universe,New_Poly),
	ppl_Polyhedron_add_constraints(New_Poly,Cons_Sys2),
	ASub_Sorted = (New_Poly,Sorted_Vars).

sort_ll(L,SL):-
	sort_ll_(L,[],SL).
sort_ll_([],LL,SLL):-
	sort(LL,SLL).
sort_ll_([H|T],LL,SLL):-
	sort(H,SH),
	append(LL,[SH],New_LL),!,
	sort_ll_(T,New_LL,SLL).

test_polyhedron_sort_:-
	ppl_initialize,
	Vars = [[_5],[_2,_4,_1],[_0],[_3]],
	ppl_new_Polyhedron_from_space_dimension(nnc,4,universe,Poly),
	ppl_Polyhedron_add_constraints(Poly,
                                     ['$VAR'(2)<5,'$VAR'(3)='$VAR'(1)+8]),
	ASub = (Poly,Vars),
	polyhedron_sort_(ASub,ASub_Sorted),
	print_absu(ASub_Sorted),
	ppl_finalize.

%-------------------------------------------------------------------------
%-------------------------------------------------------------------------
%                      ABSTRACT IDENTICAL
%-------------------------------------------------------------------------
%------------------------------------------------------------------------%
polyhedron_identical_abstract('$bottom','$bottom'):-!.
polyhedron_identical_abstract(ASub1,ASub2):-
	ppl_initialize,
	display('Comparing...'),nl,
	print_absu(ASub1),
	display('with...'),
	print_absu(ASub2),nl,nl,
	polyhedron_identical_abstract_(ASub1,ASub2),	
	ppl_finalize.

polyhedron_identical_abstract_(ASub1,_ASub2):-
	ASub1 = (Poly1,_Vars1),
	ppl_Polyhedron_is_empty(Poly1),!.
polyhedron_identical_abstract_(ASub1,ASub2):-
	ASub1 = (Poly1,Vars1),
	ASub2 = (Poly2,Vars2),
	Vars1 == Vars2,
	ppl_Polyhedron_equals_Polyhedron(Poly1,Poly2).


%-------------------------------------------------------------------------
%-------------------------------------------------------------------------
%                            WIDENINGs
% BHRZ03_widening(P1,P0) only works (check manual) if P1>=P0
%-------------------------------------------------------------------------
%------------------------------------------------------------------------%
% widening requires Dim0 = Dim1
polyhedron_widencall('$bottom',Poly1,Poly1):-!.
polyhedron_widencall(Poly0,'$bottom',Poly0):-!.
polyhedron_widencall(Prime0,Prime1,New_Prime):- 
	Prime0 = (Poly0,Vars),
	display('Poly0'),nl,
	print_absu(Prime0),
	Prime1 = (Poly1,Vars),
	display('Poly1'),nl,
	print_absu(Prime1),
	polyhedron_widencall_(Poly0,Poly1,Poly_Widen),	
        New_Prime = (Poly_Widen,Vars),
	display('Widen'),nl,
	print_absu(New_Prime),
	display('---------------------'),nl,nl.

% suppose Poly0 is more recent...
polyhedron_widencall_(Poly0,Poly1,Poly_Widen):- 
	ppl_Polyhedron_contains_Polyhedron(Poly1,Poly0),!,
	ppl_new_Polyhedron_from_Polyhedron(nnc,Poly1,nnc,New_Poly1),
	ppl_Polyhedron_BHRZ03_widening_assign(New_Poly1,Poly0),	
	Poly_Widen = New_Poly1.
polyhedron_widencall_(Poly0,Poly1,Poly_Widen):- 
	ppl_Polyhedron_contains_Polyhedron(Poly0,Poly1),!,
	ppl_new_Polyhedron_from_Polyhedron(nnc,Poly0,nnc,New_Poly0),
	ppl_Polyhedron_BHRZ03_widening_assign(New_Poly0,Poly1),	
	Poly_Widen = New_Poly0.
polyhedron_widencall_(Poly0,Poly1,Poly_Widen):- 
	ppl_new_Polyhedron_from_Polyhedron(nnc,Poly0,nnc,New_Poly0),
	ppl_Polyhedron_poly_hull_assign(New_Poly0,Poly1),
	ppl_Polyhedron_BHRZ03_widening_assign(New_Poly0,Poly1),	
	Poly_Widen = New_Poly0.

polyhedron_widen(Prime0,Prime1,New_Prime):- 
	polyhedron_widencall(Prime0,Prime1,New_Prime).

%-------------------------------------------------------------------------
%-------------------------------------------------------------------------
%                      ABSTRACT LESS OR EQUAL
%-------------------------------------------------------------------------
%------------------------------------------------------------------------%
polyhedron_less_or_equal(ASub1,ASub2):-
	ppl_initialize,
	ASub1 = (Poly1,Vars),
	ASub2 = (Poly2,Vars),
	ppl_Polyhedron_contains_Polyhedron(Poly1,Poly2),
	ppl_finalize.

%-------------------------------------------------------------------------
%-------------------------------------------------------------------------
%                      ABSTRACT LUB
%-------------------------------------------------------------------------
%------------------------------------------------------------------------%
polyhedron_compute_lub(ASubList,Lub):-
	ppl_initialize,
	polyhedron_compute_lub_(ASubList,Lub),
	ppl_finalize.

polyhedron_compute_lub_('$bottom','$bottom').
polyhedron_compute_lub_([ASub1],ASub1).
polyhedron_compute_lub_([E1,E2|Rest],Lub):-
	polyhedron_compute_lub_el(E1,E2,PartialLub),
	polyhedron_compute_lub_([PartialLub|Rest],Lub).

polyhedron_compute_lub_el(ASub1,'$bottom',ASub1):- !.
polyhedron_compute_lub_el('$bottom',ASub2,ASub2):- !.
polyhedron_compute_lub_el(ASub1,ASub2,Lub):-
	ASub1 = (_Poly1,Vars1),
	ASub2 = (_Poly2,Vars2),
	ord_intersection(Vars1,Vars2,Vars),
	list_to_list_of_lists(Tmp,Vars),
	polyhedron_project(ASub1,Tmp,New_ASub1),
	polyhedron_project(ASub2,Tmp,New_ASub2),
	New_ASub1 = (Poly1,Vars),
	New_ASub2 = (Poly2,Vars),
	ppl_Polyhedron_poly_hull_assign_and_minimize(Poly1,Poly2),!,
        Lub = (Poly1,Vars).
polyhedron_compute_lub_el(_ASub1,_ASub2,'$bottom').

test_polyhedron_compute_lub:-
  	ppl_initialize,
	ppl_new_Polyhedron_from_constraints(nnc,['$VAR'(0)>=3],Poly1),
	ppl_new_Polyhedron_from_constraints(nnc,['$VAR'(0)>=2],Poly2),
	ASub1 = (Poly1,[[_1]]),
	ASub2 = (Poly2,[[_1]]), 
	polyhedron_compute_lub([ASub1,ASub2],ASub_Lub),
	print_absu(ASub_Lub),
	ppl_finalize.


%-------------------------------------------------------------------------
%-------------------------------------------------------------------------
%                      ABSTRACT GLB
%-------------------------------------------------------------------------
%------------------------------------------------------------------------%
polyhedron_glb(ASub1,ASub2,Glb):-
	ppl_initialize,
	polyhedron_glb_(ASub1,ASub2,Glb),
	ppl_finalize.

polyhedron_glb_(('$bottom',_),_ASub2,'$bottom'):- !.	
polyhedron_glb_(_ASub1,('$bottom',_),'$bottom'):- !.	
polyhedron_glb_(ASub1,ASub2,Glb):-
	ASub1 = (_Poly1,Vars1),
	ASub2 = (_Poly2,Vars2),
	ord_intersection(Vars1,Vars2,Vars),
	list_to_list_of_lists(Tmp,Vars),
	polyhedron_project(ASub1,Tmp,New_ASub1),
	polyhedron_project(ASub2,Tmp,New_ASub2),
	New_ASub1 = (Poly1,Vars),
	New_ASub2 = (Poly2,Vars),
	ppl_Polyhedron_intersection_assign_and_minimize(Poly1,Poly2),!,
        Glb = (Poly1,Vars).

test_polyhedron_glb:-
	ppl_initialize,
	ppl_new_Polyhedron_from_constraints(nnc,['$VAR'(0)>=2],Poly1),
	ppl_new_Polyhedron_from_constraints(nnc,['$VAR'(0)=<5],Poly2),
	ASub1 = (Poly1,[[_1]]),
	ASub2 = (Poly2,[[_1]]), 
	polyhedron_glb(ASub1,ASub2,ASub_Glb),
	print_absu(ASub_Glb),
	ppl_finalize.

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                      ABSTRACT PROJECTION
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
polyhedron_project(ASub,Vars,Proj):-
	ppl_initialize,
	polyhedron_project_(ASub,Vars,Proj),
	ppl_finalize.

polyhedron_project_('$bottom',_,'$bottom'):- !.
polyhedron_project_(ASub,Vars,Proj):-
	ASub = (Poly,Poly_Vars),
	ppl_new_Polyhedron_from_Polyhedron(nnc,Poly,nnc,Poly_Proj),
	project_on_dimensions(Poly_Proj,0,Poly_Vars,Vars,New_Vars),
	Proj = (Poly_Proj,New_Vars).

project_on_dimensions(_Poly,_Dim,[],_Vars,[]):-!.
project_on_dimensions( Poly,Dim,[Varset|Rest_Varset],Vars,New_Varset):-
	ord_disjoint(Varset,Vars),!,
	ppl_Polyhedron_remove_space_dimensions(Poly,['$VAR'(Dim)]),
	project_on_dimensions(Poly,Dim,Rest_Varset,Vars,New_Varset).
project_on_dimensions( Poly,Dim,[Varset|Rest_Varset],Vars,New_Varset):-
	ord_intersection(Varset,Vars,Vars_Common),
	Dim1 is Dim + 1,
	New_Varset = [Vars_Common|Rest_New_Varset],
	project_on_dimensions(Poly,Dim1,Rest_Varset,Vars,Rest_New_Varset).

test_polyhedron_project:-
	ppl_initialize,
	X = '$VAR'(0),
	Y = '$VAR'(1),
	ppl_new_Polyhedron_from_constraints(nnc,[X+Y=<4,0=<Y,Y=<X-2],Poly1),
	ASub1 = (Poly1,[[_1,_2],[_4]]),
	polyhedron_add_dimensions(ASub1,[_3,_T],ASub2),
	polyhedron_project(ASub2,[_2],ASub_Proj),
	print_absu(ASub_Proj),
	ppl_finalize.

%-------------------------------------------------------------------------
%-------------------------------------------------------------------------
%                      ABSTRACT Extend
%-------------------------------------------------------------------------
%------------------------------------------------------------------------%
polyhedron_extend(Prime,Sv,Call,Success):-
	ppl_initialize,
	polyhedron_extend_(Prime,Sv,Call,Success),
	ppl_finalize.

polyhedron_extend_('$bottom',_Sv,_Call,'$bottom').
polyhedron_extend_(Prime,Sv,Call,Success):- 
	polyhedron_merge(Call,Prime,Sv,Success).


test_polyhedron_extend:-
	% call
	ppl_new_Polyhedron_from_space_dimension(nnc,6,universe,Poly1),
	ppl_Polyhedron_add_constraints(Poly1,
	['$VAR'(5)=<3*'$VAR'(1),'$VAR'(5)=<4*'$VAR'(2),'$VAR'(5)>2*'$VAR'(4)]),
	Vars_Call = [[_1,_2],[_3],[_4],[_5,_6,_7,_8],[_9],[_10]],
        ASub_Call = (Poly1,Vars_Call),
	% prime
	ppl_new_Polyhedron_from_space_dimension(nnc,2,universe,Poly2),
	Vars_Prime = [[_3,_4,_9],[_5,_6,_7,_8]],
        ASub_Prime = (Poly2,Vars_Prime),

	Sv = [_1,_2,_3,_4,_5,_6,_7,_8,_9],
	polyhedron_extend(ASub_Prime,Sv,ASub_Call,ASub_Success),
	print_absu(ASub_Success).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                    ABSTRACT Call To Entry
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
polyhedron_call_to_entry(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	ppl_initialize,
	polyhedron_call_to_entry_(Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo),
	ppl_finalize.

polyhedron_call_to_entry_(_Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo):-
	Proj=(Poly1,Vars),
	ppl_new_Polyhedron_from_Polyhedron(nnc,Poly1,nnc,Poly2),
	Temp1=(Poly2,Vars),
	ord_union(Hv,Fv,HvFv),
	polyhedron_add_dimensions(Temp1,HvFv,Temp2),
	polyhedron_simplify_equations(Sg,Head,Binds),
	abs_gunify(Temp2,Binds,Upd_Proj,_NewBinds),
	polyhedron_project(Upd_Proj,HvFv,Entry),
	ExtraInfo = (Upd_Proj,HvFv).

%-------------------------------------------------------------------------
%-------------------------------------------------------------------------
%                      ABSTRACT Exit To Prime
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
polyhedron_exit_to_prime(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime):-
	ppl_initialize,
	polyhedron_exit_to_prime_(Sg,Hv,Head,Sv,Exit,ExtraInfo,Prime),
	ppl_finalize.

polyhedron_exit_to_prime_(_Sg,_Hv,_Head,_Sv,'$bottom',_ExtraInfo,'$bottom'):-
	!.
polyhedron_exit_to_prime_(_Sg,_Hv,_Head,Sv,Exit,ExtraInfo,Prime):-
	ExtraInfo = (Upd_Proj,HvFv),
	polyhedron_merge(Upd_Proj,Exit,HvFv,Tmp),
	polyhedron_project(Tmp,Sv,Prime).

test_polyhedron_exit_to_prime:-
	ppl_initialize,
	% proj
	ppl_new_Polyhedron_from_space_dimension(nnc,5,universe,Poly1),
	ppl_Polyhedron_add_constraints(Poly1,['$VAR'(2)>'$VAR'(0)]),
	Upd_Proj = (Poly1,[[_Sv1],[_Sv2,_Hv1],[_Sv3,_Sv4,_Hv2],[_Sv5],[_Fv1]]),
	%exit
	ppl_new_Polyhedron_from_space_dimension(nnc,1,universe,Poly2),
	ppl_Polyhedron_add_constraints(Poly2,
	                              ['$VAR'(0)=7]),
	Exit = (Poly2,[[_Hv1,_Hv2]]),
	Hv =   [_Hv1,_Hv2],
	Sv=    [_Sv1,_Sv2,_Sv3,_Sv4,_Sv5],
	ord_union(Hv,[_Fv1],HvFv),
	ExtraInfo = (Upd_Proj,HvFv),
	polyhedron_exit_to_prime(_Sg,Hv,_Head,Sv,Exit,ExtraInfo,Prime),
	print_absu(Prime),
	ppl_finalize.

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                   ABSTRACT Call to Success Fact                        %
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% Specialized version of call_to_entry + exit_to_prime + extend for facts%
%-------------------------------------------------------------------------
polyhedron_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	ppl_initialize,
	polyhedron_call_to_success_fact_(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ),
	ppl_finalize.

polyhedron_call_to_success_fact_(Sg,Hv,Head,Sv,Call,Proj,Prime,Succ):-
	Proj = (Poly1,Vars),
	ppl_new_Polyhedron_from_Polyhedron(nnc,Poly1,nnc,Poly2),
	New_Proj = (Poly2,Vars),
	polyhedron_add_dimensions(New_Proj,Hv,Temp1),
	polyhedron_simplify_equations(Sg,Head,Binds),
	abs_gunify(Temp1,Binds,Entry,_NewBinds),
	polyhedron_project(Entry,Sv,Prime),
	polyhedron_extend(Prime,Sv,Call,Succ).

test_polyhedron_call_to_success_fact:-
	ppl_initialize,
	Sv1 = '$VAR'(0),
	ppl_new_Polyhedron_from_space_dimension(nnc,3,universe,Poly1),
	ppl_Polyhedron_add_constraint(Poly1,Sv1 >= 0),
	ASub1 = (Poly1,[[_Sv1],[_Sv2],[_Sv3]]),
	Sg = p(_Sv1,7),
	Sv = [_Sv1],
	Hv = [_Hv1],
	Head = p(4,_Hv1),
	Call=ASub1,
	polyhedron_project(Call,Sv,Proj),

	polyhedron_call_to_entry(_Sv,Sg,Hv,Head,[_Fv1],Proj,Entry,EI),
	Entry = Exit,
	polyhedron_exit_to_prime(_Sg,_Hv,_Head,Sv,Exit,EI,Prime),
	polyhedron_extend(Prime,Sv,Call,Succ1),
	polyhedron_call_to_success_fact(Sg,Hv,Head,Sv,Call,Proj,_Prime,Succ2),
	polyhedron_identical_abstract(Succ1,Succ2),
	ppl_finalize.


%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                Unknow & Empty Entry,Unknow Call                        %
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
polyhedron_unknown_entry(Vars,Entry):- 
	ppl_initialize,
	polyhedron_get_empty_Asub(Empty),
	polyhedron_add_dimensions(Empty,Vars,Entry),
	ppl_finalize.

polyhedron_empty_entry(Vars,Entry):- 
	ppl_initialize,
	polyhedron_unknown_entry(Vars,Entry),
	ppl_finalize.

% The unknown call might only impose more constraints on Vars, so Call is
% a valid approximation for Succ, even for those dimensions that could be
% instantiated as non-numeric values in the unknown call
polyhedron_unknown_call(Call,_Vars,Succ):-
	ppl_initialize,
	Succ = Call,
	ppl_finalize.


%------------------------------------------------------------------------%
%                         HANDLING BUILTINS                              %
%------------------------------------------------------------------------%

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                          Special Builtin
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
polyhedron_special_builtin(SgKey,Sg,Type,Condvars):-
	ppl_initialize,
	polyhedron_special_builtin_(SgKey,Sg,Type,Condvars),
	ppl_finalize.

polyhedron_special_builtin_('=/2',=(X,Y),unification,Condvars):-
	Condvars = (X,Y).	
polyhedron_special_builtin_('is/2',is(X,Y),constraint,Condvars):-
	Condvars = '='(X,Y).	
polyhedron_special_builtin_('=</2',Sg,constraint,Condvars):-
	Condvars = Sg.	
polyhedron_special_builtin_('>=/2',Sg,constraint,Condvars):-
	Condvars = Sg.	
polyhedron_special_builtin_('</2',Sg,constraint,Condvars):-
	Condvars = Sg.	
polyhedron_special_builtin_('>/2',Sg,constraint,Condvars):-
	Condvars = Sg.	
polyhedron_special_builtin_('true/0',_,unchanged,_).
polyhedron_special_builtin_('read:read/1',_,unchanged,_).

%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
%                          Success Builtin
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
	
% We only pay attention to the subset [=,<,>,>=,=<] if they relate linear
% equations.
polyhedron_success_builtin(Type,Sv,Condv,Call,Succ):-
	ppl_initialize,
	polyhedron_success_builtin_(Type,Sv,Condv,Call,Succ),
	ppl_finalize.

polyhedron_success_builtin_(unchanged,_,_,Call,Succ):-
	Call = Succ.
polyhedron_success_builtin_(unification,_Sv,Condv,Call,Succ):-
	Condv = (Term1,Term2),
	polyhedron_simplify_equations(Term1,Term2,Binds),
	abs_gunify(Call,Binds,Succ,_NewBinds).
polyhedron_success_builtin_(constraint,_Sv,Condv,Call,Succ):-
	Call = (Poly1,Vars),
	dim2var_constraint(Condv,Vars,Condv_As_PPL_Cons),!,
	ppl_new_Polyhedron_from_Polyhedron(nnc,Poly1,nnc,Poly2),
	ppl_Polyhedron_add_constraint(Poly2,Condv_As_PPL_Cons),
	Succ = (Poly2,Vars).
polyhedron_success_builtin_(constraint,_Sv,_Condv,Call,Succ):-
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%%%%%%%% Remove all variables implied
	Call = Succ.

%------------------------------------------------------------------------%
%                       Call to Success Builtin
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
polyhedron_call_to_success_builtin(SgKey,Sh,Sv,Call,Proj,Succ):-
	ppl_initialize,
	polyhedron_call_to_success_builtin_(SgKey,Sh,Sv,Call,Proj,Succ),
	ppl_finalize.

polyhedron_call_to_success_builtin_(_SgKey,_Sh,_Sv,Call,_Proj,Succ):-
	Call = Succ.

%-------------------------------------------------------------------------
%-------------------------------------------------------------------------
%            Assertion(I/O) related Functions                            %
%-------------------------------------------------------------------------
%-------------------------------------------------------------------------

polyhedron_input_user_interface(Cons_Sys,Qv,New_ASub):-
	ppl_initialize,
	length(Qv,Dims),
	ppl_new_Polyhedron_from_space_dimension(nnc,Dims,universe,Poly),
	list_to_list_of_lists(Qv,Vars),
	ASub = (Poly,Vars),
	input2builtin(Cons_Sys,ASub,New_ASub),
	ppl_finalize.

input2builtin([],ASub,ASub).
input2builtin([=(A,B)|Rest_Cons],ASub,New_ASub):- !,
	polyhedron_success_builtin(unification,_,(A,B),ASub,ASub1),
	input2builtin(Rest_Cons,ASub1,New_ASub).
% it can only be < ,>, >=, =< as filtered by constraint/1
input2builtin([In_Equality|Rest_Cons],ASub,New_ASub):-
	polyhedron_success_builtin(constraint,_,In_Equality,ASub,ASub1),
	input2builtin(Rest_Cons,ASub1,New_ASub).
	

polyhedron_input_interface(InputUser,_Kind,Old_Cons_Sys,New_Cons_sys):-
	ppl_initialize,
	polyhedron_input_interface_(InputUser,_Kind,Old_Cons_Sys,New_Cons_sys),
	ppl_finalize.

polyhedron_input_interface_(constraint(Cons_Sys),perfect,
	                    Old_Cons_Sys,New_Cons_Sys):-
	append(Old_Cons_Sys,Cons_Sys,New_Cons_Sys). 

polyhedron_asub_to_native('$bottom',_Qv,[constraint([0=1])]).
polyhedron_asub_to_native(ASub1,Qv,[Output]):-
	ppl_initialize,
	ASub1 = (Poly,Vars),
	ppl_Polyhedron_get_minimized_constraints(Poly,Cons_Sys),
	dim2var(Cons_Sys,Vars,Named_Cons_Sys),
	list_to_list_of_lists(Tmp,Vars),
	ord_subtract(Qv,Tmp,Vars_NotInt),
	Output = (constraint(Named_Cons_Sys),vars(Vars),not_int(Vars_NotInt)),
	ppl_finalize.


%-------------------------------------------------------------------------
%-------------------------------------------------------------------------
%            Intermediate Functions                                      %
%-------------------------------------------------------------------------
%-------------------------------------------------------------------------

:- push_prolog_flag(multi_arity_warnings,off).

print_absu('$bottom') :- 
	display('No solution'),!.
print_absu((Poly,Vars)) :-
	ppl_Polyhedron_get_minimized_constraints(Poly,Poly_Cons),!,	
	ppl_Polyhedron_space_dimension(Poly,Dims),
	display('Dims: '),write(Dims),nl,
	display('Cons: '),write(Poly_Cons),nl,
	display('Vars: '),display(Vars),nl.
print_absu(Poly):-
	print_absu((Poly,whatever_vars)).

:- pop_prolog_flag(multi_arity_warnings).


% Dimension Dim  associated with Var
get_dimension(ASub,Var,Dim):-
	ASub = (_Poly,Vars),!,
	my_nth(Vars,Var,Dim,not_instantiate).
get_dimension(Vars,Var,Dim):-
	list(Vars),
	var(Dim),!,
	my_nth(Vars,Var,Dim,not_instantiate).
get_dimension(Vars,Var,Dim):-
	ground(Dim),
	my_nth(Vars,Var,Dim,instantiate).

test_get_dimension:-
	Vars = [[_A],[_C],[_E]],
	get_dimension(Vars,Var,1),
	Var = _C,
	get_dimension(Vars,_E,2).

my_nth(Vars,Var,Dim,Flag):-
	my_nth_(0,Vars,Var,Dim,Flag).
my_nth_(N,[Vars|_Rest],Pattern,N,not_instantiate):-
	ord_member(Pattern,Vars),!.
my_nth_(N,[Vars|_Rest],Pattern,N,instantiate):-
	member(Pattern,Vars),!.
my_nth_(N,[_Var|Rest],Pattern,Dim,Flag):-
	N1 is N+1,
	my_nth_(N1,Rest,Pattern,Dim,Flag).

polyhedron_get_empty_Asub(ASub_Empty):-
	ppl_new_Polyhedron_from_space_dimension(nnc,0,universe,Poly),
        ASub_Empty = (Poly,[]). 
 

:- push_prolog_flag(multi_arity_warnings,off).

polyhedron_add_dimension((Poly,Vars),Added_Vars,(Poly,New_Vars)):-
	ppl_Polyhedron_add_space_dimensions_and_embed(Poly,1),
	append(Vars,[Added_Vars],New_Vars).
polyhedron_add_dimensions((Poly,Vars),New_Dims,(Poly,New_Vars)):-
	length(New_Dims,No_New_Dims),
	ppl_Polyhedron_add_space_dimensions_and_embed(Poly,No_New_Dims),
	list_to_list_of_lists(New_Dims,New_Dims_LL),
	append(Vars,New_Dims_LL,New_Vars).

:- pop_prolog_flag(multi_arity_warnings).


% % % Alert: the dimensions to remove MUST be sorted
% polyhedron_remove_dimensions(ASub,Vars,New_ASub):-
% 	ASub = (Poly,Varset),
% 	remove_dimensions(Poly,0,Varset,Vars,New_Varset),
% 	New_ASub = (Poly,New_Varset).

% remove_dimensions(_Poly,_Dim,[],_Vars,[]):-!.
% remove_dimensions( Poly,Dim,[Varset|Rest_Varset],Vars,New_Varset):-
% 	ord_intersect(Varset,Vars),!,
% 	ppl_Polyhedron_remove_space_dimensions(Poly,['$VAR'(Dim)]),
% 	remove_dimensions(Poly,Dim,Rest_Varset,Vars,New_Varset).
% remove_dimensions( Poly,Dim,[Varset|Rest_Varset],Vars,New_Varset):-
% 	Dim1 is Dim + 1,
% 	New_Varset = [Varset|Rest_New_Varset],
% 	remove_dimensions(Poly,Dim1,Rest_Varset,Vars,Rest_New_Varset).

% test_polyhedron_remove_dimensions:-
% 	ppl_initialize,
% 	ppl_new_Polyhedron_from_space_dimension(nnc,5,universe,Poly),
% 	ppl_Polyhedron_add_constraints(Poly,['$VAR'(2)>'$VAR'(0)]),
% 	ASub = (Poly,[[_Sv1],[_Sv2,_Hv1],[_Sv3,_Sv4,_Hv2],[_Sv5],[_Fv1]]),
% 	polyhedron_remove_dimensions(ASub,[_Hv1,_Sv5,_Not_Exits],New_ASub),
% 	print_absu(New_ASub),
% 	ppl_finalize.

% polyhedron_fold(ASub,Eq_Vars,Var,Folded_ASub):-
% 	ASub = (Poly,Vars),
% 	polyhedron_fold_var(Vars,Eq_Vars,Var,Folded_Vars),
% 	ppl_Polyhedron_get_minimized_constraints(Poly,Cons_Sys),
% 	dim2var(Cons_Sys,Vars,Renum_Cons_Sys),
% 	dim2var(Renum_Cons_Sys,Folded_Vars,Cons_Sys2),
% 	ppl_Polyhedron_space_dimension(Poly,Dims),
% 	Dims1 is Dims - 1,  
% 	ppl_new_Polyhedron_from_space_dimension(nnc,Dims1,universe,Poly2),
% 	ppl_Polyhedron_add_constraints(Poly2,Cons_Sys2),
% 	Folded_ASub = (Poly2,Folded_Vars).

% polyhedron_fold_var(Vars,Var1,Var2,Folded_Vars):-
% 	Var1 = '$VAR'(Dim1),
% 	Var2 = '$VAR'(Dim2),
% 	Dim1 =< Dim2,
% 	fold_var(Vars,Dim1,Dim2,Folded_Vars).
% polyhedron_fold_var(Vars,Var1,Var2,Folded_Vars):-
% 	Var1 = '$VAR'(Dim1),
% 	Var2 = '$VAR'(Dim2),
% 	Dim1 > Dim2,
% 	fold_var(Vars,Dim2,Dim1,Folded_Vars).

% fold_var(Vars,Dim1,Dim2,Folded_Vars):-
% 	fold_var_(Vars,_X,0,Dim1,Dim2,Folded_Vars).

% fold_var_([H|T],Vars_Max,N,_Min,N,Folded_Vars):-
% 	Vars_Max = H,
% 	Folded_Vars = T.
% fold_var_([H|T],Vars_Max,N,N,Max,Folded_Vars):-
% 	N1 is N + 1,
% 	fold_var_(T,Vars_Max,N1,N,Max,Rest_Folded_Vars),
% 	ord_union(H,Vars_Max,New_H),
% 	Folded_Vars = [New_H|Rest_Folded_Vars].
% fold_var_([H|T],Vars_Max,N,Min,Max,Folded_Vars):-
% 	Folded_Vars = [H|Rest_Folded_Vars],
% 	N1 is N + 1,
% 	fold_var_(T,Vars_Max,N1,Min,Max,Rest_Folded_Vars).
	

% This is complicated; comment!
polyhedron_merge(Old_ASub,New_ASub,Init_Vars_New,Merge):-
	Old_ASub= (Poly_Old,Vars_Old),
	ppl_new_Polyhedron_from_Polyhedron(nnc,Poly_Old,nnc,Poly_Old2),
	Old_ASub2 = (Poly_Old2,Vars_Old),	
        New_ASub=(Poly,Vars),
	ppl_new_Polyhedron_from_Polyhedron(nnc,Poly,nnc,Poly2),
	New_ASub2 = (Poly2,Vars),
	polyhedron_merge_vars(Vars_Old,Init_Vars_New,New_ASub2,Merge),
	polyhedron_merge_poly(Old_ASub2,Merge).

% merge according to ASub2 the (renumbered) constraints of both 
% ASub2 contains more recent information about the variables so its used
% as reference
polyhedron_merge_poly(ASub1,ASub2):-
	ASub2=( Poly2,Vars2),
	list_to_list_of_lists(Tmp,Vars2),
	polyhedron_project(ASub1,Tmp,New_ASub1),
	New_ASub1=(Poly1,Vars2),
	ppl_Polyhedron_get_minimized_constraints(Poly1,Cons_Sys1),	
	ppl_Polyhedron_add_constraints(Poly2,Cons_Sys1).	

	
% mix the set of variables of both substitutions	
polyhedron_merge_vars([],_HvFv,Exit,Sorted_Exit):-
	polyhedron_sort_(Exit,Sorted_Exit).
polyhedron_merge_vars([Vars|Rest_Vars],HvFv,Exit,Sorted_Exit):-
	ord_disjoint(Vars,HvFv),!,
	polyhedron_add_dimension(Exit,Vars,New_Exit),
	polyhedron_merge_vars(Rest_Vars,HvFv,New_Exit,Sorted_Exit).
polyhedron_merge_vars([Vars|Rest_Vars],HvFv,Exit,Sorted_Exit):-
	Exit=(Poly_Exit,Vars_Exit),
	match_vars(Vars,Vars_Exit,New_Vars_Exit),
	New_Exit=(Poly_Exit,New_Vars_Exit),
	polyhedron_merge_vars(Rest_Vars,HvFv,New_Exit,Sorted_Exit).

match_vars(_Sinonym,[],[]).
match_vars(Sinonym,[Vars|Rest_Vars],New_Varset):-
	ord_intersect(Sinonym,Vars),!,
	ord_union(Sinonym,Vars,New_Vars),
	New_Varset = [New_Vars|Rest_Vars].
match_vars(Sinonym,[Vars|Rest_Vars],New_Varset):-
	New_Varset = [Vars|Rest_New_Varset],
	match_vars(Sinonym,Rest_Vars,Rest_New_Varset).


%-------------------------------------------------------------------------
%-------------------------------------------------------------------------
%                     Standard Peel Equations
%-------------------------------------------------------------------------
%-------------------------------------------------------------------------

polyhedron_simplify_equations(Term1,Term2,Binds):-
	polyhedron_free_peel(Term1,Term2,Binds,[]).

polyhedron_free_peel(Term1,Term2,Binds,Rest) :-
	var(Term1), !,
	varset(Term2,List),
	Binds = [(Term1,Term2,List)|Rest].
polyhedron_free_peel(Term1,Term2,Binds,Rest) :-
	var(Term2), !,
	varset(Term1,List),
	Binds = [(Term2,Term1,List)|Rest].
polyhedron_free_peel(Term1,Term2,Binds,Rest) :-
	Term1 == Term2, !,
	Binds = Rest.
polyhedron_free_peel(Term1,Term2,Binds,Rest) :- 
	functor(Term1,F,N),
	functor(Term2,F,N),
	polyhedron_free_peel_args(0,N,Term1,Term2,Binds,Rest).
	
polyhedron_free_peel_args(N,N,_,_,Binds,Rest) :- !,
	Binds = Rest.
polyhedron_free_peel_args(N1,N,Term1,Term2,Binds,Rest) :-
	N2 is N1 + 1,
	arg(N2,Term1,A1),
	arg(N2,Term2,A2),
	polyhedron_free_peel(A1,A2,Binds,Rest1),
	polyhedron_free_peel_args(N2,N,Term1,Term2,Rest1,Rest).

%-------------------------------------------------------------------------
%-------------------------------------------------------------------------
%                     ABSTRACT UNIFICATION
%-------------------------------------------------------------------------
%-------------------------------------------------------------------------

abs_gunify(Proj,Binds,NewProj,NewBinds):-
	ab_unify(Binds,Proj,Proj1,Binds1),
	fixpoint_gunify(Proj,Binds,Proj1,Binds1,NewProj,NewBinds).

fixpoint_gunify(Proj,Binds,Proj1,Binds1,NewProj,NewBinds):-
	Proj == Proj1,
	Binds == Binds1,!,
	NewProj = Proj1,
	NewBinds = Binds1.
fixpoint_gunify(_,_,Proj1,Binds1,NewProj,NewBinds):-
	abs_gunify(Proj1,Binds1,NewProj,NewBinds).

ab_unify([],Proj,Proj,[]).
ab_unify([(X,Y,_Tv)|Binds],Proj,New_Proj,NewBinds):-
	var(X),
	var(Y),!,
	ab_unify_variables(X,Y,Proj,Proj1),
 	ab_unify(Binds,Proj1,New_Proj,NewBinds).
ab_unify([(X,Term,[])|Binds],Proj,New_Proj,NewBinds):-
	Proj = (Poly,_Vars),
	var(X),
	ground(Term),
	number(Term),!,
	dim2var_var(X,Proj,Named_X),
	ppl_Polyhedron_add_constraint(Poly,Named_X = Term),	
 	ab_unify(Binds,Proj,New_Proj,NewBinds).
ab_unify([(X,Term,[])|Binds],Proj,New_Proj,NewBinds):-
	var(X),
	ground(Term),!,
	polyhedron_remove_nonint_dims(Proj,X,Proj1),
 	ab_unify(Binds,Proj1,New_Proj,NewBinds).
ab_unify([_Bind|Binds],Proj,Proj,NewBinds):-
 	ab_unify(Binds,Proj,Proj,NewBinds).

ab_unify_variables(X,Y,Proj,Folded_Proj):-
	dim2var_var(Y,Proj,Named_Y),
	dim2var_var(X,Proj,Named_X),!,
	Proj = (Poly,_Vars),
	ppl_Polyhedron_add_constraint(Poly,Named_X = Named_Y),
        Folded_Proj = Proj. 
ab_unify_variables(X,_Y,Proj,New_Proj):-
	polyhedron_remove_nonint_dims(Proj,X,New_Proj),!.
ab_unify_variables(_X,Y,Proj,New_Proj):-
	polyhedron_remove_nonint_dims(Proj,Y,New_Proj).

polyhedron_remove_nonint_dims(ASub,Invalid_Dim,New_ASub):-
	ASub = (_Poly,Vars),
	polyhedron_find_nonint_dims(ASub,Invalid_Dim,Invalid_Dims),
	dim2var_list_var(Invalid_Dims,Vars,Named_Invalid_Dims),
	sort(Named_Invalid_Dims,Sorted_Invalid_Dims),
	list_to_list_of_lists(Tmp,Vars),
	ord_subtract(Tmp,Sorted_Invalid_Dims,Valid_Dims),
	polyhedron_project(ASub,Valid_Dims,New_ASub).

polyhedron_find_nonint_dims(ASub,Start,Invalid_Dims):-
	ASub = (Poly,Vars),
	dim2var_var(Start,Vars,Num_Start),
	list_to_list_of_lists(Flatten_Vars,Vars),
	dim2var_list_var(Flatten_Vars,Vars,Num_Vars),
	find_nonint_dims([Num_Start],Poly,Num_Vars,Num_Vars,Invalid_Dims).

find_nonint_dims([],_Poly,_Vars,_Vars,[]).
find_nonint_dims([Dim|Rest_Dim],Poly,[],Vars,[Dim|Rest_Result]):-
	find_nonint_dims(Rest_Dim,Poly,Vars,Vars,Rest_Result).
find_nonint_dims([Dim|Rest_Dim],Poly,[Var|Rest_Vars],Vars,Result):-
	Dim \== Var,
	ppl_Polyhedron_relation_with_constraint(Poly,=(Dim,Var),Relation),
	member(is_included,Relation),
	append([Dim|Rest_Dim],[Var],Dims_Not_Int),
	ord_subtract(Vars,[Var],New_Vars),
	find_nonint_dims(Dims_Not_Int,Poly,Rest_Vars,New_Vars,Result).
find_nonint_dims([Dim|Rest_Dim],Poly,[_|Rest_Vars],Vars,Result):-
	find_nonint_dims([Dim|Rest_Dim],Poly,Rest_Vars,Vars,Result).

test_polyhedron_remove_nonint_dims:-
	ppl_initialize,
	ppl_new_Polyhedron_from_space_dimension(nnc,6,universe,Poly1),
	ppl_Polyhedron_add_constraints(Poly1,
              ['$VAR'(3)='$VAR'(1),'$VAR'(2)='$VAR'(1),'$VAR'(5)=<'$VAR'(0)] ),
	ASub = (Poly1,[[_1],[_2],[_3],[_4],[_5],[_6]]),	
	polyhedron_remove_nonint_dims(ASub,_3,New_ASub),	
	print_absu(New_ASub),
	ppl_finalize.

test_ab_unify:-
	ppl_initialize,
	ppl_new_Polyhedron_from_space_dimension(nnc,6,universe,Poly1),
	ppl_Polyhedron_add_constraints(Poly1,
            ['$VAR'(3) =< 3 *'$VAR'(1),'$VAR'(2) ='$VAR'(3)+'$VAR'(4)]),
	ASub1 = (Poly1,[[_1,_4],[_3,_5],[_7],[_8],[_9,_11],[_Hv1]]),
	polyhedron_simplify_equations((_1,_3,_7,_9),(a,5,_Hv1,_Hv1),Binds),
	abs_gunify(ASub1,Binds,ASub2,_New_Binds),
	print_absu(ASub2),
	ppl_finalize.

%-------------------------------------------------------------------------
%-------------------------------------------------------------------------
%                     CONSTRAINT MANIPULATION/ TYPE CHECKER
%-------------------------------------------------------------------------
%-------------------------------------------------------------------------

dim2var_var(Var,Vars_Or_ASub,Renamed_Var):-
	var(Var),!,
	get_dimension(Vars_Or_ASub,Var,Dim_Var),
	Renamed_Var = '$VAR'(Dim_Var).
dim2var_var(Var,Vars,Name_Var):-
	ground(Var),
	Var='$VAR'(Dim),!,
	get_dimension(Vars,Name_Var,Dim).
dim2var_list_var([],_Vars,[]).
dim2var_list_var([Var|Rest_Var],Vars,[Ren_Var|Rest_Ren_Var]):-
	dim2var_var(Var,Vars,Ren_Var),
	dim2var_list_var(Rest_Var,Vars,Rest_Ren_Var).
	
dim2var_coefficient(Coeff):-
	ground(Coeff),
	int(Coeff).

:- push_prolog_flag(multi_arity_warnings,off).
:- push_prolog_flag(discontiguous_warnings,off).

dim2var_lin_expr(PPL_Var,Vars,PPL_Dim):-dim2var_var(PPL_Var,Vars,PPL_Dim),!.
dim2var_lin_expr(Coeff,_Vars,Coeff):-dim2var_coefficient(Coeff).
dim2var_lin_expr(+(Lin_Expr),Vars,+(New_Lin_Expr)):-
	dim2var_lin_expr(Lin_Expr,Vars,New_Lin_Expr).
dim2var_lin_expr(-(Lin_Expr),Vars,-(New_Lin_Expr)):-
	dim2var_lin_expr(Lin_Expr,Vars,New_Lin_Expr).
dim2var_lin_expr(+(Lin_Expr1,Lin_Expr2),Vars,+(New_Lin_Expr1,New_Lin_Expr2)):-
	dim2var_lin_expr(Lin_Expr1,Vars,New_Lin_Expr1),
	dim2var_lin_expr(Lin_Expr2,Vars,New_Lin_Expr2).
dim2var_lin_expr(-(Lin_Expr1,Lin_Expr2),Vars,-(New_Lin_Expr1,New_Lin_Expr2)):-
	dim2var_lin_expr(Lin_Expr1,Vars,New_Lin_Expr1),
	dim2var_lin_expr(Lin_Expr2,Vars,New_Lin_Expr2).
dim2var_lin_expr(*(Coeff,Lin_Expr),Vars,*(Coeff,New_Lin_Expr)):-
	dim2var_coefficient(Coeff),
	dim2var_lin_expr(Lin_Expr,Vars,New_Lin_Expr).
dim2var_lin_expr(*(Lin_Expr,Coeff),Vars,*(Coeff,New_Lin_Expr)):-
	dim2var_coefficient(Coeff),
	dim2var_lin_expr(Lin_Expr,Vars,New_Lin_Expr).

:- pop_prolog_flag(discontiguous_warnings).
:- pop_prolog_flag(multi_arity_warnings).

dim2var_constraint(=(Lin_Expr1,Lin_Expr2),Vars,=(Lin_Expr21,Lin_Expr22)):-
	dim2var_lin_expr(Lin_Expr1,Vars,Lin_Expr21),
	dim2var_lin_expr(Lin_Expr2,Vars,Lin_Expr22).
dim2var_constraint(=<(Lin_Expr1,Lin_Expr2),Vars,=<(Lin_Expr21,Lin_Expr22)):-
	dim2var_lin_expr(Lin_Expr1,Vars,Lin_Expr21),
	dim2var_lin_expr(Lin_Expr2,Vars,Lin_Expr22).
dim2var_constraint(>=(Lin_Expr1,Lin_Expr2),Vars,>=(Lin_Expr21,Lin_Expr22)):-
	dim2var_lin_expr(Lin_Expr1,Vars,Lin_Expr21),
	dim2var_lin_expr(Lin_Expr2,Vars,Lin_Expr22).
dim2var_constraint(<(Lin_Expr1,Lin_Expr2),Vars,<(Lin_Expr21,Lin_Expr22)):-
	dim2var_lin_expr(Lin_Expr1,Vars,Lin_Expr21),
	dim2var_lin_expr(Lin_Expr2,Vars,Lin_Expr22).
dim2var_constraint(>(Lin_Expr1,Lin_Expr2),Vars,>(Lin_Expr21,Lin_Expr22)):-
	dim2var_lin_expr(Lin_Expr1,Vars,Lin_Expr21),
	dim2var_lin_expr(Lin_Expr2,Vars,Lin_Expr22).

dim2var_constraint_system([],_Vars,[]).
dim2var_constraint_system([Cons|Rest],Vars,[New_Cons|Rest_New_Cons]):-
	dim2var_constraint(Cons,Vars,New_Cons),!,
	dim2var_constraint_system(Rest,Vars,Rest_New_Cons ).
dim2var_constraint_system([_Cons|Rest],Vars,Rest_New_Cons):-
	dim2var_constraint_system(Rest,Vars,Rest_New_Cons ).

dim2var(Cons_Sys,Vars,Renamed_Cons_Sys):-
	dim2var_constraint_system(Cons_Sys,Vars,Renamed_Cons_Sys).

