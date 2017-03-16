:- module(post_processing,[post_process_decomp/0,post_process_decomp/1],[regtypes,andprolog]).

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists), [append/3, length/2]).
:- use_module(library(write), [write/1]).

:- use_module(library(vndict), [create_pretty_dict/2]).
:- use_module(program(p_unit), [program/2]).
:- use_module(program(clause_db), [source_clause/3]).
:- use_module(program(clidlist), 
 	[inverse_rewrite_source_program/2, 
 	 rewrite_source_clause/4,clause_key/2]).
:- use_module(program(itf_base_db), [defines/3, exports/2]).
:- use_module(program(itf_db), [curr_file/2]).
:- use_module(ciaopp(api(api_order)), [pr_order_clean/0, pr_order_get/1, pr_order_set/1]).
:- use_module(spec(unfold_operations), [body2list/2, list2body/2]).
:- use_module(library(jvm_in_ciao(interpreter(heap_operations))), [filter_heap/2]).
:- use_module(library(jvm_in_ciao(interpreter(jvml))), [methodSignature_name/2]).
:- use_module(library(jvm_in_ciao(interpreter(loaded_classes))), [main_class/1]).


:- data new_pred/1.
:- data heap/1.
:- data import_class/3.

post_process_decomp :- post_process_decomp([heap(on)]).
post_process_decomp(Flags) :-
	set_flags(Flags),
	retractall_fact(new_pred(_)),
	retractall_fact(source_clause(_,directive(_),_)),
	program(P,_),
	retractall_fact(source_clause(_,_,_)),
	inverse_rewrite_source_program(P,P_p),
	member(clause(H,B),P_p),
	body2list(B,BList),
	transform_atoms([H|BList],[H_p|BList_p]),
	list2body(BList_p,B_p),
	clause_key(H_p,NewClId), 
	rewrite_source_clause(H_p,B_p,NewClId,NewCl),
	create_pretty_dict(NewCl,NewDict),
	assertz_fact(source_clause(NewClId,NewCl,NewDict)),
	fail.
post_process_decomp(_) :- 
	update_pr_order,
	update_module_header,
	add_directives,
	cleanup.

set_flags([Flag|Fs]) :-
	set_flag(Flag),
	set_flags(Fs).
set_flags([]).

set_flag(heap(V)) :- set_fact(heap(V)).

cleanup :- 
	retractall_fact(new_pred(_)),
	retractall_fact(heap(_)),
	retractall_fact(import_class(_,_,_)).

transform_atoms([Atom|As],[Atom_p|As_p]) :-
	module_functor_args(Atom,_M,main,[MSigOrSMN|RealArgs]),!,
	(functor(MSigOrSMN,methodSignature,_) -> 
	    MSigOrSMN = methodSignature(methodName(_,shortMethodName(SMN)),_,_)
	;
	    SMN = MSigOrSMN),
	length(RealArgs,Arity),
	add_new_pred(SMN,Arity), 
	transform_terms(RealArgs,RealArgs_p),
	Atom_p =..[SMN|RealArgs_p],
	transform_atoms(As,As_p).
transform_atoms([Atom|As],[Atom_p|As_p]) :-
	module_functor_args(Atom,_M,res_invoke,[CallMain]),!,
	CallMain =..[main,MSig|MethodArgs],
	methodSignature_name(MSig,methodName(className(packageName(PN),shortClassName(SCN)),
                                             shortMethodName(SMN))),
	add_import_class(PN,SCN,SMN),
	transform_terms(MethodArgs,MethodArgs_p),
	atom_concat([SCN,:,SMN],QualMN),
	Atom_p =..[QualMN|MethodArgs_p],
	transform_atoms(As,As_p).
transform_atoms([A|As],[A_p|As_p]) :-
	A =..[Af|AArgs],
	transform_terms(AArgs,AArgs_p),
	A_p =..[Af|AArgs_p],
	transform_atoms(As,As_p).
transform_atoms([],[]).

transform_terms(T,T) :- var(T),!.
transform_terms([T|Ts],[T|Ts_p]) :-
	var(T),!,
	transform_terms(Ts,Ts_p).
%transform_terms([heap(DH,SH)|Ts],[FH|Ts_p]) :-
%	!,filter_heap(heap(DH,SH),FH),
%	transform_terms(Ts,Ts_p).
transform_terms([num(int(N))|Ts],[N|Ts_p]) :- 
	current_fact(heap(on)),!,
	transform_terms(Ts,Ts_p).
transform_terms([ref(loc(R))|Ts],[r(R)|Ts_p]) :- 
	current_fact(heap(on)),!,
	transform_terms(Ts,Ts_p).
transform_terms([refE(loc(R))|Ts],[rE(R)|Ts_p]) :- 
	current_fact(heap(on)),!,
	transform_terms(Ts,Ts_p).
transform_terms([heap(dynamicHeap(DH),staticHeap(SH))|Ts],[(DH_p,SH_p)|Ts_p]) :- 
	current_fact(heap(on)),!,
	(nonvar(DH) -> transform_terms(DH,DH_p)
	             ; DH_p = DH),
	(nonvar(SH) -> transform_terms(SH,SH_p)
	             ; SH_p = SH),
	transform_terms(Ts,Ts_p).
transform_terms([object(_,Fs)|Ts],[Fs_p|Ts_p]) :- 
	current_fact(heap(on)),!,
	(nonvar(Fs) -> transform_terms(Fs,Fs_p)
	             ; Fs_p = Fs),
	transform_terms(Ts,Ts_p).
transform_terms([array(locationArray(L,_T),Vs)|Ts],[arr(L,Vs_p)|Ts_p]) :- 
	current_fact(heap(on)),!,
	(nonvar(Vs) -> transform_terms(Vs,Vs_p)
	             ; Vs_p = Vs),
	transform_terms(Ts,Ts_p).
transform_terms([OF|Ts],[(SFN,V_p)|Ts_p]) :-
	current_fact(heap(on)),
	OF = objectField(fieldSignature(fieldName(_CN,shortFieldName(SFN)),_Type),V),!,
	transform_terms([V],[V_p]),
	transform_terms(Ts,Ts_p).
transform_terms([(FS,V)|Ts],[(SFN,V_p)|Ts_p]) :-
	current_fact(heap(on)),
	functor(FS,FS_f,_),FS_f == fieldSignature,!,
	FS = fieldSignature(fieldName(_CN,shortFieldName(SFN)),_Type),
	transform_terms([V],[V_p]),
	transform_terms(Ts,Ts_p).
transform_terms([fieldSignature(fieldName(_CN,shortFieldName(SFN)),_T)|Ts],[SFN|Ts_p]) :- 
	current_fact(heap(on)),!,
	transform_terms(Ts,Ts_p).
transform_terms([refType(classType(className(packageName(_),shortClassName(SCN))))|Ts],[SCN|Ts_p]) :-
	current_fact(heap(on)), !, 
	transform_terms(Ts,Ts_p).
transform_terms([T|Ts],[T_p|Ts_p]) :-
	T =..[Func|Args],!,
	transform_terms(Args,Args_p),
	T_p =..[Func|Args_p],
	transform_terms(Ts,Ts_p).
transform_terms([],[]).

update_module_header :-
	%loaded_classes:main_class(className(_,shortClassName(SCN))),
	%atom_concat(SCN,'_pe',ModName),
	curr_file(_,ModName),
	generate_qualified(ModName,main,3,MainQPred,MainQGoal),
	retractall_fact(itf_base_db:defines(MainQPred,3,ModName)),
	retractall_fact(itf_base_db:exports(MainQGoal,ModName)),
	add_new_pred(init,0),
	new_pred(NewPred),
	functor(NewPred,SMN,Arity),
	generate_qualified(ModName,SMN,Arity,QPred,QGoal),
	assertz_fact(itf_base_db:defines(QPred,Arity,ModName)),
	assertz_fact(itf_base_db:exports(QGoal,ModName)),
	fail.
update_module_header.

generate_qualified(ModName,PredName,Arity,QPred,QGoal) :-
	atom_concat([ModName,':',PredName],QPred),
	length(Args,Arity),
	QGoal =..[QPred|Args].


update_pr_order :-
	pr_order_get(PrOrder),
	%transform_atoms(PrOrder,PrOrder_p),
	%update_pr_order_(PrOrder,PrOrder_p),
	findall(NewPred,new_pred(NewPred),PrOrderNewPreds),
	filter_pr_order(PrOrder,PrOrder_f),
	append(PrOrderNewPreds,PrOrder_f,NewPrOrder),
	pr_order_set(NewPrOrder).

filter_pr_order([Pred|R],R_p) :-
	module_functor_args(Pred,_M,main,_),!,
	filter_pr_order(R,R_p).
filter_pr_order([Pred|R],[Pred|R_p]) :-
	filter_pr_order(R,R_p).
filter_pr_order([],[]).

add_new_pred(SMN,Arity) :-
	length(Args,Arity),
	NewPred =..[SMN|Args],
	(current_fact(new_pred(NewPred)) 
	    -> true
	     ; assertz_fact(new_pred(NewPred))).

module_functor_args(Atom,M,F,Args) :-
	Atom =..[Atom_f|Args],
	atom_concat([M,:,F],Atom_f).

add_import_class(PN,SCN,SMN) :-
	loaded_classes:main_class(MainClass),
	MainClass \= className(packageName(PN),shortClassName(SCN)),
	\+ current_fact(import_class(PN,SCN,SMN/2)),!,
	assertz_fact(import_class(PN,SCN,SMN/2)).
add_import_class(_,_,_).

write_imported_classes :-
	current_fact(import_class(PN,SCN,_)),
	findall(MN,import_class(PN,SCN,MN),MNs),
	MNs \= [],
	write_imported_class(PN,SCN,MNs),
	retractall_fact(import_class(PN,SCN,_)),
	fail.
write_imported_classes.
	
write_imported_class(PN,SCN,MNs) :-
	atom_concat('java/',_,PN),!,
	atom_concat(['jvm_in_ciao/java_libraries/decompiled/',PN,SCN],ModPath),
	assertz_fact(source_clause(1/0/1,directive(use_module(library(ModPath),MNs)),dic([],[]))).
write_imported_class(PN,SCN,MNs) :-
	atom_concat('javax/',_,PN),!,
	atom_concat(['jvm_in_ciao/java_libraries/decompiled/',PN,SCN],ModPath),
	assertz_fact(source_clause(1/0/1,directive(use_module(library(ModPath),MNs)),dic([],[]))).
write_imported_class(PN,SCN,MNs) :-
	atom_concat([PN,SCN],ModPath),
	assertz_fact(source_clause(1/0/1,directive(use_module(ModPath,MNs)),dic([],[]))).

add_directives :-
	assertz_fact(source_clause(1/0/1,directive(include(library(jvm_in_ciao(interpreter(exec_header))))),dic([],[]))),
	write_imported_classes.
%	assertz_fact(source_clause(1/0/1,directive(use_module(library(jvm_in_ciao/interpreter/loaded_classes),[get_class/1,main_class/1])),dic([],[]))).

%% print_directives :-
%% 	current_fact(source_clause(A,directive(B),C)),
%% 	write(source_clause(A,directive(B),C)),nl,
%% 	fail.
%% print_directives.
