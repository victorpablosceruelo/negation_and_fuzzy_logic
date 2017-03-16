:- module(classfile_reader,[read_classfile/1,read_classfile/2,reset/0,read_file/3,
                            set_options/1,get_bytecode/1,cleanup_bytecode/0]).

:- use_module(stats).
:- use_module(data_reader).
:- use_module(descriptors_grammar).
:- use_module(flags_parser).
:- use_module(library(jvm_in_ciao(interpreter(loaded_classes))), 
	      [add_class/1]).

:- use_module(library(filenames)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(format), [format/2]).

:- data cpitem/2.
:- data current_method/1.
:- data this_class/1.
:- data bytecodeMethod/4.
:- data module_name/1.
:- data access_flags/1.
:- data bytecode/5.

:- data factorization/1.
:- data refs_resolution/1.
:- data output/1.
:- data loading_mode/1.
:- data class_fact/1.
:- data time_info/1.
:- data modularity/1.
:- data lcs_mode/1.
:- data assert_bc_mode/1.
:- data no_write/1.
:- data stats/1.

read_classfile(PathName) :-
	read_classfile(PathName,[]).

read_classfile(PathName,Options) :-
        statistics(runtime,[Time1,_]),
	default_options(PathName),
	analize_options(Options),
	read_classfile_(PathName),
	print_time(Time1).

default_options(PathName) :-
	set_fact(factorization(off)),
	set_fact(refs_resolution(off)),
	no_path_file_name(PathName,Filename),
	basename(Filename,Basename),
	%make_dir_if_neccesary(Basename),
	%atom_concat([Basename,'/',Basename,'_class.pl'],PathOut),
	atom_concat([Basename,'_class.pl'],PathOut),
	set_fact(output(PathOut)),
	set_fact(loading_mode(off)),
	set_fact(class_fact(off)),
	set_fact(time_info(off)),
	set_fact(modularity(off)),
	set_fact(lcs_mode(off)),
	set_fact(assert_bc_mode(off)),
	set_fact(no_write(off)),
	set_fact(stats(off)).

set_options(Opts) :- analize_options(Opts).

analize_options([]).
analize_options([Op|Opts]) :-
	process_option(Op),
	analize_options(Opts).

process_option(factorization(X)) :- !,assert_replace(factorization(X)).
process_option(f(X)) :- !,assert_replace(factorization(X)).
process_option(refs_resolution(X)) :- !,assert_replace(refs_resolution(X)).
process_option(r(X)) :- !,assert_replace(refs_resolution(X)).
process_option(output(F)) :- ground(F),!,assert_replace(output(F)).
process_option(output(F)) :- var(F),!,output(F).
process_option(o(F)) :- process_option(output(F)).
process_option(loading_mode(X)) :- !,assert_replace(loading_mode(X)).
process_option(l(X)) :- !,assert_replace(loading_mode(X)).
process_option(class_fact(X)) :- !,assert_replace(class_fact(X)).
process_option(c(X)) :- !,assert_replace(class_fact(X)).
process_option(time_info(X)) :- !,assert_replace(time_info(X)).
process_option(t(X)) :- !,assert_replace(time_info(X)).
process_option(mod(X)) :- !,assert_replace(modularity(X)).
process_option(lcs(X)) :- !,assert_replace(lcs_mode(X)).
process_option(assert_bc(X)) :- !,assert_replace(assert_bc_mode(X)).
process_option(a(X)) :- !,assert_replace(assert_bc_mode(X)).
process_option(no_write(X)) :- !,assert_replace(no_write(X)).
process_option(stats(X)) :- !,assert_replace(stats(X)).

assert_replace(Fact) :- set_fact(Fact).

get_bytecode(bytecode(A,B,C,D,E)) :-
	current_fact(bytecode(A,B,C,D,E)).

print_time(Time1) :-
	current_fact(time_info(on)),!,
	statistics(runtime,[Time2,_]),
	Time is Time2 - Time1,
	format("processed in ~2f ms\n",[Time]).
print_time(_).

read_classfile_(Pathname) :-
	open_streams(Pathname,FIn,FOut,_),
	read_file(FIn,FOut,_),
	close(FIn),
	close(FOut).

read_file(FIn,FOut,WrittenTerm) :-
	write_header(FOut),
	read_magic(FIn,FOut),
	read_version(FIn,FOut),
	read_conspool(FIn,FOut),
	read_classinfo(FIn,FOut,[],ClassTerm1),
	read_fields(FIn,FOut,ClassTerm1,ClassTerm2),
	read_methods(FIn,FOut,ClassTerm2,ClassTerm3),
	read_classfile_attributes(FIn,FOut),
	write_class_or_int_term(FOut,ClassTerm3,WrittenTerm),
	delete_asserts.

open_streams(Pathname,FIn,FOut,PathOut) :-
	open(Pathname,read,FIn),
	current_fact(output(PathOut)),
	open(PathOut,write,FOut).

write_header(FOut) :-
	current_fact(output(PathOut)),
	no_path_file_name(PathOut,FileName),
	basename(FileName,ModuleName),
	assertz_fact(module_name(ModuleName)),
	write_header_(FOut,ModuleName).

write_header_(FOut,ModuleName) :-
	current_fact(loading_mode(off)),!,
        format_class_pred(ClassPred),
	atom_concat('bytecode/4',ClassPred,ExportedPreds),
	format(FOut,":- module(~q,[~w],[assertions]).\n\n",[ModuleName,ExportedPreds]),
	write(FOut,':- discontiguous bytecode/4,method/2,code/2,exception_table/3,code_attributes/3,method_attributes/3.\n\n').

write_header_(_,_).

format_class_pred(',class/1') :- current_fact(class_fact(on)).
format_class_pred('') :- current_fact(class_fact(off)).

/*
make_dir_if_neccesary(DirName) :-
	file_exists(DirName),!.
make_dir_if_neccesary(DirName) :-
	make_directory(DirName).
*/

write_class_or_int_term(FOut,ClassTerm,WrittenTerm) :-
 	current_fact(access_flags(Flags)),
 	get_interface_flag(Flags,true),!,
 	write_interface_term(FOut,ClassTerm,WrittenTerm).
write_class_or_int_term(FOut,ClassTerm,WrittenTerm) :-
	write_class_term(FOut,ClassTerm,WrittenTerm).

write_class_term(FOut,RClassTerm,ClassTerm):-
	reverse(RClassTerm,ClassTermArgs),
	ClassTerm =..[class|ClassTermArgs],
	build_class_fact(ClassTerm,ClassFact),
	portray_clause_cond_c(FOut,ClassFact).

build_class_fact(ClassTerm,class(ModuleName,ClassTerm)) :-
	current_fact(loading_mode(on)),!,
	current_fact(module_name(ModuleName)).
build_class_fact(ClassTerm,class(ClassTerm)).

write_interface_term(FOut,RClassTerm,InterfaceTerm):-
	reverse(RClassTerm,ClassTermArgs),
	ClassTermArgs = [className(PN,SCN),F1,F2,F3,_,Ints,Fields,Methods],
	InterfaceTerm =..[interface,interfaceName(PN,SCN),
	                  F1,F2,F3,Ints,Fields,Methods],
        build_interface_fact(InterfaceTerm,InterfaceFact),
	portray_clause_cond_c(FOut,InterfaceFact).

build_interface_fact(InterfaceTerm,interface(ModuleName,InterfaceTerm)) :-
	current_fact(loading_mode(on)),!,
	current_fact(module_name(ModuleName)).
	
% Miky: It was interface instead of class(20-02-08)
build_interface_fact(InterfaceTerm,class(InterfaceTerm)).

reset :- delete_asserts.

delete_asserts :-
	retractall_fact(cpitem(_,_)),
	retractall_fact(current_method(_)),
	retractall_fact(this_class(_)),
	retractall_fact(module_name(_)),
	retractall_fact(bytecodeMethod(_,_,_,_)),
	retractall_fact(access_flags(_)),
	retractall_fact(factorization(_)),
	retractall_fact(refs_resolution(_)),
	retractall_fact(output(_)),
	retractall_fact(loading_mode(_)),
	retractall_fact(class_fact(_)),
	retractall_fact(time_info(_)),
	retractall_fact(modularity(_)),
	retractall_fact(lcs_mode(_)),
	retractall_fact(assert_bc_mode(_)),
	retractall_fact(no_write(_)),
	retractall_fact(stats(_)).

cleanup_bytecode :-
	retractall_fact(bytecode(_,_,_,_,_)).

portray_clause_cond_l(FOut,Fact) :-
	current_fact(loading_mode(off)),!,
	portray_clause_cond_nw(FOut,Fact).
portray_clause_cond_l(_,_).

portray_clause_cond_c(FOut,Fact) :-
	current_fact(class_fact(on)),!,
	(lcs_mode(off) -> portray_clause_cond_nw(FOut,Fact)
	                ; ((Fact = class(_Id,ClassTerm);Fact = interface(_Id,ClassTerm)),
			   loaded_classes:add_class(ClassTerm))).
portray_clause_cond_c(_,_).

portray_clause_cond_bc(FOut,Fact) :- 
	portray_clause_cond_nw(FOut,Fact),
	((current_fact(loading_mode(on)),
	  current_fact(assert_bc_mode(on)))
	      -> assertz_fact(Fact)
	       ; true).

portray_clause_cond_nw(FOut,Fact) :-
	current_fact(no_write(off)),!,
	portray_clause(FOut,Fact).
portray_clause_cond_nw(_,_).

read_magic(FIn,FOut) :-
	read_u4(FIn,U4),
	portray_clause_cond_l(FOut,magic(U4)).

read_version(FIn,FOut) :-
	read_u2(FIn,U2M),
	read_u2(FIn,U2m),
	portray_clause_cond_l(FOut,version(major(U2M),minor(U2m))).

read_conspool(FIn,FOut) :-
	read_u2(FIn,Count),
	portray_clause_cond_l(FOut,constant_pool_count(Count)),
	read_conspool(FIn,FOut,1,Count).

read_classinfo(FIn,FOut,CT1,CT5) :-
	read_flags(FIn,FOut,CT1,CT2),
	read_this(FIn,FOut,CT2,CT3),
	read_super(FIn,FOut,CT3,CT4),
	read_interfaces(FIn,FOut,CT4,CT5).

read_flags(FIn,FOut,CT1,[Abstract,Public,Final|CT1]) :-
	read_u2(FIn,Flags),
	portray_clause_cond_l(FOut,access_flags(Flags)),
	assertz_fact(access_flags(Flags)),
	classFlags_terms(Flags,Final,Public,Abstract).

classFlags_terms(Flags,final(Final),public(Public),abstract(Abstract)) :-
	get_final_flag(Flags,Final),
	get_public_flag(Flags,Public),
	get_abstract_flag(Flags,Abstract).

read_this(FIn,FOut,CT2,CT3) :-
	read_u2(FIn,This),
	assertz_fact(this_class(This)),
	portray_clause_cond_l(FOut,this_class(This)),
	current_fact(cpitem(This,class(Ref))),
	className_term(Ref,T),
	append(CT2,[T],CT3).

read_super(FIn,FOut,CT3,[T|CT3]) :-
	read_u2(FIn,Super),
	portray_clause_cond_l(FOut,super_class(Super)),

        ( Super == 0 -> Ref = 0 ; current_fact(cpitem(Super,class(Ref))) ), % in case of Object there is not super class

	className_term(Ref,T).

read_interfaces(FIn,FOut,CT4,[InterfacesTerm|CT4]) :-
	read_u2(FIn,Count),
	portray_clause_cond_l(FOut,interfaces_count(Count)),
	read_interfaces(FIn,FOut,0,Count,[],InterfacesTerm).

:- push_prolog_flag(multi_arity_warnings,off).
read_interfaces(_,_,Count,Count,AcuTerm,RAcuTerm) :- !, reverse(AcuTerm,RAcuTerm).
read_interfaces(FIn,FOut,I,Count,AcuTerm,InterfacesTerm) :-
	read_u2(FIn,Interface),
	portray_clause_cond_l(FOut,interface(I,Interface)),
	NewI is I + 1,
	current_fact(cpitem(Interface,class(Ref))),
	interfaceName_term(Ref,InterfaceName),
	read_interfaces(FIn,FOut,NewI,Count,[InterfaceName|AcuTerm],InterfacesTerm).
:- pop_prolog_flag(multi_arity_warnings).

read_fields(FIn,FOut,CT2,[FieldsTerm|CT2]) :-
	read_u2(FIn,Count),
	portray_clause_cond_l(FOut,fields_count(Count)),
	read_fields(FIn,FOut,0,Count,[],FieldsTerm).

:- push_prolog_flag(multi_arity_warnings,off).
read_fields(_,_,Count,Count,AcuTerm,RAcuTerm) :- !,reverse(AcuTerm,RAcuTerm).
read_fields(FIn,FOut,I,Count,AcuTerm,FieldsTerm) :-
	read_field(FIn,Field),
	portray_clause_cond_l(FOut,field(I,Field)),
	NewI is I + 1,
	field_term(Field,FieldTerm),
	read_fields(FIn,FOut,NewI,Count,[FieldTerm|AcuTerm],FieldsTerm).
:- pop_prolog_flag(multi_arity_warnings).

read_field(FIn,Field) :-
	read_u2(FIn,Flags),
	read_u2(FIn,Name),
	read_u2(FIn,Desc),
	read_u2(FIn,AttrCount),
	read_attributes(FIn,_,Attributes,[],AttrCount),
	Field = field_info(Flags,Name,Desc,AttrCount,Attributes).

read_classfile_attributes(FIn,FOut) :-
	read_u2(FIn,Count),
	%portray_clause_cond_l(FOut,attributes_count(Count)),
	read_attributes(FIn,_,Atts,[],Count),
	portray_clause_cond_l(FOut,attributes(Count,Atts)).

read_attributes(_,_,Atts,Atts,0) :- !.
read_attributes(FIn,FOut,Atts,Acu,N) :-
	read_attribute(FIn,FOut,Att),
	NewN is N - 1,
	read_attributes(FIn,FOut,Atts,[Att|Acu],NewN).

read_attribute(FIn,FOut,Att) :-
	read_u2(FIn,AttName),
	current_fact(cpitem(AttName,utf8(S))),
	read_attribute(FIn,FOut,S,Att).

:- push_prolog_flag(multi_arity_warnings,off).
read_attribute(FIn,_,'SourceFile',Att) :-
	!,read_u4(FIn,_),
	read_u2(FIn,Index),
	Att = source_file(Index).

read_attribute(FIn,_,'ConstantValue',Att) :-
	!,read_u4(FIn,_),
	read_u2(FIn,Index),
	Att = constant_value(Index).

read_attribute(FIn,_,'Deprecated',Att) :-
	!,read_u4(FIn,_),
	Att = deprecated.

read_attribute(FIn,_,'Synthetic',Att) :-
	!,read_u4(FIn,_),
	Att = synthetic.

read_attribute(FIn,FOut,'Code',Att) :-
	!,current_fact(current_method(I)),
	read_u4(FIn,AttrLenght),
	read_u2(FIn,MaxStack),
	read_u2(FIn,MaxLocals),
	read_u4(FIn,CodeLenght),
	portray_clause_cond_l(FOut,code(I,code_info(attribute_length(AttrLenght),
	                                     max_stack(MaxStack),
					     max_locals(MaxLocals),
					     code_length(CodeLenght)))),
        current_fact(factorization(F)),
	current_fact(refs_resolution(R)),
	current_fact(loading_mode(L)),
	read_bytecode(FIn,FOut,0,CodeLenght,I,F,R,L),
	read_u2(FIn,ETLenght),
	%portray_clause_cond_l(FOut,exception_table_lenght(ETLenght)),
	read_exception_table(FIn,FOut,0,ETLenght,[],ExTable),
	portray_clause_cond_l(FOut,exception_table(I,ETLenght,ExTable)),
	read_u2(FIn,AttrCount),
	read_attributes(FIn,_,Attributes,[],AttrCount),
	portray_clause_cond_l(FOut,code_attributes(I,AttrCount,Attributes)),
	Att = code(AttrLenght),
	exceptionHandlers_term(ExTable,EHTerm),
	assertz_fact(bytecodeMethod(MaxLocals,MaxStack,0,EHTerm)).

read_attribute(FIn,FOut,'Exceptions',Att) :-
	!,read_u4(FIn,L),
	read_u2(FIn,N),
	read_exceptions(FIn,FOut,N,[],Exs),
	Att = exceptions(L,Exs).

read_attribute(FIn,_,'LineNumberTable',Att) :-
	!,read_u4(FIn,L),
	skip(FIn,L),
	Att = line_number_table(L).

read_attribute(FIn,_,'LocalVariableTable',Att) :-
	!,read_u4(FIn,L),
	skip(FIn,L),
	Att = local_variable_table(L).

read_attribute(FIn,_,'InnerClasses',Att) :-
	!,read_u4(FIn,L),
	%skip(FIn,L),
	read_u2(FIn,N),
	read_inner_classes(FIn,N,[],InnerClasses),
	Att = inner_classes(L,InnerClasses).

read_attribute(FIn,_,AttrName,Att) :- % If we are here means that we haven't recognized the attribute
	!,read_u4(FIn,L),
	skip(FIn,L),
        Att = skipped_attribute(AttrName,L).

:- pop_prolog_flag(multi_arity_warnings).

read_exceptions(_,_,0,L,L) :- !.
read_exceptions(FIn,FOut,I,Ac,Exs) :- 
	read_u2(FIn,Ex),
	NewI is I - 1,
	read_exceptions(FIn,FOut,NewI,[Ex|Ac],Exs).

read_inner_classes(_,0,Acu,Acu) :- !.
read_inner_classes(FIn,N,Acu,InnerClasses) :-
	read_u2(FIn,InnerIndex),
	read_u2(FIn,OuterIndex),
	read_u2(FIn,InnerName),
	read_u2(FIn,Flags),
	NewN is N-1,
	read_inner_classes(FIn,NewN,[[InnerIndex,OuterIndex,InnerName,Flags]|Acu],
	                   InnerClasses).

read_bytecode(_,_,L,L,_,_,_,_) :- !.
read_bytecode(FIn,FOut,I,CodeLenght,Current,F,R,L) :-
	(stats(on) -> current_fact(module_name(Class)),stats:inc_class_num_insts(Class)
	            ; true),
	read_u1(FIn,Opcode),
	opcode_package(F,OpcodePackage),
	OpcodePackage:opcode(Opcode,Mnemonic,OpsDesc),
	adjust_mnemonic(Mnemonic,F,AdjustedMnemonic),
	read_operands(FIn,OpsDesc,AdjustedMnemonic,LOps,0,OpsBytes,I),
	resolve_params(Opcode,LOps,ResolvedLOps,R),
	InstBytes is (OpsBytes + 1),
	Inst =..ResolvedLOps,
	build_bytecode_fact(I,Current,Inst,InstBytes,BytecodeFact),
	portray_clause_cond_bc(FOut,BytecodeFact),
	%portray_clause_cond_nw(FOut,bytecode(I,Current,Inst,InstBytes)),
	NewI is (I + InstBytes),
	read_bytecode(FIn,FOut,NewI,CodeLenght,Current,F,R,L).

opcode_package(off,opcodes).
opcode_package(on,factorized_opcodes).

adjust_mnemonic(Mnemonic,off,[Mnemonic]).
adjust_mnemonic(Mnemonic,on,AdjustedMnemonic) :-
	reverse(Mnemonic,AdjustedMnemonic).

build_bytecode_fact(I,Current,Inst,InstBytes,BytecodeFact) :-
	current_fact(loading_mode(on)),!,
	module_name(ModuleName),
	BytecodeFact = bytecode(I,ModuleName,Current,Inst,InstBytes).
build_bytecode_fact(I,Current,Inst,InstBytes,BytecodeFact) :-
	BytecodeFact = bytecode(I,Current,Inst,InstBytes).

read_operands(_,[],Ops,ROps,N,N,_) :- reverse(Ops,ROps).
read_operands(FIn,[OpDesc|LOpsDesc],Acu,Ops,N,NumBytes,_) :-
	read_operand(FIn,OpDesc,Op),
	NewN is (N + abs(OpDesc)),
	read_operands(FIn,LOpsDesc,[Op|Acu],Ops,NewN,NumBytes,_).
read_operands(FIn,tableswitch,[Mnemonic],LOps,_,NBytes,I) :- 
	!,skip_padding(FIn,I,NPad),
	read_s4(FIn,Default),
	read_s4(FIn,Low),
	read_s4(FIn,High),
	NOffSets is High-Low+1,
	read_ts_offsets(FIn,NOffSets,[],Offsets),
	NBytes is (NPad+12+(NOffSets*4)), 
	LOps = [Mnemonic,Default,Low,High,Offsets].
read_operands(FIn,lookupswitch,[Mnemonic],LOps,_,NBytes,I) :- 
	!,skip_padding(FIn,I,NPad),
	read_s4(FIn,Default),
	read_s4(FIn,NPairs),
	read_ls_pairs(FIn,NPairs,[],Pairs),
	NBytes is (NPad+8+(NPairs*8)), 
	LOps = [Mnemonic,Default,Pairs].

skip_padding(FIn,I,NPad) :-
	NPad is (4-((I+1) rem 4)) rem 4,
	skip(FIn,NPad).

read_ts_offsets(_,0,ROffSets,OffSets) :- !,reverse(ROffSets,OffSets).
read_ts_offsets(FIn,N,Acu,OffSets) :-
	read_s4(FIn,OffSet),
	NewN is N - 1,
	read_ts_offsets(FIn,NewN,[OffSet|Acu],OffSets).

read_ls_pairs(_,0,RPairs,Pairs) :- !,reverse(RPairs,Pairs).
read_ls_pairs(FIn,N,Acu,Pairs) :-
	read_s4(FIn,Match),
	read_s4(FIn,Offset),
	NewN is N - 1,
	read_ls_pairs(FIn,NewN,[(Match,Offset)|Acu],Pairs).

read_operand(FIn,1,Op) :- read_u1(FIn,Op).
read_operand(FIn,2,Op) :- read_u2(FIn,Op).
read_operand(FIn,4,Op) :- read_u4(FIn,Op).
read_operand(FIn,-1,Op) :- read_s1(FIn,Op).
read_operand(FIn,-2,Op) :- read_s2(FIn,Op).
read_operand(FIn,-4,Op) :- read_s4(FIn,Op).

resolve_params(_,L,L,off).

resolve_params(Opcode,[Ins,Op],[Ins|ROp],on) :- 
	Opcode >= 0x12,Opcode =< 0x14,!,
	current_fact(cpitem(Op,T)),
	T =..[Type|Ref],
	resolve_literal(Type,Ref,ROp).

resolve_params(Opcode,[Ins,Op],[Ins,ROp],on) :- 
	Opcode >= 0xb2,Opcode =< 0xb5,!,
	current_fact(cpitem(Op,fieldref(I,J))),
	current_fact(cpitem(I,class(ClassRef))),
	current_fact(cpitem(J,name_and_type(Name,Desc))),
	fieldSignature_term(ClassRef,Name,Desc,ROp).

resolve_params(Opcode,[Ins,Op],[Ins_p|[ROp|ITMode]],on) :- 
	Opcode >= 0xb6,Opcode =< 0xb8,!,
	(current_fact(modularity(on)) 
	     -> (Ins_p = invoke_mod, 
	         (Opcode == 0xb8 -> ITMode = [static]
		                  ; ITMode = [nonstatic]))
	      ; (Ins_p = Ins,ITMode = [])),
	current_fact(cpitem(Op,methodref(I,J))),
	current_fact(cpitem(I,class(ClassRef))),
	current_fact(cpitem(J,name_and_type(Name,Desc))),
	methodSignature_term(ClassRef,Name,Desc,ROp).

resolve_params(Opcode,[Ins,Op,_,_],[Ins_p|[ROp|ITMode]],on) :- 
	Opcode == 0xb9,!,
	(current_fact(modularity(on)) 
	     -> (Ins_p = invoke_mod,ITMode = [nonstatic])
	      ; (Ins_p = Ins,ITMode = [])),
	current_fact(cpitem(Op,interface_methodref(I,J))),
	current_fact(cpitem(I,class(ClassRef))),
	current_fact(cpitem(J,name_and_type(Name,Desc))),
	methodSignature_term(ClassRef,Name,Desc,ROp).

resolve_params(Opcode,[Ins,Op|Rest],[Ins,ROp|Rest],on) :- 
	(Opcode = 0xbb;Opcode = 0xbd;Opcode = 0xc1;Opcode = 0xc5),!,
	current_fact(cpitem(Op,class(ClassRef))),
	className_term(ClassRef,ROp).

resolve_params(Opcode,[Ins,Op|Rest],[Ins,ROp|Rest],on) :- 
	Opcode = 0xbc,!,
	atype(Op,ROp).

resolve_params(_,L,L,on).


method_term(Flags,Name,Desc,MethodId,Term) :-
	current_fact(this_class(I)),
	current_fact(cpitem(I,class(Class))),
	methodSignature_term(Class,Name,Desc,MST),

	get_abstract_flag(Flags,Abstract),
        get_native_flag(Flags,Native),
        ((Abstract == true;Native == true) -> AbstractOrNative = true;
	    AbstractOrNative = false),
	bytecodeMethod_term(AbstractOrNative,MethodId,BMT),
	get_final_flag(Flags,Final),
	get_static_flag(Flags,Static),
	get_private_flag(Flags,Private),
	get_access_mod(Private,AccessMod),
	Term = method(MST,BMT,final(Final),static(Static),AccessMod).

field_term(field_info(Flags,Name,Desc,_,_),Term) :-
	current_fact(this_class(I)),
	current_fact(cpitem(I,class(Class))),
	fieldSignature_term(Class,Name,Desc,FST),
	get_final_flag(Flags,Final),
	get_static_flag(Flags,Static),
	%get_public_flag(Flags,Public),
	get_private_flag(Flags,Private),
	get_access_mod(Private,AccessMod),
	Term = field(FST,final(Final),static(Static),AccessMod,initialValue(undef)).

get_access_mod(false,public).
get_access_mod(true,private).

bytecodeMethod_term(true,_,_) :- !.
bytecodeMethod_term(false,Id,Term) :-
	current_fact(bytecodeMethod(Local,Stack,First,Exs)),
	retract_fact(bytecodeMethod(_,_,_,_)),
	current_fact(module_name(ModuleName)),
	Term = bytecodeMethod(Local,Stack,First,methodId(ModuleName,Id),Exs).

fieldSignature_term(Class,Name,DescRef,Term) :-
	className_term(Class,ClassName),
	current_fact(cpitem(Name,utf8(SFN))),
	current_fact(cpitem(DescRef,utf8(Desc))),
        parse_field_descriptor(Type,Desc),%%Conversion of types representation
	Term = fieldSignature(fieldName(ClassName,shortFieldName(SFN)),Type).

methodSignature_term(ClassRef,NameRef,DescRef,Term) :-
	className_term(ClassRef,ClassName),
	current_fact(cpitem(NameRef,utf8(SMN))),
	current_fact(cpitem(DescRef,utf8(Desc))),
	parse_method_descriptor(Params,Ret,Desc),
	Term = methodSignature(methodName(ClassName,shortMethodName(SMN)),
	                      Params,Ret).

className_term(ClassRef,Term) :-
	current_fact(cpitem(ClassRef,utf8(FullName))),
	no_path_file_name(FullName,ClassOnly),
	atom_concat(Package,ClassOnly,FullName),
	Term = className(packageName(Package),shortClassName(ClassOnly)).
className_term(0,none).


interfaceName_term(Ref,Term) :-
	current_fact(cpitem(Ref,utf8(FullName))),
	no_path_file_name(FullName,ClassOnly),
	atom_concat(Package,ClassOnly,FullName),
	Term = interfaceName(packageName(Package),shortInterfaceName(ClassOnly)).

exceptionHandlers_term([],[]).
exceptionHandlers_term([EH1|ExTable],[EHTerm|EHTerms]) :-
	EH1 = [StartPc,EndPc,HandlerPc,CatchType],
	resolve_catch_type(CatchType,ClassName),
	EHTerm = exceptionHandler(ClassName,StartPc,EndPc,HandlerPc),
	exceptionHandlers_term(ExTable,EHTerms).

resolve_catch_type(0,none) :- !.
resolve_catch_type(CatchType,ClassName) :-
	CatchType > 0,
	current_fact(cpitem(CatchType,class(I))),
	className_term(I,ClassName).

resolve_literal(integer,[I],[primitiveType(int),I]) :- !.
resolve_literal(float,[F],[primitiveType(float),F]) :- !.
resolve_literal(long,[L],[primitiveType(long),L]) :- !.
resolve_literal(double,[D],[primitiveType(double),D]) :- !.
resolve_literal(string,[Ref],[string,S]) :-
	current_fact(cpitem(Ref,T)),
	arg(1,T,S).

read_exception_table(_,_,ETLenght,ETLenght,T,RT) :- !,reverse(T,RT).
read_exception_table(FIn,FOut,I,ETLenght,Ac,T) :-
	read_u2(FIn,StartPc),
	read_u2(FIn,EndPc),
	read_u2(FIn,HandlerPc),
	read_u2(FIn,CatchType),
	%portray_clause_cond_l(exception_table(I,StartPc,EndPc,HandlerPc,CatchType)),
	NewI is I + 1,
	read_exception_table(FIn,FOut,NewI,ETLenght,
	                     [[StartPc,EndPc,HandlerPc,CatchType]|Ac],T).

read_methods(FIn,FOut,CT2,[MethodsTerm|CT2]) :-
	read_u2(FIn,Count), 
	(stats(on) -> current_fact(module_name(Class)),stats:set_class_num_methods(Class,Count)
	            ; true),
	portray_clause_cond_l(FOut,methods_count(Count)),
	read_methods(FIn,FOut,0,Count,[],MethodsTerm).

:- push_prolog_flag(multi_arity_warnings,off).
read_methods(_,_,Count,Count,AcuTerm,RAcuTerm) :- !,reverse(AcuTerm,RAcuTerm).
read_methods(FIn,FOut,I,Count,AcuTerm,MethodsTerm) :-
	read_method(FIn,FOut,I,MethodTerm),
	NewI is I + 1,
	read_methods(FIn,FOut,NewI,Count,[MethodTerm|AcuTerm],MethodsTerm).
:- pop_prolog_flag(multi_arity_warnings).

read_method(FIn,FOut,I,MethodTerm) :-
	assertz_fact(current_method(I)),
	read_u2(FIn,Flags),
	read_u2(FIn,NameIndex),
	read_u2(FIn,DescIndex),
	read_u2(FIn,AttrCount),
	%portray_clause_cond_l(FOutMethod,attributes_count(AttrCount)),
	portray_clause_cond_l(FOut,method(I,method_info(access_flags(Flags),
	                             name_index(NameIndex),
	                             descriptor_index(DescIndex),
				     attributes_count(AttrCount)))),
	read_attributes(FIn,FOut,Atts,[],AttrCount),
	portray_clause_cond_l(FOut,method_attributes(I,AttrCount,Atts)),
	method_term(Flags,NameIndex,DescIndex,I,MethodTerm),
	retract_fact(current_method(_)).

:- push_prolog_flag(multi_arity_warnings,off).	
read_conspool(_,_,Count,Count) :- !.
read_conspool(FIn,FOut,I,Count) :-
	read_u1(FIn,Tag),
	read_cpitem(Tag,FIn,Term,NEntries),
	NewI is I + NEntries,
	portray_clause_cond_l(FOut,cpitem(I,Term)),
	assertz_fact(cpitem(I,Term)),
	read_conspool(FIn,FOut,NewI,Count).
:- pop_prolog_flag(multi_arity_warnings).

read_cpitem(0x1,FIn,Term,1) :-
	read_utf8(FIn,UTF),
	atom_codes(AUTF,UTF),
	Term = utf8(AUTF).

read_cpitem(3,FIn,Term,1) :-
	read_s4(FIn,Bytes),
	Term = integer(Bytes).

read_cpitem(4,FIn,Term,1) :-
	read_float(FIn,Bytes),
	Term = float(Bytes).

read_cpitem(5,FIn,Term,2) :-
%	read_u4(FIn,HBytes),
%	read_u4(FIn,LBytes),
	read_long(FIn,Long),
	Term = long(Long).
%	Term = long(high_bytes(HBytes),low_bytes(LBytes)).

read_cpitem(6,FIn,Term,2) :-
%	read_u4(FIn,HBytes),
%	read_u4(FIn,LBytes),
	read_double(FIn,Double),
	Term = double(Double). 
%	Term = double(high_bytes(HBytes),low_bytes(LBytes)).

read_cpitem(7,FIn,Term,1) :-
	read_u2(FIn,Name),
	Term = class(Name).

read_cpitem(8,FIn,Term,1) :-
	read_u2(FIn,Index),
	Term = string(Index).

read_cpitem(9,FIn,Term,1) :-
	read_u2(FIn,Class),
	read_u2(FIn,NAndT),
	Term = fieldref(Class,NAndT).

read_cpitem(10,FIn,Term,1) :-
	read_u2(FIn,Class),
	read_u2(FIn,NAndT),
	Term = methodref(Class,NAndT).

read_cpitem(11,FIn,Term,1) :-
	read_u2(FIn,Class),
	read_u2(FIn,NAndT),
	Term = interface_methodref(Class,NAndT).

read_cpitem(12,FIn,Term,1) :-
	read_u2(FIn,Name),
	read_u2(FIn,Descriptor),
	Term = name_and_type(Name,Descriptor).

atype(4,primitiveType(boolean)).
atype(5,primitiveType(char)).
atype(6,primitiveType(float)).
atype(7,primitiveType(double)).
atype(8,primitiveType(byte)).
atype(9,primitiveType(short)).
atype(10,primitiveType(int)).
atype(11,primitiveType(long)).
