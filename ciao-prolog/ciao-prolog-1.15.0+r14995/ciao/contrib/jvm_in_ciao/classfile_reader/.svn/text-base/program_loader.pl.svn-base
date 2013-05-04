:- module(program_loader,[load_program/1,load_program/2,load_program/4,load_program_exec/1]).

:- use_module(stats).
:- use_module(classfile_reader).
:- use_module(mis_grammar).
:- use_module(special_points).
:- use_module(library(jvm_in_ciao(interpreter(jvml))), [class_name/2,
	get_method_id/2,split_smnwa/4,msig_from_smnwa/2]).
:- use_module(library(jvm_in_ciao(interpreter(loaded_classes))), 
	[set_main_class/1,cleanup_loaded_classes/0]).

:- use_module(library(lists), [last/2]).
:- use_module(library(filenames)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(prolog_sys), [statistics/2]).

load_program(Program) :- load_program(Program,_,'',non_modular).
load_program(Program,MIS) :-
	load_program(Program,MIS,'',non_modular).
load_program(Program,MIS,OutputPath,Flag) :-
	loaded_classes:cleanup_loaded_classes,
	stats:init_class_stats('0'),
	statistics(runtime,[Time1,_]),
	(seems_MIS(MIS) -> (mis_methodName(MIS,MethodName),
	                    prepare_stream(Program,MethodName,OutputPath,FOut,ModuleName),
		            write_header(FOut,ModuleName,MethodName))
% First case: MIS denotes a real MIS
	                 ; (prepare_stream(Program,MIS,OutputPath,FOut,ModuleName),
	                    write_header(FOut,ModuleName,MIS))),
% Otherwise: MIS denotes the first arg of the entry (var or shortMethodName)

	load_classes(Program,FOut,[],ClassTerms1,0,Flag),
	load_main_class(ClassTerms1),
	statistics(runtime,[Time2,_]),
	(Flag \== non_modular -> write_bytecodes(FOut,MIS) ; true),
	format(FOut,"\n:- include(library(~q)).\n\n",
	       ['jvm_in_ciao/interpreter/pe_i.pl']),	
	(Flag == optimal -> infer_special_points([conv,div,exception],FOut)
	                  ; true),
	(Flag == offline -> format(FOut,":- trust comp execute/2 + memo.\n\n",[])
	                  ; true),
%	write_stats(FOut),
%	(var(MIS) -> write_entries(FOut) ; true),
	close(FOut),
%	statistics(runtime,[Time3,_]),
	cleanup_bytecode, %cleanup_stats,
	print_times(Time1,Time2,_Time3).

load_program_exec(Program) :-
	loaded_classes:cleanup_loaded_classes,
	statistics(runtime,[Time1,_]),
	prepare_stream(Program,_,'',FOut,ModuleName),
	write_header_exec(FOut,ModuleName),
	load_classes(Program,FOut,[],ClassTerms1,0,non_modular),
	load_main_class(ClassTerms1),
	statistics(runtime,[Time2,_]),
	include_builtins(FOut),
	format(FOut,":- include(library(~q)).\n\n",
	       ['jvm_in_ciao/interpreter/pe_i.pl']),	
	close(FOut),
	cleanup_bytecode,
	print_times(Time1,Time2,_Time3).

%% load_program_nope(Program,MIS,ModuleName) :-
%% 	loaded_classes:cleanup_loaded_classes,
%% 	prepare_stream_nope(Program,FOut,ModuleName),
%% 	write_header_nope(FOut,ModuleName),
%% 	load_classes(Program,FOut,[],ClassTerms1,0,'classic'),
%% 	load_main_class(ClassTerms1),
%% %	builtins_list(BL),
%% %	load_builtins(FOut,BL,ClassTerms1,ClassTerms2),
%% %	load_source_program(FOut,ClassTerms1,ProgramTerm),
%% 	include_builtins(FOut),
%% 	load_current_mis(FOut,MIS,_ProgramTerm,'classic'),
%% 	cleanup_bytecode,
%% 	close(FOut).

seems_MIS(MIS) :- var(MIS),!,fail.
seems_MIS(MIS) :- atom_concat([_,' ',_,'.',_,')'],MIS).

prepare_stream(Program,MethodName,OutputPath,FOut,ModuleName) :-
	Program = [ClassFile|_],
	no_path_file_name(ClassFile,Filename),
	basename(Filename,Basename),
	(ground(MethodName) -> 
	   (split_smnwa(MethodName,_RealSMN,_Ar,AdjSMN),
	    atom_concat([Basename,'_',AdjSMN,'_pe'],ModuleName))
	;
	    atom_concat([Basename,'_pe'],ModuleName)),
	(OutputPath == '' -> OptBar = '' ; OptBar = '/'),
	atom_concat([OutputPath,OptBar,ModuleName,'.pl'],OutputName),
	open(OutputName,write,FOut).

%% prepare_stream_nope(Program,FOut,ModuleName) :-
%% 	Program = [ClassFile|_],
%% 	no_path_file_name(ClassFile,Filename),
%% 	basename(Filename,Basename),
%% 	atom_concat([Basename,'_jvmlr'],ModuleName),
%% 	atom_concat(ModuleName,'.pl',OutputName),
%% 	open(OutputName,write,FOut).

write_header(FOut,ModuleName,SMNWA) :-
%	atom(MethodName),!,
%	format(FOut,":- module(~q,[main/3,this_class/1],[assertions]).\n\n",[ModuleName]),
	format(FOut,":- module(~q,[main/3],[assertions]).\n\n",[ModuleName]),
%	format(FOut,":- discontiguous bytecode/5, class/2.\n\n",[]),
%	format(FOut,":- entry this_class/1.\n\n",[]),
%	(var(MethodName) 
%	    -> true ; 
	format(FOut,":- entry main(~q,_,_).\n\n",[SMNWA]).

write_header_exec(FOut,ModuleName) :-
	format(FOut,":- module(~q,[main_t/4],[assertions]).\n\n",[ModuleName]),
	format(FOut,":- discontiguous bytecode/5, class/2.\n\n",[]).

%% write_header_nope(FOut,ModuleName) :-
%% 	format(FOut,":- module(~q,_,[]).\n\n",[ModuleName]),
%% 	format(FOut,":- discontiguous bytecode/5, class/2.\n\n",[]).
/*
write_entries(FOut) :-
	loaded_classes:main_class(MainClassName),
	program_class(MainClassName,MainClass),
	MSig = methodSignature(methodName(_,shortMethodName(M)),_,_),
	class_method(MainClass,MSig,MethodTerm),
	M \== '<clinit>',
	\+ method_isAbstract(MethodTerm),
	format(FOut,"\n:- entry main(~q,_,_).",[M]),
	fail.
write_entries(_).
*/

print_times(Time1,Time2,_Time3) :-
	ClassesTime is Time2 - Time1,
%	BuiltinsTime is Time3 - Time2,
%	TotalTime is Time3 - Time1,
%	format("Classes loading in ~2f \nBuiltins loading in ~2f\nTotal loading time: ~2f\n",[ClassesTime,BuiltinsTime,TotalTime]).
	format("User classes loading in ~2f\n",[ClassesTime]).

load_main_class(RClassTerms) :-
	last(RClassTerms,MainCT),
	class_name(MainCT,MainCN),
	loaded_classes:set_main_class(MainCN).
load_classes([ClassFile1|R],FOut,ClassTermsAcu,ClassTerms,ClassId,Flag) :-
	open(ClassFile1,read,FIn),
	build_options_list(ClassFile1,ClassId,Flag,Opts),
	set_options(Opts),
	read_file(FIn,FOut,ClassTerm1),
%	loaded_classes:add_class(ClassTerm1),
	nl(FOut),
	close(FIn),
	NewClassId is ClassId + 1,
	load_classes(R,FOut,[ClassTerm1|ClassTermsAcu],ClassTerms,NewClassId,Flag).
load_classes([],_,L,L,_,_).
	
build_options_list(_ClassFile,ClassId,Flag,
	           [f(on),r(on),l(on),o(PathOut),c(on),mod(Mod),lcs(on),a(A),stats(A),no_write(Mod)]) :-
	%no_path_file_name(ClassFile,Filename),
	%basename(Filename,Basename),
	%atom_concat([Basename,'.pl'],PathOut),
        (Flag == non_modular -> Mod = off
	                      ; Mod = on), % Flag might be modular or optimal
	atom_number(AtomId,ClassId),
	atom_concat([AtomId,'.pl'],PathOut),
	(ClassId == 0 -> A = on ; A = off).

write_bytecodes(FOut,M) :-
	(nonvar(M) -> 
	   (msig_from_smnwa(M,MSig),
	    get_method_id(MSig,MId))
	;
	    true), 
	get_bytecode(bytecode(I,'0',MId,Inst,Offnext)),
	portray_clause(FOut,bytecode(I,'0',MId,Inst,Offnext)),
	fail.
write_bytecodes(_,_).

/*
write_stats(FOut) :-
	stats:class_num_insts(Class,CNumInsts),
	stats:class_num_methods(Class,CNumMethods),
	(stats:class_num_divs(Class,_) -> class_num_divs(Class,CNumDivs),class_num_convs(Class,CNumConvs)
	                                ; true),
	format(FOut,"\nc_stats(~q,~q,~q,~q,~q).\n\n",[Class,CNumInsts,CNumMethods,CNumDivs,CNumConvs]),
	stats:method_num_insts(M,MNumInsts),
	method_num_divs(M,MNumDivs),
	method_num_convs(M,MNumConvs),
	format(FOut,"m_stats(~q,~q,~q,~q).\n",[M,MNumInsts,MNumDivs,MNumConvs]),
	fail.
write_stats(_).

%load_builtins(_,[],L,L).
%load_builtins(FOut,[B1|RBs],ClassTermsAcu,ClassTerms) :-
%	load_builtin(FOut,B1,ClassTerm),
%	load_builtins(FOut,RBs,[ClassTerm|ClassTermsAcu],ClassTerms).
%
%load_builtin(FOut,Builtin,ClassTerm) :-
%	load_bytecodes(FOut,Builtin),
%	builtins:class(Builtin,ClassTerm),
%	portray_clause(FOut,class(Builtin,ClassTerm)),nl(FOut).
%
%load_bytecodes(FOut,Builtin) :-
%	builtins:bytecode(A,Builtin,B,C,D),
%	portray_clause(FOut,bytecode(A,Builtin,B,C,D)),
%	fail.
%load_bytecodes(_,_).
	
load_source_program(FOut,ClassTermsR,Program) :-
	reverse(ClassTermsR,ClassTerms),
	Program = program(ClassTerms,[]),
	portray_clause(FOut,source_program(Program)).

load_current_mis(FOut,MIS_J,Program,IssdFlag) :-
	(MIS_J \== all 
	-> (parse_mis(MIS_P,MIS_J,IssdFlag),
	    infer_method_options(MIS_P,Opts),
	    portray_clause(FOut,current_mis(MIS_P,Opts)),
	    fail)
	;   generate_current_mis(FOut,Program)).
load_current_mis(_,_,_,_).

generate_current_mis(FOut,program(ClassTerms,_)) :-
	ClassTerms = [FirstClass|_],
	% For the moment I will take into account just the first class
	FirstClass = class(_Name,_,_,_,_,_,_Fields,Methods),
	member(method(MSig,_,_,static(IsStatic),_),Methods),
	MSig = methodSignature(_,ParamsTypes,_),
	generate_input_args(ParamsTypes,InArgs),
	(IsStatic == false -> InArgs_p = [ref(loc(_))|InArgs]
	                    ; InArgs_p = InArgs),
	MIS = mis(MSig,InArgs_p,heap(dynamicHeap(_),staticHeap([]))),
	infer_method_options(MIS,Opts),
	portray_clause(FOut,current_mis(MIS,Opts)),
	fail.
generate_current_mis(_,_).

generate_input_args([refType(_)|RTypes],[ref(loc(_))|RInArgs]) :- 
	generate_input_args(RTypes,RInArgs).
generate_input_args([refType(_)|RTypes],[null|RInArgs]) :- 
	generate_input_args(RTypes,RInArgs).
generate_input_args([primitiveType(PrimType)|RTypes],[Arg|RInArgs]) :- !,
	ArgAux =..[PrimType,_],
	Arg =..[num,ArgAux],
	generate_input_args(RTypes,RInArgs).
generate_input_args([],[]).

infer_method_options(mis(methodSignature(_,_,none),_,_),[heap]) :- !.
infer_method_options(_,[top,heap]).

load_current_options(FOut,Options) :-
	portray_clause(FOut,options(Options)).
*/

include_builtins(FOut) :- % Only used in the coxtext of JVM execution
	format(FOut,":- include(library(~q)).\n\n",
	       ['jvm_in_ciao/interpreter/builtins_i.pl']).	
