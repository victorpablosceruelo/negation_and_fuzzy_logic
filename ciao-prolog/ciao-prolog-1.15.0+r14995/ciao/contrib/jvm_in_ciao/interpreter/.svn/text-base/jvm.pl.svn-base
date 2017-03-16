:- module(jvm,[current_jvmlr_module/1,load_and_run/5],
	[assertions,isomodes,regtypes]).

:- push_prolog_flag(unused_pred_warnings, no).

:- use_module(library(compiler), [use_module/1, ensure_loaded/1]).
:- use_module(library(lists), [length/2, nth/3, append/3, reverse/2, reverse/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(filenames)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).

:- use_module(library(jvm_in_ciao(classfile_reader(program_loader))), 
	               [load_program/1,load_program_exec/1]).
:- use_module(library(jvm_in_ciao(interpreter(domain)))).
:- use_module(library(jvm_in_ciao(interpreter(jvml)))).
:- use_module(library(jvm_in_ciao(interpreter(exception)))).
:- use_module(library(jvm_in_ciao(interpreter(extended_lists))), 
	               [init/3,replace_first/4]).
:- use_module(library(jvm_in_ciao(interpreter(heap_operations)))).
:- use_module(library(jvm_in_ciao(interpreter(residual_invocations))), 
	              [res_invoke/1]).
:- use_module(library(jvm_in_ciao(interpreter(loaded_classes))), 
	              [get_class/1,main_class/1]).

:- include(library(jvm_in_ciao(interpreter(jvm_i)))).
:- include(library(jvm_in_ciao(interpreter(mis_i)))).

:- data current_jvmlr_module/1.

load_and_run(ClassFiles,SMN,InArgs,Out,Trace) :-
	load_program_exec(ClassFiles),
	statistics(runtime,[Time1,_]),
	generate_module_name(ClassFiles,ModuleName),
	use_module(ModuleName),
	statistics(runtime,[Time2,_]),
	heap_operations:reset_counter,
	build_real_args(InArgs,RealArgs),
	ModuleName:main_t(SMN,RealArgs,Out,Trace),
	statistics(runtime,[Time3,_]),
	print_times(Time1,Time2,Time3).

print_times(Time1,Time2,Time3) :-
	TModule is Time2 - Time1,
	TExec is Time3 - Time2,
	format("Module loaded in ~2f\n",[TModule]),
	format("Program executed in ~2f\n",[TExec]).

generate_module_name(Program,ModuleName) :-
	Program = [ClassFile|_],
	no_path_file_name(ClassFile,Filename),
	basename(Filename,Basename),
	atom_concat([Basename,'_pe'],ModuleName).

build_real_args(Params,[Params_p,heap(dynamicHeap([]),staticHeap([]))]) :-
	translate_params(Params,Params_p).

translate_params([X|Ps],[num(int(X))|Ps_p]) :-
	number(X),!,
	translate_params(Ps,Ps_p).
translate_params([r(R)|Ps],[ref(loc(R))|Ps_p]) :- !,
	translate_params(Ps,Ps_p).
translate_params([X|Ps],[X|Ps_p]) :-
	translate_params(Ps,Ps_p).
translate_params([],[]).

%% % Work in progress
%% main(ClassFiles,MIS_J,Output,Trace) :-	
%% 	load_program_exec(ClassFiles),
%% 	% Translate MIS_J
%% 	initial_state_mis(MIS,InitialSt),
%% 	heap_operations:reset_counter,
%% 	execute_t(InitialSt,FinalSt,Trace),
%% 	build_output(Opts,FinalSt,Output),
%% 	retractall_fact(current_jvmlr_module(_)).
%% 
%% main(FinalState,Trace) :-
%% 	initial_state(InitialState),
%% 	execute_t(InitialState,FinalState,Trace).

build_output([],_,[]).
build_output([Opt|Opts],Sf,[Out|Outs]) :-
	process_option(Opt,Sf,Out),
	build_output(Opts,Sf,Outs).
process_option(top,st(_,fr(_,_,[Top],_),_),Top).
process_option(heap,st(H,_,_),Hf) :- filter_heap(H,Hf).

% Formalization of Java small step semantics. 
% Based on The "Java (TM) Virtual Machine Specification, Second Edition, Tim Lindholm, Frank Yellin" 


% Initialisation and execution are not specified by Bicolano
% only the step predicate is specified

% This simply avoids warnings and compilation errors. Special loop handling is not used
% in the JVM execution, just in PE.
get_loop_info(_,_,_).

:- pop_prolog_flag(unused_pred_warnings).
