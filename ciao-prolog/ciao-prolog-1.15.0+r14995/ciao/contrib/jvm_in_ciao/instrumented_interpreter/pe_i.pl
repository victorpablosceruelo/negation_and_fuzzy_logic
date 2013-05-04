:- use_package([assertions,regtypes,basicmodes,nativeprops]).

:- use_module(library(jvm_in_ciao(interpreter(heap_operations))), 
	      [create_object/4,create_array/4,
	       reset_counter/0,infer_counter/1,
	       filter_heap/2,build_heapout_struct/2]).
:- use_module(library(jvm_in_ciao(interpreter(exception)))).
:- use_module(library(lists), [length/2, nth/3, append/3, reverse/3, reverse/2]).
:- use_module(library(jvm_in_ciao(interpreter(binary_operations)))).
:- use_module(library(jvm_in_ciao(interpreter(extended_lists))), 
	              [nth_g/3,update/4,member_g/2,replace_first/4,append_g/3]).
:- use_module(library(term_filtering(term_filtering)), [filter_term/4]).
:- use_module(library(jvm_in_ciao(interpreter(residual_invocations))), 
	              [res_invoke/1]).

:- discontiguous bytecode/5, class/2.
:- push_prolog_flag(multi_arity_warnings,off).
:- include(library(jvm_in_ciao(interpreter(extended_lists_i)))).
:- include(library(jvm_in_ciao(interpreter(domain_i)))).
:- include(library(jvm_in_ciao(interpreter(jvml_i)))).
:- include(library(jvm_in_ciao(instrumented_interpreter(jvm_i)))).
:- include(library(jvm_in_ciao(interpreter(mis_i)))).
:- include(library(jvm_in_ciao(interpreter(heap_operations_i)))).
%:- include(library(jvm_in_ciao(instrumented_interpreter(cost_arefs_domain)))).
:- include(library(jvm_in_ciao(instrumented_interpreter(traces_domain)))).
:- pop_prolog_flag(multi_arity_warnings).

%:- entry main/2.

main(SMN,InParams,Results,ASf) :-
	prepare_states_etc(InParams,Results,P,InitialSt,FinalSt,_Tr,_TrW,SMN,AS0),
	execute(P,InitialSt,FinalSt,AS0,ASf).

:- trust comp prepare_states_etc/9 + (eval,sideff(free),bind_ins).
prepare_states_etc(InParams,Results,P,InitialSt,FinalSt,Trace,TraceWanted,SMN,AS0) :-
	options(GlobalOpts),
	current_mis(MIS,MethodOpts),
	(member(trace,GlobalOpts) -> append(MethodOpts,[trace],Options)
	                           ; Options = MethodOpts),
	MIS=mis(MSig,Arg,InitialHeap),
	MSig = methodSignature(methodName(_,shortMethodName(SMN)),_,_),
	heap_operations:reset_counter, % Set the heap counter to 0
	heap_operations:infer_counter(Arg), % Takes into account the initial heap
	trace_wanted(Options,TraceWanted),heapin_wanted(Options,HeapInWanted),
	process_options(Options,FinalSt,Trace,Results),
	build_inparams(HeapInWanted,Arg,InitialHeap,InParams),
	source_program(P),
	initial_state_mis(MIS,P,InitialSt),
	alpha_initial_state(AS0).

%:- trust comp build_inparams/4 + (eval,sideff(free),bind_ins).
build_inparams(0,Arg,_,[Arg]).
build_inparams(1,Arg,H,[Arg,H]).%:- filter_heap(H,FH).

%:- trust comp process_options(Ops,_,_,_) : ground(Ops) + (eval,sideff(free),bind_ins).
process_options([],_,_,[]).
process_options([Op|Options],FinalState,Trace,NewResults) :-
	process_option(Op,FinalState,Trace,Result),
	process_options(Options,FinalState,Trace,Results),
	(Result == [] -> NewResults = Results;
	                 NewResults = [Result|Results]).

process_option(trace,_,Trace,Trace).
process_option(top,st(_,fr(_Pc,_M,[Top|_],_LV),_),_,Top).
process_option(heap,st(H,fr(_Pc,_M,_,_LV),_),_,H).
process_option(heapout,st(H,fr(_Pc,_M,_,_LV),_),_,H).
process_option(heapin,st(_H,fr(_Pc,_M,_,_LV),_),_,[]).

%:- trust comp trace_wanted(Ops,_) : ground(Ops) + (eval,sideff(free),bind_ins).
trace_wanted(Options,1) :- member(trace,Options),!.
trace_wanted(_,0).

%:- trust comp heapin_wanted(Ops,_) : ground(Ops) + (eval,sideff(free),bind_ins).
heapin_wanted(Options,1) :- member(heap,Options),!.
heapin_wanted(Options,1) :- member(heapin,Options),!.
heapin_wanted(_,0).

:- trust comp bytecode(A,B,C,_,_) : (ground(A),ground(B),ground(C)) + eval.
:- trust comp bytecode/5 + sideff(free).

% to remove run-time checks
:- trust comp check(_) + eval.
:- trust comp check(_) + sideff(free).
check(_).

% The two following predicates are different in the 'normal' interpreter and in
% the version for partial evaluation (because of the differences in using modules)
:- trust comp bytecodeMethod_instructionAt(bytecodeMethod(_,_,_,methodId(Module,MethodIndex),_),PC,Instruction) :
	(ground(Module),ground(MethodIndex),ground(PC)) + eval.
bytecodeMethod_instructionAt(bytecodeMethod(_,_,_,methodId(Module,MethodIndex),_),PC,Instruction):-
	%Module:bytecode(PC,MethodIndex,Instruction,_).
	bytecode(PC,Module,MethodIndex,Instruction,_).

:- trust comp bytecodeMethod_next(bytecodeMethod(_,_,_,methodId(Module,MethodIndex),_),PC,PCb) :
	(ground(Module),ground(MethodIndex),ground(PC)) + eval.
bytecodeMethod_next(bytecodeMethod(_,_,_,methodId(Module,MethodIndex),_),PC,PCb):-
	%Module:bytecode(PC,MethodIndex,_,Size),
	bytecode(PC,Module,MethodIndex,_,Size),
	PCb is PC+Size.

:- trust comp initial_state_mis(A,B,C) : (nonvar(A),ground(B)) + eval. 
:- trust comp initial_state_mis/3 + sideff(free).

:- trust comp class_method(A,B,C) : (ground(A), ground(B)) + eval.
:- trust comp class_method(A,B,C) + sideff(free).
:- trust comp class_isAbstract/1 + (sideff(free), eval).

:- trust comp  lookup_here(A,B,C,D) : (nonvar(A), nonvar(B), nonvar(C)) + eval.
:- trust comp  lookup_here(A,B,C,D) + sideff(free).

:- trust comp  lookup_here_succeeds(A,B,C) : (nonvar(A), nonvar(B), nonvar(C)) + eval.
:- trust comp  lookup_here_succeeds(A,B,C) + sideff(free).

:- trust comp class_has_method(A,B) : (ground(A), ground(B)) + eval.
:- trust comp class_has_method(A,B) + sideff(free).

:- trust comp init(+list,+term,+term) + (eval, is_det).
:- trust comp init/3 + eval.
:- trust comp init/3 + sideff(free).

% Added for AHR interpreter
:- trust comp compat_refType/3 + (eval,sideff(free)).
