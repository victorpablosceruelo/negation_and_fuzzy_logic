:- use_package([assertions,regtypes,basicmodes,nativeprops]).

:- use_module(library(aggregates), [findall/3]).
% Uncomment this line and comment the following one to residualize heap ops
%:- use_module(library(jvm_in_ciao(interpreter(heap_operations)))).
:- use_module(library(jvm_in_ciao(interpreter(heap_operations))), 
 	      [new_ref/1,filter_heap/2,build_heapout_struct/2]).

%:- use_module(library(jvm_in_ciao(interpreter(exception)))).
:- use_module(library(lists), [length/2, nth/3, append/3, reverse/3, reverse/2]).
:- use_module(library(jvm_in_ciao(interpreter(binary_operations)))).
:- use_module(library(jvm_in_ciao(interpreter(extended_lists))), 
	              [nth_g/3,update/4,member_g/2,replace_first/4]).
:- use_module(library(term_filtering(term_filtering)), [filter_term/4]).
:- use_module(library(jvm_in_ciao(interpreter(residual_invocations))), 
	              [res_invoke/1]).
:- use_module(library(jvm_in_ciao(interpreter(loaded_classes))), 
	              [get_class/1,main_class/1]).
:- use_module(library(jvm_in_ciao(interpreter(domain))), 
	              [assign_compatible/3,not_assign_compatible/3,heap_new_OA/2,init_array/3]).
:- use_module(library(jvm_in_ciao(interpreter(jvml))), [subclass_name/2, not_subclass_name/2, 
	msig_from_smnwa/2,isStatic/1]).

:- discontiguous bytecode/5, class/2, class_method/3, lookup_here/3, lookup_here_succeeds/2, class_has_method/2.
:- push_prolog_flag(multi_arity_warnings,off).
:- include(library(jvm_in_ciao(interpreter(extended_lists_i)))).
:- include(library(jvm_in_ciao(interpreter(domain_i)))).
:- include(library(jvm_in_ciao(interpreter(jvml_i)))).
:- include(library(jvm_in_ciao(interpreter(jvm_i)))).
%:- include(library(jvm_in_ciao(interpreter(mis_i)))).
% Comment this line to residualize heap ops
:- include(library(jvm_in_ciao(interpreter(heap_operations_i)))).
:- include(library(jvm_in_ciao(interpreter(exception_i)))).
:- pop_prolog_flag(multi_arity_warnings).


:- trust comp main/3 : f_sig * i_sig * i_sig + pe_type.
main(M,In,Out) :-
	initialize_all(M,In,Out,S0,Sf),
	execute(S0,Sf).

:- trust comp main_t/4 : f_sig * i_sig * i_sig * i_sig + pe_type.
main_t(M,In,Out,Trace) :-
	initialize_all(M,In,Out,S0,Sf),
	execute_t(S0,Sf,Trace).

:- trust comp initialize_all/5 + (eval,sideff(free),bind_ins).
initialize_all(M,In,Out,S0,Sf) :-	
%	source_program(P),
	check_generate_method(M,MethodTerm), MethodTerm = method(MSig,_,_,_,_),
	methodSignature_result(MSig,Result),
	In = [Args,H0],
	check_generate_args(MethodTerm,Args),
	build_initial_state(MSig,Args,H0,S0),
	Sf = st(Hf,fr(_PC,_M,OS,_LV),_FS),
	(Result == none -> Out = [Hf]
	                 ; OS = [Top|_],Out = [Top,Hf]).
%	heap_operations:reset_counter. % Set the heap counter to 0
%	heap_operations:infer_counter(Args). % Takes into account the initial heap

check_generate_method(M,MethodTerm) :-
% Case for standard PE entry. We generate all methods inside the main class.
	var(M),!, 
	main_class(MainClassName),
	program_class(MainClassName,MainClass),
	%P = program([Class1|_],_), 
	MSig = methodSignature(methodName(_,shortMethodName(M)),_,_),
	class_method(MainClass,MSig,MethodTerm),
	\+ method_isAbstract(MethodTerm),
	M \== '<clinit>'.
check_generate_method(MSig,MethodTerm) :-
% Case for big-step invocations. We just look for the corresponding method term.
	MSig = methodSignature(methodName(CN,shortMethodName(_SMN)),_,_),!,
	program_class(CN,Class),
	class_method(Class,MSig,MethodTerm).
check_generate_method(SMNWA,MethodTerm) :-
% Case for advanced PE entries where M is the (short) method name in SMNWA format.
	ground(SMNWA),!,
	msig_from_smnwa(SMNWA,MSig),
	loaded_classes:main_class(MainClassName),
	program_class(MainClassName,MainClass),
	class_method(MainClass,MSig,MethodTerm).

check_generate_args(Method,Args) :-
	Method = method(MSig,_,_,_,_),	
	methodSignature_parameters(MSig,Params),
	check_generate_args_(Params,ArgsWThis),
	(method_isStatic(Method) -> Args = ArgsWThis
	                          ; Args = [ref(loc(_))|ArgsWThis]).
check_generate_args_([],[]).
check_generate_args_([refType(_)|RP],[_|RA]) :- 
	check_generate_args_(RP,RA).
%check_generate_args_([refType(_)|RP],[ref(loc(_))|RA]) :- 
%	check_generate_args_(RP,RA).
%check_generate_args_([refType(_)|RP],[null|RA]) :- 
%	check_generate_args_(RP,RA).
check_generate_args_([primitiveType(_)|RP],[num(int(_))|RA]) :- 
	check_generate_args_(RP,RA).

build_initial_state(MSig,Args,H0,S0) :-
	methodSignature_name(MSig,methodName(CN,_)),
	program_class(CN,Cl),
	class_method(Cl,MSig,Method),
%	\+ class_isAbstract(Cl),
	% recovery of datas
	method_body(Method,MethodBody),
	bytecodeMethod_localVarSize(MethodBody,LocalVarSize),
	bytecodeMethod_firstAddress(MethodBody,PC),
	% building the frame
	init_localVars(LocalVarSize,Args,Method,MSig,LocalVar),
%% 	length(LocalVar,LocalVarSize),
%% 	init_localVarArg(LocalVar,LocalVarSize,Args),
	OperandStack = [],
	Frame = fr(Method,PC,OperandStack,LocalVar),
	% building the state
	FrameStack = [],
	S0 = (st(H0,Frame,FrameStack)).

init_localVars(LocalVarSize,Args,Method,MSig,LocalVar) :-
	methodSignature_parameters(MSig,ParamTypes),
	(method_isStatic(Method) -> AdjParamTypes = ParamTypes
                                  ; AdjParamTypes = [this|ParamTypes]),
	init_localVar_args(Args,AdjParamTypes,LocalVar1),
	length(LocalVar1,L1),
	L2 is LocalVarSize - L1,
	init(LocalVar2,num(int(0)),L2),
	append(LocalVar1,LocalVar2,LocalVar).

init_localVar_args([],[],[]).
init_localVar_args([Arg|Args],[primitiveType(long)|RTs],[Arg,noused|RLV]) :-
	!, init_localVar_args(Args,RTs,RLV).
init_localVar_args([Arg|Args],[primitiveType(double)|RTs],[Arg,noused|RLV]) :-
	!, init_localVar_args(Args,RTs,RLV).
init_localVar_args([Arg|Args],[_|RTs],[Arg|RLV]) :-
	init_localVar_args(Args,RTs,RLV).

 
%% init_localVarArg(L,I,Arguments) :-
%% 	length(Arguments,ArgumentsSize),
%% 	S is I-ArgumentsSize,
%% 	length(RL,S),
%% 	init(RL,num(int(0)),S),
%% 	append(Arguments,RL,L).

:- trust comp bytecode(A,B,C,_,_) : (ground(A),ground(B),ground(C)) + eval.
:- trust comp bytecode/5 + sideff(free).
bytecode(_, _, _, _, _).

% to remove run-time checks
:- trust comp check(_) + eval.
:- trust comp check(_) + sideff(free).
check(_).

% The two following predicates are different in the 'normal' interpreter and in
% the version for partial evaluation (because of the differences in using modules)
:- trust comp bytecodeMethod_instructionAt(bytecodeMethod(_,_,_,methodId(Module,MethodIndex),_),PC,Instruction) :
	(ground(Module),ground(MethodIndex),ground(PC)) + eval.
bytecodeMethod_instructionAt(bytecodeMethod(_,_,_,methodId(_Module,MethodIndex),_),PC,Instruction):-
	%Module:bytecode(PC,MethodIndex,Instruction,_).
	Module = '0', % Trick for speeding up the experiments
	bytecode(PC,Module,MethodIndex,Instruction,_).
%	loaded_classes:get_bytecode(bytecode(PC,Module,MethodIndex,Instruction,_)).

:- trust comp bytecodeMethod_next(bytecodeMethod(_,_,_,methodId(Module,MethodIndex),_),PC,PCb) :
	(ground(Module),ground(MethodIndex),ground(PC)) + eval.
bytecodeMethod_next(bytecodeMethod(_,_,_,methodId(_Module,MethodIndex),_),PC,PCb):-
	%Module:bytecode(PC,MethodIndex,_,Size),
	Module = '0', % Trick for speeding up the experiments
	bytecode(PC,Module,MethodIndex,_,Size),
%	loaded_classes:get_bytecode(bytecode(PC,Module,MethodIndex,_,Size)),
	PCb is PC+Size.

:- trust comp get_loop_info/3 + (eval,sideff(free)).
get_loop_info(M,LoopStartPC,PostLoopPC) :-
	method_body(M,Body),
	bytecodeMethod_methodId(Body,methodId(_,MId)),
	loop_info(MId,LoopStartPC,PostLoopPC).

loop_info(0,0,0).

:- trust comp class_method(A,B,C) : (ground(A), ground(B)) + eval.
:- trust comp class_method(A,B,C) + sideff(free).
:- trust comp class_isAbstract/1 + (sideff(free), eval).

:- trust comp  lookup_here(B,C,D) : (nonvar(B), nonvar(C)) + eval.
:- trust comp  lookup_here(B,C,D) + sideff(free).

:- trust comp  lookup_here_succeeds(B,C) : (nonvar(B), nonvar(C)) + eval.
:- trust comp  lookup_here_succeeds(B,C) + sideff(free).

:- trust comp class_has_method(A,B) : (ground(A), ground(B)) + eval.
:- trust comp class_has_method(A,B) + sideff(free).

:- trust comp init(+list,+term,+term) + (eval, is_det).
:- trust comp init/3 + eval.
:- trust comp init/3 + sideff(free).

% Added for AHR interpreter
:- trust comp compat_refType/2 + (eval,sideff(free)).


%:- trust comp this_class/1 + (eval,sideff(free)).
this_class(MainClass) :- 
	get_this_class(MainClass).

:- trust comp get_this_class/1 + (eval,sideff(free)).
get_this_class(MainClass) :-
	main_class(MainClassName),
	program_class(MainClassName,MainClass).

 
