:- module(mis,_,[assertions,regtypes,basicmodes]).

:- use_module(library(lists), [length/2, nth/3, append/3, reverse/2, reverse/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(jvm_in_ciao(interpreter(binary_operations)))).
:- use_module(library(jvm_in_ciao(interpreter(heap_operations))), 
	      [new_ref/1,build_heapout_struct/2]).
:- use_module(exception).
:- use_module(library(jvm_in_ciao(interpreter(extended_lists))), 
	              [nth_g/3,update/4,member_g/2,replace_first/4]).
% Added to avoid warnings regarding bytecodeMethod_next and bytecodeMethod_instructionAt
:- use_module(library(jvm_in_ciao(interpreter(jvml)))). 
:- use_module(library(jvm_in_ciao(interpreter(residual_invocations))), 
	              [res_invoke/1]).
:- use_module(library(jvm_in_ciao(interpreter(loaded_classes))), 
	              [get_class/1,main_class/1]).
:- use_module(library(jvm_in_ciao(interpreter(domain))), 
	              [assign_compatible/3,not_assign_compatible/3,heap_new_OA/2,init_array/3]).

:- include(library(jvm_in_ciao(interpreter(heap_operations_i)))).
:- include(library(jvm_in_ciao(interpreter(extended_lists_i)))).
:- include(library(jvm_in_ciao(interpreter(domain_i)))).
% See above (now is included from the module jvml).
%:- include(library(jvm_in_ciao(interpreter(jvml_i)))).
:- include(library(jvm_in_ciao(interpreter(jvm_i)))).
:- include(library(jvm_in_ciao(interpreter(mis_i)))).

check(_).

:- regtype mis/1.
mis(mis(MethodSignature,Arguments,Heap)):-
	methodSignature(MethodSignature),
	list(Arguments,value),
	heap(Heap).

% This simply avoids warnings and compilation errors. Special loop handling is not used
% in the JVM execution, just in PE.
get_loop_info(_,_,_).
