
% Operations: Heap ops, loaded_classes and jvml
:- use_module(library(jvm_in_ciao(interpreter(heap_operations))), [new_ref/1, reset_counter/0]).
:- include(library(jvm_in_ciao(interpreter(heap_operations_i)))).
:- use_module(library(jvm_in_ciao(interpreter(extended_lists))), 
	              [nth_g/3,update/4,member_g/2,replace_first/4]).
:- use_module(library(lists), [length/2]).

% We need to provide a special treatment for assign_compatible
:- use_module(library(jvm_in_ciao(interpreter(domain))), [heap_new_OA/2,init_array/3]).
:- use_module(library(jvm_in_ciao(interpreter(jvml))), [javaLangString/1,program_class/2,class_fields/2]).


% Initialization
init :- initialization.
initialization :-
	heap_operations:reset_counter.
% Here we have to load all user classes (and cleanup the loaded_classes properly?)
