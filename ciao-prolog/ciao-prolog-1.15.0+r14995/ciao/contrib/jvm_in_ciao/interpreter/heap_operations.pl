:- module(heap_operations,[get_object/3,get_array/3,get_field/4,get_static_field/3,
			   get_array_elem/4,set_array_elem/5,set_field/5,
	                   set_static_field/4,create_object/4,create_array/5,create_string/4,
			   reset_counter/0,infer_counter/1,new_ref/1,
			   filter_heap/2,build_heapout_struct/2], [assertions]).

:- use_module(library(jvm_in_ciao(interpreter(extended_lists))), 
	      [update/4,replace_first/4,member_g/2,nth_g/3]).
:- use_module(library(jvm_in_ciao(interpreter(domain))), [heap_new_OA/2, init_array/3]).
:- use_module(library(jvm_in_ciao(interpreter(jvml))), [javaLangString/1, program_class/2, class_fields/2]).
:- use_module(library(term_filtering)).
:- use_module(library(lists), [list_to_list_of_lists/2, length/2]).

:- doc(title,"Set of basic operations over the heap (represented as a term)").

:- doc(author, "M. Zamalloa").

:- doc(module,"This module provides basic operations over the heap which is 
	represented as a 'heap' term which basically consists in lists. Operations 
	includes objects, arrays and fields creation, accessing and setting").

:- doc(representation,"Heap is represented as a heap term. For instance, 
	a heap with one Rational object in location '3' and an array in location '2' 
	might be:
    heap(dynamicHeap([(3,array(locationArray(_,primitiveType(int)),_)),
                      (2,object(locationObject(className(packageName(''),shortClassName('Rational'))),[objectField(fieldSignature(fieldName(className(packageName(''),shortClassName('Rational')),shortFieldName(num)),primitiveType(int)),num(int(_))),objectField(fieldSignature(fieldName(className(packageName(''),shortClassName('Rational')),shortFieldName(den)),primitiveType(int)),num(int(_)))])]),staticHeap([]))").

:- data counter/1.

% Uncomment this line to residualize heap ops
:- include('heap_ops_memos.pl').

:- include('heap_operations_i.pl').

/*
:- pred cleanup_heap #"It retracts the counter fact".
:- trust comp cleanup_heap/0 + (eval,sideff(free)).
cleanup_heap :-
	retractall_fact(counter(_)).
*/

:- pred reset_counter #"It initialize the counter to 0".
:- trust comp reset_counter/0 + (eval,sideff(free)).
reset_counter :-
	set_fact(counter(0)).

:- pred get_counter(C) #"@var{C} is the current counter value".
get_counter(C) :-
	current_fact(counter(C)).

:- pred inc_counter # "The counter is incremented by one".
inc_counter :-
	current_fact(counter(C)),
	NewC is C+1,
	set_fact(counter(NewC)).

:- pred new_ref/1 # "A new fresh reference is obtained. It will remain residual in PE".
:- trust comp new_ref/1 + (memo,bind_ins,sideff(free)).
new_ref(Ref) :- 
	get_counter(Ref), 
	inc_counter.

:- pred infer_counter(Args) #"@It infers and sets the heap counter from the 
	arguments list @var{Args}".
:- trust comp infer_counter/1 + (eval,sideff(free)).
infer_counter([]) :- !.
infer_counter([num(_)|Args]) :- !,infer_counter(Args).
infer_counter([ref(Ref)|Args]) :- 
	(ground(Ref) -> inc_counter ; true),
	infer_counter(Args). 
infer_counter([null|Args]) :- !,infer_counter(Args).

:- trust comp filter_heap/2 + (eval,sideff(free)).
filter_heap(H,H) :- var(H),!.
filter_heap(heap(dynamicHeap(DH),_),FH) :- 
	filter_dynHeap(DH,FH).

filter_dynHeap(H,H) :- var(H),!.
filter_dynHeap([],[]).
filter_dynHeap([O|DH],[(R,Fs_p)|FH]) :-
	filter_term(O,(0,object(1,[objectField(1,0)])),_,[R,[Fs]]),!,
	list_to_list_of_lists(Fs_p,Fs),
	filter_dynHeap(DH,FH).
filter_dynHeap([O|DH],[(R,array(L,FO))|FH]) :-
	filter_term(O,(0,array(locationArray(0,1),0)),_,[R,[[L],FO]]),!,
	filter_dynHeap(DH,FH).
filter_dynHeap([O|DH],[FO|FH]) :-
	O = (R,object(_,[])),!,
	FO = (R,[]),
	filter_dynHeap(DH,FH).

:- pred build_heapout_struct(H,H_p) #"It builds a convenient structure of the heap 
	after a method invocation. It keeps the known part of the heap and 
	generalizes all the objects in it".
:- trust comp build_heapout_struct/2 + (eval,sideff(free)).
build_heapout_struct(heap(dynamicHeap(DH),SH),heap(dynamicHeap(DH_p),SH)) :-
	build_dynheapout_struct(DH,DH_p).

build_dynheapout_struct(H,_H_p) :-
	var(H),!.
build_dynheapout_struct([],[]).
build_dynheapout_struct([Ob|RH],[GOb|RH_p]) :-
	filter_term(Ob,(1,object(1,[objectField(1,0)])),GOb,gen),!,
	build_dynheapout_struct(RH,RH_p).
build_dynheapout_struct([Ob|RH],[GOb|RH_p]) :-
	filter_term(Ob,(1,array(locationArray(1,1),0)),GOb,gen),!,
	build_dynheapout_struct(RH,RH_p).
build_dynheapout_struct([Ob|RH],[Ob|RH_p]) :-
	build_dynheapout_struct(RH,RH_p).
	
