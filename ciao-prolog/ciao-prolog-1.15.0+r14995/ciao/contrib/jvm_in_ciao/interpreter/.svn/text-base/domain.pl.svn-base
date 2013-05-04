:-module(domain,
	[value/1, locationType/1, location/1, numeric/1, addressingMode/1, 
	 heap/1, heap_typeof/3, heap_update/4, heap_get/3, heap_new/4, heap_new_OA/2, 
	 operandstack/1, operandstack_size/2, operandstack_push/3, operandstack_empty/1,
	 localVar/1, localVar_update/4, localVar_get/3, 
	 init_field_value/2,init_array/3,
	 state/1, frame/1, callstack/1, exceptionFrame/1, state_get_sf/2, 
	 state_get_m/2,state_get_pc/2, not_assign_compatible/3,
	 assign_compatible/3, compatible_param/3, isReference/1,
	 i2b/2, i2s/2, l2i/2, f2i/2, i2bool/2, negInt/2, addInt/3, semBinopInt/4, 
	 semCompInt/3, noSemCompInt/3],
	[assertions,regtypes,basicmodes,nativeprops]).
% Formalization of Java semantic domain. 
% Based on The "Java (TM) Virtual Machine Specification, Second Edition, Tim Lindholm, Frank Yellin"

:- use_module(library(lists), [length/2, nth/3]).
:- use_module(library(jvm_in_ciao(interpreter(extended_lists)))).
:- use_module(library(jvm_in_ciao(interpreter(binary_operations)))).
:- use_module(library(jvm_in_ciao(interpreter(jvml)))).
:- use_module(library(jvm_in_ciao(interpreter(heap_operations)))).

:-regtype numeric/1.

% conversions
:- entry i2b(in(I),-B) : int*term.
:- pred i2b(in(I),?B) : int*term => int*int.

:- entry i2s(in(I),-S) : int*term.
:- success i2s(in(I),-S) : int*term => int*int.

:- entry i2bool(in(I),-B) : int*term.
:- success i2bool(in(I),-B) : int*term => int*int.

% operations on int
:- entry addInt(in(I1),in(I2),-R) : int*int*term.
:- success addInt(in(I1),in(I2),-R) : int*int*term => int*int*int.
/*
:- entry subInt(in(I1),in(I2),-R) : int*int*term.
:- success subInt(in(I1),in(I2),-R) : int*int*term => int*int*int.
*/
% A comment from the JVM specification of Sun :
%  An int division rounds towards 0; that is, the quotient produced for int values
% in n/d is an int value q whose magnitude is as large as possible while satisfying 
% |d.q| =< |n|.
% Moreover, q is positive when |n|>=|d| and n and d have the same sign, but q is negative when
% |n| >= |d| and n and d have opposite signs. 
%  There is one special case that does not satisfy this rule: if the dividend is the
% negative integer of largest possible magnitude for the int type, and the divisor 
% is -1, then overflow occurs, and the result is equal to the dividend. Despite the 
% overflow, no exception is thrown in this case. 
/*
:- entry divInt(in(I1),in(I2),-R) : int*int*term.
:- success divInt(in(I1),in(I2),-R) : int*int*term => int*int*int.
:- entry mulInt(in(I1),in(I2),-R) : int*int*term.
:- success mulInt(in(I1),in(I2),-R) : int*int*term => int*int*int.
:- entry remInt(in(I1),in(I2),-R) : int*int*term.
:- success remInt(in(I1),in(I2),-R) : int*int*term => int*int*int.
:- entry shlInt(in(I1),in(I2),-R) : int*int*term.
:- success shlInt(in(I1),in(I2),-R) : int*int*term => int*int*int.
:- entry shrInt(in(I1),in(I2),-R) : int*int*term.
:- success shrInt(in(I1),in(I2),-R) : int*int*term => int*int*int.
:- entry ushrInt(in(I1),in(I2),-R) : int*int*term.
:- success ushrInt(in(I1),in(I2),-R) : int*int*term => int*int*int.
:- entry orInt(in(I1),in(I2),-R) : int*int*term.
:- success orInt(in(I1),in(I2),-R) : int*int*term => int*int*int.
:- entry andInt(in(I1),in(I2),-R) : int*int*term.
:- success andInt(in(I1),in(I2),-R) : int*int*term => int*int*int.
:- entry xorInt(in(I1),in(I2),-R) : int*int*term.
:- success xorInt(in(I1),in(I2),-R) : int*int*term => int*int*int.
*/
:- entry negInt/2 : int*term.
:- success negInt/2 : int*term => int*int.

% Location is the domain of adresses in the heap
:- regtype location/1.

:- regtype value/1.

:- entry init_field_value/2 : field*value.

% Domain of local variables
% localVar_XXX : operations on local variables
:- regtype localVar/1.
:- entry localVar_get/3 : localVar * variable * value.
:- entry localVar_update/4 : localVar * variable * value * term.
:- success localVar_update(_,_,_,_1) => localVar(_1). 


:-regtype operandstack/1.
:-regtype heap/1.
:-regtype dynamicCell/1.
:-regtype objectField/1.
:-regtype staticCell/1.
:-regtype addressingMode/1.
:-regtype locationType/1.

:- entry heap_get/3: heap*addressingMode*term.
:- success heap_get/3: heap*addressingMode*term => heap*addressingMode*value.
:- entry heap_update/4 : heap*addressingMode*value*term.
:- success heap_update/4 : heap*addressingMode*value*term => heap*addressingMode*value*heap.
% typeof h loc = None -> no object, no array allocated at location loc
:- entry heap_typeof/3 : heap*location*term.
:- success heap_typeof/3 => heap*location*locationType.

% program is required to compute the size of the allocated element, 
% i.e. to know the Class associated with a ClassName
:- entry heap_new/4 : heap*locationType*location*compat(heap).
:- pred heap_new/4 : ground*ground*ground*term => heap*locationType*location*heap + eval.

% Domain of frames
:- regtype frame/1.

% Domain of call stacks
:- regtype callstack/1.

:- regtype exceptionFrame/1.

% Domain of states
:- regtype state/1.


% This last predicate is a modification to the bicolano specification 
% by Laurent Hubert. Up to now (on the 24th of Apr 2006), Bicolano is not compatible
% with the JVM on this point. I've sent an e-mail to D. Pichardie and Bicolano should be
% updated shortly.
% In this version, we consider a num type can be assigned to a primitiveType because an int
% can be assign to all other types and

:- pred compatible_param(in(heap),+list(value),+list(type)).
:- comp compatible_param/3 : ground*ground*ground + eval.

:- entry semBinopInt/4 : binopInt*int*int*term.

:- entry semCompInt/3 : compInt*int*int.

:- include(library('jvm_in_ciao/interpreter/domain_i.pl')).

% assign_compatible P H source target holds if a value source can be 
% assigned to a variable of type target

:- trust comp assign_compatible(_,L,T) : (ground(L),ground(T)) + eval.
:- trust comp assign_compatible(_,L,T) : nonground(L) + memo.
:- trust comp assign_compatible(_,L,T) : nonground(T) + memo.
:- trust comp assign_compatible/3 + (bind_ins,sideff(free)).
assign_compatible(_H,null,refType(_)).
assign_compatible(H,ref(loc(Loc)),refType(T)) :-
	get_object(H,Loc,object(locationObject(CN),_)),
	compat_refType(refType(classType(CN)),refType(T)).
assign_compatible(H,ref(loc(Loc)),refType(T)) :-
	get_array(H,Loc,array(locationArray(_Lenght,TP),_)),
	compat_refType(refType(arrayType(TP)),refType(T)).
assign_compatible(_H,num(_N),primitiveType(_T)).
% This last predicate is a modification to the bicolano specification 
% by Laurent Hubert. Up to now (on the 24th of Apr 2006), Bicolano is not compatible
% with the JVM on this point. I've sent an e-mail to D. Pichardie and Bicolano should be
% updated shortly.
% In this version, we consider a num type can be assigned to a primitiveType 
% because an int can be assign to all other types and

:- trust comp not_assign_compatible(_,L,T) : (ground(L),ground(T)) + eval.
:- trust comp not_assign_compatible(_,L,T) : nonground(L) + memo.
:- trust comp not_assign_compatible(_,L,T) : nonground(T) + memo.
:- trust comp not_assign_compatible/3 + (bind_ins,sideff(free)).
not_assign_compatible(H,V,T) :-
	\+ assign_compatible(H,V,T).
%not_assign_compatible(H,ref(Loc),refType(T)):-
%	heap_typeof(H,Loc,locationObject(CN)),
%	not_compat_refType(refType(classType(CN)),refType(T)).
%not_assign_compatible(H,ref(Loc),refType(T)):-
%	heap_typeof(H,Loc,locationArray(_Length,TP)),
%	not_compat_refType(refType(arrayType(TP)),refType(T)).

:- trust comp heap_new_OA/2 + (eval,sideff(free)).
heap_new_OA([],[]).
heap_new_OA([Field|RF],[OF|RO]):-
	field_isNotStatic(Field),
	init_field_value(Field,InitialValue),
	field_signature(Field,FS),
	OF=objectField(FS,InitialValue),
	heap_new_OA(RF,RO).
heap_new_OA([Field|RF],RO):-
	field_isStatic(Field),
	heap_new_OA(RF,RO).

:- trust comp init_array/3 + (eval,sideff(free)).
init_array(Length,Type,NewArray) :-
	(ground(Length) ->
	    length(NewArray,Length),
	    init_value(Type,InitValue),
	    init(NewArray,InitValue,Length)
	;
	    true).
