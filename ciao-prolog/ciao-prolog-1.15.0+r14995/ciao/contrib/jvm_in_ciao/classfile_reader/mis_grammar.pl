:- module(mis_grammar,[parse_mis/3,parse_mis/2,mis_methodName/2],[dcg]).

:- use_module(library(filenames)).
:- use_module(library(read_from_string), [read_from_atom_atmvars/2]).
:- use_module(library(lists)).

:- use_module(library(jvm_in_ciao(interpreter(jvml))), 
	[program_class/2,class_method/3,class_fields/2,field_signature/2]).

% Parameters = formal parameters
% Arguments = effective parameters

:- data mis_flag/1. % Possible flags: {non_modular,modular}

mis_methodName(Mis,MethodName) :-
	atom_codes(Mis,SMis),
	mis_methodName_(MethodName,SMis,"").
mis_methodName_(MethodName) -->
	parse_result(_),
	parse_methodName(methodName(_,shortMethodName(MethodName))),
	parse_class(_),!.

parse_mis(M,D) :- parse_mis(M,D,non_modular).

parse_mis(mis(MethodSignature,Arguments,Heap),D,Flag) :-
	retractall_fact(mis_flag(_)),
	((Flag == non_modular ; Flag == modular) 
	    -> set_fact(mis_flag(Flag))
	    ;  set_fact(mis_flag(non_modular))),
	Heap = heap(dynamicHeap(DynHeap_p),staticHeap([])),
	atom_codes(D,SD),
	parse_mis_(MethodSignature,Arguments,DynHeap,SD,""),
	(DynHeap == [],mis_flag(modular) -> true
	                               ; DynHeap_p = DynHeap).

parse_mis_(methodSignature(MethodName,Parameters,Result),Arguments,DynHeap) --> 
	parse_result(Result), 
	%" ", 
	parse_methodName(MethodName),
	"(", 
	parse_params([],Parameters,[],ArgumentsPre,[],DynHeapPre),
	")",
	{init_this_object(methodSignature(MethodName,Parameters,Result),
	                  ArgumentsPre,Arguments,DynHeapPre,DynHeap)}.

init_this_object(MethodSignature,ArgsPre,Args,HeapPre,Heap) :-
	MethodSignature = methodSignature(methodName(ClassName,_),_,_),
	program_class(ClassName,Class),
	class_method(Class,MethodSignature,Method),
	Method = method(_,_,_,static(IsStatic),_),
	init_this_object_(ClassName,IsStatic,ArgsPre,Args,HeapPre,Heap).

init_this_object_(_,true,Args,Args,H,H).
init_this_object_(ClassName,false,ArgsPre,[Arg|ArgsPre],HeapPre,Heap) :-
	(mis_flag(non_modular)
	  -> update_heap(ClassName,HeapPre,Heap,Arg)
	  ; Arg = ref(loc(_))).

parse_methodName(methodName(ClassName,shortMethodName(SMN))) --> 
	parse_className(ClassName),
	".",
	parse_string(SMN).

parse_className(className(packageName(PackageName),shortClassName(SCN))) -->
	parse_string(PackClass),
	{no_path_file_name(PackClass,SCN),atom_concat(PackageName,SCN,PackClass)}.

parse_result(none) --> "void ",!.
parse_result(F) --> parse_field_type(F).

parse_params(TsAcu,Ts,VsAcu,Vs,HsAcu,Hs) --> 
	parse_field_type(T), 
	%" ",
	parse_value(V,T,HsAcu,NewHsAcu),
	",",
	%!,
	parse_params([T|TsAcu],Ts,[V|VsAcu],Vs,NewHsAcu,Hs).
parse_params(TsAcu,Ts,VsAcu,Vs,HsAcu,NewHsAcu) --> 
	parse_field_type(T), 
	%" ",
	parse_value(V,T,HsAcu,NewHsAcu),
	{reverse([T|TsAcu],Ts),reverse([V|VsAcu],Vs)}.
parse_params([],[],[],[],[],[]) --> "".

parse_value(num(int(V)),primitiveType(int),H,H) --> 
	parse_string(S),{read_from_atom_atmvars(S,V),integer(V)}.
parse_value(num(int(_)),primitiveType(int),H,H) --> "_".
parse_value(ref(loc(N)),refType(arrayType(primitiveType(int))),H,NewH) --> 
	"r",{(mis_flag(non_modular)
	       -> (append(H,[(N,array(locationArray(_,primitiveType(int)),_))],NewH),
	           length(H,N))
	       ; true)}.
parse_value(Ref,refType(classType(ClassName)),H,NewH) --> 
	"r",{(mis_flag(non_modular)
	       -> update_heap(ClassName,H,NewH,Ref)
	       ; Ref = ref(loc(_)))}.
parse_value(ref(loc(N)),refType(arrayType(primitiveType(int))),H,NewH) --> 
	parse_array(Size,Elements),
	{(mis_flag(non_modular)
	   -> (append(H,[(N,array(locationArray(Size,primitiveType(int)),Elements))],
	             NewH),
	       length(H,N))
	   ; true)}.
parse_value(ref(loc(N)),refType(arrayType(primitiveType(int))),H,NewH) --> 
	"_",{(mis_flag(non_modular)
	       -> (append(H,[(N,array(locationArray(_,primitiveType(int)),_))],NewH),
	           length(H,N))
	       ; true)}.
parse_value(Ref,refType(classType(ClassName)),H,NewH) --> 
	"_",{(mis_flag(non_modular)
	       -> (update_heap(ClassName,H,NewH,Ref))
	       ; Ref = ref(loc(_)))}.
parse_value(null,refType(_),H,H) --> 
	"null".
parse_value(null,refType(_),H,H) --> 
	"_".

parse_string(S) --> 
	parse_string_(S1),
	{atom_codes(S,S1)}.
parse_string_([S|Ss]) --> 
	[S],
	{member(S,"/0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_-")},
	!,
	parse_string_(Ss).
parse_string_([]) --> "".

parse_field_type(F) --> parse_base_type(F)," ",!.
parse_field_type(F) --> parse_array_type(F)," ",!.
parse_field_type(F) --> parse_object_type(F)," ".

parse_base_type(primitiveType(byte)) --> "byte".
parse_base_type(primitiveType(char)) --> "char".
parse_base_type(primitiveType(double)) --> "double".
parse_base_type(primitiveType(float)) --> "float".
parse_base_type(primitiveType(int)) --> "int".
parse_base_type(primitiveType(long)) --> "long".
parse_base_type(primitiveType(short)) --> "short".
parse_base_type(primitiveType(boolean)) --> "boolean".

parse_object_type(refType(classType(className(packageName(Package),
 	                                      shortClassName(ClassOnly))))) --> 
 	parse_class(Class),
 	{atom_codes(ClassAtom,Class),
 	no_path_file_name(ClassAtom,ClassOnly),
 	atom_concat(Package,ClassOnly,ClassAtom)}.

parse_array_type(refType(arrayType(F))) --> parse_base_type(F),"[]".
parse_array_type(refType(arrayType(F))) --> parse_object_type(F),"[]".


parse_class([C]) --> [C].
parse_class([C|R]) --> [C],parse_class(R).

parse_array(0,[]) --> "[]",!.
parse_array(Size,Es) --> 
	"[",parse_array_(0,[],Size,Es).
parse_array_(NAc,EsAc,N,Es) -->
	parse_string(S),
	",",
	{read_from_atom_atmvars(S,V),integer(V),NewAc is NAc + 1},
	parse_array_(NewAc,[num(int(V))|EsAc],N,Es).
parse_array_(NAc,EsAc,N,Es) -->
	parse_string(S),
	"]",
	{read_from_atom_atmvars(S,V),integer(V),N is NAc + 1,
	 reverse([num(int(V))|EsAc],Es)}.

%I am not handleing yet cases in which an array is a member of an object.
update_heap(ClassName,H,NewH,ref(loc(N))) :-
	length(H,N),%NPlus1 is N+1,
	program_class(ClassName,Class),
	class_fields(Class,Fields),
	append(H,[(N,object(locationObject(ClassName),ObjectFields))],H2),
	update_with_fields(Fields,H2,NewH,[],ObjectFields).

update_with_fields([],H,H,OFR,OF) :- reverse(OFR,OF).
update_with_fields([Field|Fields],H,NewH,OFs,NewOFs) :-
	update_with_field(Field,H,HPrime,OF),
	update_with_fields(Fields,HPrime,NewH,[OF|OFs],NewOFs).

update_with_field(Field,H,H,objectField(Signature,num(Num))) :-
	field_type(Field,primitiveType(Type)),
	field_signature(Field,Signature),
	Num =..[Type,_].
update_with_field(Field,H,NewH,objectField(Signature,Ref)) :-
	field_type(Field,refType(classType(ClassName))),
	field_signature(Field,Signature),
	update_heap(ClassName,H,NewH,Ref).

field_type(field(fieldSignature(_,Type),_Final,_Static,_Visibility,_Init),Type).
