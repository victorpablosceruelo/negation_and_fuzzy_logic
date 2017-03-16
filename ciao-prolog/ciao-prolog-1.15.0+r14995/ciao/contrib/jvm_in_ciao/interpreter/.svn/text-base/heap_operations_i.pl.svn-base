
:- pred create_object(H,Ob,Loc,Hnew) # "It puts a new object @var{Ob} of class @var{CN}
	in the heap and returns its assigned location @var{Loc}".
create_object(heap(dynamicHeap(DH),SH),CN,Loc,
	      heap(dynamicHeap([(Loc,Object)|DH]),SH)) :- 
	program_class(CN,ClassTerm),
	class_fields(ClassTerm,FieldSigs),
	heap_new_OA(FieldSigs,Fields),
	Object = object(locationObject(CN),Fields),
	new_ref(Loc). 

:- pred create_array(H,Type,Length,Loc,Hnew) #"It puts a new array
	@var{Arr} of type @var{Type} and length @var{Length} in the
	heap and returns its assigned location @var{Loc}".

create_array(heap(dynamicHeap(DH),SH),Type,Length,Loc,
	     heap(dynamicHeap([(Loc,array(locationArray(Length,Type),Arr))|DH]),SH)) :- 
	init_array(Length,Type,Arr),
	new_ref(Loc).

:- pred get_object(H,Loc,Ob) #"Object at location @var{Loc} is @var{Ob}".
get_object(heap(dynamicHeap(DH),_),Loc,Ob_p) :- 
	get_(DH,Loc,Ob,Flag),
	residualize_get(Flag,DH,Loc,Ob,Ob_p).

:- trust comp get_/4 + (eval,bind_ins,sideff(free)).
get_(DH,_,_,false) :- var(DH),!.
get_([(Ref1,Ob)|_],Ref2,Ob,true) :- Ref1 == Ref2,!.
get_([(_,_)|HR],Ref2,Ob2,Flag) :- get_(HR,Ref2,Ob2,Flag).

residualize_get(true,_,_,Ob,Ob).
residualize_get(false,DH,Loc,_,Ob_p) :- member_g((Loc,Ob_p),DH).

:- pred get_array(H,Loc,Arr) #"There is an array @var{Arr} at location @var{Loc}".
get_array(heap(dynamicHeap(DH),_),Loc,Arr_p) :- 
	get_(DH,Loc,Arr,Flag),
	residualize_get(Flag,DH,Loc,Arr,Arr_p).

:- pred get_field(H,Loc,FS,V) #"The field @var{FS} of the object at location @var{Loc} 
	  has the value @var{V}".
get_field(H,Loc,FS,V_p) :- 
	get_object(H,Loc,object(_LocObj,Fields)),
	get_field_(Fields,FS,V,Flag),
	residualize_get_field(Flag,Fields,FS,V,V_p).

:- trust comp get_field_/4 + (eval,bind_ins,sideff(free)).
get_field_(Fields,_,_,false) :- var(Fields),!.
get_field_([objectField(FS,V)|_],FS,V,true) :- !.
get_field_([objectField(_,_)|FieldsR],FS2,V,Flag) :- 
	get_field_(FieldsR,FS2,V,Flag).

residualize_get_field(true,_,_,V,V).
residualize_get_field(false,Fields,FS,_,V_p) :- 
	member_g(objectField(FS,V_p),Fields).

:- pred get_array_elem(H,Loc,I,V) #"It returns in @var{V} the I-th element of the 
	array in @var{Loc}".
get_array_elem(H,Loc,I,V_p) :- 
	get_array(H,Loc,array(_LocArr,Vs)),
	IPlus1 is I + 1, %res_susc
	get_array_elem_(Vs,IPlus1,V,Flag),
	residualize_get_array_elem(Flag,Vs,IPlus1,V,V_p).

:- trust comp get_array_elem_/4 + (eval,bind_ins,sideff(free)).
get_array_elem_(Vs,_I,_,false) :- var(Vs),!.
get_array_elem_(_Vs,I,_,false) :- var(I),!.
get_array_elem_(Vs,I,V,true) :- nth_g(I,Vs,V).

residualize_get_array_elem(true,_,_,V,V).
residualize_get_array_elem(false,Vs,IPlus1,_,V_p) :- 
	nth_g(IPlus1,Vs,V_p).

:- pred set_field(H,Loc,FS,V,Hnew) #"It writes the value @var{V} in the field @var{FS} 
	  of the object at location @var{Loc}".
set_field(H,Loc,FS,V,heap(dynamicHeap(DHnew_p),staticHeap(SH))) :- 
	get_object(H,Loc,object(LocObj,Fields)),
	H = heap(dynamicHeap(DH),staticHeap(SH)),
	set_field_(DH,LocObj,Fields,Loc,FS,V,DHnew,Flag),
	residualize_set_field(Flag,DH,LocObj,Fields,Loc,FS,V,DHnew,DHnew_p).

:- trust comp set_field_/8 + (eval,bind_ins,sideff(free)).
set_field_(_,_,Fields,_,_FS,_,_DHn,false) :- var(Fields),!.
set_field_(DH,LocObj,Fields,Loc,FS,V,DHnew,true) :-
	replace_first(Fields,objectField(FS,_),objectField(FS,V),NewFields),
	replace_first(DH,(Loc,object(_LocObj,_Fields)),
	                 (Loc,object(LocObj,NewFields)),DHnew).

residualize_set_field(true,_,_,_,_,_FS,_,DHnew,DHnew).
residualize_set_field(false,DH,LocObj,Fields,Loc,FS,V,_,DHnew_p) :-
	replace_first(Fields,objectField(FS,_),objectField(FS,V),NewFields),
	replace_first(DH,(Loc,object(_LocObj,_Fields)),
	                 (Loc,object(LocObj,NewFields)),DHnew_p).

:- pred set_array_elem(H,Loc,I,V,Hnew) #"It replaces the I-th element of the array in 
	@var{Loc} by @var{V}".
set_array_elem(H,Loc,I,V,heap(dynamicHeap(DHnew_p),staticHeap(SH))) :- 
	get_array(H,Loc,array(LA,Elems)),
	H = heap(dynamicHeap(DH),staticHeap(SH)),
	IPlus1 is I + 1,
	set_array_elem_(DH,LA,Elems,Loc,IPlus1,V,NewElems,DHnew,Flag),
	residualize_set_array_elem(Flag,DH,LA,Elems,NewElems,Loc,IPlus1,V,DHnew,DHnew_p).

:- trust comp set_array_elem_/9 + (eval,bind_ins,sideff(free)).
set_array_elem_(_,_,Elems,_,_,_,_,_DHn,false) :- var(Elems),!.
set_array_elem_(DH,LA,_Elems,Loc,IPlus1,_,NewElems,DHnew,med) :- var(IPlus1),!,
	replace_first(DH,(Loc,array(_LA,_Elems)),(Loc,array(LA,NewElems)),DHnew).
set_array_elem_(DH,LA,Elems,Loc,IPlus1,V,_,DHnew,true) :-
	update(Elems,IPlus1,V,NewElems),
	replace_first(DH,(Loc,array(_LA,_Elems)),(Loc,array(LA,NewElems)),DHnew).

residualize_set_array_elem(true,_,_,_,_,_,_I,_,DHnew,DHnew).
residualize_set_array_elem(med,_,_,Elems,NewElems,_,IPlus1,V,DHnew,DHnew) :-
	update(Elems,IPlus1,V,NewElems).
residualize_set_array_elem(false,DH,LA,Elems,_,Loc,IPlus1,V,_,DHnew_p) :-
	update(Elems,IPlus1,V,NewElems),
	replace_first(DH,(Loc,array(_LA,_Elems)),(Loc,array(LA,NewElems)),DHnew_p).


:- pred get_static_field(H,FS,V) #"The static field @var{FS} has the value @var{V}".
%:- trust comp get_static_field(_H,FS,_V) : (ground(FS)) + eval.
:- trust comp get_static_field/3 + (bind_ins,sideff(free)).
get_static_field(heap(_,staticHeap(SH)),FS,V) :- 
	member_g((FS,V),SH).

:- pred set_static_field(H,FS,V,Hnew) #"It writes the value @var{V} in the static 
	field @var{FS}".
%:- trust comp set_static_field(_,Loc,FS,_,_) : (ground(FS)) + eval.
:- trust comp set_static_field/4 + (bind_ins,sideff(free)).
set_static_field(H,FS,V,heap(dynamicHeap(DH),staticHeap(SHNew))) :- 
	H = heap(dynamicHeap(DH),staticHeap(SH)),
	replace_first(SH,(FS,_OldV),(FS,V),SHNew).


:- pred create_string(H,Str,Loc,Hnew) #"It creates a new string object together with the corresponding
        char array setting their corresponding fields and returns its assigned location @var{SORefLoc}".
create_string(H,StrA,SORefLoc,H_p5) :-
% Create the char array
	create_array(H,primitiveType(char),Size,CARef,H_p),
% Create a new string object in the heap
	javaLangString(StringCN),
%	StringCN = className(packageName('java/lang/'),shortClassName('String')),
	create_object(H_p,StringCN,SORefLoc,H_p2),
	create_string_evalpart(StrA,_CAVs,Size,H_p2,CARef,StringCN,SORefLoc,H_p5).

:- trust comp create_string_evalpart/8 + (eval,sideff(free)).
create_string_evalpart(StrA,CAVs,Size,H_p2,CARef,CN,SORefLoc,H_p5) :-
% set string contents by hand in the char array
	char_array_from_atom(StrA,CAVs), 
	length(CAVs,Size),
	H_p2 = heap(dynamicHeap(DH_p2),SH),
	replace_first(DH_p2,(CARef,array(locationArray(_,T),_)),
                            (CARef,array(locationArray(Size,T),CAVs)),DH_p3),
% set by hand the value and count fields with the char array and its length res
	FSValue = fieldSignature(fieldName(CN,shortFieldName(value)),
                                 refType(arrayType(primitiveType(char)))),
	FSCount = fieldSignature(fieldName(CN,shortFieldName(count)),primitiveType(int)),
	H_p3 = heap(dynamicHeap(DH_p3),SH),
%	heap_update(H_p3,dynamicField(SORefLoc,FSValue),ref(loc(CARef)),H_p4),
	set_field(H_p3,SORefLoc,FSValue,ref(loc(CARef)),H_p4),
%	heap_update(H_p4,dynamicField(SORefLoc,FSCount),Size,H_p5).
	set_field(H_p4,SORefLoc,FSCount,Size,H_p5).
% push the string object reference and return the modified heap

char_array_from_atom(StrA,CAVs) :-
	atom_codes(StrA,StrL),
	build_char_array(StrL,CAVs).
build_char_array([],[]).
build_char_array([X|R],[num(int(X))|R_p]) :-
	build_char_array(R,R_p).
