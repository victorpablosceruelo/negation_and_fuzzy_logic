% Formalization of Java semantic domain. 
% Based on The "Java (TM) Virtual Machine Specification, Second Edition, Tim Lindholm, Frank Yellin"

% numerics values
:-regtype numeric/1.
numeric(int(I)):-int_or_var(I).
numeric(byte(B)):-int_or_var(B).
numeric(short(S)):-int_or_var(S).
numeric(char(S)):-int_or_var(S). % byMiky

%MGZ: This way I define the state, heap, frame, etc compatibility types for PE time.
:- regtype int_or_var/1.
%int_or_var(X) :- int(X). % Keep an eye on this
int_or_var(X) :- term(X).

% conversions

:- trust comp i2b(I,B) : integer(I) + eval.
%:- trust comp i2b(I,B) : var(I) + memo.
:- trust comp i2b/2 + (bind_ins,sideff(free)).
i2b(I,B):-
	bin_i2b(I,B).

:- trust comp i2s(I,B) : integer(I) + eval.
%:- trust comp i2s(I,B) : var(I) + memo.
:- trust comp i2s/2 + (bind_ins,sideff(free)).
i2s(I,B):-
	bin_i2s(I,B).

:- trust comp l2i(I,B) : integer(I) + eval.
%:- trust comp l2i(I,B) : var(I) + memo.
:- trust comp l2i/2 + (bind_ins,sideff(free)).
l2i(I,B):-
	bin_l2i(I,B).

:- trust comp f2i(I,B) : integer(I) + eval.
%:- trust comp f2i(I,B) : var(I) + memo.
:- trust comp f2i/2 + (bind_ins,sideff(free)).
f2i(F,I):-
%	bin_f2i(I,B).
	I is truncate(F).


:- trust comp i2bool(I,B) : integer(I) + eval.
%:- trust comp i2bool(I,B) : var(I) + memo.
:- trust comp i2bool/2 + (bind_ins,sideff(free)).
i2bool(I,B):- 
	bin_i2bool(I,B).

% operations on int

addInt(I1,I2,R):-
	bin_addInt(I1,I2,R).
% subInt(I1,I2,R):-
% 	bin_subInt(I1,I2,R).

% comment from the JVM specification of Sun :
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
divInt(I1,I2,R):-
	bin_divInt(I1,I2,R).
mulInt(I1,I2,R):-
	bin_mulInt(I1,I2,R).
remInt(I1,I2,R):-
	bin_remInt(I1,I2,R).
*/
shlInt(I1,I2,R):-
	bin_shlInt(I1,I2,R).
shrInt(I1,I2,R):-
	bin_shrInt(I1,I2,R).
ushrInt(I1,I2,R):-
	bin_ushrInt(I1,I2,R).
orInt(I1,I2,R):-
	bin_orInt(I1,I2,R).
andInt(I1,I2,R):-
	bin_andInt(I1,I2,R).
xorInt(I1,I2,R):-
	bin_xorInt(I1,I2,R).

negInt(-2147483648,-2147483648).
	% -2147483648 = -2**31, it is not changed by the neg operation because of the representation
	% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ineg 
negInt(I1,R):-
	I1 =\= -2147483648,
	R is -I1.

% Location is the domain of adresses in the heap
:-regtype location/1.
%location(loc(Loc)):-nnegint(Loc)
location(loc(Loc)):-int_or_var(Loc).

:-regtype value/1.
value(null).
value(num(Num)):-numeric(Num).
value(ref(L)):-location(L).


init_value(refType(_),null).
init_value(primitiveType(_),num(int(0))).


init_field_value(field(_,_,_,_,initialValue(int(I))),num(int(I))).
init_field_value(field(_,_,_,_,initialValue(null)),null).
init_field_value(field(fieldSignature(_,Type),_,_,_,initialValue(undef)),Default):-
	init_value(Type,Default).

% Domain of local variables
% localVar_XXX : operations on local variables
:-regtype localVar/1.
localVar(L):-list(L,value).

:- trust comp localVar_get(L,X,V): (list(L), ground(X)) + eval.
:- trust comp localVar_get(L,X,V) + sideff(free).
localVar_get(L,X,V):-
	N is X+1,
	nth(N,L,V).

:- trust comp localVar_update(L1,X,V,L2): (list(L1), ground(X)) + eval.
:- trust comp localVar_update/4 + sideff(free).
localVar_update(Old,X,V,New):-
	N is X+1,
	update(Old,N,V,New).

% operandstack_XXX : operations on the stack of operands
:-regtype operandstack/1.
operandstack(OS):-list(OS,value).
operandstack_empty([]).
operandstack_push(V,OS,[V|OS]).
operandstack_size(OS,N):-length(OS,N).

% heap_XXX : operations on the heap
:-regtype heap/1.
%heap(H):-term(H).
heap(heap(dynamicHeap(DH),staticHeap(SH))):-
	list(DH,dynamicCell), % allocated with new, anew, etc. instructions
	list(SH,staticCell). % static fields
:-regtype dynamicCell/1.
dynamicCell(array(LT,Values)):-
	locationType(LT),
	arrayValues(Values).
%	list(Values,value).
% MGZ: Values could be a variable rather than a list in PE time.
%dynamicCell(array(LT,Values)):-
%	locationType(LT),
%	var(Values).
dynamicCell(object(LT,O)):-
	locationType(LT),
	list(O,objectField). % an object is a list of field values
:- regtype arrayValues/1.
arrayValues(L) :- term(L).
arrayValues([]).
arrayValues([V|L]) :- value(V),arrayValues(L).
:-regtype objectField/1.
objectField(objectField(FS,V)):-
	fieldSignature(FS),
	value(V).
:-regtype staticCell/1.
staticCell(staticCell(FS,V)):-
	fieldSignature(FS),
	value(V).
:-regtype addressingMode/1.
addressingMode(staticField(FS)):-
	fieldSignature(FS).
addressingMode(dynamicField(L,FS)):-
	location(L),
	fieldSignature(FS).
addressingMode(arrayElement(L,I)):-
	location(L),
	nnegint(I).
%MGZ: 'I' could be a variable in E time
addressingMode(arrayElement(L,I)):-
	location(L),
	term(I).
:-regtype locationType/1.
locationType(locationObject(CL)):-
	className(CL).
locationType(locationArray(Length,T)):-
	nnegint(Length),
	type(T).
%MGZ: Length could be a variable in PE time
locationType(locationArray(Length,T)):-
	term(Length),
	type(T).
/*
% type of a non ground heap sufficiently instantiated to be accessed
% (basically, with unknown values but with a known structure)
:- prop heap_ng/1.
heap_ng(heap(dynamicHeap(DH),staticHeap(SH))):-
	list(DH,dynamicCell_ng), % allocated with new, anew, etc. instructions
	list(SH,staticCell_ng). % static fields

:- prop dynamicHeap_ng/1.
dynamicHeap_ng(DH) :- list(DH,dynamicCell_ng).

:- prop staticHeap_ng/1.
staticHeap_ng(SH) :- list(SH,staticCell_ng).

:- prop dynamicCell_ng/1.
dynamicCell_ng(array(LT,Values)):-
	locationType(LT),
	arrayValues(Values).
dynamicCell_ng(object(LT,O)):-
	locationType(LT),
	list(O,objectField_ng). % an object is a list of field values
:- prop objectField_ng/1.
objectField_ng(objectField(FS,V)):-
	fieldSignature(FS),
	value_ng(V).
:- prop staticCell_ng/1.
staticCell_ng(staticCell(FS,V)):-
	fieldSignature(FS),
	value_ng(V).
:- prop value_ng/1.
value_ng(V):-
	var(V).
value_ng(null).
value_ng(num(Num)):-
	numeric_ng(Num).
value_ng(ref(L)):-
	location(L).
:- prop numeric_ng/1.
numeric_ng(int(I)):-integer(I).
numeric_ng(byte(B)):-integer(B).
numeric_ng(short(S)):-integer(S).
numeric_ng(char(S)):-integer(S). %byMiky
numeric_ng(int(I)):-var(I).
numeric_ng(byte(B)):-var(B).
numeric_ng(short(S)):-var(S).
numeric_ng(char(S)):-var(S). %byMiky
*/

% MGZ: They are not neccesary as they are always called in eval contexts
%:- trust comp has_staticCell(FS,SH) : (ground(FS),staticHeap(SH)) + eval.
%:- trust comp has_staticCell(FS,SH) + sideff(free).
%:- trust comp has_not_dynamicField(A,B,C) : (ground(A),ground(B),dynamicHeap(C)) + eval.
%:- trust comp has_not_dynamicField/3 + sideff(free).
%:- trust comp has_arrayElement(A,B,C) : (ground(A),ground(B),dynamicHeap(C)) + eval.
%:- trust comp has_arrayElement/3 + (sideff(free),bind_ins).
%:- trust comp has_not_arrayElement(A,B,C): (ground(A),ground(B),dynamicHeap(C)) + eval.
%:- trust comp has_not_arrayElement/3 + (sideff(free),bind_ins).
%has_staticCell(FS,SH):-
%	member(staticCell(FS,_),SH).
%has_not_dynamicField(L,FS,DH):-
%	nth(L,DH,object(_,O)),
%	\+ member(objectField(FS,_),O).
%has_arrayElement(L,I,DH):-
%	nth(L,DH,array(_,A)),
%	IPlus1 is I + 1,
%	nth(IPlus1,A,_).
%has_not_arrayElement(L,I,DH) :-
%	\+ has_arrayElement(L,I,DH).

%:- trust comp heap_get(H,AM,_) : (ground(AM)) + eval.
%:- trust comp heap_get/3 + sideff(free).
heap_get(H,staticField(FS),V):-
	get_static_field(H,FS,V).
%	member(staticCell(FS,V),SH).
%heap_get(heap(_DH,staticHeap(SH)),staticField(FS),none):-
%	\+ has_staticCell(FS,SH). 
heap_get(H,dynamicField(loc(L),FS),V):-
	get_field(H,L,FS,V).
%	nth(L,DH,object(_LT,O)),
%	member(objectField(FS,V),O).
%heap_get(heap(dynamicHeap(DH),_SH),dynamicField(loc(L),FS),none):-
%	has_not_dynamicField(L,FS,DH).
heap_get(H,arrayElement(loc(L),I),V):-
	get_array_elem(H,L,I,V).
%	nth(L,DH,array(_LT,A)),
%	IPlus1 is I + 1, % res_susc
%	mynth(IPlus1,A,V). % res_susc
%heap_get(heap(dynamicHeap(DH),_SH),arrayElement(loc(L),I),none):-
%	\+ has_arrayElement(L,I,DH).

%:- trust comp heap_update(H,AM,_V,_) : (ground(AM)) + eval.
%:- trust comp heap_update/4 + sideff(free).
heap_update(H,staticField(FS),V,Hnew):-
	set_static_field(H,FS,V,Hnew).
%	member(staticCell(FS,Vold),SH),
%	replace(SH,staticCell(FS,Vold),staticCell(FS,V),SHnew).
heap_update(H,dynamicField(loc(L),FS),V,Hnew):-
	set_field(H,L,FS,V,Hnew).
%	nth(L,DH,object(LT,Oold)),
%	member(objectField(FS,Vold),Oold),
%	replace(Oold,objectField(FS,Vold),objectField(FS,V),Onew),
%	update(DH,L,object(LT,Onew),DHnew).
heap_update(H,arrayElement(loc(L),I),V,Hnew):-
	set_array_elem(H,L,I,V,Hnew).
%	nth(L,DH,array(LT,Aold)),
%	IPlus1 is I + 1, % res_susc
%	update(Aold,IPlus1,V,Anew), % res_susc
%	update(DH,L,array(LT,Anew),DHnew).

%:- trust comp heap_typeof(H,Loc,LT) : (ground(Loc)) + eval.
%:- trust comp heap_typeof/3 + sideff(free).
heap_typeof(H,loc(Location),locationArray(Lenght,Type)):-
	get_array(H,Location,array(locationArray(Lenght,Type),_)).
%	nth(Location,DH,array(LocationType,_)).
heap_typeof(H,loc(Location),locationObject(Cn)):-
	get_object(H,Location,object(locationObject(Cn),_)).
%	nth(Location,DH,object(LocationType,_)).
%heap_typeof(heap(dynamicHeap(DH),_SH),loc(Location),none):-
%	\+ nth(Location,DH,array(_LocationType,_)),
%	\+ nth(Location,DH,object(_LocationType,_)).


%:- trust comp heap_new/4 + (eval,sideff(free)).
heap_new(Heap,locationObject(CL),loc(Location),HeapNew) :-
	create_object(Heap,CL,Location,HeapNew).
%% 	program_class(CL,CurrentClass),
%% 	class_fields(CurrentClass,Fields),
%% 	heap_new_OA(Fields,Object),
%% 	create_object(Heap,object(locationObject(CL),Object),Location,HeapNew).
%% %	append(DH,[object(locationObject(CL),Object)],DHNew),
%% %	length(DHNew,Location),
%% %	HeapNew=heap(dynamicHeap(DHNew),SH).
heap_new(Heap,locationArray(Length,Type),loc(Location),HeapNew):-
	create_array(Heap,Type,Length,Location,HeapNew).
%% 	init_array(Length,Type,NewArray),
%% 	create_array(Heap,array(locationArray(Length,Type),NewArray),Location,HeapNew).
%% %	append(DH,[array(locationArray(Length,Type),NewArray)],DHNew),
%% %	length(DHNew,Location),
%% %	HeapNew=heap(dynamicHeap(DHNew),SH).

% Domain of frames
:- regtype frame/1.
frame(fr(Method,PC,OperandStack,LocalVar)):-
	method(Method),
	pc(PC),
	operandstack(OperandStack),
	localVar(LocalVar).

% Domain of call stacks
:- regtype callstack/1.
callstack(CS):-
	list(CS,frame). %MGZ: Keep an eye on this when handling recursion.

:- regtype exceptionFrame/1.
exceptionFrame(frE(Method,PC,Location,LocalVar)):-
	method(Method),
	pc(PC),
	location(Location),
	localVar(LocalVar).

% Domain of states
:- regtype state/1.
state(st(Heap,Frame,Callstack)):-
	heap(Heap),
	frame(Frame),
	callstack(Callstack).
state(stE(Heap,ExceptionFrame,Callstack)):-
	heap(Heap),
	exceptionFrame(ExceptionFrame),
	callstack(Callstack).

state_get_sf(st(_,_,CS),CS).
state_get_sf(stE(_,_,CS),CS).

state_get_m(st(_,fr(M,_,_,_),_),M).
state_get_m(stE(_,frE(M,_,_,_),_),M).

state_get_pc(st(_,fr(_,PC,_,_),_),PC).
state_get_pc(stE(_,frE(_,PC,_,_),_),PC).

/*
% assign_compatible_num source target holds if a numeric value source 
% can be assigned to a variable of type target.
% This point is not clear in the JVM spec.
:- trust comp assign_compatible_num/2 + (eval,sideff(free)).
assign_compatible_num(num(int(_)),primitiveType(int)).
assign_compatible_num(num(char(_)),primitiveType(int)).
assign_compatible_num(num(short(_)),primitiveType(int)).
assign_compatible_num(num(byte(_)),primitiveType(int)).
assign_compatible_num(num(short(_)),primitiveType(short)).
assign_compatible_num(num(byte(_)),primitiveType(byte)).
assign_compatible_num(num(byte(_)),primitiveType(boolean)).
assign_compatible_num(num(char(_)),primitiveType(char)).
*/

:- trust comp compatible_param(_,Vs,_) : ground(Vs) + (eval,sideff(free)).
compatible_param(_H,[],[]).
compatible_param(H,[V|VL],[T|TL]):-
	assign_compatible(H,V,T),
	compatible_param(H,VL,TL).
	
:- trust comp isReference/1 + (eval,sideff(free)).
%isReference(null).
%isReference(ref(Loc)):-value(ref(Loc)).
isReference(_).

eqInt(I1,I1).

neInt(I1,I2):-
	I1 \= I2.
ltInt(I1,I2):-
	I1 < I2.
leInt(I1,I2):-
	I1 =< I2.
geInt(I1,I2):-
	I1 >= I2.
gtInt(I1,I2):-
	I1 > I2.

:- trust comp semBinopInt(Op,I1,I2,R) : (ground(Op),ground(I1),ground(I2)) + eval.
:- trust comp semBinopInt(Op,I1,I2,R) + sideff(free).
%% higher-order is a problem for Partial Evaluation
% semBinopInt(Op,I1,I2,R):-
% 	Op(I1,I2,R).
semBinopInt(addInt,I1,I2,R):-
	R is I1 + I2. % addInt(I1,I2,R).
semBinopInt(subInt,I1,I2,R):-
	R is I1 - I2. % subInt(I1,I2,R).
semBinopInt(divInt,I1,I2,R):-
	R is I1 // I2. % divInt(I1,I2,R).
semBinopInt(mulInt,I1,I2,R):-
	R is I1 * I2. % mulInt(I1,I2,R).
semBinopInt(remInt,I1,I2,R):-
	R is I1 rem I2. % remInt(I1,I2,R).
semBinopInt(andInt,I1,I2,R):-
	andInt(I1,I2,R).
semBinopInt(orInt,I1,I2,R):-
	orInt(I1,I2,R).
semBinopInt(shlInt,I1,I2,R):-
	shlInt(I1,I2,R).
semBinopInt(shrInt,I1,I2,R):-
	shrInt(I1,I2,R).
semBinopInt(ushrInt,I1,I2,R):-
	ushrInt(I1,I2,R).
semBinopInt(xorInt,I1,I2,R):-
	xorInt(I1,I2,R).

:- trust comp semCompInt(Op,I1,I2) : (ground(Op),ground(I1),ground(I2)) + eval.
:- trust comp semCompInt(Op,I1,I2) + sideff(free).
%% higher-order is a problem for Partial Evaluation
% semCompInt(Op,I1,I2):-
% 	Op(I1,I2).
semCompInt(eqInt,I1,I2):-
	eqInt(I1,I2).
semCompInt(neInt,I1,I2):-
	neInt(I1,I2).
semCompInt(ltInt,I1,I2):-
	ltInt(I1,I2).
semCompInt(leInt,I1,I2):-
	leInt(I1,I2).
semCompInt(geInt,I1,I2):-
	geInt(I1,I2).
semCompInt(gtInt,I1,I2):-
	gtInt(I1,I2).

:- trust comp noSemCompInt(Op,I1,I2) : (ground(Op),ground(I1),ground(I2)) + eval.
:- trust comp noSemCompInt(Op,I1,I2) + sideff(free).
noSemCompInt(eqInt,I1,I2):-
	neInt(I1,I2).
noSemCompInt(neInt,I1,I2):-
	eqInt(I1,I2).
noSemCompInt(ltInt,I1,I2):-
	geInt(I1,I2).
noSemCompInt(leInt,I1,I2):-
	gtInt(I1,I2).
noSemCompInt(geInt,I1,I2):-
	ltInt(I1,I2).
noSemCompInt(gtInt,I1,I2):-
	leInt(I1,I2).


