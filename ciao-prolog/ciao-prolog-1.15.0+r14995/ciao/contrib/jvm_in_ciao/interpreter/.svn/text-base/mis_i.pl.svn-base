% initial_state initialize the state such that :
%  - the callstack is empty
%  - the current frame is a new frame 
%     - the current method is the "static void main" method of the first class file
%     - the pc counter point the first instruction
%     - the operand stack is empty
%     - the local variable are initialized with default values
%  - the heap is initialized
%     - the static field of all the (loaded) class of the program are initialized
% :- calls initial_state/2 : program*var.
% :- success initial_state/2 : program*var => program*state.

% :- regtype mis/1.
% mis(mis(MethodSignature,Arguments,Heap)):-
% 	methodSignature(MethodSignature),
% 	list(Arguments,value),
% 	heap(Heap).

initial_state_mis(mis(MethodSignature,Arguments,HeapInit),State):-
	methodSignature_name(MethodSignature,methodName(CN,_)),
	program_class(CN,Cl),
	class_method(Cl,MethodSignature,Method),
%	\+ class_isAbstract(Cl),
	% recovery of datas
	method_body(Method,MethodBody),
	bytecodeMethod_localVarSize(MethodBody,LocalVarSize),
	bytecodeMethod_firstAddress(MethodBody,PC), 
	% building the frame
	length(LocalVar,LocalVarSize),
	%Method = method(_,_,_,static(IsStatic),_),
	init_localVarArg(LocalVar,LocalVarSize,Arguments),
	OperandStack=[],
	Frame=fr(Method,PC,OperandStack,LocalVar),
	%check(method(Method)),
	% building the state
	Callstack=[],
	heap_init(HeapInit,Heap),
	State=(st(Heap,Frame,Callstack)).
%	check(state(State)).

init_localVarArg(L,I,Arguments):-
	length(Arguments,ArgumentSize),
	S is I-ArgumentSize,
	length(RL,S),
	init(RL,num(int(0)),S),
	append(Arguments,RL,L).
%init_localVarArg(false,L,I,Arguments):-
%	length(Arguments,ArgumentSize),
%	S is I-ArgumentSize-1,
%	length(RL,S),
%	init(RL,num(int(0)),S),
%	append([ref(loc(1))|Arguments],RL,L).

%heap_init(program(Classes,_),HeapInit,Heap):-
heap_init(HeapInit,Heap):-
	var(HeapInit),
	findall(Class,loaded_classes:get_class(Class),Classes),
	HeapInit=heap(dynamicHeap([]),staticHeap([])),
	load_static_fields(HeapInit,Classes,Heap).
heap_init(Heap,Heap):-
	nonvar(Heap).
