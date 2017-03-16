:- include(library(jvm_in_ciao(instrumented_interpreter(pe_guides)))).
%:- include(library(jvm_in_ciao(instrumented_interpreter(cost_domain)))).

% Formalization of Java small step semantics. 
% Based on The "Java (TM) Virtual Machine Specification, 
%             Second Edition, Tim Lindholm, Frank Yellin" 

:- pred execute(+P,+Sin,-Sout,+ASin,-ASout) # "Main loop of the abstract
	interpreter. Given a Java bytecode program @var{P}, the initial concrete
        state @var{Sin} and the initial abstract state @var{ASin} it computes 
        the concrete state @var{Sout} and the abstract state @var{ASout} after 
        executing the program".
execute(Program,S1,S3,AS1,AS3):-
	fetch_instruction(S1,Inst),
	step(Inst,TraceStep,Program,S1,S2),
	alpha_step(Inst,TraceStep,AS1,AS2),
	execute(Program,S2,S3,AS2,AS3).
execute(P,S1,Sf,AS1,ASf) :-
	S1 = st(H,fr(M,PC,S,L),SF),
	S2 = st(Hb,fr(M,PCb,Sb,L),SF),
	instructionAt(M,PC,invoke_issd(Mid)),
	invoke_issd_evalpart(P,M,Mid,PC,PCb,S,Sb,H,Hb,MN,InArgs,OutArgsHb),
	alpha_step(invoke_issd(Mid),invoke_issd,AS1,AS2),
	ResCall =..[MN,[InArgs,H],OutArgsHb,ASout],
	res_invoke(ResCall),
	alpha_add(AS2,ASout,AS3),
	execute(P,S2,Sf,AS3,ASf).
execute(_Program,S1,Sf,AS1,ASf):-
	S1=st(_H,fr(M,PC,_S,_L),[]),
	instructionAt(M,PC,return),
	alpha_step(return,normal_end,AS1,ASf),
	state_cleanup(S1,Sf).
execute(_Program,S1,Sf,AS1,ASf):-
	S1=st(_H,fr(M,PC,[num(int(_I))|_S],_L),[]),
	instructionAt(M,PC,ireturn),
	alpha_step(ireturn,normal_end,AS1,ASf),
	state_cleanup(S1,Sf).
execute(_Program,S1,Sf,AS1,ASf):-
	S1=st(_H,fr(M,PC,[ref(loc(_I))|_S],_L),[]),
	instructionAt(M,PC,areturn),
	alpha_step(areturn,normal_end,AS1,ASf),
	state_cleanup(S1,Sf).
execute(Program,S1,Sf,AS1,ASf):-
 	S1=stE(H,frE(M,PC,Loc,_),[]),
 	method_body(M,BM),
 	bytecodeMethod_exceptionHandlers(BM,ExL),
 	not_lookup_handlers(Program,ExL,H,PC,Loc,_),
	alpha_step(aborting_end,aborting_end,AS1,ASf),
	state_cleanup(S1,Sf).

fetch_instruction(State,Inst) :-
	state_get_m(State,M),
	state_get_pc(State,PC),
	instructionAt(M,PC,Inst).

state_cleanup(st(H,fr(_M,_PC,OS,_LV),_CS),st(H,fr(_,_,OS,_),_)).% :-
%	filter_heap(H,FH).
state_cleanup(stE(H,frE(_M,_PC,Loc,_LV),_CS),st(H,fr(_,_,[refE(Loc)],_),_)).% :-
%	filter_heap(H,FH).


% Exception Management
% The current exception is caught in the current method
step(_,exception_caught, P,
	stE(H,frE(M,PC,Loc,L),SF),
	st(H,fr(M,PCb,[ref(Loc)],L),SF)):-
 method_body(M,BM),
 bytecodeMethod_exceptionHandlers(BM,ExL),
 lookup_handlers(P,ExL,H,PC,Loc,PCb).
% The current exception is uncaught
step(_,exception_uncaught, P,
	stE(H,frE(M,PC,Loc,_L),[fr(Mb,PCb,_Sb,Lb)|SF]),
	stE(H,frE(Mb,PCb,Loc,Lb),SF)):-
 method_body(M,BM),
 bytecodeMethod_exceptionHandlers(BM,ExL),
 not_lookup_handlers(P,ExL,H,PC,Loc,_).

% aaload: Load reference from array 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#aaload
step(aaload,aaload_step_ok, _P,
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[Val|S],L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,refType(_RT))),
 0 =< I, % res_susc
 I < Length, % res_susc
 heap_get(H,arrayElement(Loc,I),Val).
step(aaload,aaload_step_NullPointerException, P,
	st(H,fr(M,PC,[num(int(_)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Loc,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NPE)),Loc,Hb).
step(aaload,aaload_step_ArrayIndexOutOfBoundsException, P,
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,refType(_RT))),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,P,locationObject(className(JL,AIOOBE)),Locb,Hb).

% aastore: Store into reference array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#aastore
step(aastore,aastore_step_ok, P,
	st(H,fr(M,PC,[Val,num(int(I)),ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,TP)),
 assign_compatible(P,H,Val,TP),
 0 =< I, I < Length, % res_susc
 heap_update(H,arrayElement(Loc,I),Val,Hb).
step(aastore,aastore_step_NullPointerException, P,
	st(H,fr(M,PC,[_Val,num(int(_I)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NPE)),Locb,Hb).
step(aastore,aastore_step_ArrayIndexOutOfBoundsException, P,
	st(H,fr(M,PC,[_Val,num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,refType(_RT))),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,P,locationObject(className(JL,AIOOBE)),Locb,Hb).
step(aastore,aastore_step_ArrayStoreException, P,
	st(H,fr(M,PC,[Val,num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,TP)),
 0 =< I, I < Length, % res_susc
 not_assign_compatible(P,H,Val,TP),
 arrayStoreException(ASE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,ASE)),Locb,Hb).

% aconst_null: Push null 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#aconst_null
step(aconst_null,aconst_null, _P,
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,[null|S],L),SF)):-
 next(M,PC,PCb).

% aload: Load reference from local variable
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#aload
step(aload(X),aload_step_ok, _P,
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,[Val|S],L),SF)):-
 next(M,PC,PCb),
 localVar_get(L,X,Val),
 isReference(Val).

% anewarray: Create new array of reference 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#anewarray
% OutOfMemory is not considered in Bicolano
step(anewarray(T),anewarray_step_ok, P,
	st(H,fr(M,PC,[num(int(Length))|S],L),SF),
	st(Hb,fr(M,PCb,[ref(Loc)|S],L),SF)):-
 next(M,PC,PCb),
 0 =< Length, % res_susc
 heap_new(H,P,locationArray(Length,T),Loc,Hb).
step(anewarray(_T),anewarray_step_NegativeArraySizeException, P,
	st(H,fr(M,PC,[num(int(Length))|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 Length < 0, % res_susc
 negativeArraySizeException(NASE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NASE)),Locb,Hb).

% areturn: Return reference from method
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#areturn
step(areturn,areturn_step_ok, P,
	st(H,fr(M,_PC,[Val|_S],_L),CallStack),
	st(H,fr(Mb,PCbb,[Val|Sb],Lb),SF)):-
 nonvar(CallStack),
 CallStack = [fr(Mb,PCb,Sb,Lb)|SF],
 next(Mb,PCb,PCbb),
 method_signature(M,MSig),
 methodSignature_result(MSig,refType(RT)),
 assign_compatible(P,H,Val,refType(RT)).

% arraylength: Get length of array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#arraylength
step(arraylength,arraylength_step_ok, _P,
	st(H,fr(M,PC,[ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(Length))|S],L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,_TP)).
step(arraylength,arraylength_step_NullPointerException, P,
	st(H,fr(M,PC,[null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NPE)),Locb,Hb).

% astore: Store reference into local variable
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#astore
step(astore(X),astore_step_ok,_P,
	st(H,fr(M,PC,[Val|S],L),SF),
	st(H,fr(M,PCb,S,Lb),SF)):-
 isReference(Val),
 next(M,PC,PCb),
 localVar_update(L,X,Val,Lb).

% athrow: Throw exception or error
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#athrow
step(athrow,athrow_step, _P,
	st(H,fr(M,PC,[ref(Loc)|_S],L),SF),
	stE(H,frE(M,PC,Loc,L),SF)).
step(athrow,athrow_step_NullPointerException, P,
	st(H,fr(M,PC,[null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NPE)),Locb,Hb).

% baload: Load byte or boolean from array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#baload
step(baload,baload_step_ok, _P,
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(B))|S],L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(TP))),
 0 =< I, % res_susc
 I < Length, % res_susc
 (TP=boolean;TP=byte),
 heap_get(H,arrayElement(Loc,I),num(byte(B))).
step(baload,baload_step_NullPointerException, P,
	st(H,fr(M,PC,[num(int(_)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NPE)),Locb,Hb).
step(baload,baload_step_ArrayIndexOutOfBoundsException, P,
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,_RT)),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,P,locationObject(className(JL,AIOOBE)),Locb,Hb).

% bastore: Store into byte or boolean array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#bastore
step(bastore,bastore_step_ok, _P,
	st(H,fr(M,PC,[num(int(Ib)),num(int(I)),ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(byte))),
 0 =< I, I < Length, % res_susc
 i2b(Ib,B), % res_susc
 heap_update(H,arrayElement(Loc,I),num(byte(B)),Hb). % res_susc into
step(bastore,bastore_step_ok, _P,
	st(H,fr(M,PC,[num(int(Ib)),num(int(I)),ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(boolean))),
 0 =< I, I < Length, % res_susc
 i2bool(Ib,B), % res_susc
 heap_update(H,arrayElement(Loc,I),num(byte(B)),Hb). % res_susc into
step(bastore,bastore_step_NullPointerException, P,
	st(H,fr(M,PC,[num(int(_)),num(int(_I)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NPE)),Locb,Hb).
step(bastore,bastore_step_ArrayIndexOutOfBoundsException, P,
	st(H,fr(M,PC,[num(int(_)),num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,refType(_RT))),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,P,locationObject(className(JL,AIOOBE)),Locb,Hb).

% checkcast: Check whether object is of given type
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc2.html#checkcast
step(checkcast(T),checkcast_step_ok, P,
	st(H,fr(M,PC,[Val|S],L),SF),
	st(H,fr(M,PCb,[Val|S],L),SF)):-
 next(M,PC,PCb),
 assign_compatible(P,H,Val,refType(T)).
step(checkcast(T),checkcast_step_ClassCastException, P,
	st(H,fr(M,PC,[Val|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 not_assign_compatible(P,H,Val,refType(T)),
 classCastException(CCE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,CCE)),Locb,Hb).

% const : Push numeric constant (bipush, iconst, ldc(no string constant supported), sipush)
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#bipush
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#iconst_i
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc8.html#ldc
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc13.html#sipush
step(const(_T,Z),const_step_ok, _P,
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,[num(int(Z))|S],L),SF)):-
 next(M,PC,PCb).

% dup: Duplicate the top operand stack value
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc3.html#dup
step(dup,dup_step_ok, _P,
	st(H,fr(M,PC,[V|S],L),SF),
	st(H,fr(M,PCb,[V,V|S],L),SF)):-
 next(M,PC,PCb).

% dup_x1: Duplicate the top operand stack value and insert two values down
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc3.html#dup_x1
step(dup_x1,dup_x1_step_ok, _P,
	st(H,fr(M,PC,[V1,V2|S],L),SF),
	st(H,fr(M,PCb,[V1,V2,V2|S],L),SF)):-
 next(M,PC,PCb).

% dup_x2: Duplicate the top operand stack value and insert two or three values down
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc3.html#dup_x2
step(dup_x2,dup_x2_step_ok, _P,
	st(H,fr(M,PC,[V1,V2,V3|S],L),SF),
	st(H,fr(M,PCb,[V1,V2,V3,V1|S],L),SF)):-
 next(M,PC,PCb).

% dup2: Duplicate the top one or two operand stack values
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc3.html#dup2
step(dup2,dup2_step_ok, _P,
	st(H,fr(M,PC,[V1,V2|S],L),SF),
	st(H,fr(M,PCb,[V1,V2,V1,V2|S],L),SF)):-
 next(M,PC,PCb).

% dup2_x1: Duplicate the top one or two operand stack values and insert two or three values down
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc3.html#dup2_x1
step(dup2_x1,dup2_x1_step_ok, _P,
	st(H,fr(M,PC,[V1,V2,V3|S],L),SF),
	st(H,fr(M,PCb,[V1,V2,V3,V1,V2|S],L),SF)):-
 next(M,PC,PCb).

% dup2_x2: Duplicate the top one or two operand stack values and insert two, three, or four values down
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc3.html#dup2_x2
step(dup2_x2,dup2_x2_step_ok, _P,
	st(H,fr(M,PC,[V1,V2,V3,V4|S],L),SF),
	st(H,fr(M,PCb,[V1,V2,V3,V4,V1,V2|S],L),SF)):-
 next(M,PC,PCb).

% getfield: Fetch field from object
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc5.html#getfield
step(getfield(F),getfield_step_ok, _P,
	st(H,fr(M,PC,[ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[V|S],L),SF)):-
 next(M,PC,PCb),
 heap_get(H,dynamicField(Loc,F),V).
step(getfield(_F),getfield_step_NullPointerException, P,
	st(H,fr(M,PC,[null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NPE)),Locb,Hb).

% getstatic: Get static field from class
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc5.html#getstatic
step(getstatic(F),getstatic_step_ok, P,
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,[V|S],L),SF)):-
 next(M,PC,PCb),
 isStatic(P,F),
 heap_get(H,staticField(F),V).

% goto: Branch always
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc5.html#goto
step(goto(O),goto_step_ok, _P,
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 PCb is PC+O.

% i2b: Convert int to byte 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#i2b
step(i2b,i2b_step_ok, _P,
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,[num(int(Ib))|S],L),SF)):-
 next(M,PC,PCb),
 i2b(I,Ib). % res_susc

% i2s: Convert int to short 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#i2s
step(i2s,i2s_step_ok, _P,
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,[num(int(Ib))|S],L),SF)):-
 next(M,PC,PCb),
 i2s(I,Ib). % res_susc

% ibinop (iadd, iand, idiv, imul, ior, irem, ishl, ishr, isub, iushr, ixor): Binary operations on int
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#iadd
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#iand
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#idiv
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#imul
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ior
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#irem
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ishl
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ishr
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#isub
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#iushr
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ixor
step(ibinop(Op),ibinop_step_ok, _P,
	st(H,fr(M,PC,[num(int(I2)),num(int(I1))|S],L),SF),
	st(H,fr(M,PCb,[num(int(R))|S],L),SF)):-
 ibinop_step_cond(Op,I2),
 next(M,PC,PCb),
 semBinopInt(Op,I1,I2,R).
step(ibinop(Op),ibinop_ArithmeticException, P,
	st(H,fr(M,PC,[num(int(0)),num(int(_I1))|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 (Op=divInt;Op=remInt),
 arithmeticException(AE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,AE)),Locb,Hb).

% iaload: Load int from array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#iaload
step(iaload,iaload_step_ok, _P,
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(Ib))|S],L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(int))),
 0 =< I, % res_susc
 I < Length, % res_susc
 heap_get(H,arrayElement(Loc,I),num(int(Ib))).
step(iaload,iaload_step_NullPointerException, P,
	st(H,fr(M,PC,[num(int(_)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NPE)),Locb,Hb).
step(iaload,iaload_step_ArrayIndexOutOfBoundsException, P,
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,primitiveType(int))),
 (I < 0 ; I >= Length), %res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,P,locationObject(className(JL,AIOOBE)),Locb,Hb).

% iastore: Store into int array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#iastore
step(iastore,iastore_step_ok, _P,
	st(H,fr(M,PC,[num(int(Ib)),num(int(I)),ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(int))),
 0 =< I, I < Length, % res_susc
 heap_update(H,arrayElement(Loc,I),num(int(Ib)),Hb).
step(iastore,iastore_step_NullPointerException, P,
	st(H,fr(M,PC,[num(int(_)),num(int(_I)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NPE)),Locb,Hb).
step(iastore,iastore_step_ArrayIndexOutOfBoundsException, P,
	st(H,fr(M,PC,[num(int(_)),num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,refType(int))),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,P,locationObject(className(JL,AIOOBE)),Locb,Hb).

% if_acmpeq: Branch if reference comparison succeeds
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#if_acmp
step(if_acmpeq(O),if_acmpeq_step_jump, _P,
	st(H,fr(M,PC,[V1,V1|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 isReference(V1),
 PCb is PC+O.
step(if_acmpeq(_O),if_acmpeq_step_continue, _P,
	st(H,fr(M,PC,[V1,V2|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 isReference(V1),
 isReference(V2),
 V1 \= V2.

% if_acmpne: Branch if reference comparison does not succeed
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#if_acmp
step(if_acmpne(O),if_acmpne_step_jump, _P,
	st(H,fr(M,PC,[V1,V2|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 isReference(V1),
 isReference(V2),
 V1 \= V2,
 PCb is PC+O.
step(if_acmpne(_O),if_acmpne_step_continue, _P,
	st(H,fr(M,PC,[V1,V1|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 isReference(V1).

% if_icmp: Branch if int comparison succeeds
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#if_icmp
step(if_icmp(Cmp,O),if_icmp_step_jump, _P,
	st(H,fr(M,PC,[num(int(I2)),num(int(I1))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 semCompInt(Cmp,I1,I2),
 PCb is PC+O.
step(if_icmp(Cmp,_O),if_icmp_step_continue, _P,
	st(H,fr(M,PC,[num(int(I2)),num(int(I1))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 noSemCompInt(Cmp,I1,I2).

% if0 (ifcond): Branch if int comparison with zero succeeds
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ifcond
step(if0(Cmp,O),if0_step_jump, _P,
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 semCompInt(Cmp,I,0),
 PCb is PC+O.
step(if0(Cmp,_O),if0_step_continue, _P,
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 noSemCompInt(Cmp,I,0),
 next(M,PC,PCb).

% ifnonnull: Branch if reference not null 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ifnonnull
step(ifnonnull(O),ifnonnull_step_jump, _P,
	st(H,fr(M,PC,[ref(_Loc)|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 PCb is PC+O.
step(ifnonnull(_O),ifnonnull_step_continue, _P,
	st(H,fr(M,PC,[null|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb).

% ifnull: Branch if reference is null 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ifnull
step(ifnull(O),ifnull_step_jump, _P,
	st(H,fr(M,PC,[null|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 PCb is PC+O.
step(ifnull(_O),ifnull_step_continue, _P,
	st(H,fr(M,PC,[ref(_Loc)|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb).

% iinc: Increment local variable by constant
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#iinc
step(iinc(X,Z),iinc_step, _P,
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,S,Lb),SF)):-
 next(M,PC,PCb),
 localVar_get(L,X,num(int(I))),
 semBinopInt(addInt,I,Z,R),
 localVar_update(L,X,num(int(R)),Lb).

% iload: Load int from local variab
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#iload
step(iload(X),iload_step, _P,
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,[num(int(I))|S],L),SF)):-
 next(M,PC,PCb),
 localVar_get(L,X,num(int(I))).

% ineg: Negate int 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ineg
step(ineg,ineg_step, _P,
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,[num(int(Ib))|S],L),SF)):-
 next(M,PC,PCb),
 negInt(I,Ib).

% instanceof: Determine if object is of given type
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#instanceof
step(instanceof(T),instanceof_step_ok1, P,
	st(H,fr(M,PC,[ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(1))|S],L),SF)):-
 next(M,PC,PCb),
 assign_compatible(P,H,ref(Loc),refType(classType(T))).
step(instanceof(T),instanceof_step_ok2, P,
	st(H,fr(M,PC,[ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(0))|S],L),SF)):-
 next(M,PC,PCb),
 not_assign_compatible(P,H,ref(Loc),refType(classType(T))).
% the following code describe what is specified in bicolano but 
% it does NOT correspond to the official specification
% (cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#instanceof)
% step(instanceof_step_ClassCastException, P
% 	st(H,fr(M,PC,[ref(Loc)|S],L),SF),
% 	stE(Hb,frE(M,PC,Locb,L),SF)):-
%  \+ assign_compatible(P,H,ref(Loc),T)
%  classCastException(CCE),
%  javaLang(JL),
%  heap_new(H,P,locationObject(className(JL,CCE)),Locb,Hb).

% invokeinterface: Invoke interface method
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#invokeinterface
% invokespecial: Invoke instance method; 
%  special handling for superclass, private, and instance initialization method invocations
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#invokespecial
step(invokespecial(Mid),invokespecial_step_here_ok, P,
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(Mb,PCb,[],Lb),[fr(M,PC,Sb,L)|SF])):-
 methodSignature_name(Mid,methodName(MidCn,_)),
 resolve_method(P,MidCn,Mid,Meth),

 method_signature(Meth,MethSig),
 methodSignature_parameters(MethSig,Param),
 length(Param,NbParam),
 length(Args,NbParam),
 append(Args,[ref(Loc)|Sb],S), % the stack contains the parameters, the reference and the rest

 %heap_typeof(H,Loc,locationObject(Cn)), %due to AHR
 method_signature(Meth,Meths),% Miky: This call is already done with MethSig, no?
 methodSignature_name(Meths,Methn),
 Methn = methodName(_Methcl,Methsmn),
 method_signature(M,Msig),
 methodSignature_name(Msig,methodName(_CCn,_)),
 % program_class(P,CCn,CC),
 % class_superClass(CC,SCCn),   

% byMiky: Commented due to AHR. In fact I have never understood very well what it does
% ( ( method_visibility(Meth,protected),
%      subclass_name(P,CCn,Methcl),
%      subclass_name(P,Cn,CCn))
%  ;
%     (method_visibility(Meth,Visib),
%      Visib \= protected)),

%% The following might be wrong...
%  \+ ( subclass_name(P,SCCn,Methcl),
%        Methsmn\=shortMethodName('<init>')),

%% the following might be to restrictive... (but it works with partial evaluation)
%  if we only use invokespecial to call constructor
 Methsmn=shortMethodName('<init>'),

 Mb=Meth,
 compatible_param(P,H,Args,Param),
 method_body(Mb,BMb),
 bytecodeMethod_firstAddress(BMb,PCb),
 bytecodeMethod_localVarSize(BMb,Llength),
 RLlength is Llength - NbParam -1,
 length(RL,RLlength),
 init_localVar(RL,RLlength),
 reverse(Args,RL,Lb1),
 Lb = [ref(Loc)|Lb1],
 check(length(Lb,Llength)).
%% by commenting the following definition, we will only handle invokespecial to constructors
% step(invokespecial_step_ok, P,
% 	st(H,fr(M,PC,S,L),SF),
% 	st(H,fr(Mb,PCb,[],Lb),[fr(M,PC,Sb,L)|SF])):-
%  instructionAt(M,PC,invokespecial(Mid)),
%  methodSignature_name(Mid,methodName(MidCn,_)),
%  resolve_method(P,MidCn,Mid,Meth),

%  method_signature(Meth,MethSig),
%  methodSignature_parameters(MethSig,Param),
%  length(Param,NbParam),
%  length(Args,NbParam),
%  append(Args,[ref(Loc)|Sb],S), % the stack contains the parameters, the reference and the rest

%  heap_typeof(H,Loc,locationObject(Cn)),
%  method_signature(Meth,Meths),
%  methodSignature_name(Meths,Methn),
%  Methn = methodName(Methcl,Methsmn),
%  method_signature(M,Msig),
%  methodSignature_name(Msig,methodName(CCn,_)),
%  program_class(P,CCn,CC),
%  class_superClass(CC,SCCn),     
%  ( ( method_visibility(Meth,protected),
%       subclass_name(P,CCn,Methcl),
%       subclass_name(P,Cn,CCn))
%   ;
%    ( method_visibility(Meth,Visib),
%      Visib \= protected)),
%  subclass_name(P,SCCn,Methcl),
%  Methsmn \= shortMethodName('<init>'),
%  lookup(P,SCCn,Meths,Mb),
%  compatible_param(P,H,Args,Param),
%  method_body(Mb,BMb),
%  bytecodeMethod_firstAddress(BMb,PCb),
%  bytecodeMethod_localVarSize(BMb,Llength),
%  RLlength is Llength - NbParam -1,
%  length(RL,RLlength),
%  init_localVar(RL,RLlength),
%  reverse(Args,RL,Lb1),
%  Lb = [ref(Loc)|Lb1],
%  check(length(Lb,Llength)).
step(invokespecial(Mid),invokespecial_step_NullPointerException, P,
	st(H,fr(M,PC,S,L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 methodSignature_parameters(Mid,Param),
 length(Param,NbParam),
 length(Args,NbParam),
 append(Args,[null|_Sb],S), % the stack contains the parameters, the reference and the rest
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NPE)),Locb,Hb).


% invokestatic: Invoke a class (static) method
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#invokestatic
step(invokestatic(Mid),invokestatic_step_ok, P,
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(Mb,PCb,[],Lb),[fr(M,PC,Sb,L)|SF])):-
 methodSignature_name(Mid,methodName(CN,_SMN)),
 resolve_method(P,CN,Mid,Mb),
 method_isStatic(Mb),
 method_body(Mb,Bm),
 bytecodeMethod_firstAddress(Bm,PCb),
 methodSignature_parameters(Mid,Param),
 length(Param,NbParam),
 length(Args,NbParam),
 append(Args,Sb,S),
 bytecodeMethod_localVarSize(Bm,Llength),
 RLlength is Llength - NbParam,
 length(RL,RLlength),
 init_localVar(RL,RLlength),
 reverse(Args,RL,Lb),
 check(length(Lb,Llength)).

% invokevirtual: Invoke instance method; dispatch based on class
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#invokevirtual
step(invokevirtual(Mid),invokevirtual_step_ok, P,
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(Mb,PCb,[],Lb),[fr(M,PC,Sb,L)|SF])):-
 methodSignature_name(Mid,methodName(MidCn,_)),
 resolve_method(P,MidCn,Mid,Meth),

 method_signature(Meth,MethSig),

 methodSignature_name(MethSig,methodName(MethCln,Methsmn)),
 Methsmn \= shortMethodName('<init>'),
 Methsmn \= shortMethodName('<clinit>'), 

 methodSignature_parameters(MethSig,Param),
 length(Param,NbParam),
 length(Args,NbParam),
 append(Args,[ref(Loc)|Sb],S), % the stack contains the parameters, the reference and the rest

 heap_typeof(H,Loc,locationObject(Cn)),
 method_signature(M,Msig),
 methodSignature_name(Msig,methodName(CCn,_)),
 ( ( method_visibility(Meth,protected), % if/then
      subclass_name(P,CCn,MethCln),
      subclass_name(P,Cn,CCn))
  ;
   ( method_visibility(Meth,Visib),
     Visib \= protected)),
 lookup(P,Cn,MethSig,Mb),
 method_body(Mb,Bm),
 bytecodeMethod_firstAddress(Bm,PCb),
 bytecodeMethod_localVarSize(Bm,Llength),
 RLlength is Llength - NbParam -1,
 length(RL,RLlength),
 init_localVar(RL,RLlength),
 reverse(Args,RL,Lb1),
 Lb = [ref(Loc)|Lb1],
 check(length(Lb,Llength)).
step(invokevirtual(Mid),invokevirtual_step_NullPointerException, P,
	st(H,fr(M,PC,S,L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 methodSignature_parameters(Mid,Param),
 length(Param,NbParam),
 length(Args,NbParam),
 append(Args,[null|_Sb],S), % the stack contains the parameters, the reference and the rest
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NPE)),Locb,Hb).

% the following implements the bicolano specification of the 24th of April
% step(invokevirtual_step_ok, P,
% 	st(H,fr(M,PC,S,L),SF),
% 	st(H,fr(Mb,PCb,[],Lb),[fr(M,PC,Sb,L)|SF])):-
%  methodSignature_parameters(Mid,Param),
%  length(Param,NbParam),
%  length(Args,NbParam),
%  append(Args,[ref(Loc)|Sb],S), % the stack contains the parameters, the reference and the rest
%  heap_typeof(H,Loc,locationObject(Cn)),
%  lookup(P,Cn,Mid,Mb),
%  method_body(Mb,Bm),
%  bytecodeMethod_firstAddress(Bm,PCb),
%  bytecodeMethod_localVarSize(Bm,Llength),
%  RLlength is Llength - NbParam -1,
%  length(RL,RLlength),
%  init_localVar(RL,RLlength),
%  reverse(Args,RL,Lb1),
%  Lb = [ref(Loc)|Lb1],
%  check(length(Lb,Llength)).
% step(invokevirtual_step_NullPointerException, P,
% 	st(H,fr(M,PC,S,L),SF),
% 	stE(Hb,frE(M,PC,Locb,L),SF)):-
%  instructionAt(M,PC,invokevirtual(Mid)),
%  methodSignature_parameters(Mid,Param),
%  length(Param,NbParam),
%  length(Args,NbParam),
%  append(Args,[null|_Sb],S), % the stack contains the parameters, the reference and the rest
%  nullPointerException(NPE),
%  javaLang(JL),
%  heap_new(H,P,locationObject(className(JL,NPE)),Locb,Hb).

% ireturn: Return int from method
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ireturn

% Version of invoke for ISSD. It lets the corresponding residual call and continues 
% with the same method.

% For the moment it is explicitly included as a special case of predicate execute
%step(invoke_issd(Mid),invoke_issd_step_ok, P,
%	st(H,fr(M,PC,S,L),SF),
%	st(Hb,fr(M,PCb,Sb,L),SF)):-
% % Keep an eye on the heap. In general it will change, and how?
% invoke_issd_evalpart(P,M,Mid,PC,PCb,S,Sb,H,Hb,MN,InArgs,OutArgsHb),
%% filter_heap(H,FH),
% ResCall =..[MN,[InArgs,H],OutArgsHb],
% res_invoke(ResCall).

step(ireturn,ireturn_step_ok,_P,
	st(H,fr(M,_PC,[num(int(I))|_S],_L),CallStack),
	st(H,fr(Mb,PCbb,[num(int(I))|Sb],Lb),SF)):-
 nonvar(CallStack),
 CallStack = [fr(Mb,PCb,Sb,Lb)|SF],
 next(Mb,PCb,PCbb),
 method_signature(M,MSig),
 methodSignature_result(MSig,primitiveType(_)).

% istore: Store int into local variable
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#istore
step(istore(X),istore_step_ok, _P,
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,S,Lb),SF)):-
 next(M,PC,PCb),
 localVar_update(L,X,num(int(I)),Lb).

% lookupswitch: Access jump table by key match and jump
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc8.html#lookupswitch
step(lookupswitch(_Def,ListKey),lookupswitch_step_ok1, _P,
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 member((I,O),ListKey),
 PCb is PC+O.
step(lookupswitch(Def,ListKey),lookupswitch_step_ok2, _P,
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 \+ member((I,_O),ListKey),
 PCb is PC+Def.

% multianewarray: Create new multidimensional array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc9.html#multianewarray
% new: Create new object
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc10.html#new
step(new(C),new_step_ok, P,
	st(H,fr(M,PC,S,L),SF),
	st(Hb,fr(M,PCb,[ref(Loc)|S],L),SF)):-
 next(M,PC,PCb),
 heap_new(H,P,locationObject(C),Loc,Hb).

% newarray: Create new array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc10.html#newarray
step(newarray(T),newarray_step_ok, P,
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(Hb,fr(M,PCb,[ref(Loc)|S],L),SF)):-
 next(M,PC,PCb),
 0 =< I,
 heap_new(H,P,locationArray(I,T),Loc,Hb).
step(newarray(_T),newarray_step_NegativeArraySizeException, P,
	st(H,fr(M,PC,[num(int(I))|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 I<0,
 negativeArraySizeException(NASE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NASE)),Locb,Hb).

% nop: Do nothing
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc10.html#nop
step(nop,nop_step_ok, _P,
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb).

% pop: Pop the top operand stack value
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc11.html#pop
step(pop,pop_step_ok, _P,
	st(H,fr(M,PC,[_V|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb).

% pop2: Pop the top one or two operand stack values
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc11.html#pop2
step(pop2,pop2_step_ok, _P,
	st(H,fr(M,PC,[_V1,_V2|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb).

% putfield: Set field in object
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc11.html#putfield
step(putfield(F),putfield_step_ok, P,
	st(H,fr(M,PC,[V,ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationObject(_Cn)),
 %defined_field(P,_Cn,F),
 fieldSignature_type(F,FT),
 assign_compatible(P,H,V,FT),
 heap_update(H,dynamicField(Loc,F),V,Hb).
step(putfield(_F),putfield_step_NullPointerException, P,
	st(H,fr(M,PC,[_V,null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NPE)),Locb,Hb).

% putstatic: Set static field in class
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc11.html#putstatic
step(putstatic(F),putstatic_step_ok, P,
	st(H,fr(M,PC,[V|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 isStatic(P,F),
 fieldSignature_type(F,FT),
 assign_compatible(P,H,V,FT),
 heap_update(H,staticField(F),V,Hb).

% return: Return void from method
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc12.html#return
step(return,return_step_ok, _P,
	st(H,fr(M,_PC,_S,_L),CallStack),
	st(H,fr(Mb,PCbb,Sb,Lb),SF)):-
 nonvar(CallStack),
 CallStack = [fr(Mb,PCb,Sb,Lb)|SF],
 next(Mb,PCb,PCbb),
 method_signature(M,MSig),
 methodSignature_result(MSig,none).

% saload: Load short from array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc13.html#saload
step(saload,saload_step_ok, _P,
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(Sh))|S],L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(short))),
 0 =< I,
 I < Length,
 heap_get(H,arrayElement(Loc,I),num(short(Sh))).
step(saload,saload_step_NullPointerException, P,
	st(H,fr(M,PC,[num(int(_)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NPE)),Locb,Hb).
step(saload,saload_step_ArrayIndexOutOfBoundsException, P,
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,primitiveType(short))),
 (I < 0 ; I >= Length),
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,P,locationObject(className(JL,AIOOBE)),Locb,Hb).

% sastore: Store into short array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc13.html#sastore
step(sastore,sastore_step_ok, _P,
	st(H,fr(M,PC,[num(int(ISh)),num(int(I)),ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(short))),
 0 =< I, I < Length,
 i2s(ISh,Sh),
 heap_update(H,arrayElement(Loc,I),num(short(Sh)),Hb).
step(sastore,sastore_step_NullPointerException, P,
	st(H,fr(M,PC,[num(int(_)),num(int(_I)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,P,locationObject(className(JL,NPE)),Locb,Hb).
step(sastore,sastore_step_ArrayIndexOutOfBoundsException, P,
	st(H,fr(M,PC,[num(int(_)),num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,refType(_RT))),
 (I < 0 ; I >= Length),
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,P,locationObject(className(JL,AIOOBE)),Locb,Hb).

% swap: Swap the top two operand stack values
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc13.html#swap
step(swap,swap_step_ok, _P,
	st(H,fr(M,PC,[V1,V2|S],L),SF),
	st(H,fr(M,PCb,[V2,V1|S],L),SF)):-
 next(M,PC,PCb).

% tableswitch: Access jump table by index and jump
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc14.html#tableswitch
step(tableswitch(Def,Low,High,List_offset),tableswitch_step_ok1, _P,
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 (I < Low ; High < I),
 check((length(List_offset,N), N = High - Low +1)),
 PCb is PC + Def.
step(tableswitch(_Def,Low,High,List_offset),tableswitch_step_ok2, _P,
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 Low =< I,
 I =< High,
 check((length(List_offset,N), N = High - Low +1)),
 Nth is I-Low+1,
 nth(Nth,List_offset,O),
 PCb is PC+O.

invoke_issd_evalpart(P,M,Mid,PC,PCb,OS,OSb,H,Hb,MN,InArgs,OutArgsHb) :-
 next(M,PC,PCb),
 methodSignature_name(Mid,methodName(CN,shortMethodName(MN))),
 resolve_method(P,CN,Mid,Mb),
 methodSignature_parameters(Mid,Param),
 length(Param,NbParam),
 length(RInArgs,NbParam),
 methodSignature_result(Mid,MidResult),
 (MidResult == none -> OutArgs = []
                     ; OutArgs = [_Out]),
 (method_isStatic(Mb) -> (
     append(RInArgs,PreOS,OS),
     reverse(RInArgs,InArgs),
     append(OutArgs,PreOS,OSb))
 ; (
     append(RInArgs,[This|PreOS],OS),
     reverse(RInArgs,InArgs_p),
     InArgs = [This|InArgs_p],
     append(OutArgs,PreOS,OSb))),
 build_heapout_struct(H,Hb),
 append(OutArgs,[Hb],OutArgsHb).


binopInt_no_div(addInt).
binopInt_no_div(andInt).
binopInt_no_div(mulInt).
binopInt_no_div(orInt).
binopInt_no_div(shlInt).
binopInt_no_div(shrInt).
binopInt_no_div(subInt).
binopInt_no_div(ushrInt).
binopInt_no_div(xorInt).

binopInt_div(divInt).
binopInt_div(remInt).

ibinop_step_cond(Op,_I2):-
	binopInt_no_div(Op).
ibinop_step_cond(Op,I2):-
	binopInt_div(Op),
	I2 =\= 0.

% Initialisation and execution are not specified by Bicolano
% only the step predicate is specified

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
initial_state(program([Cl1|RCL],_),State):-
	\+ class_isAbstract(Cl1),
	% recovery of datas
	JavaLangString=className(packageName('java/lang/'),shortClassName('String')),
	JavaLangStringArray=refType(arrayType(refType(classType(JavaLangString)))),
	methodSignature_name(MainSignature,methodName(_,shortMethodName('main'))),
	methodSignature_parameters(MainSignature,[JavaLangStringArray]),
	methodSignature_result(MainSignature,none),
	class_method(Cl1,MainSignature,Main),
	method_body(Main,MainBody),
	bytecodeMethod_localVarSize(MainBody,LocalVarSize),
	bytecodeMethod_firstAddress(MainBody,PC), 
	% building the frame
	length(LocalVar,LocalVarSize),
	init_localVar(LocalVar,LocalVarSize),
	OperandStack=[],
	Frame=fr(Main,PC,OperandStack,LocalVar),
	%check(method(Main)),
	% building de state
	Callstack=[],
	HeapInit=heap(dynamicHeap([]),staticHeap([])),
	load_classes(HeapInit,[Cl1|RCL],Heap),
	State=(st(Heap,Frame,Callstack)),
	check(state(State)).

init_localVar(L,I):-
	length(L,I),
	init(L,num(int(0)),I).

load_classes(Heap,[],Heap).
load_classes(HeapOld,[CL|RCL],HeapNew):-
	load_class(HeapOld,CL,HeapI),
	load_classes(HeapI,RCL,HeapNew).

% Note : the super class must also be loaded
load_class(Heap,none,Heap).
load_class(HeapOld,Class,HeapNew):-
	class_fields(Class,Fields),
	load_class_fields(HeapOld,Fields,HeapNew).
load_class_fields(Heap,[],Heap).
load_class_fields(HeapOld,[Field|RF],HeapNew):-
	Field=field(Signature,_,static(true),_,_),
	HeapOld=heap(_,staticHeap(SH)),
	member(staticCell(Signature,_),SH),
	load_class_fields(HeapOld,RF,HeapNew).
load_class_fields(HeapOld,[Field|RF],HeapNew):-
	Field=field(Signature,_,static(true),_,_),
	HeapOld=heap(DH,staticHeap(SH)),
	\+ member(staticCell(Signature,_),SH),
	init_field_value(Field,InitialValue),
	HeapI=heap(DH,staticHeap([staticCell(Signature,InitialValue)|SH])),
	load_class_fields(HeapI,RF,HeapNew).
load_class_fields(HeapOld,[field(_,_,static(false),_,_)|RF],HeapNew):-
	load_class_fields(HeapOld,RF,HeapNew).
