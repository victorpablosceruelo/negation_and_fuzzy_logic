:- include(library(jvm_in_ciao(interpreter(pe_guides)))).

% Formalization of Java small step semantics. 
% Based on The "Java (TM) Virtual Machine Specification, 
%             Second Edition, Tim Lindholm, Frank Yellin" 


:- pred execute_t(+Sin,-Sout,-Tr) # "Main loop of the
	interpreter. Given a Java bytecode program stored in loaded_classes and the 
        initial state @var{Sin} it computes the state @var{Sout} after executing the
        program returning also the trace in @var{Tr}".
execute_t(S1,S3,[Step|RTrace]) :-
	fetch_instruction(S1,Inst),
	step(Inst,Step,S1,S2),
	execute_t(S2,S3,RTrace).
execute_t(S1,Sf,[normal_end]) :-
	S1=st(_H,fr(M,PC,_S,_L),[]),
	instructionAt(M,PC,return),
	state_cleanup(S1,Sf).
execute_t(S1,Sf,[normal_end]) :-
	S1=st(_H,fr(M,PC,[num(int(_I))|_S],_L),[]),
	instructionAt(M,PC,ireturn),
	state_cleanup(S1,Sf).
execute_t(S1,Sf,[normal_end]) :-
	S1=st(_H,fr(M,PC,[_Ref|_S],_L),[]),
	instructionAt(M,PC,areturn),
	state_cleanup(S1,Sf).
execute_t(S1,Sf,[exception_uncaught,aborting_end]) :-
 	S1=stE(H,frE(M,PC,Loc,_),[]),
	exception_uncaught(M,H,PC,Loc),
% 	method_body(M,BM),
% 	bytecodeMethod_exceptionHandlers(BM,ExL),
% 	not_lookup_handlers(ExL,H,PC,Loc,_),
	state_cleanup(S1,Sf).

:- pred execute(+Sin,-Sout) # "Main loop of the
	interpreter. Given a Java bytecode program in loaded_classes and the initial
        state @var{Sin} it computes the state @var{Sout} after executing the
        program".
execute(S1,S3) :-
	fetch_instruction(S1,Inst),
	step(Inst,_,S1,S2),
	execute(S2,S3).
execute(S1,Sf) :-
	S1=st(_H,fr(M,PC,_S,_L),[]),
	instructionAt(M,PC,return),
	state_cleanup(S1,Sf).
execute(S1,Sf) :-
	S1=st(_H,fr(M,PC,[num(int(_I))|_S],_L),[]),
	instructionAt(M,PC,ireturn),
	state_cleanup(S1,Sf).
execute(S1,Sf) :-
	S1=st(_H,fr(M,PC,[_Ref|_S],_L),[]),
	instructionAt(M,PC,areturn),
	state_cleanup(S1,Sf).
execute(S1,Sf) :-
 	S1=stE(H,frE(M,PC,Loc,_),[]),
	exception_uncaught(M,H,PC,Loc),
% 	method_body(M,BM),
% 	bytecodeMethod_exceptionHandlers(BM,ExL),
% 	not_lookup_handlers(ExL,H,PC,Loc,_),
	state_cleanup(S1,Sf).
execute(S1,Sf) :- % Special management for loops (preloop(_) and loopend(_))
	fetch_instruction(S1,loopend(Inst)),
	step(Inst,_,S1,Sf),
	is_postloop_state(Sf).


fetch_instruction(State,Inst) :-
	state_get_m(State,M),
	state_get_pc(State,PC),
	instructionAt(M,PC,Inst).

is_postloop_state(S) :-
	state_get_pc(S,PostLoopPC),
	state_get_m(S,M),
	get_loop_info(M,_,PostLoopPC).
not_postloop_state(S) :-
	state_get_pc(S,PC),
	state_get_m(S,M),
	\+ get_loop_info(M,_,PC).

state_cleanup(st(H,fr(_M,_PC,OS,_LV),_CS),st(H,fr(_,_,OS,_),_)).
state_cleanup(stE(H,frE(_M,_PC,Loc,_LV),_CS),st(H,fr(_,_,[refE(Loc)],_),_)).


% Special management for loops (preloop(_) and loopend(_))
step(preloop(Inst),StepName,S1,S2) :- 
 step(Inst,StepName,S1,Sloop), 
 Sloop = st(_H,fr(M,LoopStartPC,_OS,LV),FS),
 get_loop_info(M,LoopStartPC,PostLoopPC),
 generalize_local_vars(LV,GenLV),
 S2 = st(_,fr(M,PostLoopPC,_,GenLV),FS),
 execute(Sloop,S2).

step(loopend(Inst),StepName,S1,S2) :-
 step(Inst,StepName,S1,S2),
 not_postloop_state(S2).

% Exception Management
% The current exception is caught in the current method
step(_,exception_caught, 
	stE(H,frE(M,PC,Loc,L),SF),
	st(H,fr(M,PCb,[ref(Loc)],L),SF)):-
 exception_caught(M,H,PC,Loc,PCb).
%method_body(M,BM),
%bytecodeMethod_exceptionHandlers(BM,ExL),
%lookup_handlers(ExL,H,PC,Loc,PCb).
% The current exception is uncaught
step(_,exception_uncaught, 
	stE(H,frE(M,PC,Loc,_L),[fr(Mb,PCb,_Sb,Lb)|SF]),
	stE(H,frE(Mb,PCb,Loc,Lb),SF)):-
 exception_uncaught(M,H,PC,Loc).
%method_body(M,BM),
%bytecodeMethod_exceptionHandlers(BM,ExL),
%not_lookup_handlers(ExL,H,PC,Loc,_).

% aaload: Load reference from array 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#aaload
step(aaload,aaload_step_ok, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[Val|S],L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,refType(_RT))),
 0 =< I, % res_susc
 I < Length, % res_susc
 heap_get(H,arrayElement(Loc,I),Val).
step(aaload,aaload_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Loc,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Loc,Hb).
step(aaload,aaload_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,refType(_RT))),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).

% aastore: Store into reference array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#aastore
step(aastore,aastore_step_ok, 
	st(H,fr(M,PC,[Val,num(int(I)),ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,TP)),
 assign_compatible(H,Val,TP),
 0 =< I, I < Length, % res_susc
 heap_update(H,arrayElement(Loc,I),Val,Hb).
step(aastore,aastore_step_NullPointerException, 
	st(H,fr(M,PC,[_Val,num(int(_I)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(aastore,aastore_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[_Val,num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,refType(_RT))),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).
step(aastore,aastore_step_ArrayStoreException, 
	st(H,fr(M,PC,[Val,num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,TP)),
 0 =< I, I < Length, % res_susc
 not_assign_compatible(H,Val,TP),
 arrayStoreException(ASE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,ASE)),Locb,Hb).

% aconst_null: Push null 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#aconst_null
step(aconst_null,aconst_null, 
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,[null|S],L),SF)):-
 next(M,PC,PCb).

% aload: Load reference from local variable
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#aload
step(aload(X),aload_step_ok, 
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,[Val|S],L),SF)):-
 next(M,PC,PCb),
 localVar_get(L,X,Val),
 isReference(Val).

% anewarray: Create new array of reference 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#anewarray
% OutOfMemory is not considered in Bicolano
step(anewarray(CN),anewarray_step_ok, 
	st(H,fr(M,PC,[num(int(Length))|S],L),SF),
	st(Hb,fr(M,PCb,[ref(Loc)|S],L),SF)):-
 next(M,PC,PCb),
 0 =< Length, % res_susc
 heap_new(H,locationArray(Length,refType(classType(CN))),Loc,Hb).
step(anewarray(_T),anewarray_step_NegativeArraySizeException, 
	st(H,fr(M,PC,[num(int(Length))|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 Length < 0, % res_susc
 negativeArraySizeException(NASE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NASE)),Locb,Hb).

% areturn: Return reference from method
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#areturn
step(areturn,areturn_step_ok, 
	st(H,fr(M,_PC,[Val|_S],_L),CallStack),
	st(H,fr(Mb,PCbb,[Val|Sb],Lb),SF)):-
 nonvar(CallStack),
 CallStack = [fr(Mb,PCb,Sb,Lb)|SF],
 next(Mb,PCb,PCbb),
 method_signature(M,MSig),
 methodSignature_result(MSig,refType(RT)),
 assign_compatible(H,Val,refType(RT)).

% arraylength: Get length of array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#arraylength
step(arraylength,arraylength_step_ok, 
	st(H,fr(M,PC,[ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(Length))|S],L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,_TP)).
step(arraylength,arraylength_step_NullPointerException, 
	st(H,fr(M,PC,[null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).

% astore: Store reference into local variable
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#astore
step(astore(X),astore_step_ok,
	st(H,fr(M,PC,[Val|S],L),SF),
	st(H,fr(M,PCb,S,Lb),SF)):-
 isReference(Val),
 next(M,PC,PCb),
 localVar_update(L,X,Val,Lb).

% athrow: Throw exception or error
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc.html#athrow
step(athrow,athrow_step, 
	st(H,fr(M,PC,[ref(Loc)|_S],L),SF),
	stE(H,frE(M,PC,Loc,L),SF)).
step(athrow,athrow_step_NullPointerException, 
	st(H,fr(M,PC,[null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).

% baload: Load byte or boolean from array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#baload
step(baload,baload_step_ok, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(B))|S],L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(TP))),
 0 =< I, % res_susc
 I < Length, % res_susc
 (TP=boolean;TP=byte),
 heap_get(H,arrayElement(Loc,I),num(int(B))).
step(baload,baload_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(baload,baload_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,_RT)),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).

% caload: Load char from array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#caload
step(caload,caload_step_ok, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(B))|S],L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(char))),
 0 =< I, % res_susc
 I < Length, % res_susc
 heap_get(H,arrayElement(Loc,I),num(int(B))).
step(caload,caload_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(caload,caload_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,_RT)),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).

% saload: Load short from array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#saload
step(saload,saload_step_ok, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(B))|S],L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(short))),
 0 =< I, % res_susc
 I < Length, % res_susc
 heap_get(H,arrayElement(Loc,I),num(int(B))).
step(saload,saload_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(saload,saload_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,_RT)),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).

% laload: Load long from array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#laload
step(laload,laload_step_ok, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(B))|S],L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(long))),
 0 =< I, % res_susc
 I < Length, % res_susc
 heap_get(H,arrayElement(Loc,I),num(int(B))).
step(laload,laload_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(laload,laload_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,_RT)),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).


% faload: Load float from array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#faload
step(faload,faload_step_ok, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(B))|S],L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(float))),
 0 =< I, % res_susc
 I < Length, % res_susc
 heap_get(H,arrayElement(Loc,I),num(int(B))).
step(faload,faload_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(faload,faload_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,_RT)),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).

% daload: Load double from array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#daload
step(daload,daload_step_ok, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(B))|S],L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(double))),
 0 =< I, % res_susc
 I < Length, % res_susc
 heap_get(H,arrayElement(Loc,I),num(int(B))).
step(daload,daload_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(daload,daload_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,_RT)),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).

% bastore: Store into byte or boolean array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#bastore
step(bastore,bastore_step_ok, 
	st(H,fr(M,PC,[num(int(Ib)),num(int(I)),ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(byte))),
 0 =< I, I < Length, % res_susc
 i2b(Ib,B), % res_susc
 heap_update(H,arrayElement(Loc,I),num(int(B)),Hb). % res_susc into
step(bastore,bastore_step_ok, 
	st(H,fr(M,PC,[num(int(Ib)),num(int(I)),ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(boolean))),
 0 =< I, I < Length, % res_susc
 i2bool(Ib,B), % res_susc
 heap_update(H,arrayElement(Loc,I),num(int(B)),Hb). % res_susc into
step(bastore,bastore_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),num(int(_I)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(bastore,bastore_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(_)),num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,refType(_RT))),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).

% castore: Store into char array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#castore
step(castore,castore_step_ok, 
	st(H,fr(M,PC,[num(int(Ib)),num(int(I)),ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(char))),
 0 =< I, I < Length, % res_susc
 i2s(Ib,C), % res_susc
 heap_update(H,arrayElement(Loc,I),num(int(C)),Hb). % res_susc into
step(castore,castore_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),num(int(_I)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(castore,castore_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(_)),num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,refType(_RT))),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).

% sastore: Store into short array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#sastore
step(sastore,sastore_step_ok, 
	st(H,fr(M,PC,[num(int(Ib)),num(int(I)),ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(short))),
 0 =< I, I < Length, % res_susc
 i2s(Ib,Short), % res_susc
 heap_update(H,arrayElement(Loc,I),num(int(Short)),Hb). % res_susc into
step(sastore,sastore_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),num(int(_I)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(sastore,sastore_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(_)),num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,refType(_RT))),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).

% lastore: Store into long array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#lastore
step(lastore,lastore_step_ok, 
	st(H,fr(M,PC,[num(int(Ib)),num(int(I)),ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(long))),
 0 =< I, I < Length, % res_susc
 %i2b(Ib,B), % res_susc
 heap_update(H,arrayElement(Loc,I),num(int(Ib)),Hb). % res_susc into
step(lastore,lastore_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),num(int(_I)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(lastore,lastore_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(_)),num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,refType(_RT))),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).

% fastore: Store into float array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#fastore
step(fastore,fastore_step_ok, 
	st(H,fr(M,PC,[num(int(Ib)),num(int(I)),ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(float))),
 0 =< I, I < Length, % res_susc
 %i2b(Ib,B), % res_susc
 heap_update(H,arrayElement(Loc,I),num(int(Ib)),Hb). % res_susc into
step(fastore,fastore_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),num(int(_I)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(fastore,fastore_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(_)),num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,refType(_RT))),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).

% dastore: Store into double array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#dastore
step(dastore,dastore_step_ok, 
	st(H,fr(M,PC,[num(int(Ib)),num(int(I)),ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(double))),
 0 =< I, I < Length, % res_susc
 %i2b(Ib,B), % res_susc
 heap_update(H,arrayElement(Loc,I),num(int(Ib)),Hb). % res_susc into
step(dastore,dastore_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),num(int(_I)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(dastore,dastore_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(_)),num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,refType(_RT))),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).

% checkcast: Check whether object is of given type
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc2.html#checkcast
step(checkcast(T),checkcast_step_ok, 
	st(H,fr(M,PC,[Val|S],L),SF),
	st(H,fr(M,PCb,[Val|S],L),SF)):-
 next(M,PC,PCb),
 assign_compatible(H,Val,refType(T)).
step(checkcast(T),checkcast_step_ClassCastException, 
	st(H,fr(M,PC,[Val|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 not_assign_compatible(H,Val,refType(T)),
 classCastException(CCE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,CCE)),Locb,Hb).

% Code for evho mode.
%% step(const(string,StrA),const_step_ok, 
%% 	st(H,fr(M,PC,S,L),SF),
%% 	st(H_p5,fr(M,PCb,[ref(SORefLoc)|S],L),SF)):-
%%  next(M,PC,PCb),
%% %create char array
%% % heap_new(H,locationArray(0,primitiveType(char)),loc(CARef),H_p),
%%  create_array(H,array(locationArray(Size,primitiveType(char)),CAVs),CARef,H_p),
%% %call heap_new, to create a new string object in the heap
%%  javaLangString(CN),
%%  heap_new(H_p,locationObject(CN),SORefLoc,H_p2),
%%  const_string_evalpart(StrA,CAVs,Size,H_p2,CARef,CN,SORefLoc,H_p5).

% Code for resho mode. 
step(const(string,StrA),const_step_ok, 
	st(H,fr(M,PC,S,L),SF),
	st(H_p,fr(M,PCb,[ref(loc(StrRef))|S],L),SF)):-
 next(M,PC,PCb),
 create_string(H,StrA,StrRef,H_p).

% const : Push numeric constant (bipush, iconst, ldc(no string constant supported), sipush)
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc1.html#bipush
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#iconst_i
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc8.html#ldc
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc13.html#sipush
step(const(T,Z),const_step_ok, 
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,[num(int(Z))|S],L),SF)):-
 T \== string,
 next(M,PC,PCb).

% dup: Duplicate the top operand stack value
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc3.html#dup
step(dup,dup_step_ok, 
	st(H,fr(M,PC,[V|S],L),SF),
	st(H,fr(M,PCb,[V,V|S],L),SF)):-
 next(M,PC,PCb).

% dup_x1: Duplicate the top operand stack value and insert two values down
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc3.html#dup_x1
step(dup_x1,dup_x1_step_ok, 
	st(H,fr(M,PC,[V1,V2|S],L),SF),
	st(H,fr(M,PCb,[V1,V2,V2|S],L),SF)):-
 next(M,PC,PCb).

% dup_x2: Duplicate the top operand stack value and insert two or three values down
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc3.html#dup_x2
step(dup_x2,dup_x2_step_ok, 
	st(H,fr(M,PC,[V1,V2,V3|S],L),SF),
	st(H,fr(M,PCb,[V1,V2,V3,V1|S],L),SF)):-
 next(M,PC,PCb).

% dup2: Duplicate the top one or two operand stack values
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc3.html#dup2
step(dup2,dup2_step_ok, 
	st(H,fr(M,PC,[V1,V2|S],L),SF),
	st(H,fr(M,PCb,[V1,V2,V1,V2|S],L),SF)):-
 next(M,PC,PCb).

% dup2_x1: Duplicate the top one or two operand stack values and insert two or three values down
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc3.html#dup2_x1
step(dup2_x1,dup2_x1_step_ok, 
	st(H,fr(M,PC,[V1,V2,V3|S],L),SF),
	st(H,fr(M,PCb,[V1,V2,V3,V1,V2|S],L),SF)):-
 next(M,PC,PCb).

% dup2_x2: Duplicate the top one or two operand stack values and insert two, three, or four values down
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc3.html#dup2_x2
step(dup2_x2,dup2_x2_step_ok, 
	st(H,fr(M,PC,[V1,V2,V3,V4|S],L),SF),
	st(H,fr(M,PCb,[V1,V2,V3,V4,V1,V2|S],L),SF)):-
 next(M,PC,PCb).

% getfield: Fetch field from object
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc5.html#getfield
step(getfield(F),getfield_step_ok, 
	st(H,fr(M,PC,[ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[V|S],L),SF)):-
 next(M,PC,PCb),
 heap_get(H,dynamicField(Loc,F),V).
step(getfield(_F),getfield_step_NullPointerException, 
	st(H,fr(M,PC,[null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).

% getstatic: Get static field from class
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc5.html#getstatic
step(getstatic(F),getstatic_step_ok, 
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,[V|S],L),SF)):-
 next(M,PC,PCb),
 isStatic(F),
 heap_get(H,staticField(F),V).

% goto: Branch always
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc5.html#goto
step(goto(O),goto_step_ok, 
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 PCb is PC+O.

% i2b: Convert int to byte 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#i2b
step(i2b,i2b_step_ok, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,[num(int(Ib))|S],L),SF)):-
 next(M,PC,PCb),
 i2b(I,Ib). % res_susc

% i2s: Convert int to short 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#i2s
step(i2s,i2s_step_ok, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,[num(int(Ib))|S],L),SF)):-
 next(M,PC,PCb),
 i2s(I,Ib). % res_susc

% i2c: Convert int to char 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#i2c
step(i2c,i2c_step_ok, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,[num(int(Ib))|S],L),SF)):-
 next(M,PC,PCb),
 i2s(I,Ib). % res_susc

% l2i: Convert long to int 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#l2i
step(l2i,l2i_step_ok, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,[num(int(Ib))|S],L),SF)):-
 next(M,PC,PCb),
 l2i(I,Ib). % res_susc

% f2i: Convert float to int 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#f2i
step(f2i,f2i_step_ok, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,[num(int(Ib))|S],L),SF)):-
 next(M,PC,PCb),
 f2i(I,Ib). % res_susc

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
step(ibinop(Op),ibinop_step_ok, 
	st(H,fr(M,PC,[num(int(I2)),num(int(I1))|S],L),SF),
	st(H,fr(M,PCb,[num(int(R))|S],L),SF)):-
 ibinop_step_cond(Op,I2),
 next(M,PC,PCb),
 semBinopInt(Op,I1,I2,R).
step(ibinop(Op),ibinop_ArithmeticException, 
	st(H,fr(M,PC,[num(int(0)),num(int(_I1))|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 (Op=divInt;Op=remInt),
 arithmeticException(AE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,AE)),Locb,Hb).

% iaload: Load int from array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#iaload
step(iaload,iaload_step_ok, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(Ib))|S],L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(int))),
 0 =< I, % res_susc
 I < Length, % res_susc
 heap_get(H,arrayElement(Loc,I),num(int(Ib))).
step(iaload,iaload_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(iaload,iaload_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,primitiveType(int))),
 (I < 0 ; I >= Length), %res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).

% iastore: Store into int array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#iastore
step(iastore,iastore_step_ok, 
	st(H,fr(M,PC,[num(int(Ib)),num(int(I)),ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationArray(Length,primitiveType(int))),
 0 =< I, I < Length, % res_susc
 heap_update(H,arrayElement(Loc,I),num(int(Ib)),Hb).
step(iastore,iastore_step_NullPointerException, 
	st(H,fr(M,PC,[num(int(_)),num(int(_I)),null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).
step(iastore,iastore_step_ArrayIndexOutOfBoundsException, 
	st(H,fr(M,PC,[num(int(_)),num(int(I)),ref(Loc)|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 heap_typeof(H,Loc,locationArray(Length,refType(int))),
 (I < 0 ; I >= Length), % res_susc
 javaLang(JL),
 arrayIndexOutOfBoundsException(AIOOBE),
 heap_new(H,locationObject(className(JL,AIOOBE)),Locb,Hb).

% if_acmpeq: Branch if reference comparison succeeds
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#if_acmp
step(if_acmpeq(O),if_acmpeq_step_jump, 
	st(H,fr(M,PC,[V1,V1|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 isReference(V1),
 PCb is PC+O.
step(if_acmpeq(_O),if_acmpeq_step_continue, 
	st(H,fr(M,PC,[V1,V2|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 isReference(V1),
 isReference(V2),
 V1 \= V2.

% if_acmpne: Branch if reference comparison does not succeed
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#if_acmp
step(if_acmpne(O),if_acmpne_step_jump, 
	st(H,fr(M,PC,[V1,V2|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 isReference(V1),
 isReference(V2),
 V1 \= V2,
 PCb is PC+O.
step(if_acmpne(_O),if_acmpne_step_continue, 
	st(H,fr(M,PC,[V1,V1|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 isReference(V1).

% if_icmp: Branch if int comparison succeeds
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#if_icmp
step(if_icmp(Cmp,O),if_icmp_step_jump, 
	st(H,fr(M,PC,[num(int(I2)),num(int(I1))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 semCompInt(Cmp,I1,I2),
 PCb is PC+O.
step(if_icmp(Cmp,_O),if_icmp_step_continue, 
	st(H,fr(M,PC,[num(int(I2)),num(int(I1))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 noSemCompInt(Cmp,I1,I2).

% if0 (ifcond): Branch if int comparison with zero succeeds
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ifcond
step(if0(Cmp,O),if0_step_jump, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 semCompInt(Cmp,I,0),
 PCb is PC+O.
step(if0(Cmp,_O),if0_step_continue, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 noSemCompInt(Cmp,I,0),
 next(M,PC,PCb).

% lcmp: It pops -1,0,1 depending on the comparison of the two top elements of the stack
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#lcmp
step(lcmp,lcmp_step_-1, 
	st(H,fr(M,PC,[num(int(I)),num(int(Ib))|S],L),SF),
	st(H,fr(M,PCb,[num(int(-1))|S],L),SF)):-
 semCompInt(gtInt,I,Ib),
 next(M,PC,PCb).
step(lcmp,lcmp_step_0, 
	st(H,fr(M,PC,[num(int(I)),num(int(Ib))|S],L),SF),
	st(H,fr(M,PCb,[num(int(0))|S],L),SF)):-
 semCompInt(eqInt,I,Ib),
 next(M,PC,PCb).
step(lcmp,lcmp_step_1, 
	st(H,fr(M,PC,[num(int(I)),num(int(Ib))|S],L),SF),
	st(H,fr(M,PCb,[num(int(1))|S],L),SF)):-
 semCompInt(ltInt,I,Ib),
 next(M,PC,PCb).

% ifnonnull: Branch if reference not null 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ifnonnull
step(ifnonnull(O),ifnonnull_step_jump, 
	st(H,fr(M,PC,[ref(_Loc)|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 PCb is PC+O.
step(ifnonnull(_O),ifnonnull_step_continue, 
	st(H,fr(M,PC,[null|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb).

% ifnull: Branch if reference is null 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ifnull
step(ifnull(O),ifnull_step_jump, 
	st(H,fr(M,PC,[null|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 PCb is PC+O.
step(ifnull(_O),ifnull_step_continue, 
	st(H,fr(M,PC,[ref(_Loc)|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb).

% iinc: Increment local variable by constant
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#iinc
step(iinc(X,Z),iinc_step, 
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,S,Lb),SF)):-
 next(M,PC,PCb),
 localVar_get(L,X,num(int(I))),
 semBinopInt(addInt,I,Z,R),
 localVar_update(L,X,num(int(R)),Lb).

% iload: Load int from local variab
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#iload
step(iload(X),iload_step, 
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,[num(int(I))|S],L),SF)):-
 next(M,PC,PCb),
 localVar_get(L,X,num(int(I))).

% ineg: Negate int 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#ineg
step(ineg,ineg_step, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,[num(int(Ib))|S],L),SF)):-
 next(M,PC,PCb),
 negInt(I,Ib).

% lneg: Negate int 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc8.html#lneg
step(lneg,lneg_step, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,[num(int(Ib))|S],L),SF)):-
 next(M,PC,PCb),
 negInt(I,Ib).

% fneg: Negate int 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc4.html#fneg
step(fneg,fneg_step, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,[num(int(Ib))|S],L),SF)):-
 next(M,PC,PCb),
 negInt(I,Ib).

% dneg: Negate int 
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc3.html#dneg
step(dneg,dneg_step, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,[num(int(Ib))|S],L),SF)):-
 next(M,PC,PCb),
 negInt(I,Ib).

% instanceof: Determine if object is of given type
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#instanceof
step(instanceof(T),instanceof_step_ok1, 
	st(H,fr(M,PC,[ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(1))|S],L),SF)):-
 next(M,PC,PCb),
 assign_compatible(H,ref(Loc),refType(classType(T))).
step(instanceof(T),instanceof_step_ok2, 
	st(H,fr(M,PC,[ref(Loc)|S],L),SF),
	st(H,fr(M,PCb,[num(int(0))|S],L),SF)):-
 next(M,PC,PCb),
 not_assign_compatible(H,ref(Loc),refType(classType(T))).
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
step(invokespecial(Mid),invokespecial_step_here_ok, 
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(Mb,PCb,[],Lb),[fr(M,PC,Sb,L)|SF])):-
 methodSignature_name(Mid,methodName(MidCn,_)),
 resolve_method(MidCn,Mid,Meth),

 method_signature(Meth,MethSig),
 methodSignature_parameters(MethSig,Param),
 length(Param,NbParam),
 length(Args,NbParam),
 append(Args,[ref(Loc)|Sb],S), % the stack contains the parameters, the reference and the rest

 %heap_typeof(H,Loc,locationObject(Cn)), %due to AHR
 method_signature(Meth,Meths),% Miky: This call is already done with MethSig, no?
 methodSignature_name(Meths,Methn),
 Methn = methodName(_Methcl,_Methsmn),
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
% Methsmn=shortMethodName('<init>'),Commented to include here private methods cases

 Mb=Meth,
% compatible_param(H,Args,Param),
 method_body(Mb,BMb),
 bytecodeMethod_firstAddress(BMb,PCb),
 bytecodeMethod_localVarSize(BMb,Llength),
 RLlength is Llength - NbParam -1,
 length(RL,RLlength),
 init_localVar(RL,RLlength),
 reverse(Args,RL,Lb1),
 Lb = [ref(Loc)|Lb1].
% check(length(Lb,Llength)).

% The following is supposed to hadle invokespecial for private methods. For the moment they are handled naively in the previous case
%%  step(invokespecial_step_ok, P,
%%  	st(H,fr(M,PC,S,L),SF),
%%  	st(H,fr(Mb,PCb,[],Lb),[fr(M,PC,Sb,L)|SF])):-
%%   instructionAt(M,PC,invokespecial(Mid)),
%%   methodSignature_name(Mid,methodName(MidCn,_)),
%%   resolve_method(P,MidCn,Mid,Meth),
%% 
%%   method_signature(Meth,MethSig),
%%   methodSignature_parameters(MethSig,Param),
%%   length(Param,NbParam),
%%   length(Args,NbParam),
%%   append(Args,[ref(Loc)|Sb],S), % the stack contains the parameters, the reference and the rest
%%   heap_typeof(H,Loc,locationObject(Cn)),
%%   method_signature(Meth,Meths),
%%   methodSignature_name(Meths,Methn),
%%   Methn = methodName(Methcl,Methsmn),
%%   method_signature(M,Msig),
%%   methodSignature_name(Msig,methodName(CCn,_)),
%%   program_class(P,CCn,CC),
%%   class_superClass(CC,SCCn),     
%%   ( ( method_visibility(Meth,protected),
%%        subclass_name(P,CCn,Methcl),
%%        subclass_name(P,Cn,CCn))
%%    ;
%%     ( method_visibility(Meth,Visib),
%%       Visib \= protected)),
%%   subclass_name(P,SCCn,Methcl),
%%   Methsmn \= shortMethodName('<init>'),
%%   lookup(P,SCCn,Meths,Mb),
%%   compatible_param(P,H,Args,Param),
%%   method_body(Mb,BMb),
%%   bytecodeMethod_firstAddress(BMb,PCb),
%%   bytecodeMethod_localVarSize(BMb,Llength),
%%   RLlength is Llength - NbParam -1,
%%   length(RL,RLlength),
%%   init_localVar(RL,RLlength),
%%   reverse(Args,RL,Lb1),
%%   Lb = [ref(Loc)|Lb1],
%%   check(length(Lb,Llength)).

step(invokespecial(Mid),invokespecial_step_NullPointerException, 
	st(H,fr(M,PC,S,L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 methodSignature_parameters(Mid,Param),
 length(Param,NbParam),
 length(Args,NbParam),
 append(Args,[null|_Sb],S), % the stack contains the parameters, the reference and the rest
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).


% invokestatic: Invoke a class (static) method
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#invokestatic
step(invokestatic(Mid),invokestatic_step_ok, 
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(Mb,PCb,[],Lb),[fr(M,PC,Sb,L)|SF])):-
 methodSignature_name(Mid,methodName(CN,_SMN)),
 resolve_method(CN,Mid,Mb),
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
 reverse(Args,RL,Lb).
% check(length(Lb,Llength)).

% invokevirtual: Invoke instance method; dispatch based on class
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#invokevirtual
step(invokevirtual(Mid),invokevirtual_step_ok, 
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(Mb,PCb,[],Lb),[fr(M,PC,Sb,L)|SF])):-
 methodSignature_name(Mid,methodName(MidCn,_)),
 resolve_method(MidCn,Mid,Meth),

 method_signature(Meth,MethSig),

 methodSignature_name(MethSig,methodName(_MethCln,Methsmn)),
 Methsmn \= shortMethodName('<init>'),
 Methsmn \= shortMethodName('<clinit>'), 

 methodSignature_parameters(MethSig,Param),
 length(Param,NbParam),
 length(Args,NbParam),
 append(Args,[ref(Loc)|Sb],S), % the stack contains the parameters, the reference and the rest

%% Commented to enable non-modular decompilation in 'resho' mode. We simply ignore virtual invocations and
%%           interpret them as if they were static.
%%  heap_typeof(H,Loc,locationObject(Cn)),
%%  method_signature(M,Msig),
%%  methodSignature_name(Msig,methodName(CCn,_)),
%%  ( ( method_visibility(Meth,protected), % if/then
%%       subclass_name(CCn,MethCln),
%%       subclass_name(Cn,CCn))
%%   ;
%%    ( method_visibility(Meth,Visib),
%%      Visib \= protected)),
%%  lookup(Cn,MethSig,Mb),
%% 
 Mb = Meth, % Delete this line when come back to the real virtual invocation
 method_body(Mb,Bm),
 bytecodeMethod_firstAddress(Bm,PCb),
 bytecodeMethod_localVarSize(Bm,Llength),
 RLlength is Llength - NbParam -1,
 length(RL,RLlength),
 init_localVar(RL,RLlength),
 reverse(Args,RL,Lb1),
 Lb = [ref(Loc)|Lb1].
% check(length(Lb,Llength)).
step(invokevirtual(Mid),invokevirtual_step_NullPointerException, 
	st(H,fr(M,PC,S,L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 methodSignature_parameters(Mid,Param),
 length(Param,NbParam),
 length(Args,NbParam),
 append(Args,[null|_Sb],S), % the stack contains the parameters, the reference and the rest
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).

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
step(invoke_mod(MSig,IType),invoke_mod_step_ok, 
	st(H,fr(M,PC,S,L),SF),
	st(Hb,fr(M,PCb,Sb,L),SF)):-
 invoke_mod_evalpart(M,MSig,IType,PC,PCb,S,Sb,H,Hb,InArgs,OutArgsHb),
% filter_heap(H,FH), %obsolete
% Modular exclusive model
% ResCall =..[MN,[InArgs,H],OutArgsHb], %obsolete
% res_invoke(ResCall). % obsolete
 res_invoke(main(MSig,[InArgs,H],OutArgsHb)).
% General model
% main(MN,[InArgs,H],OutArgsHb). % obsolete
% main(MSig,[InArgs,H],OutArgsHb).

step(ireturn,ireturn_step_ok,
	st(H,fr(M,_PC,[num(int(I))|_S],_L),CallStack),
	st(H,fr(Mb,PCbb,[num(int(I))|Sb],Lb),SF)):-
 nonvar(CallStack),
 CallStack = [fr(Mb,PCb,Sb,Lb)|SF],
 next(Mb,PCb,PCbb),
 method_signature(M,MSig),
 methodSignature_result(MSig,primitiveType(_)).

% istore: Store int into local variable
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc6.html#istore
step(istore(X),istore_step_ok, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,S,Lb),SF)):-
 next(M,PC,PCb),
 localVar_update(L,X,num(int(I)),Lb).

% lookupswitch: Access jump table by key match and jump
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc8.html#lookupswitch
step(lookupswitch(_Def,ListKey),lookupswitch_step_ok1, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 member((I,O),ListKey),
 PCb is PC+O.
step(lookupswitch(Def,ListKey),lookupswitch_step_ok2, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 \+ member((I,_O),ListKey),
 PCb is PC+Def.

% multianewarray: Create new multidimensional array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc9.html#multianewarray
% new: Create new object
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc10.html#new
step(new(C),new_step_ok, 
	st(H,fr(M,PC,S,L),SF),
	st(Hb,fr(M,PCb,[ref(Loc)|S],L),SF)):-
 next(M,PC,PCb),
 heap_new(H,locationObject(C),Loc,Hb).

% newarray: Create new array
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc10.html#newarray
step(newarray(T),newarray_step_ok, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(Hb,fr(M,PCb,[ref(Loc)|S],L),SF)):-
 next(M,PC,PCb),
 0 =< I,
 heap_new(H,locationArray(I,T),Loc,Hb).
step(newarray(_T),newarray_step_NegativeArraySizeException, 
	st(H,fr(M,PC,[num(int(I))|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 I<0,
 negativeArraySizeException(NASE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NASE)),Locb,Hb).

% nop: Do nothing
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc10.html#nop
step(nop,nop_step_ok, 
	st(H,fr(M,PC,S,L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb).

% pop: Pop the top operand stack value
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc11.html#pop
step(pop,pop_step_ok, 
	st(H,fr(M,PC,[_V|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb).

% pop2: Pop the top one or two operand stack values
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc11.html#pop2
step(pop2,pop2_step_ok, 
	st(H,fr(M,PC,[_V1,_V2|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb).

% putfield: Set field in object
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc11.html#putfield
step(putfield(F),putfield_step_ok, 
	st(H,fr(M,PC,[V,ref(Loc)|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 heap_typeof(H,Loc,locationObject(_Cn)),
 %defined_field(_Cn,F),
 fieldSignature_type(F,FT),
 assign_compatible(H,V,FT),
 heap_update(H,dynamicField(Loc,F),V,Hb).
step(putfield(_F),putfield_step_NullPointerException, 
	st(H,fr(M,PC,[_V,null|_S],L),SF),
	stE(Hb,frE(M,PC,Locb,L),SF)):-
 nullPointerException(NPE),
 javaLang(JL),
 heap_new(H,locationObject(className(JL,NPE)),Locb,Hb).

% putstatic: Set static field in class
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc11.html#putstatic
step(putstatic(F),putstatic_step_ok, 
	st(H,fr(M,PC,[V|S],L),SF),
	st(Hb,fr(M,PCb,S,L),SF)):-
 next(M,PC,PCb),
 isStatic(F),
 fieldSignature_type(F,FT),
 assign_compatible(H,V,FT),
 heap_update(H,staticField(F),V,Hb).

% return: Return void from method
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc12.html#return
step(return,return_step_ok, 
	st(H,fr(M,_PC,_S,_L),CallStack),
	st(H,fr(Mb,PCbb,Sb,Lb),SF)):-
 nonvar(CallStack),
 CallStack = [fr(Mb,PCb,Sb,Lb)|SF],
 next(Mb,PCb,PCbb),
 method_signature(M,MSig),
 methodSignature_result(MSig,none).

% swap: Swap the top two operand stack values
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc13.html#swap
step(swap,swap_step_ok, 
	st(H,fr(M,PC,[V1,V2|S],L),SF),
	st(H,fr(M,PCb,[V2,V1|S],L),SF)):-
 next(M,PC,PCb).

% tableswitch: Access jump table by index and jump
% cf http://java.sun.com/docs/books/vmspec/2nd-edition/html/Instructions2.doc14.html#tableswitch
step(tableswitch(Def,Low,High,List_offset),tableswitch_step_ok1, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 (I < Low ; High < I),
% check((length(List_offset,N), N = High - Low +1)),
 length(List_offset,N), N = High - Low +1,
 PCb is PC + Def.
step(tableswitch(_Def,Low,High,List_offset),tableswitch_step_ok2, 
	st(H,fr(M,PC,[num(int(I))|S],L),SF),
	st(H,fr(M,PCb,S,L),SF)):-
 Low =< I,
 I =< High,
% check((length(List_offset,N), N = High - Low +1)),
 Nth is I-Low+1,
 nth(Nth,List_offset,O),
 PCb is PC+O.

invoke_mod_evalpart(M,MSig,IType,PC,PCb,OS,OSb,H,Hb,InArgs,OutArgsHb) :-
 next(M,PC,PCb),
% methodSignature_name(MSig,methodName(className(_,shortClassName(SCN)),
%                                      shortMethodName(MN))),
% resolve_method(P,CN,MSig,Mb),
 methodSignature_parameters(MSig,Param),
 length(Param,NbParam),
 length(RInArgs,NbParam),
 methodSignature_result(MSig,MSigResult),
 (MSigResult == none -> OutArgs = []
                     ; OutArgs = [_Out]),
% (method_isStatic(Mb) -> (
  (IType == static -> (
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
:- pred initial_state/1 : var => state.
:- trust pred initial_state/1  + eval.
:- trust pred initial_state/1  + sideff(free).

initial_state(State):-
	loaded_classes:main_class(MCN),
	program_class(MCN,Cl1),
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
	% building the state
	Callstack=[],
	HeapInit=heap(dynamicHeap([]),staticHeap([])),
	findall(Class,loaded_classes:get_class(Class),Classes),
	load_static_fields(HeapInit,Classes,Heap),
	State=(st(Heap,Frame,Callstack)).
%	check(state(State)).

:- pred init_localVar(_1,_2) : list*int => (list(_1),ground(_1)) + eval.

init_localVar(L,I):-
	length(L,I),
	init(L,num(int(0)),I).

:- pred load_static_fields/3 : heap*list(class)*var => heap*list(class)*heap + eval.

load_static_fields(Heap,[],Heap).
load_static_fields(HeapOld,[CL|RCL],HeapNew):-
	load_class(HeapOld,CL,HeapI),
	load_static_fields(HeapI,RCL,HeapNew).

% Note : the super class must also be loaded
:- pred load_class/3 : heap*class*var =>  heap*class*heap + eval.

load_class(Heap,none,Heap).
load_class(HeapOld,Class,HeapNew):-
	class_fields(Class,Fields),
	load_class_fields(HeapOld,Fields,HeapNew).

:- pred load_class_fields/3 : heap*list(field)*var => heap*list(field)*heap + eval.

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

generalize_local_vars([],[]).
generalize_local_vars([_V|T],[_|GenT]) :-
	generalize_local_vars(T,GenT).
