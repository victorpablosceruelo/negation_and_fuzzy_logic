:- module( _, _, [assertions , regtypes , basicmodes , nativeprops] ).

:- include(library(jvm_in_ciao(interpreter(exec_header)))).

'<init>'([[ref(loc(_1))],A],[A]).

equals([[ref(loc(A)),B],C],[D,E]) :-
        B\=ref(loc(A)),
        execute_1(C,[num(int(0))],ref(loc(A)),B,E,_1,_2,_3,_4,D,_5) .
equals([[ref(loc(A)),ref(loc(A))],B],[C,D]) :-
        execute_1(B,[num(int(1))],ref(loc(A)),ref(loc(A)),D,_1,_2,_3,_4,C,_5) .

wait([[ref(loc(A)),num(int(B)),num(int(C))],D],[E]) :-
        0>B,
        execute_2(D,[num(int(-1))],ref(loc(A)),num(int(B)),num(int(C)),num(int(0)),E,_1,_2,_3,_4,_5) .
wait([[ref(loc(A)),num(int(0)),num(int(B))],C],[D]) :-
        execute_2(C,[num(int(0))],ref(loc(A)),num(int(0)),num(int(B)),num(int(0)),D,_1,_2,_3,_4,_5) .
wait([[ref(loc(A)),num(int(B)),num(int(C))],D],[E]) :-
        0<B,
        execute_2(D,[num(int(1))],ref(loc(A)),num(int(B)),num(int(C)),num(int(0)),E,_1,_2,_3,_4,_5) .
wait([[ref(loc(A))],heap(dynamicHeap(C),B)],[heap(dynamicHeap(D),B)]) :-
        'Object':wait([[ref(loc(A)),num(int(0))],heap(dynamicHeap(C),B)],[heap(dynamicHeap(D),B)]) .

finalize([[ref(loc(_1))],A],[A]).

%'<clinit>'([[],heap(dynamicHeap(B),A)],[heap(dynamicHeap(C),A)]) :-
%        'Object':registerNatives([[],heap(dynamicHeap(B),A)],[heap(dynamicHeap(C),A)]) .

execute_1(A,[num(int(C))|B],_1,_2,A,_3,_4,_5,_6,num(int(C)),B).

execute_2(A,[num(int(M))|L],B,C,D,E,F,G,H,I,J,K) :-
        M>=0,
        execute_3(A,L,B,C,D,E,F,G,H,I,J,K) .

execute_3(A,B,C,D,E,num(int(L)),F,G,H,I,J,K) :-
        execute_4(A,[num(int(L))|B],C,D,E,num(int(L)),F,G,H,I,J,K) .

execute_4(A,[num(int(M))|L],B,C,D,E,F,G,H,I,J,K) :-
        M<0,
        execute_5(A,L,B,C,D,E,F,G,H,I,J,K) .
execute_4(A,[num(int(L))|K],B,C,D,num(int(M)),E,F,G,H,I,J) :-
        L>=0,
        execute_6(A,[num(int(999999)),num(int(M))|K],B,C,D,num(int(M)),E,F,G,H,I,J) .

execute_5(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12) :-
        fail .

execute_6(A,[num(int(L)),num(int(N))|M],B,C,D,E,F,G,H,I,J,K) :-
        N=<L,
        execute_7(A,M,B,C,D,E,F,G,H,I,J,K) .
execute_6(A,[num(int(L)),num(int(N))|M],B,C,D,E,F,G,H,I,J,K) :-
        N>L,
        execute_5(A,M,B,C,D,E,F,G,H,I,J,K) .

execute_7(A,B,C,D,E,num(int(L)),F,G,H,I,J,K) :-
        execute_8(A,[num(int(500000)),num(int(L))|B],C,D,E,num(int(L)),F,G,H,I,J,K) .

execute_8(A,[num(int(L)),num(int(N))|M],B,C,D,E,F,G,H,I,J,K) :-
        N>=L,
        execute_9(A,M,B,C,D,E,F,G,H,I,J,K) .
execute_8(A,[num(int(K)),num(int(M))|L],B,C,D,num(int(N)),E,F,G,H,I,J) :-
        M<K,
        execute_11(A,[num(int(N))|L],B,C,D,num(int(N)),E,F,G,H,I,J) .

execute_9(A,B,C,num(int(L)),D,E,F,G,H,I,J,K) :-
        M is L+1,
        execute_10(A,B,C,num(int(M)),D,E,F,G,H,I,J,K) .

execute_10(heap(dynamicHeap(D),C),A,B,num(int(E)),_1,_2,heap(dynamicHeap(F),C),_3,_4,_5,A,_6) :-
        'Object':wait([[B,num(int(E))],heap(dynamicHeap(D),C)],[heap(dynamicHeap(F),C)]) .

execute_11(A,[num(int(0))|L],B,C,D,E,F,G,H,I,J,K) :-
        execute_10(A,L,B,C,D,E,F,G,H,I,J,K) .
execute_11(A,[num(int(L))|K],B,num(int(M)),C,D,E,F,G,H,I,J) :-
        L\=0,
        0>M,
        execute_12(A,[num(int(-1))|K],B,num(int(M)),C,D,E,F,G,H,I,J) .
execute_11(A,[num(int(L))|K],B,num(int(0)),C,D,E,F,G,H,I,J) :-
        L\=0,
        execute_12(A,[num(int(0))|K],B,num(int(0)),C,D,E,F,G,H,I,J) .
execute_11(A,[num(int(L))|K],B,num(int(M)),C,D,E,F,G,H,I,J) :-
        L\=0,
        0<M,
        execute_12(A,[num(int(1))|K],B,num(int(M)),C,D,E,F,G,H,I,J) .

execute_12(A,[num(int(M))|L],B,C,D,E,F,G,H,I,J,K) :-
        M\=0,
        execute_10(A,L,B,C,D,E,F,G,H,I,J,K) .
execute_12(A,[num(int(0))|L],B,C,D,E,F,G,H,I,J,K) :-
        execute_9(A,L,B,C,D,E,F,G,H,I,J,K) .

:- entry this_class(_1).

this_class(class(className(packageName('java/lang/'),shortClassName('Object')),final(false),public(true),abstract(false),none,[],[],[method(methodSignature(methodName(className(packageName('java/lang/'),shortClassName('Object')),shortMethodName('<init>')),[],none),bytecodeMethod(1,0,0,methodId('0',0),[]),final(false),static(false),public),method(methodSignature(methodName(className(packageName('java/lang/'),shortClassName('Object')),shortMethodName(registerNatives)),[],none),_1,final(false),static(true),private),method(methodSignature(methodName(className(packageName('java/lang/'),shortClassName('Object')),shortMethodName(getClass)),[],refType(classType(className(packageName('java/lang/'),shortClassName('Class'))))),_2,final(true),static(false),public),method(methodSignature(methodName(className(packageName('java/lang/'),shortClassName('Object')),shortMethodName(hashCode)),[],primitiveType(int)),_3,final(false),static(false),public),method(methodSignature(methodName(className(packageName('java/lang/'),shortClassName('Object')),shortMethodName(equals)),[refType(classType(className(packageName('java/lang/'),shortClassName('Object'))))],primitiveType(boolean)),bytecodeMethod(2,2,0,methodId('0',4),[]),final(false),static(false),public),method(methodSignature(methodName(className(packageName('java/lang/'),shortClassName('Object')),shortMethodName(clone)),[],refType(classType(className(packageName('java/lang/'),shortClassName('Object'))))),_4,final(false),static(false),public),method(methodSignature(methodName(className(packageName('java/lang/'),shortClassName('Object')),shortMethodName(toString)),[],refType(classType(className(packageName('java/lang/'),shortClassName('String'))))),bytecodeMethod(1,2,0,methodId('0',6),[]),final(false),static(false),public),method(methodSignature(methodName(className(packageName('java/lang/'),shortClassName('Object')),shortMethodName(notify)),[],none),_5,final(true),static(false),public),method(methodSignature(methodName(className(packageName('java/lang/'),shortClassName('Object')),shortMethodName(notifyAll)),[],none),_6,final(true),static(false),public),method(methodSignature(methodName(className(packageName('java/lang/'),shortClassName('Object')),shortMethodName(wait)),[primitiveType(long)],none),_7,final(true),static(false),public),method(methodSignature(methodName(className(packageName('java/lang/'),shortClassName('Object')),shortMethodName(wait)),[primitiveType(long),primitiveType(int)],none),bytecodeMethod(4,4,0,methodId('0',10),[]),final(true),static(false),public),method(methodSignature(methodName(className(packageName('java/lang/'),shortClassName('Object')),shortMethodName(wait)),[],none),bytecodeMethod(1,3,0,methodId('0',11),[]),final(true),static(false),public),method(methodSignature(methodName(className(packageName('java/lang/'),shortClassName('Object')),shortMethodName(finalize)),[],none),bytecodeMethod(1,0,0,methodId('0',12),[]),final(false),static(false),public),method(methodSignature(methodName(className(packageName('java/lang/'),shortClassName('Object')),shortMethodName('<clinit>')),[],none),bytecodeMethod(0,0,0,methodId('0',13),[]),final(false),static(true),public)])).


