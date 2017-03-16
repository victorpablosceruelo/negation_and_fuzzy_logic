
:-regtype optionTypeb/1.
:-regtype optionClassName/1.
:-regtype optionBytecodeMethod/1.
optionTypeb(none).
optionTypeb(primitiveType(int)).
optionTypeb(primitiveType(boolean)).
optionTypeb(primitiveType(short)).
optionTypeb(primitiveType(byte)).
optionTypeb(primitiveType(char)). %byMiky
optionTypeb(refType(arrayType(T))):-type(T).
optionTypeb(refType(classType(T))):-className(T).
optionTypeb(refType(interfaceType(T))):-interfaceName(T).
optionClassName(none).
optionClassName(className(PN,SCN)):-
	packageName(PN),
	shortClassName(SCN).
optionBytecodeMethod(none).
optionBytecodeMethod(bytecodeMethod(LocalVarSize,StackSize,FirstAddress,methodId(Module,MethodIndex),ExceptionHandlers)):-
	nnegint(StackSize),
	nnegint(LocalVarSize),
	pc(FirstAddress),
	gnd(Module),
	gnd(MethodIndex),
	list(ExceptionHandlers,exceptionHandler).

% the boolean type does NOT correspond the the Java type but 
% as a prolog type to write this specification. 
% It probably exists a similar stuff in Ciao that should be used instead.
:- regtype boolean/1.
boolean(true).
boolean(false).



%Handling of qualified names 
:- regtype packageName/1.
:- regtype shortClassName/1.
:- regtype shortMethodName/1.
:- regtype shortFieldName/1.
packageName(packageName(PN)):-atm(PN).
shortClassName(shortClassName(SCN)):-atm(SCN).
shortMethodName(shortMethodName(SMN)):-atm(SMN).
shortFieldName(shortFieldName(SFN)):-atm(SFN).

:- regtype className/1.
className(className(PN,SCN)):-packageName(PN),shortClassName(SCN).

:- regtype interfaceName/1.
interfaceName(interfaceName(PN,SCN)):-packageName(PN),shortClassName(SCN).

:- regtype methodName/1.
methodName(methodName(CN,SMN)):-className(CN),shortMethodName(SMN).

:- regtype fieldName/1.
fieldName(fieldName(CN,SFN)):-className(CN),shortFieldName(SFN).

% Some constants
object(shortClassName('Object')).
jstring(shortClassName('String')).
javaLang(packageName('java/lang/')).
javaLangObject(className(JavaLang,Object)):-javaLang(JavaLang),object(Object).

:- trust comp javaLangString/1 + (eval,sideff(free)).
javaLangString(className(JavaLang,String)):-javaLang(JavaLang),jstring(String).

% Native Exceptions
nullPointerException(shortClassName('NullPointerException')).
arrayIndexOutOfBoundsException(shortClassName('ArrayIndexOutOfBoundsException')).
arrayStoreException(shortClassName('ArrayStoreException')).
negativeArraySizeException(shortClassName('NegativeArraySizeException')).
classCastException(shortClassName('ClassCastException')).
arithmeticException(shortClassName('ArithmeticException')).


%visibility modifiers
:- regtype visibility/1.
visibility(package).
visibility(protected).
visibility(private).
visibility(public).

% Types handled by the JVM (minus type not handled by this specification
% i.e. long, double, and floats)
:- regtype type/1.
type(T):-primitiveType(T).
type(T):-refType(T).
:- regtype refType/1.
refType(refType(arrayType(TYPE))):-type(TYPE).
refType(refType(classType(CT))):-className(CT).
refType(refType(interfaceType(IT))):-interfaceName(IT).
:- regtype primitiveType/1.
primitiveType(primitiveType(boolean)).
primitiveType(primitiveType(byte)).
primitiveType(primitiveType(char)). %byMiky
primitiveType(primitiveType(short)).
primitiveType(primitiveType(int)).
primitiveType(primitiveType(long)). % New added
primitiveType(primitiveType(float)).
primitiveType(primitiveType(double)).

:- regtype compInt/1.
compInt(eqInt).
compInt(neInt).
compInt(ltInt).
compInt(leInt).
compInt(geInt).
compInt(gtInt).

:- regtype binopInt/1.
binopInt(addInt).
binopInt(andInt).
binopInt(divInt).
binopInt(mulInt).
binopInt(orInt).
binopInt(remInt).
binopInt(shlInt).
binopInt(shrInt).
binopInt(subInt).
binopInt(ushrInt).
binopInt(xorInt).


:- regtype pc/1.
%:- success pc(_1) : term(_1) => pc(_1).
pc(PC):-nnegint(PC).

:- regtype offset/1.
%:- success offset(_1) : term(_1) => offset(_1).
offset(O):-int(O).

% Variables are indexed by integer
:- regtype variable/1.
%:- success variable(_1) : term(_1) => variable(_1).
variable(X):-nnegint(X).


% the jump type is used (only) by the lookupswitch instruction
% if there is a way to express a predicate like list(JL,(int,offset)),
% that is to say, JL is a list of pair (int,offset), then 
% this jump type is no longer usefull.
:- regtype jump/1.
%:- success jump(_1) : term(_1) => jump(_1).
jump((Z,O)):-int(Z),offset(O).

% Parser translation :
% aload_<n> --> aload n
% astore_<n> --> astore n
% goto_w --> goto
% iconst_<n> --> const INT n
% iload_<n> --> iload n
% istore_<n> --> istore n
% ldc_w --> ldc
% iadd --> ibinop iadd
% isub --> ibinop isub
% ...
:- regtype instruction/1.
%:- success instruction(_1) : term(_1) => instruction(_1).
instruction(aaload).
instruction(aastore).
instruction(aconst_null).
instruction(aload(X)):-variable(X).
instruction(anewarray(T)):-refType(T).
instruction(areturn).
instruction(arraylength).
instruction(astore(X)):-variable(X).
instruction(athrow).
instruction(baload).
instruction(bastore).
instruction(checkcast(T)):-refType(T).
instruction(const(T,Z)):-primitiveType(T),int(Z). %can be more precise
instruction(dup).
instruction(dup_x1).
instruction(dup_x2).
instruction(dup2).
instruction(dup2_x1).
instruction(dup2_x2).
instruction(getfield(F)):-fieldSignature(F).
instruction(getstatic(F)):-fieldSignature(F).
instruction(goto(O)):-offset(O).
instruction(i2b).
instruction(i2s).
instruction(ibinop(Op)):-binopInt(Op).
instruction(iaload).
instruction(iastore).
instruction(if_acmpeq(O)):-offset(O).
instruction(if_acmpne(O)):-offset(O).
instruction(if_icmp(Op,O)):-offset(O),compInt(Op). 
instruction(if0(Op,O)):-offset(O),compInt(Op).
instruction(ifnonnull(O)):-offset(O).
instruction(ifnull(O)):-offset(O).
instruction(iinc(X,Z)):-variable(X),int(Z). %can be more precise
instruction(iload(X)):-variable(X).
instruction(ineg).
instruction(instanceof(T)):-refType(T).
instruction(invokeinterface(M)):-methodSignature(M).
instruction(invokespecial(M)):-methodSignature(M).
instruction(invokestatic(M)):-methodSignature(M).
instruction(invokevirtual(M)):-methodSignature(M).
instruction(ireturn).
instruction(istore(X)):-variable(X).
instruction(lookupswitch(Default,Jumps)):-offset(Default),list(Jumps,jump).% jumpList(Jumps) or list(Jumps,^ (int,offset))
instruction(multianewarray(T)):-refType(T).
instruction(new(CL)):-className(CL).
instruction(newarray(T)):-primitiveType(T).
instruction(nop).
instruction(pop).
instruction(pop2).
instruction(putfield(F)):-fieldSignature(F).
instruction(putstatic(F)):-fieldSignature(F).
instruction(return).
instruction(saload).
instruction(sastore).
instruction(swap).
instruction(tableswitch(Default,Low,High,Offsets)):-offset(Default),int(Low),int(High),list(Offsets,offset).


% Content of a Java class
:- regtype class/1.
% class(class(Name,OptionClassName,SuperInterfaces,Fields,Methods,final(Bool1),public(Bool2),abstract(Bool3))):-
% 	className(Name),
% 	optionClassName(OptionClassName),  % direct superclass
% 	list(SuperInterfaces,interfaceName), % list of implemented interfaces
% 	list(Fields,field),
% 	list(Methods,method),
% 	boolean(Bool1),
% 	boolean(Bool2),
% 	boolean(Bool3).
class(class(Name,final(Bool1),public(Bool2),abstract(Bool3),OptionClassName,SuperInterfaces,Fields,Methods)):-
	className(Name),
	optionClassName(OptionClassName),  % direct superclass
	list(SuperInterfaces,interfaceName), % list of implemented interfaces
	list(Fields,field),
	list(Methods,method),
	boolean(Bool1),
	boolean(Bool2),
	boolean(Bool3).

class_name(class(ClassName,_,_,_,_,_,_,_),ClassName).

:- trust comp class_fields/2 + (eval,sideff(free)).
class_fields(class(_,_,_,_,_,_,Fields,_),Fields).

class_field(class(ClassName,_,_,_,_,_,Fields,_),ShortFieldName,Field):-
	Field=field(fieldSignature(fieldName(ClassName,ShortFieldName),_Type),_Final,_Static,_Visibility,_InitialValue),
	member(Field,Fields).

class_superClass(class(_Name,_,_,_,Super,_,_,_),Super).

:- trust comp class_method(A,B,C) : (ground(A), ground(B)) + eval.
:- trust comp class_method(A,B,C) + sideff(free).
class_method(class(_,_,_,_,_,_,_,Methods),methodSignature(methodName(_ClassName,ShortMethodName),Parameters,Result),Method):-
	Method=method(methodSignature(methodName(_,ShortMethodName),Parameters,Result),_,_,_,_),
	member(Method,Methods).

class_superInterfaces(class(_Name,_,_,_,_,SuperInterfaces,_,_),SuperInterfaces).

class_isFinal(class(_,final(true),_,_,_,_,_,_)).

class_isPublic(class(_,_,public(true),_,_,_,_,_)).

class_isAbstract(class(_,_,_,abstract(true),_,_,_,_)).

% Content of a Java interface
:- regtype interface/1.
%:- success interface(_1) : term(_1) => interface(_1).
interface(interface(Name,final(Bool1),public(Bool2),abstract(Bool3),SuperInterfaces,Fields,Methods)):-
	interfaceName(Name),
	list(SuperInterfaces,interfaceName),
	list(Fields,field),
	list(Methods,method),
	boolean(Bool1),
	boolean(Bool2),
	boolean(Bool3).

interface_name(interface(Name,_,_,_,_SuperInterfaces,_Fields,_Methods),Name).

interface_superInterfaces(interface(_Name,_,_,_,SuperInterfaces,_Fields,_Methods),SuperInterfaces).

interface_field(interface(_InterfaceName,_,_,_,_,Fields,_),ShortFieldName,Field):-
	Field=field(fieldSignature(fieldName(_InterfaceName_,ShortFieldName),_),_,_,_Vis,_InitV),
% There is not a matching between the interfaceName in interface/7 -> 2nd arg and in the 
% fieldName -> 1st arg (this last one is a className).
	member(Field,Fields).

% Content of a Java Program
% this is the main type, using the others
:- regtype program/1.
program(program(Classes,Interfaces)):-
	list(Classes,class),
	list(Interfaces,interface).

:- trust comp program_class(CN,CL) : (ground(CN)) + eval.
:- trust comp program_class(CN,CL) + sideff(free).
% accessor to a class from its qualified name
%program_class(program(CL,_Interfaces),ClassName,Class):-
%	Class=class(ClassName,_Final,_Pub,_Abstract,_S,_SI,_Fields,_M),
%	member(Class,CL).
program_class(ClassName,Class):-
	Class=class(ClassName,_Final,_Pub,_Abstract,_S,_SI,_Fields,_M),
	loaded_classes:get_class(Class).


:- trust comp program_interface(CN,CL) : (ground(CN)) + eval.
:- trust comp program_interface(CN,CL) + sideff(free).
% accessor to an interface from its qualified name
%program_interface(program(_,IL),InterfaceName,Interface):-
%	Interface=interface(InterfaceName,_,_,_,_,_,_),
%	member(Interface,IL).
program_interface(InterfaceName,Interface):-
	Interface=interface(InterfaceName,_,_,_,_,_,_),
	loaded_classes:get_class(Interface).

% Content of a method signature
:- regtype methodSignature/1.
%:- success methodSignature(_1) : term(_1) => methodSignature(_1).
methodSignature(methodSignature(Name,Parameters,Result)):-
	methodName(Name),
	list(Parameters,type),
	optionTypeb(Result).

methodSignature_name(methodSignature(Name,_Parameters,_Result),Name).

% Java types for parameters values
methodSignature_parameters(methodSignature(_Name,Parameters,_Result),Parameters).

% Java type for return value, the constructor _none_ of type option being used for the _void_ type
methodSignature_result(methodSignature(_Name,_Parameters,Result),Result).

% Content of a method
:- regtype method/1.
%:- success method(_1) : term(_1) => method(_1).
method(method(Signature,Body,final(Bool1),static(Bool2),Visibility)):-
	methodSignature(Signature),
	optionBytecodeMethod(Body), 
	boolean(Bool1),
	boolean(Bool2),
	visibility(Visibility).

method_name(method(methodSignature(MethodName,_,_),_,_,_,_),MethodName).

method_signature(method(S,_,_,_,_),S).

%:- trust comp method_body(M,B) : var(M) + memo.

% A method that is not abstract has a method body
method_body(method(_,Body,_,_,_),Body).

method_isAbstract(method(_,Body,_,_,_)) :- var(Body).

method_isFinal(method(_,_,final(true),_,_)).

method_isStatic(method(_,_,_,static(true),_)).

method_visibility(method(_,_,_,_,V),V).

:- regtype bytecodeMethod/1.
%:- success bytecodeMethod(_1) : term(_1) => bytecodeMethod(_1).
bytecodeMethod(bytecodeMethod(LocalVarSize,StackSize,FirstAddress,methodId(Module,MethodIndex),ExceptionHandlers)):-
	nnegint(StackSize), % max number of elements on the operand stack
	nnegint(LocalVarSize), % max number of local variables
	pc(FirstAddress),
	%list(Instructions,instruction),
	gnd(Module),
	gnd(MethodIndex),
	list(ExceptionHandlers,exceptionHandler).

% Operations on bytecode methods

bytecodeMethod_firstAddress(bytecodeMethod(_,_,FirstAddress,_,_),FirstAddress).

bytecodeMethod_exceptionHandlers(bytecodeMethod(_,_,_,_,ExceptionHandlers),ExceptionHandlers).

% max number of local variables
bytecodeMethod_localVarSize(bytecodeMethod(LocalVarSize,_,_,_,_),LocalVarSize).

bytecodeMethod_methodId(bytecodeMethod(_,_,_,MethodId,_),MethodId).

%% As the next two predicates are handled differently in the 'pe' and the 'normal' version of
%% this interpreter, the code is respectively in 'pe.pl' and 'jvml.pl' 
%bytecodeMethod_instructionAt(bytecodeMethod(_,_,_,methodId(Module,MethodIndex),_),PC,Instruction):-
% 	%Module:bytecode(PC,MethodIndex,Instruction,_).
% 	bytecode(PC,Module,MethodIndex,Instruction,_).
%bytecodeMethod_next(bytecodeMethod(_,_,_,methodId(Module,MethodIndex),_),PC,PCb):-
% 	%Module:bytecode(PC,MethodIndex,_,Size),
% 	bytecode(PC,Module,MethodIndex,_,Size),
% 	PCb is PC+Size.

:- regtype bytecode/1.
%bytecode(bytecode(PC,MethodIndex,Instruction,Offset)):-
bytecode(bytecode(PC,Module,MethodIndex,Instruction,Offset)):-
	gnd(Module),
	pc(PC),
	instruction(Instruction),
	gnd(MethodIndex),
	offset(Offset).
	
:- regtype exceptionHandler/1.
%:- success exceptionHandler(_1) : term(_1) => exceptionHandler(_1).
exceptionHandler(exceptionHandler(OptionClassName,Low,High,HandlerAddress)):-
	optionClassName(OptionClassName), 
	pc(Low),
	pc(High),
	%Low=<High,
	pc(HandlerAddress).

% Operations on exception handlers

% class of the caught exception
% The constructor None of type option being used to implement finally. It matches any exception
exceptionHandler_catchType(exceptionHandler(CL,_,_,_),CL).

% is the given PC in range of the handler ?
:- trust comp exceptionHandler_isPCinRange/2 + (eval,sideff(free)).
exceptionHandler_isPCinRange(exceptionHandler(_,Low,High,_),PC):-
	Low=<PC,
	PC<High.

:- trust comp exceptionHandler_notPCinRange/2 + (eval,sideff(free)).
exceptionHandler_notPCinRange(ExH,PC):-
	\+ exceptionHandler_isPCinRange(ExH,PC).

% location of the handler code
exceptionHandler_handler(exceptionHandler(_,_,_,HA),HA).


:- regtype field/1.
%:- success field(_1) : term(_1) => field(_1).
field(field(Signature,final(Bool1),static(Bool2),Visibility,InitialValue)):-
	fieldSignature(Signature),
	boolean(Bool1),
	boolean(Bool2),
	visibility(Visibility), % Initial (default) value. Must be compatible with the type of the field.
	initialValue(InitialValue).

% Operations on fields
field_type(field(fieldSignature(_,Type),_Final,_Static,_Visibility,_Init),Type).

field_signature(field(Signature,_Final,_Static,_Visibility,_Init),Signature).

field_isFinal(field(_Sig,final(true),_Static,_Visibility,_Init)).

field_isStatic(field(_Sig,_Final,static(true),_Visibility,_Init)).
field_isNotStatic(field(_Sig,_Final,static(false),_Visibility,_Init)). %to avoid negations

field_visibility(field(_Sig,_Final,_Static,Visibility,_Init),Visibility).

field_initValue(field(_Sig,_Final,_Static,_Visibility,Init),Init).

%initialValue is the type of values that can be used as initial values for the fields
:- regtype initialValue/1.
initialValue(initialValue(undef)).
initialValue(initialValue(null)).
initialValue(initialValue(int(I))):-int(I).

:- regtype fieldSignature/1.
%:- success fieldSignature(_1) : term(_1) => fieldSignature(_1).
fieldSignature(fieldSignature(Name,Type)):-
	fieldName(Name),
	type(Type).

% Operations on the signatures of fields

fieldSignature_name(fieldSignature(Name,_Type),Name).

fieldSignature_type(fieldSignature(_Name,Type),Type).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      Some function definitions on program      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%isStatic
direct_subclass(C,S):-
	class_superClass(C,SSig),
	program_class(_,C),
	program_class(SSig,S).

% subclass is the reflexive transitive closure of the direct_subclass relation
subclass(C,C):-
	program_class(_,C).
subclass(C1,C2):-
	direct_subclass(C1,Ci),
	subclass(Ci,C2).

direct_subclass_name(CN1,CN2):-
	var(CN1),
	program_class(CN1,C1),
	program_class(CN2,C2),
	direct_subclass(C1,C2).
direct_subclass_name(CN1,CN2):-
	nonvar(CN1),
	program_class(CN1,C1),
	direct_subclass(C1,C2),
	class_name(C2,CN2).

% Similar definitions for interfaces

direct_subinterface(I,S):-
	interface_superInterfaces(I,SL),
	member(S,SL),
	program_interface(S,_),
	program_interface(I,_).
%	member(S,IL),
%	member(I,IL).

subinterface(I,I):-
	program_interface(I,_).
%	member(I,IL).
subinterface(I,S):-
	direct_subinterface(I,Si),
	subinterface(Si,S).

subinterface_name(IN1,IN2):-
	program_interface(IN1,I1),
	program_interface(IN2,I2),
	subinterface(I1,I2).

direct_subinterface_name(IN1,IN2):-
	program_interface(IN1,I1),
	program_interface(IN2,I2),
	direct_subinterface(I1,I2).

class_declares_field(ClassName,FieldSignature,Field):-
	fieldSignature_name(FieldSignature,fieldName(ClassName,ShortFieldName)),
	program_class(ClassName,Class),
	class_field(Class,ShortFieldName,Field).

interface_declares_field(InterfaceName,fieldSignature(InterfaceName,ShortFieldName),Field):-
	program_interface(InterfaceName,Interface),
	interface_field(Interface,ShortFieldName,Field).

is_defined_field(CN,FD,F):-
	subclass_name(CN,CNb),
	class_declares_field(CNb,FD,F).
is_defined_field(CN,FD,F):-
	program_class(CN,CL),
	class_superInterfaces(CL,ILS),
	member(IN1,ILS),
	subinterface_name(IN1,Ib),
 	interface_declares_field(Ib,FD,F).

% defined_field c fd holds if the class c declares or inherits a field of signature fd 
defined_field(CN,FS):-
	is_defined_field(CN,FS,_F).

findMethod(Msig,M):-
	methodSignature_name(Msig,methodName(CN,_)),
	program_class(CN,CL),
	class_method(CL,Msig,M).

findField(FD,F):-	
	fieldSignature_name(FD,fieldName(CN,_)),
	program_class(CN,CL),
	class_field(CL,FD,F).

methodPackage(methodName(className(PackageName,_),_),PackageName).

% the check visibility method is only for the overrides method 
% (once we know the 2 classes are in the same tree)
check_visibility(public,_P1,_P2).
check_visibility(protected,_P1,_P2).
check_visibility(package,P,P).

% check_signature verifies that the two methods share the same signature
% and that the defining classes belong to the subclass relation
check_signature(M1,M2):-
	M1=method(methodSignature(methodName(ClassName1,MethodName),Parameters,Result),_,_,_,_),
	M2=method(methodSignature(methodName(ClassName2,MethodName),Parameters,Result),_,_,_,_),
	subclass_name(ClassName1,ClassName2).

% Definition of the override relation
% cf. http://java.sun.com/docs/books/jls/third_edition/html/classes.html#8.4.8

% overrides(P,M1,M2):-
% 	check_signature(P,M1,M2),
% 	method_name(M1,MethodName1),
% 	method_name(M2,MethodName2),
% 	methodPackage(MethodName1,P1),
% 	methodPackage(MethodName2,P2),
% 	method_visibility(M2,Visibility),
% 	check_visibility(Visibility,P1,P2).
% overrides(P,M1,M2):-
% 	check_signature(P,M1,M2),
% 	overrides(P,M1,M1b),
% 	overrides(P,M1b,M2).
overrides(M1,M2):-
	warning('The override relation is not the one in Bicolano and is not correct in this implementation.'),
	check_signature(M1,M2).

accessible_method(_CL,Meth):-
	method_visibility(Meth,public).
accessible_method(CL,Meth):-
	method_visibility(Meth,protected),
	method_signature(Meth,MethSig),
	methodSignature_name(MethSig,methodName(CLbn,_ShortMethodName)),
	program_class(CLbn,CLb),
	subclass(CL,CLb).
accessible_method(CL,Meth):-
	(method_visibility(Meth,protected);method_visibility(Meth,package)),
	CL=className(PackageName,_CLscn),
	method_signature(Meth,MethSig),
	methodSignature_name(MethSig,methodName(className(PackageName,_),_)).
accessible_method(CL,Meth):-
	method_visibility(Meth,private), %it's the only possibility
	method_signature(Meth,MethSig),
	class_name(CL,CLn),
	methodSignature_name(MethSig,methodName(CLn,_)).

% this method find the method Meth in class with class name Cn and that overrides 
% the method corresponding to Msig
:- trust comp  lookup_here(B,C,D) : (nonvar(B), nonvar(C)) + eval.
:- trust comp  lookup_here(B,C,D) + sideff(free).
% lookup_here(P,Cn,Msig,Meth):-
% 	findMethod(P,Msig,Meth),
% 	program_class(P,Cn,Cl),
% 	class_method(Cl,Msig,Meth).
% lookup_here(P,Cn,Msig,Meth):-
% 	findMethod(P,Msig,M),
% 	program_class(P,Cn,Cl),
% 	class_method(Cl,Msig,Meth),
% 	\+ Meth = M,
% 	overrides(P,Meth,M).
lookup_here(Cn,Msig,Meth):-
	program_class(Cn,Cl),
	class_method(Cl,Msig,Meth),
	findMethod(Msig,M),
	accessible_method(Cl,M).

:- trust comp lookup_here_succeeds(B,C) : (nonvar(B), nonvar(C)) + eval.
:- trust comp lookup_here_succeeds(B,C) + sideff(free).
lookup_here_succeeds(CN,Msig):-
	lookup_here(CN,Msig,_).

lookup(CN,Msig,Meth):-
	lookup_here(CN,Msig,Meth).
lookup(CN,Msig,Meth):-
	\+ lookup_here_succeeds(CN,Msig),
	direct_subclass_name(CN,Super),
	lookup(Super,Msig,Meth).

:- trust comp class_has_method(A,B) : (ground(A), ground(B)) + eval.
:- trust comp class_has_method(A,B) + sideff(free).
class_has_method(CL,Mid):-
	class_method(CL,Mid,_).

:- trust comp resolve_method(A,B,C) : (ground(A), ground(B)) + eval.
:- trust comp resolve_method/3 + sideff(free).
resolve_method(CN,Mid,Meth):-
	program_class(CN,CL),
	class_method(CL,Mid,Meth).
resolve_method(CN,Mid,Meth):-
	program_class(CN,CL),
	\+ class_has_method(CL,Mid),
	direct_subclass_name(CN,SCN),
	resolve_method(SCN,Mid,Meth).

:- trust comp instructionAt(M,PC,I): (ground(M), ground(PC)) + eval.
:- trust comp instructionAt(M,PC,I) + sideff(free).

% Get the instruction at the given pc
instructionAt(M,PC,I):-
	method_body(M,Body),
	bytecodeMethod_instructionAt(Body,PC,I).

:- trust comp next(M,PC,PCb): (ground(M), ground(PC)) + eval.
:- trust comp next(M,PC,PCb) + sideff(free).

% Get the next pc
next(M,PC,PCb):-
	method_body(M,Body),
	bytecodeMethod_next(Body,PC,PCb).

implements(Cn,Ib):-
	program_class(Cn,Cl),
	class_superInterfaces(Cl,IL),
	member(In,IL),
	program_interface(In,_),
	subinterface_name(In,Ib).

% compat_refType source target holds if a reference value of type source
% can be assigned to a reference variable of type target.
% cf. http://java.sun.com/docs/books/vmspec/2nd-edition/html/Concepts.doc.html#19674

compat_refType(refType(classType(Cn1)),refType(classType(Cn2))):-
	subclass_name(Cn1,Cn2).
compat_refType(refType(classType(Cn1)),refType(classType(Cn2))):-
	implements(Cn1,Cn2).
compat_refType(refType(classType(In)),refType(classType(Object))):-
	javaLangObject(Object),
	program_interface(In,_I).
compat_refType(refType(classType(In1)),refType(classType(In2))):-
	subinterface_name(In1,In2).
compat_refType(refType(arrayType(_)),refType(classType(Object))):-
	javaLangObject(Object).
compat_refType(refType(arrayType(primitiveType(T))),
	       refType(arrayType(primitiveType(T)))).
compat_refType(refType(arrayType(refType(T1))),refType(arrayType(refType(T2)))):-
	compat_refType(refType(T1),refType(T2)).



