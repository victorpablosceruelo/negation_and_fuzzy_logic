:-module(jvml,[
	    program/1,
	    program_class/2,
	    packageName/1 , 
	    interface/1, 
	    interfaceName/1, 
	    class/1,
	    className/1 ,
	    shortClassName/1,
	    class_name/2,
	    class_isAbstract/1,
	    class_isFinal/1,
	    class_isPublic/1,
	    class_fields/2,
	    class_method/3,
	    class_superClass/2,
	    method/1,
	    methodName/1,
	    shortMethodName/1,
	    method_body/2,
	    method_isStatic/1,
	    method_isFinal/1,
	    method_isAbstract/1,
	    method_signature/2,
	    method_visibility/2,
	    methodSignature/1, 
	    methodSignature_name/2,
	    methodSignature_parameters/2,
	    methodSignature_result/2,
	    get_method_id/2,
	    field/1,
	    field_signature/2,
	    field_isStatic/1,
	    field_isNotStatic/1,
	    fieldName/1,
	    shortFieldName/1,
	    fieldSignature/1, 
	    fieldSignature_type/2,
	    exceptionHandler/1,
	    exceptionHandler_handler/2, 
	    exceptionHandler_catchType/2,
	    exceptionHandler_isPCinRange/2, 
	    exceptionHandler_notPCinRange/2,
	    bytecodeMethod/1,
	    bytecodeMethod_firstAddress/2,
	    bytecodeMethod_localVarSize/2,
	    bytecodeMethod_exceptionHandlers/2,
	    bytecode/1,
	    visibility/1,
	    type/1,
	    primitiveType/1,
	    refType/1,
	    variable/1,
	    offset/1 , 
	    pc/1,
	    instruction/1,
	    binopInt/1,
	    compInt/1,
	    
	    optionClassName/1,
	    optionBytecodeMethod/1,
	    optionTypeb/1,

	    javaLang/1,
	    javaLangString/1,
	    nullPointerException/1,
	    arrayIndexOutOfBoundsException/1,
	    arrayStoreException/1,
	    negativeArraySizeException/1,
	    classCastException/1,
	    arithmeticException/1,

	    isStatic/1,
	    defined_field/2,
	    findMethod/2,
	    lookup/3,
	    resolve_method/3,
	    subclass_name/2,
	    not_subclass_name/2,
	    instructionAt/3,
	    next/3,
	    compat_refType/2,
	    split_smnwa/4,
	    msig_from_smnwa/2
	    %,load_program/2
	]
	,[assertions, nortchecks, regtypes, basicmodes]).

:- push_prolog_flag(unused_pred_warnings, no).
:- use_module(library(lists), [length/2]).
:- use_module(library(compiler)).
:- use_module(library(terms), [atom_concat/2]).

:- use_module(library(jvm_in_ciao(interpreter(loaded_classes))), [get_class/1, main_class/1]).
:- use_module(library(jvm_in_ciao(interpreter(jvm))), [current_jvmlr_module/1]).

:- include(library(jvm_in_ciao(interpreter(jvml_i)))).

% Formalization of Java programs. 
% Based on the report secsafe-tl-005: 
%  http://www.doc.ic.ac.uk/~siveroni/secsafe/docs/secsafe-tl-005.pdf 
% Simplifications have been made with the objective that a 
% straightforward classfile parser would implement this API.

% :- regtype optionTypeb/1.
% :- regtype optionClassName/1.
% :-regtype optionBytecodeMethod/1.

:- regtype boolean/1.
:- success boolean(_1) : term(_1) => boolean(_1).

%Handling of qualified names 
:- regtype packageName/1.
:- success packageName(_1) : term(_1) => packageName(_1).
:- regtype shortClassName/1.
:- success shortClassName(_1) : term(_1) => shortClassName(_1).
:- regtype shortMethodName/1.
:- success shortMethodName(_1) : term(_1) => shortMethodName(_1).
:- regtype shortFieldName/1.
:- success shortFieldName(_1) : term(_1) => shortFieldName(_1).

:- regtype className/1.
:- success className(_1) : term(_1) => className(_1).


:- regtype interfaceName/1.
:- success interfaceName(_1) : term(_1) => interfaceName(_1).


:- regtype methodName/1.
:- success methodName(_1) : term(_1) => methodName(_1).


:- regtype fieldName/1.
:- success fieldName(_1) : term(_1) => fieldName(_1).


:- pred object/1 : var => shortClassName + eval.

:- pred javaLang/1 : var => packageName + eval.

:- pred javaLangObject/1 : var => className + eval.


% Native Exceptions
:- entry nullPointerException/1 : var.
:- pred nullPointerException/1 : var => shortClassName + eval.

:- entry arrayIndexOutOfBoundsException/1 : var.
:- pred arrayIndexOutOfBoundsException/1 : var => shortClassName + eval.

:- entry arrayStoreException/1 : var.
:- pred arrayStoreException/1 : var => shortClassName + eval.

:- entry negativeArraySizeException/1 : var.
:- pred negativeArraySizeException/1 : var => shortClassName + eval.

:- entry classCastException/1 : var.
:- pred classCastException/1 : var => shortClassName + eval.

:- entry arithmeticException/1 : var.
:- pred arithmeticException/1 : var => shortClassName + eval.


%visibility modifiers
:- regtype visibility/1.
:- success visibility(_1) : term(_1) => visibility(_1).

% Types handled by the JVM (minus type not handled by this specification
% i.e. long, double, and floats)
:- regtype type/1.
:- success type(_1) : term(_1) => type(_1).

:- regtype refType/1.
:- success refType(_1) : term(_1) => refType(_1).

:- regtype primitiveType/1.
:- success primitiveType(_1) : term(_1) => primitiveType(_1).

:- regtype compInt/1.


:- regtype binopInt/1.

:- regtype pc/1.
%:- success pc(_1) : term(_1) => pc(_1).


:- regtype offset/1.
%:- success offset(_1) : term(_1) => offset(_1).


% Variables are indexed by integer
:- regtype variable/1.
%:- success variable(_1) : term(_1) => variable(_1).


% the jump type is used (only) by the lookupswitch instruction
% if there is a way to express a predicate like list(JL,(int,offset)),
% that is to say, JL is a list of pair (int,offset), then 
% this jump type is no longer usefull.
:- regtype jump/1.
%:- success jump(_1) : term(_1) => jump(_1).


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


% Content of a Java class
:- regtype class/1.


:- entry class_fields/2 : class*var.
:- pred class_fields/2 : class*var => class*field + eval.

:- calls class_field/3 : class*compat(shortFieldName)*compat(field).
:- success class_field/3 : class*compat(shortFieldName)*compat(field) => class*shortFieldName*field.

:- entry class_superClass/2 : class*compat(optionClassName).
:- calls class_superClass/2 : class*compat(optionClassName).
:- success class_superClass/2 => class*optionClassName.

:- entry class_method/3 : class*compat(methodSignature)*compat(method).
:- success class_method/3 => class*methodSignature*method.

:- calls class_superInterfaces/2 : class*compat(list(interface)).
:- success class_superInterfaces/2 : class*compat(list(interface)) => class*list(interface).

:- entry class_isFinal/1 : class.
:- calls class_isFinal/1 : class.
:- success class_isFinal/1 : class => class.

:- entry class_isPublic/1 : class.
:- calls class_isPublic/1 : class.
:- success class_isPublic/1 : class => class.

:- entry class_isAbstract/1 : class.
:- calls class_isAbstract/1 : class.
:- success class_isAbstract/1 : class => class.
:- trust comp class_isAbstract/1 + (sideff(free), eval).

% Content of a Java interface
:- regtype interface/1.
%:- success interface(_1) : term(_1) => interface(_1).


:- calls interface_name/2: interface*compat(interfaceName).
:- success interface_name/2: interface*compat(interfaceName) => interface*interfaceName.


:- calls interface_superInterfaces/2: interface*compat(list(interfaceName)).
:- success interface_superInterfaces/2: interface*compat(list(interfaceName)) => interface*list(interfaceName).


:- calls interface_field/3: interface*shortFieldName*compat(field).
:- calls interface_field/3: interface*compat(shortFieldName)*field.
:- success interface_field/3: interface*shortFieldName*compat(field) => interface*shortFieldName*field.
:- success interface_field/3: interface*compat(shortFieldName)*field => interface*shortFieldName*field.

	
% Content of a Java Program
% this is the main type, using the others
:- regtype program/1.
:- entry program/1 : program.


% accessor to a class from its qualified name
:- entry program_class(?className,?class).
:- calls program_class/2 : compat(className)*compat(class).
:- success program_class/2: compat(className)*compat(class) => className*class.

% accessor to an interface from its qualified name
:- calls program_interface/2 : compat(interfaceName)*compat(interface).
:- success program_interface/2 : compat(interfaceName)*compat(interface) => interfaceName*interface.

% Content of a method signature
:- regtype methodSignature/1.
%:- success methodSignature(_1) : term(_1) => methodSignature(_1).

:- entry methodSignature_name/2 : methodSignature*compat(methodName).
:- calls methodSignature_name/2 : methodSignature*compat(methodName).
:- success methodSignature_name/2 : methodSignature*compat(methodName) => methodSignature*methodName.

% Java types for parameters values
:- entry methodSignature_parameters/2 : methodSignature*compat(list(type)).
:- calls methodSignature_parameters/2 : methodSignature*compat(list(type)).
:- success methodSignature_parameters/2 : methodSignature*compat(list(type)) => methodSignature*list(type).

% Java type for return value, the constructor _none_ of type option being used for the _void_ type
:- entry methodSignature_result/2 : methodSignature*compat(optionTypeb).
:- calls methodSignature_result/2 : methodSignature*compat(optionTypeb).
:- success methodSignature_result/2 : methodSignature*compat(optionTypeb) => methodSignature*optionTypeb.

% Content of a method
:- regtype method/1.

:- calls method_name/2 : method*compat(methodName).
:- success method_name/2 : method*compat(methodName) => method*methodName.

:- entry method_signature/2 : method*compat(methodSignature).
:- calls method_signature/2 : method*compat(methodSignature).
:- success method_signature/2 : method*compat(methodSignature) => method*methodSignature.

% A method that is not abstract has a method body
:- entry method_body/2 : method*compat(bytecodeMethod).
:- pred method_body/2 : method*compat(bytecodeMethod) => method*bytecodeMethod.

:- entry method_isFinal/1 : method.
:- calls method_isFinal/1 : method.
:- success method_isFinal/1 : method => method.


:- entry method_isStatic/1 : method.
:- calls method_isStatic/1 : method.
:- success method_isStatic/1 : method => method.


:- entry method_visibility/2 : method*term.
:- calls method_visibility/2 : method*compat(visibility).
:- success method_visibility/2 : method*compat(visibility) => method*visibility.


:- regtype bytecodeMethod/1.
%:- success bytecodeMethod(_1) : term(_1) => bytecodeMethod(_1).

% Operations on bytecode methods
:- entry bytecodeMethod_firstAddress/2 : bytecodeMethod*compat(pc).
:- calls bytecodeMethod_firstAddress/2 : bytecodeMethod*compat(pc).
:- success bytecodeMethod_firstAddress/2 : bytecodeMethod*compat(pc) => bytecodeMethod*pc.


:- entry bytecodeMethod_exceptionHandlers/2 : bytecodeMethod*compat(list(exceptionHandler)).
:- calls bytecodeMethod_exceptionHandlers/2 : bytecodeMethod*compat(list(exceptionHandler)).
:- success bytecodeMethod_exceptionHandlers/2 : bytecodeMethod*compat(list(exceptionHandler)) => bytecodeMethod*list(exceptionHandler).


% max number of local variables
:- entry bytecodeMethod_localVarSize/2 : bytecodeMethod*var.
:- calls bytecodeMethod_localVarSize/2 : bytecodeMethod*var.
:- success bytecodeMethod_localVarSize/2 : bytecodeMethod*var => bytecodeMethod*nnegint.

% The two following predicate are different in the 'normal' interpreter and in
% the version for partial evaluation (because of the differences in using modules)
:- entry bytecodeMethod_instructionAt/3 : bytecodeMethod*pc*compat(instruction).
:- calls bytecodeMethod_instructionAt/3 : bytecodeMethod*pc*compat(instruction).
:- success bytecodeMethod_instructionAt/3 : bytecodeMethod*pc*compat(instruction) => bytecodeMethod*pc*instruction.

bytecodeMethod_instructionAt(bytecodeMethod(_,_,_,methodId(ClassId,MethodIndex),_),PC,Instruction):-
%	Module:bytecode(PC,MethodIndex,Instruction,_).
	current_jvmlr_module(Module),
	ensure_loaded(Module),
%	Goal = ':'(Module,bytecode(PC,ClassId,MethodIndex,Instruction,_)),
%	call(Goal).
	Module:bytecode(PC,ClassId,MethodIndex,Instruction,_).
bytecodeMethod_next(bytecodeMethod(_,_,_,methodId(ClassId,MethodIndex),_),PC,PCb):-
%	Module:bytecode(PC,MethodIndex,_,Size),
	current_jvmlr_module(Module),
	ensure_loaded(Module),
%	Goal = ':'(Module,bytecode(PC,ClassId,MethodIndex,_,Size)),
%	call(Goal),
	Module:bytecode(PC,ClassId,MethodIndex,_,Size),
	PCb is PC+Size.

:- regtype bytecode/1.
:- entry bytecode(in(BC)).
:- pred bytecode(in(BC)) 
	=> bytecode(BC) +eval.

:- regtype exceptionHandler/1.
%:- success exceptionHandler(_1) : term(_1) => exceptionHandler(_1).

% Operations on exception handlers

% class of the caught exception
% The constructor None of type option being used to implement finally. It matches any exception
:- entry exceptionHandler_catchType/2 : exceptionHandler*compat(optionClassName).
:- calls exceptionHandler_catchType/2 : exceptionHandler*compat(optionClassName).
:- success exceptionHandler_catchType/2 => exceptionHandler*optionClassName.


% is the given PC in range of the handler
:- entry exceptionHandler_isPCinRange/2 : exceptionHandler*pc.
:- calls exceptionHandler_isPCinRange/2 : exceptionHandler*pc.
:- success exceptionHandler_isPCinRange/2 => exceptionHandler*pc.

% location of the handler code
:- entry exceptionHandler_handler/2 : exceptionHandler*compat(pc).
:- calls exceptionHandler_handler/2 : exceptionHandler*compat(pc).
:- success exceptionHandler_handler/2 => exceptionHandler*pc.

:- regtype field/1.
%:- success field(_1) : term(_1) => field(_1).

% Operations on fields
:- calls field_type/2 : field*compat(type).
:- success field_type/2 : field*compat(type) => field*type.

:- calls field_signature/2 : field*compat(fieldSignature).
:- success field_signature/2 => field*fieldSignature.

:- calls field_isFinal/1 : field.
:- success field_isFinal/1 : field => field.

:- calls field_isStatic/1 : field.
:- success field_isStatic/1 : field => field.

:- calls field_visibility/2 : field*compat(visibility).
:- success field_visibility/2 : field*compat(visibility) => field*visibility.

:- calls field_initValue/2 : field*compat(initialValue).
:- success field_initValue/2 : field*compat(initialValue) => field*initialValue.

%initialValue is the type of values that can be used as initial values for the fields
:- regtype initialValue/1.

:- regtype fieldSignature/1.
%:- success fieldSignature(_1) : term(_1) => fieldSignature(_1).

% Operations on the signatures of fields

:- calls fieldSignature_name/2 : fieldSignature*compat(fieldName). 
:- success fieldSignature_name/2 : fieldSignature*compat(fieldName) => fieldSignature*fieldName.

:- entry fieldSignature_type/2 : fieldSignature*compat(type).
:- calls fieldSignature_type/2 : fieldSignature*compat(type).
:- success fieldSignature_type/2 : fieldSignature*compat(type) => fieldSignature*type.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      Some function definitions on program      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- entry isStatic/1 : fieldSignature.
:- calls isStatic/1 : fieldSignature.
:- success isStatic/1 : fieldSignature => fieldSignature.

:- calls direct_subclass/2 : compat(class)*compat(class).
:- success direct_subclass/2 : compat(class)*compat(class) => class*class.

% subclass is the reflexive transitive closure of the direct_subclass relation
:- calls subclass/2 : compat(class)*compat(class).
:- success subclass/2 : compat(class)*compat(class) => class*class.

:- entry subclass_name/2 : compat(className)*compat(className).
:- calls subclass_name/2 : compat(className)*compat(className).
:- success subclass_name/2 : compat(className)*compat(className) => className*className.

:- calls direct_subclass_name/2 : compat(className)*compat(className).
:- success direct_subclass_name/2 : compat(className)*compat(className) => className*className.

% Similar definitions for interfaces
:- calls direct_subinterface/2 : compat(interface)*compat(interface).
:- success direct_subinterface/2 : compat(interface)*compat(interface) => interface*interface.

:- calls subinterface/2 : compat(interface)*compat(interface).
:- success subinterface/2 : compat(interface)*compat(interface) => interface*interface.

:- calls subinterface_name/2 : compat(interfaceName)*compat(interfaceName).
:- success subinterface_name/2 : compat(interfaceName)*compat(interfaceName) => interfaceName*interface.

:- calls subinterface_name/2 : compat(interfaceName)*compat(interfaceName).
:- success direct_subinterface_name/2 : compat(interfaceName)*compat(interfaceName) => interfaceName*interface.

:- calls class_declares_field/3 : className*fieldSignature*compat(field).
:- success class_declares_field/3 : className*fieldSignature*compat(field) => className*fieldSignature*field.

:- calls interface_declares_field/3 : interfaceName*fieldSignature*compat(field).
:- success interface_declares_field/3 : interfaceName*fieldSignature*compat(field) => interfaceName*fieldSignature*field.

:- calls is_defined_field/3 : className*fieldSignature*compat(field).
:- success is_defined_field/3 : className*fieldSignature*compat(field) => className*fieldSignature*field.

% defined_field c fd holds if the class c declares or inherits a field of signature fd 
:- entry defined_field/2 : className*fieldSignature.
:- calls defined_field/2 : className*fieldSignature.
:- success defined_field/2 : className*fieldSignature => className*fieldSignature.

:- entry findMethod/2 : methodSignature*compat(method).
:- calls findMethod/2 : methodSignature*compat(method).
:- success findMethod/2 : methodSignature*compat(method) => methodSignature*method.

:- calls findField/2 : fieldSignature*compat(field).
:- success findField/2 : fieldSignature*compat(field) => fieldSignature*field.

:- calls methodPackage/2 : methodName*compat(packageName).
:- success methodPackage/2 : methodName*compat(packageName) => methodName*packageName.

% the check visibility method is only for the overrides method 
% (once we know the 2 classes are in the same tree)
:- calls check_visibility/3 : visibility*packageName*packageName.
:- success check_visibility/3 : visibility*packageName*packageName => visibility*packageName*packageName.

% check_signature verifies that the two methods share the same signature
% and that the defining classes belong to the subclass relation
:- calls check_signature/2 : compat(method)*compat(method).
:- success check_signature/2 : compat(method)*compat(method) => method*method.

% Definition of the averride relation
% cf. http://java.sun.com/docs/books/jls/third_edition/html/classes.html#8.4.8
:- calls overrides/2 : compat(method)*compat(method).
:- success overrides/2 : compat(method)*compat(method) => method*method.

% this method find the method Meth in class with class name Cn and that overrides 
% the method corresponding to Msig
:- calls lookup_here/3 : className*methodSignature*var.
:- success lookup_here/3 : className*methodSignature*var => className*methodSignature*method.

:- entry lookup/3 : className*methodSignature*compat(method).
:- success lookup/3 : className*methodSignature*compat(method) => className*methodSignature*method.

 :- trust comp class_has_method(A,B) : (ground(A), ground(B)) + eval.
 :- trust comp class_has_method(A,B) + sideff(free).

% Get the instruction at the given pc
:- entry instructionAt/3 : method*pc*compat(instruction).
:- calls instructionAt/3 : method*pc*compat(instruction).
:- success instructionAt/3 : method*pc*compat(instruction) =>  method*pc*instruction.

% Get the next pc
:- entry next/3 : method*pc*compat(pc).
:- calls next/3 : method*pc*compat(pc).
:- success next/3 : method*pc*compat(pc) => method*pc*pc.

:- calls  implements/2 : className*interfaceName.
:- success implements/2 : className*interfaceName => className*interfaceName.

% compat_refType source target holds if a reference value of type source
% can be assigned to a reference variable of type target.
% cf. http://java.sun.com/docs/books/vmspec/2nd-edition/html/Concepts.doc.html#19674
:- calls compat_refType/2 : refType*refType.
:- success compat_refType/2 : refType*refType => refType*refType.


:- trust comp subclass_name(CN1,CN2) : (ground(CN1),ground(CN2)) + (eval,sideff(free)).
:- trust comp subclass_name(CN1,CN2) : var(CN1) + (memo,bind_ins,sideff(free)).
:- trust comp subclass_name(CN1,CN2) : var(CN2) + (memo,bind_ins,sideff(free)).
subclass_name(CN1,CN2):-
	program_class(CN1,C1),
	program_class(CN2,C2),
	subclass(C1,C2).

:- trust comp not_subclass_name(CN1,CN2) : (ground(CN1),ground(CN2)) + (eval,sideff(free)).
:- trust comp not_subclass_name(CN1,CN2) : var(CN1) + (memo,bind_ins,sideff(free)).
:- trust comp not_subclass_name(CN1,CN2) : var(CN2) + (memo,bind_ins,sideff(free)).
not_subclass_name(CN1,CN2) :- 
	\+ subclass_name(CN1,CN2).

:- trust comp get_method_id/2 + (eval,sideff(free)).
get_method_id(MSig,MId) :-
	findMethod(MSig,Method),
	method_body(Method,Body),
	bytecodeMethod_methodId(Body,methodId(_,MId)).

:- trust comp msig_from_smnwa/2 + (eval,sideff(free)).
msig_from_smnwa(SMNWA,MSig) :-
	split_smnwa(SMNWA,SMN,A,_),
	loaded_classes:main_class(MainClassName), % It assumes it is from the main_class
	(integer(A) -> length(Params,A) ; true),
	MSig = methodSignature(methodName(MainClassName,shortMethodName(SMN)),Params,_).

:- trust comp split_smnwa/4 + (eval,sideff(free)).
split_smnwa(SMNWA,SMN,An,AdjSMN) :-
	atom_concat([SMN,'/',A],SMNWA),!,
	atom_concat(SMN,A,AdjSMN),
	atom_number(A,An).
split_smnwa(SMN,SMN,_,SMN).

:- trust comp isStatic/1 + (eval,sideff(free)).
isStatic(FieldSignature):-
	fieldSignature_name(FieldSignature,fieldName(ClassName,ShortFieldName)),
	program_class(ClassName,Class),!,
	class_field(Class,ShortFieldName,Field),
	field_isStatic(Field).
isStatic(FieldSignature):-
	fieldSignature_name(FieldSignature,fieldName(ClassName,ShortFieldName)),
	ClassName = className(PN,SCN), 
	InterfaceName = interfaceName(PN,SCN), 
% We need to do this conversion as the reader cannot differenciate between classes and interfaces
	program_interface(InterfaceName,Interface),
	interface_field(Interface,ShortFieldName,Field),
	field_isStatic(Field).
:- pop_prolog_flag(unused_pred_warnings).
