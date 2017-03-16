:- module(pythonrt, [ 
	python_start/0,
	python_start/2,
	python_stop/0,
	python_connect/2,
	python_disconnect/0,
	machine_name/1,
	python_constructor/1,
	python_object/1,
	prolog_goal/1,
	python_field/1,
	python_use_module/1,
	python_import_module/2,
	python_create_object/2,
	python_delete_object/1, 
	python_invoke_method/2, 
	python_method/1,
	python_module/1,
	python_get_value/2,
	python_set_value/2],
	[assertions,regtypes,isomodes]).

:- doc(title,"Prolog to Java interface").

:- doc(author,"Jes@'{u}s Correas").

:- doc(module,"
Attempt : Prolog to Python interface
").

:- use_module(library(concurrency)).
:- use_module(library(iso_byte_char)).
:- use_module(library(lists)).
:- use_module(library(read), [read/2]).
:- use_module(library(write), [write/1]).
:- use_module(library(pythonll(pythonsock))).
:- use_module(library(system)).


% /home/clip/Systems/ciao/lib/engine/basic_props.pl
%% -----------------------------------------------------------------------
%% REGTYPES
%% -----------------------------------------------------------------------
:- regtype python_module(X) # "@var{X} is the name of an loaded python module".

python_module(X) :- atm(X).

:- regtype machine_name(X) # "@var{X} is the network name of a machine.".

machine_name(X) :- atm(X).

:- regtype python_object(X) # "@var{X} is a python object (a structure with functor
	'$python_object', and argument an integer given by the python side).".

python_object('$python_object'(X)) :- int(X).

:- regtype python_field(X) # "@var{X} is a python field (structure on which
	the functor name is the field name, and the single argument is 
	the field value).".

python_field(X) :- struct(X).

:- regtype python_method(X) # "@var{X} is a python method (structure with
	functor as method name, and arguments as method ones, plus a result
	argument. This result argument is unified with the atom 'Yes' if
	the python method returns void).".

python_method(X) :- struct(X).

:- regtype python_constructor(X) # "@var{X} is a python constructor (structure
	with functor as constructor full name, and
	arguments as constructor arguments).".

python_constructor(X) :- struct(X).

:- regtype prolog_goal(X) # "@var{X} is a prolog predicate. Prolog term
	that represents the goal that must be invoked when the event
	raises on the object. The predicate arguments can be python objects,
	or even the result of python methods. These python objects will be
	evaluated when the event raises (instead of when the listener is
	added). The arguments that represent python objects must be
	instantiated to already created objects. The variables will be kept
	uninstantiated when the event raises and the predicate is
	called.".

prolog_goal(X) :- callable(X).

%% -----------------------------------------------------------------------
:- pred python_start
	# "Starts the Java server on the local machine,
	connects to it, and starts the event handling thread.".
%% -----------------------------------------------------------------------

python_start :-
	python_start(localhost, "/Users/zdrey/Work/CiaoSVN/ciao/ciao/contrib/pythonll").

%% -----------------------------------------------------------------------
:- pred python_start(+Node, +Classpath)
	:: machine_name * string # "Starts the Java server on the local machine, 
	connects to it, and starts the event handling thread. The Java
	server is started using the classpath received as argument.".
%% -----------------------------------------------------------------------

python_start(Node, Cp) :-
	set_prolog_flag(write_strings, on),
	(is_connected_to_python ->
% exceptions are not handled properly with multiple threads.
 display(python_exception('Java connection already active')),nl,
	throw(python_exception('Java connection already active')),
	fail
	;
	true
	),
	set_pythonpath(Cp),
	Command = 'python -m CiaoPython.plpythonserver',
	!,
	popen(Command,read,Stream),
%	exec('python -m CiaoPython.plpythonserver', In, Stream, Err),
%	close(Stream),close(In), close(Err),
	get_port(Stream,Port),
	python_debug('Port :'(Port)),
	python_connect_stream(Node,Port,Stream).

%% -----------------------------------------------------------------------
:- pred python_stop/0 # "Stops the interface terminating the threads
	that handle the socket connection, and finishing the Java
	interface server if it was started using python_start/n.".
%% -----------------------------------------------------------------------
python_stop :-
	is_connected_to_python,
	!,
	stop_socket_interface.

%% -----------------------------------------------------------------------
:- pred python_connect(+Machine_name,+Port_number) 
	:: machine_name * int # "Connects to an existing
	Java interface server running in @var{Machine_name} and listening at
	port @var{port_number}. To connect to a Java server located in the
	local machine, use 'localhost' as machine_name.". 
%% -----------------------------------------------------------------------
python_connect(Node,Port) :-
	start_socket_interface(Node:Port,_).

%% -----------------------------------------------------------------------
:- pred python_connect_stream(+Machine_name,+Port_number,+Stream) 
	:: machine_name * int * stream # "Connects to an existing
	Java interface server running in @var{Machine_name}, listening at port
	@var{port_number}, and with std. input stream @var{Stream}. To connect
	to a Java server located in the local machine, use 'localhost' as
	@var{Machine_name}.". 
%% -----------------------------------------------------------------------
python_connect_stream(Node,Port,Stream) :-
	start_socket_interface(Node:Port,Stream).

%% -----------------------------------------------------------------------
:- pred python_disconnect/0
	# "Closes the connection with the python process, terminating the
	threads that handle the connection to Java. This predicate does
	not terminate the Java process (this is the disconnection 
	procedure for Java servers not started from Prolog). This 
	predicate should be used when the communication is established
	with python_connect/2.".
%% -----------------------------------------------------------------------
python_disconnect :-
	is_connected_to_python,
	!,
	stop_socket_interface.

%% -----------------------------------------------------------------------
:- pred set_pythonpath(+User_classpath) :: string # "Assigns
	the CLASSPATH environment variable needed by Java to run the
	Java server and user classes.".
%% -----------------------------------------------------------------------

set_pythonpath(UserClasspath) :-
	compound_classpath(UserClasspath,CPath),
	setenvstr('PYTHONPATH',CPath).

%% -----------------------------------------------------------------------
:- pred compound_classpath(+User_classpath,-New_classpath) ::
	string * string # "Compounds a string with the
	classpath needed by Java to run the Java server and user
	classes.".
%% -----------------------------------------------------------------------

compound_classpath(UserClasspath,NewClasspath) :-
	absolute_file_name(library(pythonll(pythonrt)),AbsFileName),
	name(AbsFileName,AbsFileNameS),
	append(UClasspath,"/pythonrt.pl",AbsFileNameS),
	correct_win_path(UClasspath,CiaoClasspath,System),
	addPath(CiaoClasspath,UserClasspath,System,NewClasspath).

addPath(Cp,"",_,Cp).
 
addPath("",Cp,_,Cp).

addPath(Cp1,Cp2,windows,Cp) :-
	change_slashes(Cp1,Cp1s),
	change_slashes(Cp2,Cp2s),
	append(Cp1s,[0';|Cp2s],Cp).
%

addPath(Cp1,Cp2,other,Cp) :-
	append(Cp1,[0':|Cp2],Cp).

correct_win_path([0'/,0'c,0'y,0'g,0'd,0'r,0'i,0'v,0'e,0'/,L,0'/|Upath],[L,0':,Bs|Wpath],windows):-
	!,
	char_code('\\',Bs),
	member(L,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
	change_slashes(Upath, Wpath).

correct_win_path([0'/,0'/,L,0'/|Upath], [L,0':,Bs|Wpath],windows):-
	char_code('\\',Bs),
	member(L,"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
	change_slashes(Upath, Wpath).

correct_win_path(X,X,other).
	
change_slashes([],[]).

change_slashes([0'/|Upath],[0'\\|Wpath]):-
	!,
	change_slashes(Upath,Wpath).

change_slashes([L|Upath],[L|Wpath]):-
	change_slashes(Upath,Wpath).

%% -----------------------------------------------------------------------
:- pred get_port(+Stream,-Port)
	:: stream * atom # "Gets the @var{Port} number to connect to Java
	server, reading it from the @var{Stream} received as argument.".
%% -----------------------------------------------------------------------

get_port(Stream,Port):-
	read(Stream, Port).

%% -----------------------------------------------------------------------
:- pred python_use_module(+Module)
	:: term # "Loads a module and makes it available from Java.".
	
%% -----------------------------------------------------------------------
python_use_module(Module):-
	assertz_fact(prolog_query(0,internal_use_module(Module))),
	!.


%% Python module import (e.g. import os)
:- pred python_import_module(+Python_import,-python_module)
	:: string * python_module # "Module import. Python allows to assign
	the imported module as a variable".
%% Import, From, Module?
python_import_module(Import, Module):-
	is_connected_to_python,
	nonvar(Import),
	var(Module),
	assertz_fact(python_query(Id, '$python_import_module'(Import))),
	retract_fact(python_response(Id, Module)), % We do not need the call anymore
	!, check_error(Module)
	. % get generated python exception and throw it back to prolog side

%% -----------------------------------------------------------------------
:- pred python_create_object(+python_constructor,-python_object) 
	:: python_constructor * python_object # "New python object creation. The
	constructor must be a compound term as defined by its type, with
	the full class name as functor (e.g., 'python.lang.String'), and the
	parameters passed to the constructor as arguments of the
	structure.".
%% -----------------------------------------------------------------------

python_create_object(Constructor, Object):-
	is_connected_to_python,
	nonvar(Constructor),
	var(Object),
	Constructor =.. [Name | ArgList],
	eng_goal_id(Id),
	assertz_fact(python_query(Id,'$python_create_object'(Name,ArgList))),
	retract_fact(python_response(Id,Object)),
	!,
	check_error(Object).

%% -----------------------------------------------------------------------
:- pred python_delete_object(+python_object) 
	:: python_object # "Java object deletion. It removes the object given
	as argument from the Java object table.".
%% -----------------------------------------------------------------------

python_delete_object(Object) :-
	is_connected_to_python,
	nonvar(Object),
	eng_goal_id(Id),
	assertz_fact(python_query(Id,'$python_delete_object'(Object))),
	retract_fact(python_response(Id,T)),
	!,
	check_error(T).

%% -----------------------------------------------------------------------
:- pred python_get_value(+python_object,+python_field)
	:: python_object * python_field # "Gets the value of a field. Given a
	Java object as first argument, it instantiates the variable given
	as second argument. This field must be	uninstantiated in the
	python_field functor, or this predicate will fail.".
%% -----------------------------------------------------------------------

python_get_value(Object,Field) :-
	is_connected_to_python,
	nonvar(Object),
	Field =.. [Name, Value],
	eng_goal_id(Id),
	assertz_fact(python_query(Id,'$python_get_value'(Object,Name))),
	retract_fact(python_response(Id,Value)),
	!,
	check_error(Value).

%% -----------------------------------------------------------------------
:- pred python_set_value(+python_object,+python_field) 
	:: python_object * python_field # "Sets the value of a Java object
	field. Given a Java object reference, it assigns the value included
	in the python_field compound term. The field value in the python_field
	structure must be instantiated.".
%% -----------------------------------------------------------------------

python_set_value(Object,Field) :-
	is_connected_to_python,
	nonvar(Object),
	Field =.. [Name, Value],
	eng_goal_id(Id),
	assertz_fact(python_query(Id,'$python_set_value'(Object,Name,Value))),
	retract_fact(python_response(Id,T)),
	!,
	check_error(T).

%% -----------------------------------------------------------------------
:- pred python_invoke_method(+python_object,+python_method) 
	:: python_object * python_method # "Invokes a python method on an
	object. Given a Java object reference, invokes the method
	represented with the second argument. ".
%% -----------------------------------------------------------------------

python_invoke_method(Object,Method) :-
	is_connected_to_python,
	nonvar(Object),
	nonvar(Method),
	Method =.. [Name | ArgList],
	append(Args,[Result],ArgList),
	eng_goal_id(Id),
	assertz_fact(python_query(Id,'$python_invoke_method'(Object,Name,Args))),
	retract_fact(python_response(Id,Result)),
	!,
	check_error(Result).
%% -----------------------------------------------------------------------
:- pred check_error(+term)
	:: term
	# "Checks if a term received from python is the '$fail' term, or an
	exception has occured in Java.".
%% -----------------------------------------------------------------------
check_error('$fail') :- 
	!,
	fail.

check_error('$python_exception'(E)):-
		!,
% exceptions are not handled properly with multiple threads.
%	display('An exception was run by the python interpreter'),
%	display(E),nl,
	throw(python_exception(E)),
	fail.

check_error(_).
