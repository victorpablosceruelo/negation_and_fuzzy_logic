:- module(messages,[
	    error_message/1,
	    error_message/2,
	    error_message/3,
	    warning_message/1,
	    warning_message/2,
	    warning_message/3,
 	    note_message/1,
	    note_message/2,
	    note_message/3,
 	    simple_message/1,
	    simple_message/2,
	    optional_message/2,
	    optional_message/3,
 	    debug_message/1,
	    debug_message/2,
 	    debug_goal/2,
 	    debug_goal/3
	],
        [
            assertions,regtypes,isomodes
        ]).

%% NOTE: if you change the output format of messages you 
%%       will probably also want to change ciao.el

:- use_module(library(format), [format/3, format_control/1]).

% Other libraries
:- use_module(library(lists)).
:- use_module(library(filenames), [no_path_file_name/2]).

:- set_prolog_flag(multi_arity_warnings, off).

%% ---------------------------------------------------------------------------

:- doc(title,"Printing status and error messages").

:- doc(author,"The CLIP Group").

:- doc(module,"This is a very simple library for printing status
     and error messages to the console.").

:- doc(bug, "Debug message switching should really be done with an
   expansion, for performance.").

:- doc(doinclude,location/1).
:- regtype location/1 # "Identifies a program source line.".

location(loc(File,L1,L2)):- atm(File), int(L1), int(L2).

%% ---------------------------------------------------------------------------

:- pred error_message(Text) : string 
   # "The text provided in @var{Text} is printed as an ERROR message.".

:- '$context'(error_message/1, module).
error_message(Message) :-
	'$module'(Module),
	compose("ERROR",Module,Message).

:- pred error_message(Text,ArgList) : format_control * list 
   # "The text provided in @var{Text} is printed as an ERROR message,
     using the arguments in @var{ArgList} to interpret any
     variable-related formatting commands embedded in @var{Text}.".

:- '$context'(error_message/2, module).
error_message(Message,A) :-
	'$module'(Module),
	compose("ERROR",Module,Message,A).

:- pred error_message(Lc,Text,ArgList) : location * format_control * list 
   # "The text provided in @var{Text} is printed as an ERROR message,
     using the arguments in @var{ArgList} to interpret any
     variable-related formatting commands embedded in @var{Text}, and
     reporting error location @var{Lc} (file and line numbers).".

:- '$context'(error_message/3, module).
error_message(Loc,Message,A) :-
	nonvar(Loc),
	Loc=loc(File,LB,LE),
	!,
	'$module'(Module),
	compose("ERROR",Module,File,LB,LE,Message,A).
error_message(_Loc,Message,A) :-
	'$module'(Module),
	compose("ERROR",Module,Message,A).

%% ---------------------------------------------------------------------------

:- pred warning_message(Text) : string 
   # "The text provided in @var{Text} is printed as a WARNING message.".

:- '$context'(warning_message/1, module).
warning_message(Message) :-
	'$module'(Module),
	compose("WARNING",Module,Message).

:- pred warning_message(Text,ArgList) : format_control * list 

   # "The text provided in @var{Text} is printed as a WARNING message,
     using the arguments in @var{ArgList} to interpret any
     variable-related formatting commands embedded in @var{Text}.".

:- '$context'(warning_message/2, module).
warning_message(Message,A) :-
	'$module'(Module),
	compose("WARNING",Module,Message,A).

:- pred warning_message(Lc,Text,ArgList) : location * format_control * list 

   # "The text provided in @var{Text} is printed as a WARNING message,
     using the arguments in @var{ArgList} to interpret any
     variable-related formatting commands embedded in @var{Text}, and
     reporting error location @var{Lc} (file and line numbers).".

:- '$context'(warning_message/3, module).
warning_message(Loc,Message,A) :-
	nonvar(Loc),
	Loc=loc(File,LB,LE),
	!,
	'$module'(Module),
	compose("WARNING",Module,File,LB,LE,Message,A).
warning_message(_Loc,Message,A) :-
	'$module'(Module),
	compose("WARNING",Module,Message,A).

%% ---------------------------------------------------------------------------

:- pred note_message(Text) : string 
   # "The text provided in @var{Text} is printed as a NOTE.".

:- '$context'(note_message/1, module).
note_message(Message) :-
	'$module'(Module),
	compose("NOTE",Module,Message).

:- pred note_message(Text,ArgList) : format_control * list 

   # "The text provided in @var{Text} is printed as a NOTE, using the
     arguments in @var{ArgList} to interpret any variable-related
     formatting commands embedded in @var{Text}.".

:- '$context'(note_message/2, module).
note_message(Message,A) :-
	'$module'(Module),
	compose("NOTE",Module,Message,A).

:- pred note_message(Lc,Text,ArgList) : location * format_control * list 

   # "The text provided in @var{Text} is printed as a NOTE, using the
     arguments in @var{ArgList} to interpret any variable-related
     formatting commands embedded in @var{Text}, and reporting error
     location @var{Lc} (file and line numbers).".

:- '$context'(note_message/3, module).
note_message(Loc,Message,A) :-
	nonvar(Loc),
	Loc=loc(File,LB,LE),
	!,
	'$module'(Module),
	compose("NOTE",Module,File,LB,LE,Message,A).
note_message(_Loc,Message,A) :-
	'$module'(Module),
	compose("NOTE",Module,Message,A).

%% ---------------------------------------------------------------------------

:- pred simple_message(Text) : string 
   # "The text provided in @var{Text} is printed.".

simple_message(Message) :-
	simple_message(Message,[]).

:- pred simple_message(Text,ArgList) : format_control * list 
   # "The text provided in @var{Text} is printed as a message,
     using the arguments in @var{ArgList}.".

simple_message(Message,A) :-
	append([0'{ | Message],"}\n",NMessage),
	format(user_error,NMessage,A).

%% ---------------------------------------------------------------------------

:- pred optional_message(Text,Opts) : string * list(atm)
   # "The text provided in @var{Text} is printed as a message, but
     only if the atom @tt{-v} is a member of @var{Opts}. These
     predicates are meant to be used for optional messages, which are
     only to be printed when @em{verbose} output is requested
     explicitly.".

optional_message(Message,Opts) :-
	optional_message(Message,[],Opts).

:- pred optional_message(Text,ArgList,Opts) : format_control * list * list(atm)
   # "The text provided in @var{Text} is printed as a message, using
     the arguments in @var{ArgList}, but only if the atom @tt{-v} is a
     member of @var{Opts}. These predicates are meant to be used for
     optional messages, which are only to be printed when @em{verbose}
     output is requested explicitly.".

optional_message(Message,A,Opts) :-
	member('-v',Opts),
	!,
	simple_message(Message,A).
optional_message(_Message,_A,_Opts).

%% ---------------------------------------------------------------------------

:- pred debug_message(Text) : format_control 

   # "The text provided in @var{Text} is printed as a debugging
      message.  These messages are turned @tt{on} by defining a fact
      of @pred{issue_debug_messages/1} with the module name as
      argument.".

:- '$context'(debug_message/1, module).
debug_message(Message) :-
	'$module'(Module),
	(  issue_debug_messages(Module)
	-> compose("DEBUG",Module,Message)
	;  true ).

:- pred debug_message(Text,ArgList) : format_control * list 

   # "The text provided in @var{Text} is printed as a debugging
      message, using the arguments in @var{ArgList} to interpret any
      variable-related formatting commands embedded in
      @var{Text}. These messages are turned @tt{on} by defining a fact
      of @pred{issue_debug_messages/1} which the module name as
      argument.".

:- '$context'(debug_message/2, module).
debug_message(Message,A) :-
	'$module'(Module),
	(  issue_debug_messages(Module)
	-> compose("DEBUG",Module,Message,A)
	;  true ).

:- pred issue_debug_messages(Module) => atom

   # "Printing of debugging messages is enabled for module @var{Module}.".

:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.

%% ---------------------------------------------------------------------------

:- pred debug_goal(Goal,Text) 

   # "@var{Goal} is called.  The text provided in @var{Text} is then
      printed as a debugging message.  The whole process (including
      running @var{Goal}) is turned @tt{on} by defining a fact of
      @pred{issue_debug_messages/1} with the module name as
      argument.".

:- '$context'(debug_goal/2, module).
debug_goal(Goal,Message) :-
	'$module'(Module),
	(  issue_debug_messages(Module)
	-> call(Goal),
	   compose("DEBUG",Module,Message)
	;  true ).

:- pred debug_goal(Goal,Text,ArgList) 

   # "@var{Goal} is called.  The text provided in @var{Text} is then
      printed as a debugging message, using the arguments in
      @var{ArgList} to interpret any variable-related formatting
      commands embedded in @var{Text}. Note that the variables in
      @var{ArgList} can be computed by @var{Goal}.  The whole process
      (including running @var{Goal}) is turned @tt{on} by defining a
      fact of @pred{issue_debug_messages/1} with the module name as
      argument.".

:- '$context'(debug_goal/3, module).
debug_goal(Goal,Message,A) :-
	'$module'(Module),
	(  issue_debug_messages(Module)
	-> call(Goal),
	   compose("DEBUG",Module,Message,A)
	;  true ).

%% ---------------------------------------------------------------------------

:- pred compose(Type,Module,Mess) 
   : string * atm * string

   # "Print a generic error of type @var{Type}, flagged in module
      @var{Module}, with error message @var{Mess}.".

compose(Type,Module,Mess) :-
	append("{~s (~q): ",Mess,T1),
	append(T1,"}~n",CMess),
	prolog_flag(write_strings, Old, on),
	format(user_error,CMess,[Type,Module]),
	set_prolog_flag(write_strings, Old).

:- pred compose(Type,Module,Mess,Args) 
   : string * atm * format_control * list

   # "Print a generic error of type @var{Type}, flagged in module
      @var{Module}, with error message @var{Mess} containing arguments
      @var{Args}.".

compose(Type,Module,Mess,Args) :-
	append("{~s (~q): ",Mess,T1),
	append(T1,"}~n",CMess),
	prolog_flag(write_strings, Old, on),
	simplify_module(Module,SimplifiedModule),
	format(user_error,CMess,[Type,SimplifiedModule|Args]),
	set_prolog_flag(write_strings, Old).

simplify_module(user(Path),SimplifiedModule) :-
	no_path_file_name(Path,SimplifiedModule),
	!.
simplify_module(Path,SimplifiedModule) :-
	no_path_file_name(Path,SimplifiedModule),
	!.
simplify_module(Module,Module).


:- pred compose(Type,Module,File,LB,LE,Mess,Args) 
   : string * atm * atm * int * int * format_control * list

   # "Print a generic error of type @var{Type}, flagged in module
      @var{Module}, while processing file @var{File}, between line
      numbers @var{LB} and @var{LE}, with error message @var{Mess}
      containing arguments @var{Args}.".

compose(Type,Module,File,LB,LE,Mess,Args) :-
	append("{In ~w~n~s (~q): (lns ~w-~w) ",Mess,T1),
	append(T1,"~n}~n",CMess),
	prolog_flag(write_strings, Old, on),
	format(user_error,CMess,[File,Type,Module,LB,LE|Args]),
	set_prolog_flag(write_strings, Old).
