% TODO: the name of this module is not good
:- module(system_info, [], [pure, assertions, isomodes]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_basic)).

:- doc(title,"Gathering some basic internal info").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Carro").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module,"This module provides predicates which return basic
   internal info.").

%%---------------------------------------------------------------------

:- '$native_include_c_source'(.(system_info)).

:- export(get_arch/1).
:- true pred get_arch(?ArchDescriptor) :: atm #
	"Unifies @var{ArchDescriptor} with a simple atom which describes
         the computer architecture currently executing the predicate.".

:- doc(get_arch/1,
	"This predicate will describe the computer architecture wich
         is currently executing the predicate.

         Computer architectures are identified by a simple atom.
         This atom is implementation-defined, and may suffer any change
         from one Ciao Prolog version to another.

         For example,Ciao Prolog running on an Intel-based machine 
         will retrieve:
@begin{verbatim}
?- get_arch(I).

I = i86 ? ;

no
?- 
@end{verbatim}
	").
:- '$props'(get_arch/1, [impnat=cbool(prolog_getarch)]).

%%---------------------------------------------------------------------

:- export(get_os/1).
:- true pred get_os(?OsDescriptor) :: atm #
	"Unifies @var{OsDescriptor} with a simple atom which describes
         the running Operating System when predicate was called.".
:- doc(get_os/1,
	"This predicate will describe the Operating System which 
         is running on the machine currently executing the Prolog program.

         Operating Systems are identified by a simple atom.
         This atom is implementation-defined, and may suffer any change
         from one Ciao Prolog version to another.

         For example,Ciao Prolog running on Linux will retrieve:
@begin{verbatim}
?- get_os(I).

I = 'LINUX' ? ;

no
?- 
@end{verbatim}
	").
:- '$props'(get_os/1, [impnat=cbool(prolog_getos)]).

%%---------------------------------------------------------------------

% TODO: does not work! documenting a predicate defined in other module...
/*
:- pred current_module(Module) :: internal_module_id + native #
	"Retrieves (on backtracking) all currently loaded modules into
         your application.".
:- doc(current_module/1,
	"This predicate will successively unify its argument with all
	 module names currently loaded. Module names will be simple atoms.

         When called using a free variable as argument, it will
         retrieve on backtracking all modules currently loaded. This is 
         usefull when called from the Ciao @apl{toplevel}.

         When called using a module name as argument it will check whether
         the given module is loaded or not. This is usefull when called
         from user programs.
        ").
*/
:- reexport(engine(rt_exp), [current_module/1]).

%%---------------------------------------------------------------------
% TODO: remove!!! or change the name... 
:- export(ciao_lib_dir/1).
:- pred ciao_lib_dir(CiaoPath) :: atm(CiaoPath) #
	"@var{CiaoPath} is the path to the root of the Ciao
	libraries. Inside this directory, there are the directories
	'lib', 'library' and 'contrib', which contain library modules.".
:- '$props'(ciao_lib_dir/1, [impnat=cbool(prolog_ciao_lib_dir)]).

%%---------------------------------------------------------------------

:- export(get_so_cc/1).
:- '$props'(get_so_cc/1, [impnat=cbool(prolog_get_so_cc)]).
:- export(get_so_ld/1).
:- '$props'(get_so_ld/1, [impnat=cbool(prolog_get_so_ld)]).
:- export(get_so_cc_opts/1).
:- '$props'(get_so_cc_opts/1, [impnat=cbool(prolog_get_so_cc_opts)]).
:- export(get_so_ld_opts/1).
:- '$props'(get_so_ld_opts/1, [impnat=cbool(prolog_get_so_ld_opts)]).
:- export(get_so_libs/1).
:- '$props'(get_so_libs/1, [impnat=cbool(prolog_get_so_libs)]).

%%---------------------------------------------------------------------

% TODO: this is not the module for this predicate
:- export(this_module/1).
:- pred this_module(Module) :: internal_module_id #
	"@var{Module} is the internal module identifier for current module.".
:- '$context'(this_module/1, module).
this_module(M) :- '$module'(M).

%%---------------------------------------------------------------------

:- doc(doinclude,internal_module_id/1).

:- prop internal_module_id(M) #
	"@var{M} is an internal module identifier".

% TODO: I changed this...
%:- doc(internal_module_id/1, "For a user file it is a term user/1
%	with an argument different for each user file, for
%	other modules is just the name of the module (as an atom).").

%internal_module_id(user(M)) :-
%	atm(M).
internal_module_id(M) :- 
	atm(M).
