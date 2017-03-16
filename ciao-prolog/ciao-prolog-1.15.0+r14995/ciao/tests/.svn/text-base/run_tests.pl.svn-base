:- module(run_tests, [main/0, run_tests/0], []).

:- use_module(library(system)).
:- use_module(misc_commands).

main :- run_tests.

run_tests :-
	working_directory(ThisDir, ThisDir),
	give_message('Executing program suite'),
	this_run_test_module(suite), %% General stuff
% 	give_message('Executing debugger test'),
%	working_directory(_, debugger),
% 	this_run_test_module('debugger/embedded_debugger_test'),
	working_directory(_, ThisDir),
	give_message('Executing end-of-line tests'),
% 	End of lines of Unix, Mac & Windows
	this_run_test_module('end_of_lines/test_eol'),
% 	give_message('Trying dynamic library loading'),
% %	Includes sockets, math, FLI gen. 
%       this_run_test_module('load_dynlibs'),
	give_message('Trying active modules test'),
% %	Sockets, shell calls
	this_run_test_module(actmods_test),
	give_message('Executing Andorra tests'),
	this_run_test_module('expansions/test_andorra'), %% Expansions
	give_message('Executing feture tree tests'),
	this_run_test_module('expansions/features'), %% Expansions
	give_message('Executing functional syntax tests'),
	this_run_test_module('expansions/funct'), %% Expansions, higher order
	give_message('Executing iterative deepening tests'),
	this_run_test_module('expansions/itdep'), %% Expansions, higher order
%%      OOP tests Disabled: they requires htc which is not distributed -- EMM
% 	give_message('Executing OOP tests'),
% 	this_run_test_module(object_test), %% Objects
	working_directory(_, ThisDir),
	give_message('Executing CLP tests'),
	this_run_test_module(clp), %% CLP(Q) at the moment (see comment there)
	give_message('Executing CLP(fd) tests'),
	this_run_test_module(fd_test),
% 	give_message('Executing concurrency tests'),
% 	this_run_test_module(conc), %% Concurrency stuff
	give_message('Executing remote execution tests'),
	this_run_test_module(remote_exec), %% Check remote execution
%% Java test changes directory; why?
	give_message('Executing Java tests'),
	this_run_test_module(test_java),
%        cd(ThisDir),
% 	give_message('Executing Tcl/Tk tests (requires that tk be installed)'),
% 	this_run_test_module(test_tcltk),
	give_message('Executing script execution tests'),
	this_run_test_module(test_scripts),
	give_message('Executing Fuzzy Prolog tests'),
	this_run_test_module(test_fuzzy),
	give_message('Executing XML Path tests'),
	this_run_test_module(xml_path_test),
	give_message('Executing XDR handle tests'),
	this_run_test_module(xdr_handle_test),
	give_message('Executing Floating point printing tests'),
	this_run_test_module(float),
	give_message('Bug related with including a file that reexport a module'),
	this_run_test_module(reexport_include).
