:- module(misc_commands, [
	                  this_run_test_module/1,
	                  do_command/1, 
                          give_message/1,
                          ciaoc_compile/1,
                          ciaoc_compile_flags/2], [fsyntax]).

:- use_module(library(system)).
:- use_module(library(terms)).
:- use_module(library(format)).
:- use_module(library(unittest)).

% :- include(ciaoc_name).
:- use_module(library(lpdist(ciao_config_options)), [setlocalciao/1, ciaoc/1]).
ciaoc_name := ~atom_concat([~setlocalciao, ' ', ~ciaoc]).

this_run_test_module(Alias) :-
	get_result_test_module(Alias, [dump_output, dump_error, rtc_entry], TS),
	show_tests_summaries(TS).

do_command(AtomList):-
        working_directory(Dir, Dir),
        format("Information message: working in dir ~w~n", [Dir]),
        atom_concat(AtomList, Command),
        format("Information message: issuing command ~w~n", [Command]),
        shell(Command).

give_message(X):-
    nl, nl,
    display('+----------------------------------------------------------+'),
    nl,
    display('|  '),
    display(X),
    nl,
    display('+----------------------------------------------------------+'),
    nl, nl.


ciaoc_compile(What):-
    ciaoc_compile_flags(What, '').

ciaoc_compile_flags(What, Flags):-
    ciaoc_name(Ciaoc),
    do_command([Ciaoc, ' ', Flags, ' ', What]).
