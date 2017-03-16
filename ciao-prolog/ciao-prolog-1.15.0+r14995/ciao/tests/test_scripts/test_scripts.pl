:- module(test_scripts, [main/0], [assertions]).

:- use_module(library(system)).
:- use_module(library(format)).

:- use_module(.('../misc_commands')).

script_name('test_scripts/pldiff').
file_name(1, 'test_scripts/davinci_1.pl').
file_name(2, 'test_scripts/davinci_2.pl').

:- test main.

main:-
        format("Testing script execution~n", []),
        script_name(Script),
        file_name(1, File1),
        file_name(2, File2),
        do_command([Script, ' ', File1, ' ', File2]).
