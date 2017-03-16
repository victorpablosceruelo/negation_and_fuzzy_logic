:- module(embedded_debugger_test, [main/0], [assertions, unittestprops, fsyntax]).

:- load_test_module(library(bundle_registry(bundle_registry_load))).
:- use_module(library(strings)).
:- use_module(library(lists), [append/3]).
:- use_module(library(format)).
:- use_module(library(compiler(exemaker))).
:- use_module(library(system)).
:- use_module(library(iso_byte_char)).

:- test main.

% Name of the program to debug
to_debug(ciaosrc(tests(debugger(to_debug)))).

main:-
        to_debug(TDA),
	absolute_file_name(TDA, TDpl),
	atom_concat(TD, '.pl', TDpl),
        make_exec([TDpl], TD),
        exec(TD, In, Out),
        interact(In, Out).

% Failure loop to read interactions and to react to them
interact(In, Out):-
        interaction(WhatToDo),
          (
              action(WhatToDo, In, Out) ->
              true
          ;
              display('*** Error: expected '),
              display_error(WhatToDo),
              nl
          ),
        fail.
interact(In, Out):-
        close(In),
        close(Out).

display_error(question(E)):-      format("\"~s\"~n", [E]).
display_error(boundaries(B, E)):- format("\"~s\" ended by \"~s\"~n", [B, E]).

action(skip, _, Out):-         get_line(Out, _Line).
action(send(Char), In, _Out):- display(In, Char).
action(question(Expected), _, Out):- 
        get_question(Expected, Out, _Line).
%        format("Exact line: ~s~n", [Line]).
action(boundaries(Beginning, End), _, Out):-
        get_line(Out, Line),
%        format("Boundaried line: ~s~n", [Line]),
        append(Beginning, Rest, Line),
        append(_Middle, End, Rest).

% A question does not end in newline - it waits for us!  We read while
% the characters we expect are in the input.
get_question([], _Out, []).
get_question([E|Es], Out, [E|Rest]):-
        get_byte(Out, E), !,
        get_question(Es, Out, Rest).
get_question(_, _, []).

        
% This specifies the interaction with the debugger.  It dependes *completely*
% on the program being debugged.
interaction(skip).
interaction(skip).
interaction(boundaries("         In ", " (5-13) foo-2")).
interaction(question(" + *  *  Call: to_debug:foo([1,2,3]) ? ")).
interaction(send('\n')).
interaction(boundaries("         In ", " (16-18) bar-1")).
interaction(question("   *  *  Call: to_debug:bar(1) ? ")).
interaction(send('\n')).
interaction(boundaries("         In ", " (19-21) display-1")).
interaction(question("   *  *  Call: display(1) ? ")).
interaction(send('\n')).
interaction(boundaries("1         In ", " (19-21) display-1")).
interaction(question("   *  *  Exit: display(1) ? ")).
interaction(send('\n')).
interaction(boundaries("         In ", " (16-18) bar-1")).
interaction(question("   *  *  Exit: to_debug:bar(1) ? ")).
interaction(send('\n')).
interaction(boundaries("         In ", " (16-18) foo-2")).
interaction(question(" + *  *  Call: to_debug:foo([2,3]) ? ")).
interaction(send(l)).
interaction(send('\n')).
interaction(boundaries("2         In ", " (16-18) foo-2")).
interaction(question(" + *  *  Call: to_debug:foo([3]) ? ")).
interaction(send(-)).
interaction(send('\n')).
interaction(boundaries("{Spypoint removed from 'to_debug:foo'/1}", "")).
interaction(boundaries("         In ", " (16-18) foo-2")).
interaction(question("   *  *  Call: to_debug:foo([3]) ? ")).
interaction(send(l)).
interaction(send('\n')).
interaction(boundaries("3{The debugger is switched off}","")).
interaction(boundaries("123","")).




 %% {The debugger will first leap -- showing spypoints and breakpoints (debug)}
 %% [1,2,3]{Spypoint placed on 'to_debug:foo'/1}
 %%          In /home/clip/Systems/ciao-1.9/tests/debugger/to_debug.pl (5-13) foo-2
 %%  + *  *  Call: to_debug:foo([1,2,3]) ? 
 %%          In /home/clip/Systems/ciao-1.9/tests/debugger/to_debug.pl (16-18) bar-1
 %%    *  *  Call: to_debug:bar(1) ? 
 %%          In /home/clip/Systems/ciao-1.9/tests/debugger/to_debug.pl (19-21) display-1
 %%    *  *  Call: display(1) ? 
 %% 1         In /home/clip/Systems/ciao-1.9/tests/debugger/to_debug.pl (19-21) display-1
 %%    *  *  Exit: display(1) ? 
 %%          In /home/clip/Systems/ciao-1.9/tests/debugger/to_debug.pl (16-18) bar-1
 %%    *  *  Exit: to_debug:bar(1) ? 
 %%          In /home/clip/Systems/ciao-1.9/tests/debugger/to_debug.pl (16-18) foo-2
 %%  + *  *  Call: to_debug:foo([2,3]) ? l
 %% 2         In /home/clip/Systems/ciao-1.9/tests/debugger/to_debug.pl (16-18) foo-2
 %%  + *  *  Call: to_debug:foo([3]) ? -
 %% {Spypoint removed from 'to_debug:foo'/1}
 %%          In /home/clip/Systems/ciao-1.9/tests/debugger/to_debug.pl (16-18) foo-2
 %%    *  *  Call: to_debug:foo([3]) ? l
 %% 3{The debugger is switched off}
 %% 123
