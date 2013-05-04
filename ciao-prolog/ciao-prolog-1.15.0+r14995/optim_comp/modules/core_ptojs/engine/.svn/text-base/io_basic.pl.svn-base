:- module(io_basic, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Basic IO").
:- doc(author, "Jose F. Morales").

:- doc(bug, "[Incomplete version of engine(io_basic) for pl2js]").

% TODO: for the browser, use console.log; add stream wrappers for divs, etc.

:- if(target_platform(v8)).

% TODO: 'shdisplay' is not automatically seen!
% Use V8
:- use_module(engine(streams_v8)).
:- export(display/1).
display(X) :- streams_v8.shdisplay.display(X).
:- export(nl/0).
nl :- streams_v8.shdisplay.nl.

:- elif(target_platform(nodejs)).

% Use NodeJS by default
% TODO: 'process_stdout' is not automatically seen!
:- use_module(engine(streams_nodejs)).
:- export(display/1).
display(X) :- streams_nodejs.process_stdout.display(X).
:- export(nl/0).
nl :- streams_nodejs.process_stdout.nl.

:- else.
% TODO: not implemented, use console.log?
:- export(display/1).
display(_).
:- export(nl/0).
nl.
:- endif.


