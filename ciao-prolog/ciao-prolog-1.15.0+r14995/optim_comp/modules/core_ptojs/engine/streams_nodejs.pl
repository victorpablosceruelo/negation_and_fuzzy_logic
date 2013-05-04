:- module(streams_nodejs, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Streams for NodeJS").
:- doc(author, "Jose F. Morales").

:- doc(bug, "rewrite as a stream (see engine(streams_basic) and
   library(streams))").

:- module process_stdout {
    :- export(cons__/0).
    cons__.

    :- include(engine(io_basic_common)).
    % TODO: merge with '$to_str'... define a stream?
    show_any(A) :- print(~A.'$to_str').

    :- export(nl/0).
    nl :- print("\n").

    % ----

    :- use_package(js_lang).

    :- pred print/1 :: t_string + (det, argsbox([unbox])).
    print(S) :- js_lang.stats([process.stdout.write.[S]]).
}.

% ---------------------------------------------------------------------------
% TODO: Find a way to mimick all the libraries offered by NodeJS
%       (are there IDL? or just the manual?)

:- use_package(js_lang).

% (DEBUG)
:- js_native([vardecl('util', require.['\'util\''])]).


