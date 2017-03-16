:- module(streams_v8, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Streams for V8 shell").
:- doc(author, "Jose F. Morales").

:- doc(bug, "rewrite as a stream (see engine(streams_basic) and
   library(streams))").

:- export(display/1).
display(X) :- shdisplay.display(X).
:- export(nl/0).
nl :- shdisplay.nl.

:- module shdisplay {
    % OS tools in V8 are very limited
    % (it could be improved with foreign C libraries)
    :- attr line. % TODO: remove

    :- export(cons__/0).
    cons__. % TODO: this should not be necessary

    :- include(engine(io_basic_common)).
    % TODO: merge with '$to_str'... use a stream?
    show_any(A) :- print(~A.'$to_str').

    % ----

    :- use_package(js_lang).

    :- js_native([vardecl('buffer', '\'\'')]).

    :- pred print/1 :: t_string + (det, argsbox([unbox])).
    print(S) :- js_lang.stats(['+='('buffer', S)]).

    :- export(nl/0).
    :- pred nl/0 + det.
    nl :- js_lang.stats([
      print.['buffer'],
      'buffer' <- '\'\''
    ]).
}.

