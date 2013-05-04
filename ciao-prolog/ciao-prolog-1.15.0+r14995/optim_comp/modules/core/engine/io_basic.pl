:- module(io_basic, [], [pure, assertions, isomodes]).

:- use_module(engine(basic_props)).
:- use_module(engine(basiccontrol)).
:- use_module(engine(streams_basic)).

:- doc(title, "Basic input/output").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Mats Carlsson").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module, "This module provides predicates for character
   input/output and for canonical term output.  From the
   @concept{ISO-Prolog} predicates for character input/output, only the
   @tt{_code} versions are provided, the rest are given by
   @lib{library(iso_byte_char)}, using these.  Most predicates are
   provided in two versions: one that specifies the input or output
   stream as the first argument and a second which omits this argument
   and uses the current input or output stream.").

:- '$native_include_c_source'(.(io_basic)).

% ---------------------------------------------------------------------------
:- export(get_code/2).
:- doc(get_code(Stream, Code), "Reads from @var{Stream} the next
   character and unifies @var{Code} with its character code.  At end of
   stream, unifies @var{Code} with the integer -1.").

:- true pred get_code(+stream, ?int) + (iso, native).
:- '$props'(get_code/2, [impnat=cbool(get2)]).

% ---------------------------------------------------------------------------
:- export(get_code/1).
:- doc(get_code(Code), "Behaves like @tt{current_input(S),
   get_code(S,Code)}.").

:- true pred get_code(?int) + (iso, native).

:- '$props'(get_code/1, [impnat=cbool(get)]).

% ---------------------------------------------------------------------------
:- export(get1_code/2).
:- doc(get1_code(Stream, Code), "Reads from @var{Stream} the next
   non-layout character (see @pred{code_class/2}) and unifies @var{Code}
   with its character code.  At end of stream, unifies @var{Code} with
   the integer -1.").

:- true pred get1_code(+stream, ?int) + native.

:- '$props'(get1_code/2, [impnat=cbool(get12)]).

% ---------------------------------------------------------------------------
:- export(get1_code/1).
:- doc(get1_code(Code), "Behaves like @tt{current_input(S),
   get1_code(S,Code)}.").

:- true pred get1_code(?int) + native.

:- '$props'(get1_code/1, [impnat=cbool(get1)]).

% ---------------------------------------------------------------------------
:- export(peek_code/2).
:- doc(peek_code(Stream, Code), "Unifies @var{Code} with the
   character code of the next character of @var{Stream}, leaving the
   stream position unaltered.  At end of stream, unifies @var{Code} with
   the integer -1.").

:- true pred peek_code(+stream, ?int) + iso.

:- '$props'(peek_code/2, [impnat=cbool(peek2)]).

% ---------------------------------------------------------------------------
:- export(peek_code/1).
:- doc(peek_code(Code), "Behaves like @tt{current_input(S),
   peek_code(S,Code)}.").

:- true pred peek_code(?int) + iso.

:- '$props'(peek_code/1, [impnat=cbool(peek)]).

% ---------------------------------------------------------------------------
:- export(skip_code/2).
:- doc(skip_code(Stream, Code), "Skips just past the next character
   code @var{Code} from @var{Stream}.").

:- true pred skip_code(+stream, +int).

:- '$props'(skip_code/2, [impnat=cbool(skip2)]).

% ---------------------------------------------------------------------------
:- export(skip_code/1).
:- doc(skip_code(Code), "Behaves like @tt{current_input(S),
   skip_code(S,Code)}.").

:- true pred skip_code(+int).

:- '$props'(skip_code/1, [impnat=cbool(skip)]).

% ---------------------------------------------------------------------------
:- export(skip_line/1).
:- doc(skip_line(Stream), "Skips from @var{Stream} the remaining
   input characters on the current line.  If the end of the stream is
   reached, the stream will stay at its end.  Portable among different
   operating systems.").

:- trust pred skip_line(+stream).

:- '$props'(skip_line/1, [impnat=cbool(skip_line1)]).

% ---------------------------------------------------------------------------
:- export(skip_line/0).
:- doc(skip_line, "Behaves like @tt{current_input(S), skip_line(S)}.").

:- trust pred skip_line.

:- '$props'(skip_line/0, [impnat=cbool(skip_line)]).

% ---------------------------------------------------------------------------
:- export(put_code/2).
:- doc(put_code(Stream, Code), "Outputs to @var{Stream} the
   character corresponding to character code @var{Code}.").

:- true pred put_code(+stream, +int) + (iso, native).

:- '$props'(put_code/2, [impnat=cbool(put2)]).

% ---------------------------------------------------------------------------
:- export(put_code/1).
:- doc(put_code(Code), "Behaves like @tt{current_output(S),
   put_code(S,Code)}.").

:- true pred put_code(+int) + (iso, native).

:- '$props'(put_code/1, [impnat=cbool(put)]).

% ---------------------------------------------------------------------------
:- export(nl/1).
:- doc(nl(Stream), "Outputs a newline character to @var{Stream}.
   Equivalent to @tt{put_code(Stream, 0'\\n)}.").

:- true pred nl(+stream) + (iso, native).

:- '$props'(nl/1, [impnat=cbool(nl1)]).

% ---------------------------------------------------------------------------
:- export(nl/0).
:- doc(nl, "Behaves like @tt{current_output(S), nl(S)}.").

:- true pred nl + (iso, native).

:- '$props'(nl/0, [impnat=cbool(nl)]).

% ---------------------------------------------------------------------------
:- export(tab/2).
:- doc(tab(Stream,Num), "Outputs @var{Num} spaces to @var{Stream}.").

:- true pred tab(+stream,+int) + native.

:- '$props'(tab/2, [impnat=cbool(tab2)]).

% ---------------------------------------------------------------------------
:- export(tab/1).
:- doc(tab(Num), "Behaves like @tt{current_output(S), tab(S,Num)}.").

:- true pred tab(+int) + native.

:- '$props'(tab/1, [impnat=cbool(tab)]).

% ---------------------------------------------------------------------------
:- export(code_class/2).
:- doc(code_class(Code,Class), "Unifies @var{Class} with an integer
   corresponding to the lexical class of the character whose code is
   @var{Code}, with the following correspondence:
   @begin{verbatim}
    0 - layout (includes space, newline, tab)
    1 - small letter
    2 - capital letter (including '_')
    3 - digit
    4 - graphic (includes #$&*+-./:<=>?@@^\\`~ )
    5 - punctuation (includes !;""'%(),[]@{|@} )
   @end{verbatim}
   Note that in @concept{ISO-Prolog} the back quote @tt{`} is a punctuation
   character, whereas in Ciao it is a graphic character.  Thus, if
   compatibility with @concept{ISO-Prolog} is desired, the programmer should
   not use this character in unquoted names.").

:- true pred code_class(+int,?int).

:- '$props'(code_class/2, [impnat=cbool(code_class)]).

% ---------------------------------------------------------------------------
:- export(getct/2).
:- doc(getct(Code, Type), "Reads from the current input stream the
   next character, unifying @var{Code} with its character code, and
   @var{Type} with its lexical class.  At end of stream, unifies both
   @var{Code} and @var{Type} with the integer -1.  Equivalent to
   @begin{verbatim}
   get(Code), (Code = -1 -> Type = -1 ; code_class(Code,Type))
   @end{verbatim}").

:- true pred getct(?int, ?int).

:- '$props'(getct/2, [impnat=cbool(getct)]).

% ---------------------------------------------------------------------------
:- export(getct1/2).
:- doc(getct1(Code, Type), "Reads from the current input stream the
   next non-layout character, unifying @var{Code} with its character
   code, and @var{Type} with its lexical class (which will be nonzero).
   At end of stream, unifies both @var{Code} and @var{Type} with the
   integer -1.  Equivalent to
   @begin{verbatim}
   get1(Code), (Code = -1 -> Type = -1 ; code_class(Code,Type))
   @end{verbatim}").

:- true pred getct1(?int, ?int).

:- '$props'(getct1/2, [impnat=cbool(getct1)]).

% ---------------------------------------------------------------------------
:- export(display/2).
:- doc(display(Stream, Term), "Displays @var{Term} onto
   @var{Stream}.  Lists are output using list notation, the other
   compound terms are output in functional notation.  Similar to
   @tt{write_term(Stream, Term, [ignore_ops(ops)])}, except that 
   curly bracketed notation is not used with @tt{@{@}/1}, and
   the @tt{write_strings} flag is not honored.").

:- true pred display(+stream,@term) + native.

:- '$props'(display/2, [impnat=cbool(prolog_display2)]).

% ---------------------------------------------------------------------------
:- export(display/1).
:- doc(display(Term), "Behaves like @tt{current_output(S),
   display(S,Term)}.").

:- true pred display(@term) + native.

:- '$props'(display/1, [impnat=cbool(prolog_display)]).

% ---------------------------------------------------------------------------
:- export(displayq/2).
:- doc(displayq(Stream, Term), "Similar to @tt{display(Stream, Term)},
   but atoms and functors that can't be read back by @pred{read_term/3}
   are quoted.  Thus, similar to
   @tt{write_term(Stream, Term, [quoted(true), ignore_ops(ops)])}, with the
   same exceptions as @pred{display/2}.").

:- true pred displayq(+stream,@term).

:- '$props'(displayq/2, [impnat=cbool(prolog_displayq2)]).

% ---------------------------------------------------------------------------
:- export(displayq/1).
:- doc(displayq(Term), "Behaves like @tt{current_output(S),
   displayq(S,Term)}.").

:- true pred displayq(@term).

:- '$props'(displayq/1, [impnat=cbool(prolog_displayq)]).

% ---------------------------------------------------------------------------
:- export('$format_print_float'/3). % internal predicate
:- '$props'('$format_print_float'/3, [impnat=cbool(prolog_format_print_float)]).

% ---------------------------------------------------------------------------
:- export('$format_print_integer'/3). % internal predicate
:- '$props'('$format_print_integer'/3, [impnat=cbool(prolog_format_print_integer)]).

% ---------------------------------------------------------------------------
:- export('$display_string'/1). % internal predicate
:- '$props'('$display_string'/1, [impnat=cbool(prolog_display_string)]).

:- export('$copy_stdout'/1). % internal predicate
:- '$props'('$copy_stdout'/1, [impnat=cbool(prolog_copy_stdout)]).
