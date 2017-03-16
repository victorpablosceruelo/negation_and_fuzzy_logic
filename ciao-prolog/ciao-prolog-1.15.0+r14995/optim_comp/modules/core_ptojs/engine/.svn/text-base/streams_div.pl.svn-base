:- class(streams_div, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Streams for HTML div elements").
:- doc(author, "Jose F. Morales").

:- doc(bug, "rewrite as a stream (see engine(streams_basic) and
   library(streams))").

%:- class divdisplay {
    :- attr outdiv.

    :- export(cons__/1).
    cons__(Id) :- ~outdiv = ~js_dom.elem(Id).

    :- export(set/1).
    set(A) :- html <- A.

    :- include(engine(io_basic_common)).
    % TODO: merge with '$to_str'... use a stream?
    show_any(A) :- ~html += ~A.'$to_str'.
    clear :- html <- "".
    space :- ~html += " ".

    :- export(nl/0).
    nl :- ~html += "<br/>".

    :- export(hide/0).
    % Hide the div
    hide :- outdiv.style.display <- "none".
    :- export(show/0).
    % Show the div
    show :- outdiv.style.display <- "block".

    :- export(html/1).
    % note: This is not a constant, but an (impure) function, since it reads
    %       the contents of the dom (a mutable state).
    %       Is it possible to pass an abstraction to those kind of functions and call them later?
    %       Or is it better to define closures with an 'eval' method?
    html := ~outdiv.innerHTML. % note: this returns a mutable to the innerHTML!
%}.

