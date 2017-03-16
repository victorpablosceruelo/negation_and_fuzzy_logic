:- module(_, [], [compiler(complang)]).

:- doc(title, "Common and auxiliary code for writting C-like code from AST").
:- doc(author, "Jose F. Morales").

:- doc(module, "This module provides common code for writting C-like
	programs, that is shared by different backends.").

:- use_module(library(lists), [append/3]).

% ===========================================================================

:- public escape_codes/2.
% Obtain the escaped representation of the input string
% Note: the treatment of control characters is incomplete
escape_codes([], []).
escape_codes([X|Xs], [0'\\, Y|Ys]) :- needs_escape(X, Y), !, escape_codes(Xs, Ys).
escape_codes([X|Xs], [X|Ys]) :- escape_codes(Xs, Ys).

needs_escape(0'\\, 0'\\).
needs_escape(0'", 0'").
needs_escape(0'\n, 0'n).

% ===========================================================================

% :- doc(encoding_mode/1, "Encoding for encode_symbol_a/3").
% :- regtype encoding_mode/1 :=
%       enc_c_id  # "Encoding as C identifiers"
%     | enc_js_id # "Encoding as JavaScript identifiers (like C, but $ is allowed)"

% TODO: encoding is not efficient, see Base64 

:- public encode_symbol_a/3.
% :- fun encode_symbol_a/3 :: term * encoding_mode -> atom
% # "Like @pred{encode_symbol/3}, but returns an atom".
encode_symbol_a(X, Encoding) := Y :-
	Codes = ~encode_symbol(X, Encoding),
	atom_codes(Y, Codes).

:- public encode_symbol/3.
% :- fun encode_symbol/3 :: term * encoding_mode -> string
% # "Encodes an atom, number or functor name as a representable
%    identifier in the specified encoding."
encode_symbol(X, Encoding) := Y :-
	encoding :: any <- Encoding,
	Y = ~encode_symbol_(X).
{
:- fluid encoding :: any. % Encoding mode

encode_symbol_(':'(A,B)) := ~append(~encode_symbol_(A), [~escapechar|(~encode_symbol_(B))]) :- !. % JS-backend
encode_symbol_(A/B) := ~append(~encode_symbol_(A), [~escapechar|(~encode_number_codes(B))]) :- !.
encode_symbol_(A) := ~encode_symbol_codes(A) :- !.

encode_symbol_codes(X) := ~encode_symbol_codes_2(~atom_codes(X)) :- atom(X), !.
encode_symbol_codes(X) := [~escapechar, "n"|(~encode_number_codes(X))] :- number(X), !.

encode_number_codes(X) := ~encode_number_codes_2(~number_codes(X)).

encode_number_codes_2([]) := [] :- !.
encode_number_codes_2([0'. |Xs]) := [0'd|~encode_number_codes_2(Xs)] :- !.
encode_number_codes_2([0'- |Xs]) := [0'm|~encode_number_codes_2(Xs)] :- !.
encode_number_codes_2([X|Xs]) := [X|~encode_number_codes_2(Xs)] :- !.

encode_symbol_codes_2([]) := [].
encode_symbol_codes_2([X|Xs]) := ~append(Ys, ~encode_symbol_codes_2(Xs)) :-
	Ys = ( X = ~escapechar ? [X,X] 
	     | printable(X) ? [X]
	     | [~escapechar | ~number_codes(X, 16)]
	     ).

% The character can be part of an indentifier
printable(X) :- 0'a =< X, X =< 0'z, !.
printable(X) :- 0'A =< X, X =< 0'Z, !.
printable(X) :- 0'0 =< X, X =< 0'9, !.
printable(X) :- X = 0'_, !.

escapechar := 0'_ :- ~encoding = enc_c_id, !.
escapechar := 0'$ :- ~encoding = enc_js_id.
}.

% ===========================================================================

:- use_module(library(strings)).

% A token writer with layout options
:- public class token_writer {
    :- attr tab_size :: m_int.
    :- attr in_macro :: m_any.
    :- attr pending_sep :: m_any.

    :- constructor new_/0.
    new_ :-
        ~tab_size = 0,
        ~in_macro = no,
        ~pending_sep = tab.

    :- public inner/0.
    inner :-
        tab_size.inc(2).
    :- public outer/0.
    outer :-
        tab_size.dec(2).
    :- public newline/0.
    newline :-
        ( ~in_macro = no -> true ; display(' \\') ),
        nl,
        pending_sep <- tab.
    :- public begmacro/0.
    begmacro :-
        in_macro <- yes.
    :- public endmacro/0.
    endmacro :-
        in_macro <- no.
    :- public glue/0.
    glue :-
        pending_sep <- glue.

    :- public flush_pending/0.
    flush_pending :-
        PendingSep = ~pending_sep,
        ( PendingSep = tab ->
            tabs(~tab_size),
            pending_sep <- no
        ; PendingSep = glue ->
            pending_sep <- no
        ; display(' ')
        ).

    :- public token/1.
    token(X) :-
        flush_pending,
        ( (X = [_|_] ; X = []) ->
            display('"'),
            escape_codes(X, X2),
            write_string(X2),
            display('"')
        ; X = q(Class, Name) -> % extended C grammar: qualified names.
            display(Class),
            display('__'),
            display(Name)
        ; X = p(Prefix, Name) -> % extended C grammar: prefixed names.
            display(Prefix),
            display(Name)
        ; X = type(Class) -> % extended C grammar: type names.
            display(Class),
            display('__'),
            display(type)
        ; X = inline(X1) -> % extended C grammar: a string with C code
            write_string(X1)
        ; X = msg(X1) -> % any term
            output_message(X1)
        ; display(X)
        ).
}.

%:- public tabs/1.
% tabs(N) prints a tab of size N (using spaces)
tabs(I) :- I =< 0, !.
tabs(I0) :- I0 > 0,
	display(' '), I is I0 - 1, tabs(I).

% ---------------------------------------------------------------------------

