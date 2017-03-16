:- module(io_aux, [
        message/2, 
        error/1, warning/1, note/1, message/1, debug/1,
        inform_user/1, display_string/1, display_list/1, display_term/1],
        [pure, assertions]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(io_basic)).
:- use_module(engine(streams_basic)).
:- use_module(engine(system_info)).

:- doc(title,"Message printing primitives").

:- doc(author,"Daniel Cabeza").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module,"This module provides predicates for printing in a
   unified way informational messages, and also for printing some terms
   in a specific way.").


:- use_module(engine(internals), ['$quiet_flag'/2]).

:- import(write, [write/1, writeq/1]).

:- doc(message(Type, Message), "Output to standard error
   @var{Message}, which is of type @var{Type}. The @tt{quiet}
   @index{prolog flag} (see @ref{Changing system behaviour and various
   flags}) controls which messages are actually output, depending on its
   type. Also, for @tt{error}, @tt{warning} and @tt{note} messages, a
   prefix is output which denotes the severity of the message.
   @var{Message} is an item or a list of items from this list:
@begin{description}

@item{@tt{$$(String)}} @tt{String} is a string, which is output with
   @pred{display_string/1}.

@item{@tt{''(Term)}} @tt{Term} is output quoted.  If the module
   @lib{write} is loaded, the term is output with @pred{writeq/1}, else
   with @pred{displayq/1}.

@item{@tt{~~(Term)}} @tt{Term} is output unquoted.  If the module
   @lib{write} is loaded, the term is output with @pred{write/1}, else
   with @pred{display/1}.

@item{@tt{[](Term)}} @tt{Term} is recursively output as a message, can
   be an item or a list of items from this list.

@item{@tt{Term}} Any other term is output with @pred{display/1}.
@end{description} ").

:- true pred message(Type, Message)
        : (atm(Type), member(Type, [error,warning,note,message,debug])).

% Auxiliary IO predicates:

:- doc(error/1, "Defined as @includedef{error/1}.").
:- doc(warning/1, "Defined as @includedef{warning/1}.").
:- doc(note/1, "Defined as @includedef{note/1}.").
:- doc(message/1, "Defined as @includedef{message/1}.").
:- doc(debug/1, "Defined as @includedef{debug/1}.").

error(Message)   :- message(error, Message).
warning(Message) :- message(warning, Message).
note(Message)    :- message(note, Message).
message(Message) :- message(message, Message).
debug(Message)   :- message(debug, Message).

message(Type, Message) :-
        '$quiet_flag'(Q, Q),
        allowed_message_type(Q,Type), !,
        add_head(Type, Message, MessL),
        current_output(S),
        set_output(user_error),
        output_message(MessL), nl,
        set_output(S).
message(_,_).

:- export(message_nonl/2).
message_nonl(Type, Message) :-
        '$quiet_flag'(Q, Q),
        allowed_message_type(Q,Type), !,
        add_head(Type, Message, MessL),
        current_output(S),
        set_output(user_error),
        output_message(MessL),
        set_output(S).
message_nonl(_,_).

:- export(output_message/1).
output_message([M|Ms]) :- !,
        output_item(M),
        output_message(Ms).
output_message([]) :- !.
output_message(M) :-
        output_item(M).

output_item(V) :- var(V), !, display(V).
output_item($$(M)) :- !, display_string(M).
output_item(''(M)) :- !, (current_module(write) -> writeq(M); displayq(M)).
output_item(~~(M)) :- !, (current_module(write) -> write(M); display(M)).
output_item([](M)) :- !, output_message(M).
output_item(M) :- display(M).

:- export(allowed_message_type/2).
allowed_message_type(error, error) :- !.
allowed_message_type(warning, error) :- !.
allowed_message_type(warning, warning) :- !.
allowed_message_type(off, error) :- !.
allowed_message_type(off, warning) :- !.
allowed_message_type(off, note) :- !.
allowed_message_type(off, message) :- !.
allowed_message_type(debug,_).

add_head(message, Mess, Mess) :- !.
add_head(debug, Mess, Mess) :- !.
add_head(Type, Mess, NewMess) :-
        label(Type, Label),
        NewMess = [Label|Mess].

label(error, 'ERROR: ').
label(warning, 'WARNING: ').
label(note, 'Note: ').

:- doc(inform_user(Message), "Similar to @pred{message/1}, but
   @var{Message} is output with @pred{display_list/1}.  This predicate
   is obsolete, and may disappear in future versions.").

inform_user(MessL) :-
        '$quiet_flag'(Q, Q),
        allowed_message_type(Q,message), !,
        current_output(S),
        set_output(user_error),
        display_list(MessL), nl,
        set_output(S).
inform_user(_).

:- doc(display_list(List), "Outputs @var{List}.  If @var{List} is a
   list, do @pred{display/1} on each of its elements, else do
   @pred{display/1} on @var{List}.").

display_list([M|Ms]) :- !,
        display(M),
        display_list(Ms).
display_list([]) :- !.
display_list(M) :-
        display(M).

:- doc(display_term(Term), "Output @var{Term} in a way that a
   @pred{read/1} will be able to read it back, even if operators
   change.").

display_term(T) :- displayq(T), display(' .\n').

:- doc(display_string(String), "Output @var{String} as the sequence
   of characters it represents.").

:- true pred display_string(String) : string.

display_string([]).
display_string([C|Cs]) :- put_code(C), display_string(Cs).

:- doc(bug, "@pred{message/2} assumes that a module with name 'write'
   is library(write).").
