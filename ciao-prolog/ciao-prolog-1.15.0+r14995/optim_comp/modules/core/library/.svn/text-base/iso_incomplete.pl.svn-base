:- module(iso_incomplete, [close/2, stream_property/2], [assertions]).

:- doc(title, "Incomplete ISO Prolog predicates").

:- doc(author, "The CLIP Group").

:- doc(module, "This module implements some ISO Prolog predicates,
   but that are not complete yet.").

open(F, M, S, _) :- open(F, M, S).
close(S, _) :- close(S).

stream_property(S, P) :- % It is not complete
        current_stream(File, Mode, S),
        ( P = file_name(File)
        ; P = mode(Mode)
        ; Mode = read ->
            P = input
        ; P = output
        ).

% at_end_of_stream :- not_yet_implemented.
% at_end_of_stream(_) :- not_yet_implemented.
% 
% set_stream_position(_,_) :- not_yet_implemented.
% 
% char_conversion(_,_) :- not_yet_implemented.
% current_char_conversion(_,_) :- not_yet_implemented.
