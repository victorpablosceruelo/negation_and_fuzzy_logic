:- module(bu_messages, [message/3, error/2, warning/2, note/2, debug/2], [assertions]).

:- use_module(library(format), [format/3]).
%:- use_module(library(ttyout)).
:- use_module(.(bu_options), [get/2]).

:- pred message(Level, String, Args) : 
	(integer(Level), string(String), list(Args, term)).

message_(Level, String, Post, Args) :-
	bu_options:get(verbose_level, VL), 
	(
	    Level > VL -> true
	;
	    format(user_error, String, Args),
	    format(user_error, "~s", Post)
	).

message(L, String, Args):-
	message_(L,  String, "", Args).

error(S,L) :-
	message_(0, "[ERROR: " ||  S, "]\n", L).

warning(S,L) :-
	message_(1, "[WARNING: " || S, "]\n", L).

note(S,L) :-
	message_(2, "[NOTE: " || S, "]\n", L).

debug(S,L) :-
	message_(10, "[DEBUG: " || S, "]\n", L).
