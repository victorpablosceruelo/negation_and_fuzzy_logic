:- module(error_res, [error_message/3], [assertions]).

%
%  error.pl			Nai-Wei Lin			December, 1991
% 
%  Updated for Ciao error reporting -- Manuel Hermenegildo
%
%  This file contains the procedures for reporting error messages.
%

%:- use_module(library(messages),[warning_message/2,note_message/2,error_message/2]).
:- use_module(library(write), [numbervars/3, portray_clause/2]).
:- use_module(library(format)).

%
%  Print out an error message.
%

:- doc(bug, "This should be unified with ciao messages.").

:- impl_defined(error_message/3).
:- meta_predicate error_message(?, ?, addmodule).

error_message(Code, Source, Clause, Module) :-
	error(Code, MType, IfSource, String),
%	message_type(MType,MessageType), 
	!,
	( %% Order of numbervars is relevant to make messages more readable
	    (
		var(Clause) ->
		N = Source
	    ;
		N = [Clause, Source]
	    ),
	    numbervars(N, 0, _),
	    (
		IfSource = y ->
		P = [Source]
	    ;
		P = []
	    ),
	    message_name_stream(MType, Name, Stream),
	    generic_message(Name, Stream, String, P, Clause, Module),
	    fail
	;
	    true
	).
error_message(Code, Source, Clause, Module) :-
	error_message_("unknown error ~w: ~w", [Code, Source], Clause,
	    Module).

error_message_(String, Args, Clause, Module) :-
	generic_message('ERROR', user_error, String, Args, Clause, Module).

generic_message(Type, Stream, String, Args, Clause, Module) :-
	display(Stream, '{'),
	display(Stream, Type),
	(
	    nonvar(Module) ->
	    display(Stream, ' ('),
	    display(Stream, Module),
	    display(Stream, ')')
	;
	    true
	),
	display(Stream, ':'),
	format(Stream, String, Args),
	(
	    nonvar(Clause) ->
	    nl(Stream),
	    portray_clause(Stream, Clause),
	    nl(Stream)
	;
	    true
	),
	display(Stream, '}\n').

%% Types of errors (w,n) should be revised MH

message_name_stream(w, 'WARNING', user_error).
message_name_stream(e, 'ERROR',   user_error).
message_name_stream(n, 'NOTE',    user).

% message_type(w,warning_message).
% message_type(n,note_message).

error(arg1,  w, y, "illegal argument position ~w").
error(comp1, w, y, "unknown complexity name: ~w").
error(lit1,  w, y, "cls_init_gran_system: unknown predicate: ~w").
% error(lit1,     w, y, "unknown predicate: ~w"       ).
error(bound1, n, y, "Dependency analysis: unbound input variable: ~w"). %JNL
% More details: finding the predecessors of an argument (dependency/build_adg.pl)
%error(bound1,   n, y, "unbound input variable: ~w"  ).
error(dec1, n, y, "complexity declaration is missed for predicate: ~w").
error(dec2, w, y, "illegal declaration: ~w").
error(dec3, n, y, "Analysis check: mode is not declared for predicate: ~w").
%JNL
%error(dec3,     n, y, "mode is not declared for predicate: ~w").
error(dec4, n, y,
	    "Analysis check: measure is not declared for predicate: ~w"). %JNL
%error(dec4,     n, y, "measure is not declared for predicate: ~w").
error(dec5,     n, y, "size is not declared for predicate: ~w").
error(dec6,     n, y, "det is not declared for predicate: ~w").
error(dec7,     n, y, "time is not declared for predicate: ~w").
error(dec8,     w, n, "illegal expressions in declaration").
error(dec9,     n, y, "mutex is not declared for predicate: ~w").
error(mode1,    n, n, "use list for mode declaration").
error(mode2,    w, n, "arity inconsistency in mode declaration").
error(mode3,    w, y, "illegal mode symbol: ~w").
error(measure1, n, n, "use list for measure declaration").
error(measure2, w, n, "arity inconsistency in measure declaration").
error(measure3, w, n, "illegal measure name: ~w").
error(mutex1,   n, n, "use list for mutex declaration").
error(mutex2,   n, n, "use list of integers for mutex declaration").
error(domain1,  n, n, "use list for domain declaration").
error(domain2,  w, n, "arity inconsistency in domain declaration").
error(domain3,  n, n, "use integer interval or list for domain declaration").
error(size1,    n, n, "use list for size declaration").
error(size2,    w, n, "arity inconsistency in size declaration").
error(det1,     n, n, "do not use list for det declaration").
error(time1,    n, n, "do not use list for time declaration").
error(nocost,   e, n, "no cost analysis performed").
error(nosize,   e, n, "no size analysis performed").

error(second_order1, w, y,
	    "Dependency analysis: illegal predicate arg ~w in findall predicate"
). %JNL

% More details : Insert information about a literal into the argument dependency
% graph and the ground variables list.  
%error(second_order1, w, y, "illegal predicate arg ~w in findall predicate").
