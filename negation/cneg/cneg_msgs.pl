
:- module(cneg_msgs,
	[
	    print_msg/5, 
	    get_trace_status_list/2,
	    generate_empty_trace/1, end_trace/1,
	    add_predicate_to_trace/3,
	    generate_traces_for_conjunction/3
	],
	[assertions]).

% :- use_module(library(aggregates),[setof/3]).
:- use_module(library(prolog_sys), [statistics/0]).
:- use_module(library(write), [write/1, write/2]).
:- use_module(library('cneg/cneg_basics')).

% To access pre-frontiers from anywhere.
%:- multifile cneg_pre_frontier/6.
%:- multifile call_to/3.
:- multifile file_debug_is_activated/1.

:- comment(title, "Auxiliary predicates for printing msgs.").
:- comment(author, "V@'{i}ctor Pablos Ceruelo").
:- comment(summary, "This module offers some predicates needed to print msgs.").

:- dynamic defined_stream_to_file/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_msg(FI_1, FI_2, Mode, Pre_Msg, Msg) :- 
	var(FI_1), var(FI_2),
	print_msg_aux(1, Mode, Pre_Msg, Msg).

print_msg(FI_1, FI_2, Mode, Pre_Msg, Msg) :- 
	var(FI_1), nonvar(FI_2),
	print_msg_aux(FI_2, Mode, Pre_Msg, Msg).

print_msg(FI_1, FI_2, Mode, Pre_Msg, Msg) :- 
	nonvar(FI_1), var(FI_2),
	print_msg_aux(FI_1, Mode, Pre_Msg, Msg).

print_msg(FI, FI, Mode, Pre_Msg, Msg) :- 
	nonvar(FI), 
	print_msg_aux(FI, Mode, Pre_Msg, Msg).

print_msg(FI_1, FI_2, Mode, Pre_Msg, Msg) :- 
	nonvar(FI_1), nonvar(FI_2),
	FI_1 \== FI_2,
	print_msg_aux(FI_1, Mode, Pre_Msg, Msg),
	print_msg_aux(FI_2, Mode, Pre_Msg, Msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_msg_aux(0, _Mode, _Pre_Msg, _Msg) :- !. % No debugging.

print_msg_aux(FI, 'nl', _Pre_Msg, _Msg) :- !, 
	print_msg_real(FI, '\n').

print_msg_aux(FI, 'logo', _Pre_Msg, _Msg) :- !, 
	logo(FI, Logo),
	print_msg_real(FI, Logo).

print_msg_aux(FI, 'aux', Pre_Msg, Msg) :- !, 
	print_msg_real(FI, Pre_Msg),
	print_msg_real(FI, Msg).

print_msg_aux(FI, 'statistics', Pre_Msg, Msg) :- !, 
	print_msg_aux(FI, '', Pre_Msg, Msg),
	print_msg_statistics(FI), !.

print_msg_aux(FI, 'separation', _Pre_Msg, _Msg) :- !, 
	print_msg_aux(FI, 'nl', '', ''),
	print_msg_aux(FI, '', '', '-----------------------------------------------'),
	print_msg_aux(FI, '', '', '-----------------------------------------------'),
	print_msg_aux(FI, 'nl', '', '').

print_msg_aux(FI, 'trace', Pre_Msg, Msg) :- !, 
	print_msg_trace(FI, Pre_Msg, Msg).

print_msg_aux(FI, 'list', Pre_Msg, Msg) :- !,
	print_msg_aux(FI, '', Pre_Msg, Msg).

print_msg_aux(FI, '', Pre_Msg, Msg) :- !,
	  print_msg_normal(FI, Pre_Msg, Msg),
	  print_msg_aux(FI, 'nl', '', '').

print_msg_aux(_FI, Mode, Pre_Msg, Msg) :-
	  print_msg_normal('error', 'Erroneous mode', Mode),
	  print_msg_normal('error', Pre_Msg, Msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logo(0, '') :- !.
logo(1, '') :- !.
logo(_Any, '% ').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_msg_normal(FI, Pre_Msg, Var) :- 
	var(Var), !,
	print_msg_normal_pre_msg(_FI, Pre_Msg), !,
	print_msg_normal_aux(FI, Var).

print_msg_normal(FI, Pre_Msg, []) :- !,
	print_msg_normal_pre_msg(_FI, Pre_Msg), !,
	print_msg_normal_aux(FI, '[]').

print_msg_normal(FI, Pre_Msg, [Msg | Msgs]) :- !,
	print_msg_normal_pre_msg(FI, Pre_Msg), !,
	print_msg_normal_aux(FI, Msg), !,
	print_msg_normal_aux(FI, ' \n'), !,
	print_msg_normal(FI, Pre_Msg, Msgs), !.

print_msg_normal(FI, Pre_Msg, Msg) :- !,
	print_msg_normal_pre_msg(FI, Pre_Msg), !,
	print_msg_normal_aux(FI, Msg), !.

print_msg_normal_pre_msg(_FI, Var) :- 
	var(Var), 
	logo(FI, Logo), !, 
	print_msg_normal_aux(FI, Logo), !,
	print_msg_normal_aux(FI, Var), !,
	print_msg_normal_aux(FI, ' :: '), !.

print_msg_normal_pre_msg(_FI, '') :- !.
print_msg_normal_pre_msg(FI, Pre_Msg) :- !,
	logo(FI, Logo), !, 
	print_msg_normal_aux(FI, Logo), !,
	print_msg_normal_aux(FI, Pre_Msg), !,
	print_msg_normal_aux(FI, ' :: '), !.

print_msg_normal_aux(FI, Var) :- 
	var(Var), !, 
	print_msg_real(FI, Var).
print_msg_normal_aux(_FI, '') :- !.
print_msg_normal_aux(_FI, []) :- !.
print_msg_normal_aux(FI, [Msg | Msgs]) :- !,
	print_msg_normal_aux(FI, Msg),
	print_msg_normal_aux(FI, ' '),
	print_msg_normal_aux(FI, Msgs).
print_msg_normal_aux(FI, Msg) :- !,
	print_msg_real(FI, Msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_msg_statistics(0) :- !. % No backtracking, please.
print_msg_statistics(1) :- !,
	nl, nl, statistics, nl, !.
print_msg_statistics(Any) :-
	current_output(StdOut_Stream), % Save stdout stream.
	get_stream_to_file(Any, Statistics_Stream),
	set_output(Statistics_Stream), % Redirect stdout to stream.
	nl, nl, statistics, nl, !, % Write statistics to file.
	set_output(StdOut_Stream), % Recover stdout stream.
	!. % No backtracking, please.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_msg_trace(Any, Pre_Msg, Trace) :-
	print_msg_aux(Any, 'separation', '', ''),
	print_msg_aux(Any, 'nl', '', ''),
	print_msg_aux(Any, '', Pre_Msg, 'TRACE: '),
	get_trace_status_list(Trace, Trace_Status_List),
	print_msg_aux(Any, 'list', '', Trace_Status_List),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Meaning of values for variable Echo_Level:
%
% 0 -> no debug.
% 1 -> std output + debugging.
% 2 -> only debugging.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_msg_real(0, _Msg) :- !.
print_msg_real(1, Msg) :- !,
	write(Msg).
print_msg_real(Any, Msg) :- 
	file_debug_is_activated(true), !,
	get_file_name(Any, File_Name),
	get_stream_to_file(File_Name, Stream),
	write(Stream, Msg).
print_msg_real(_Any, _Msg) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get_file_name(Any, File_Name),
get_file_name(2, 'debug_pkg_cneg_tr.pl') :- !.
get_file_name(3, 'debug_pkg_cneg_rt.pl') :- !.
get_file_name(4, 'debug_pkg_cneg_diseqs.pl') :- !.
get_file_name(5, 'debug_pkg_cneg_statistics.pl') :- !.
get_file_name(_Any, 'debug_pkg_cneg_errors.pl') :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_stream_to_file(File_Name_Atom, _Stream) :-
	var(File_Name_Atom), !, 
	print_msg(1, '', 'fail', 'get_stream_to_file can not work if File_Name is a variable', ''),
	!, fail.
get_stream_to_file(File_Name_Atom, Stream) :-
	defined_stream_to_file(File_Name_Atom, Stream), !.
get_stream_to_file(File_Name_Atom, Stream) :-
	name(File_Name_Atom, _File_Name_String),	% Convert string to atom.
%	open(FN_Out,write,Stream), % This empties the file !!!
	open(File_Name_Atom, append, Stream),
	assertz_fact(defined_stream_to_file(File_Name_Atom, Stream)), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_empty_trace(trace([], _Out)) :- !.
generate_traces_for_conjunction(trace(In, Out), trace(In, Aux), trace(Aux, Out)) :- !.
add_predicate_to_trace(Predicate, trace(In, Out), trace([Predicate | In], Out)) :- !.
end_trace(trace(In, In)) :- !.

% They should be converted to an string to avoid problems ...
%	name(Predicate, Predicate_String), !. 
get_trace_status_list(trace(In, Out), List_Reversed) :- !,
	(
	    (   var(Out), !, List = In   )  % Get current status.
	;
	    (   nonvar(Out), !, List = Out   )   % Get final status.
	),
	reverse_list(List, [], List_Reversed), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
