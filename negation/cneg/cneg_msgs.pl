
:- module(cneg_msgs,
	[
	    echo_msg/5, echo_msg_2pm/6, echo_msg_3pm/7,
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

:- comment(title, "Auxiliary predicates for printing msgs.").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module offers some predicates needed to print msgs.").


:- dynamic defined_stream_to_file/2.

% Meaning of values for variable Echo_Level:
%
% 0 -> no debug.
% 1 -> std output + debugging.
% 2 -> only debugging.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Comment out the following sentences to disable all debugging.
%echo_msg_aux(2, _File_Name, _Msg) :- !. % No debugging at all.
	
echo_msg_aux(0, _File_Name, _Msg) :- !.
echo_msg_aux(1, File_Name, Msg) :-
	echo_msg_aux(2, File_Name, Msg),
	write(Msg).
echo_msg_aux(2, File_Name, Msg) :-
	get_stream_to_file(File_Name, Stream_1),
	write(Stream_1, Msg),
	get_stream_to_file('with_all_debug_msgs', Stream_2),
	write(Stream_2, Msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_stream_to_file(File_Name_Atom, _Stream) :-
	var(File_Name_Atom), !, 
	echo_msg(1, '', 'fail', 'get_stream_to_file can not work if File_Name is a variable', ''),
	!, fail.
get_stream_to_file(File_Name_Atom, Stream) :-
	defined_stream_to_file(File_Name_Atom, Stream), !.
get_stream_to_file(File_Name_Atom, Stream) :-
	name(File_Name_Atom, File_Name_String_1), % Convert atom to string.
	append("debug_pkg_cneg_file_", File_Name_String_1, File_Name_String_2),
	append(File_Name_String_2, ".pl", File_Name_String_3),
	name(FN_Out, File_Name_String_3),	% Convert string to atom.
%	open(FN_Out,write,Stream), % This empties the file !!!
	open(FN_Out, append, Stream),
	assertz_fact(defined_stream_to_file(File_Name_Atom, Stream)), !,
	echo_howto_information(2, File_Name_Atom).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% echo_msg(Echo_Level, Mode, File_Name, Pre_Msg, Msg).
echo_msg(0, _Mode, _File_Name, _Pre_Msg, _Msg) :- !. % No debugging.

echo_msg(Echo_Level, 'aux', File_Name, Pre_Msg, Msg) :- !,
	echo_msg_aux(Echo_Level, File_Name, Pre_Msg),
	echo_msg_aux(Echo_Level, File_Name, Msg).

echo_msg(Echo_Level, 'list', File_Name, Pre_Msg, Msg) :-
	(   (echo_msg_list(Echo_Level, File_Name, 1, Pre_Msg, Msg), !)
	;
	    (echo_msg(Echo_Level, 'normal', File_Name, Pre_Msg, Msg), !)
	).

echo_msg(1, 'logo', File_Name, Pre_Msg, Msg) :- !, % No logo to stdout.
	echo_msg(2, 'logo', File_Name, Pre_Msg, Msg).
echo_msg(Echo_Level, 'logo', File_Name, _Pre_Msg, _Msg) :-
	echo_msg_aux(Echo_Level, File_Name, '% '), 
	echo_msg_aux(Echo_Level, File_Name, File_Name), 
	echo_msg_aux(Echo_Level, File_Name, ' :: '), 
	!.

echo_msg(Echo_Level, 'statistics', File_Name, _Pre_Msg, Msg) :-
	echo_msg_statistics(Echo_Level, File_Name, Msg), !.

echo_msg(Echo_Level, 'nl', File_Name, _Pre_Msg, _Msg) :-
	echo_msg_aux(Echo_Level, File_Name, '\n'), !.

echo_msg(Echo_Level, 'separation', File_Name, _Pre_Msg, _Msg) :-
	echo_msg_separation(Echo_Level, File_Name).	

echo_msg(Echo_Level, 'trace', File_Name, Pre_Msg, Msg) :-
	echo_msg_trace(Echo_Level, File_Name, Pre_Msg, Msg).

echo_msg(Echo_Level, Mode, File_Name, Pre_Msg, Msg) :-
	  echo_msg_3pm(Echo_Level, Mode, File_Name, Pre_Msg, '', '', Msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_msg_2pm(Echo_Level, Mode, File_Name, Pre_Msg_1, Pre_Msg_2, Msg) :-
	echo_msg_3pm(Echo_Level, Mode, File_Name, Pre_Msg_1, Pre_Msg_2, '', Msg).

echo_msg_3pm(Echo_Level, Mode, File_Name, Pre_Msg_1, Pre_Msg_2, Pre_Msg_3, Msg) :-
	(Mode \== 'aux'), (Mode \== 'list'), (Mode \== 'logo'), 
	 (Mode \== 'statistics'), (Mode \== 'nl'), (Mode \== 'separation'), !,
	echo_msg(Echo_Level, 'logo', File_Name, Pre_Msg_1, Msg),
	echo_msg_aux(Echo_Level, File_Name, Pre_Msg_1),
	echo_msg_aux(Echo_Level, File_Name, Pre_Msg_2),
	echo_msg_aux(Echo_Level, File_Name, Pre_Msg_3),
	echo_msg_aux(Echo_Level, File_Name, ' :: '),
	echo_msg_aux(Echo_Level, File_Name, Msg),
	echo_msg(Echo_Level, 'nl', File_Name, '', ''), 
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%echo_msg_is_a_list([]).
%echo_msg_is_a_list([_Elto|_List]).

echo_msg_list(Echo_Level, File_Name, Index, Pre_Msg, []) :-
	echo_msg_list_aux(Echo_Level, File_Name, Index, Pre_Msg, []),
	!. % No backtracking allowed.
echo_msg_list(Echo_Level, File_Name, Index, Pre_Msg, [Msg|Msgs]) :- 
	!, % No backtracking allowed.
	echo_msg_list_aux(Echo_Level, File_Name, Index, Pre_Msg, Msg),
	NewIndex is Index +1,
	echo_msg_list(Echo_Level, File_Name, NewIndex, Pre_Msg, Msgs).

echo_msg_list_aux(Echo_Level, File_Name, Index, Pre_Msg, Msg) :-
	echo_msg(Echo_Level, 'logo', File_Name, Pre_Msg, Msg),
	echo_msg_aux(Echo_Level, File_Name, Pre_Msg),
	echo_msg_aux(Echo_Level, File_Name, ' #'),
	echo_msg_aux(Echo_Level, File_Name, Index),
%	echo_msg_aux(Echo_Level, File_Name, ')'),
	echo_msg_aux(Echo_Level, File_Name, ' :: '),
	echo_msg_aux(Echo_Level, File_Name, Msg),
	echo_msg(Echo_Level, 'nl', File_Name, '', '').
			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%echo_msg_nl(0, _File_Name) :- !.
%echo_msg_nl(1, File_Name) :-
%	echo_msg_nl(2, File_Name),
%	nl.
%echo_msg_nl(2, File_Name) :-
%	get_stream_to_file(File_Name, Stream_1),
%	nl(Stream_1),
%	get_stream_to_file('with_all_debug_msgs', Stream_2),
%	nl(Stream_2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_msg_separation(Echo_Level, File_Name) :-
	echo_msg(Echo_Level, 'nl', File_Name, '', ''), 
	echo_msg_separation_aux(Echo_Level, File_Name), 
	echo_msg_separation_aux(Echo_Level, File_Name),
	echo_msg(Echo_Level, 'nl', File_Name, '', '').

echo_msg_separation_aux(Echo_Level, File_Name) :-
	echo_msg(Echo_Level, 'logo', File_Name, '', ''), 
	echo_msg_aux(Echo_Level, File_Name, '-----------------------------------------------'),
	echo_msg_aux(Echo_Level, File_Name, '-----------------------------------------------'),
	echo_msg(Echo_Level, 'nl', File_Name, '', '').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_msg_statistics(0, _File_Name, _Msg) :- !. % No backtracking, please.
echo_msg_statistics(1, File_Name, Msg) :-
	echo_msg_statistics_aux(Msg),
	echo_msg_statistics(2, File_Name, Msg),
	!. % No backtracking, please.
echo_msg_statistics(2, File_Name, Msg) :-
	echo_msg_statistics_file(2, File_Name, Msg),
	echo_msg_statistics_file(2, 'statistics', Msg).

echo_msg_statistics_file(2, File_Name, Msg) :-
	current_output(StdOut_Stream), % Save stdout stream.
	get_stream_to_file(File_Name, Statistics_Stream),
	set_output(Statistics_Stream), % Redirect stdout to stream.
	echo_msg_statistics_aux(Msg),
	set_output(StdOut_Stream), % Recover stdout stream.
	!. % No backtracking, please.

echo_msg_statistics_aux(Msg) :-
	nl, write(Msg), nl, nl, 
	statistics, !. % Write statistics to file.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_msg_trace(Echo_Level, File_Name, Pre_Msg, Trace) :-
	echo_msg(Echo_Level, 'separation', File_Name, '', ''),
	echo_msg(Echo_Level, 'nl', File_Name, '', ''),
	get_trace_status_list(Trace, Trace_Status_List),
	echo_msg(Echo_Level, '', File_Name, Pre_Msg, 'TRACE: '),
	echo_msg(Echo_Level, 'list', File_Name, '', Trace_Status_List),
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_howto_information(Echo_Level, File_Name) :-
%	echo_msg_separation(Echo_Level, File_Name),
	echo_msg(Echo_Level, 'nl', File_Name, '', ''),
	echo_msg(Echo_Level, '', File_Name, 'Info for debugging', ' '),
	echo_msg(Echo_Level, 'nl', File_Name, '', ''),
	echo_msg(Echo_Level, '', File_Name, 'Basic frontier', 'frontier(Goal, Head, Body, FrontierTest)'),
	echo_msg(Echo_Level, '', File_Name, 'frontier_E_IE_NIE', 'frontier_E_IE_NIE(E, IE, NIE)'),
	echo_msg(Echo_Level, '', File_Name, 'frontier_E_IE_NIE_ied', 'frontier_E_IE_NIE_ied(E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb)'),
	echo_msg(Echo_Level, '', File_Name, 'Vars_Info', 'vars_info(GoalVars, UQV, ImpVars, ExpVars, RelVars, UQ_to_EQ_Vars, Dumb_Vars)'),
%	echo_msg_separation(Echo_Level, File_Name),
	echo_msg(Echo_Level, 'nl', File_Name, '', '').

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
