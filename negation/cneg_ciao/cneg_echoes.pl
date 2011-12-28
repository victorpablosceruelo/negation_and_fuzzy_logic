:- module(cneg_echoes,
	[
	    echo_msg/4, echo_msg_aux/3, echo_msg_nl/2, 
	    echo_msg_logo/2, echo_statistics/3,
	    echo_separation/2, echo_msg_for_call/5
	],
	[assertions]).

:- use_module(library(prolog_sys), [statistics/0]).
:- use_module(library(write), [write/1, write/2]).

:- comment(title, "Debug predicates for Constructive Negation").

:- comment(author, "V@'{i}ctor Pablos Ceruelo").

:- comment(summary, "This module offers debug predicates used by Constructive Negation
	Transformation Program, by Constructive Negation Library and by Disequalities Management.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic defined_stream_to_file/2.

% Use the following sentences to enable/disable debugging.

% 0 -> no debug.
% 1 -> std output + debugging.
% 2 -> only debugging.

%%% Debug (by VPC).
get_stream_to_file(File_Name_Atom, Stream) :-
	defined_stream_to_file(File_Name_Atom, Stream), !.
get_stream_to_file(File_Name_Atom, Stream) :-
	name(File_Name_Atom, File_Name_String_1), % Convert atom to string.
	append("debug_pkg_cneg", File_Name_String_1, File_Name_String_2),
	append(File_Name_String_2, ".pl", File_Name_String_3),
	name(FN_Out, File_Name_String_3),	% Convert string to atom.
%	open(FN_Out,write,Stream), % This empties the file !!!
	open(FN_Out, append, Stream),
	assertz_fact(defined_stream_to_file(File_Name_Atom, Stream)),
	echo_howto_information(2, File_Name_Atom).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_msg(0, _File_Name, _Pre_Msg, _Msg) :- !. % No debugging.
echo_msg(Echo_Level, File_Name, Pre_Msg, Msg) :-
	echo_msg_is_a_list(Msg), !,
	echo_msg_list(Echo_Level, File_Name, Pre_Msg, Msg).
echo_msg(Echo_Level, File_Name, Pre_Msg, Msg) :-
	echo_msg_logo(Echo_Level, File_Name),
	echo_msg_aux(Echo_Level, File_Name, Pre_Msg),
	echo_msg_aux(Echo_Level, File_Name, ' :: '),
	echo_msg_aux(Echo_Level, File_Name, Msg),
	echo_msg_nl(Echo_Level, File_Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_msg_is_a_list([]).
echo_msg_is_a_list([_Elto|_List]).

echo_msg_list(Echo_Level, File_Name, Pre_Msg, []) :-
	echo_msg_list_aux(Echo_Level, File_Name, Pre_Msg, []),
	!. % No backtracking allowed.
echo_msg_list(Echo_Level, File_Name, Pre_Msg, [Msg|Msgs]) :- 
	!, % No backtracking allowed.
	echo_msg_list_aux(Echo_Level, File_Name, Pre_Msg, Msg),
	echo_msg_list(Echo_Level, File_Name, Pre_Msg, Msgs).

echo_msg_list_aux(Echo_Level, File_Name, Pre_Msg, Msg) :-
	echo_msg_logo(Echo_Level, File_Name),
	echo_msg_aux(Echo_Level, File_Name, Pre_Msg),
	echo_msg_aux(Echo_Level, File_Name, ' (list)'),
	echo_msg_aux(Echo_Level, File_Name, ' :: '),
	echo_msg_aux(Echo_Level, File_Name, Msg),
	echo_msg_nl(Echo_Level, File_Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
echo_msg_aux(0, _File_Name, _Msg) :- !.
echo_msg_aux(1, File_Name, Msg) :-
	echo_msg_aux(2, File_Name, Msg),
	write(Msg).
echo_msg_aux(2, File_Name, Msg) :-
	get_stream_to_file(File_Name, Stream),
	write(Stream, Msg).

echo_msg_logo(Echo_Level, File_Name) :-
	echo_msg_aux(Echo_Level, File_Name, '% cneg :: ').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_msg_nl(0, _File_Name) :- !.
echo_msg_nl(1, File_Name) :-
	echo_msg_nl(2, File_Name),
	nl.
echo_msg_nl(2, File_Name) :-
	get_stream_to_file(File_Name, Stream),
	nl(Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_separation(Echo_Level, File_Name) :-
	echo_msg_nl(Echo_Level, File_Name), 
	echo_separation_aux(Echo_Level, File_Name), 
	echo_separation_aux(Echo_Level, File_Name),
	echo_msg_nl(Echo_Level, File_Name).

echo_separation_aux(Echo_Level, File_Name) :-
	echo_msg_logo(Echo_Level, File_Name), 
	echo_msg_aux(Echo_Level, File_Name, '-----------------------------------------------'),
	echo_msg_aux(Echo_Level, File_Name, '-----------------------------------------------'),
	echo_msg_nl(Echo_Level, File_Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_pre_msg_from_list(List, Pre_Msg) :-
	(   (   echo_pre_msg_from_list_aux_1(List, '', Pre_Msg) )
	;
	    (   echo_msg_aux(1, 'debug', 'Error in echo_pre_msg_from_list/2. Pre_Msg :: '),
		echo_msg_aux(1, 'debug', Pre_Msg),
		echo_msg_nl(1, 'debug')
	    )
	).

echo_pre_msg_from_list_aux_1([], Pre_Msg, Pre_Msg) :- !.
echo_pre_msg_from_list_aux_1([Elto|List], Pre_Msg_In, Pre_Msg_Out) :- 
	echo_pre_msg_from_list_aux_2(Elto, Pre_Msg_In, Pre_Msg_Aux), !,
	echo_pre_msg_from_list_aux_1(List, Pre_Msg_Aux, Pre_Msg_Out).
 
echo_pre_msg_from_list_aux_2(Elto, Pre_Msg_In, Pre_Msg_Out) :- 
	(
	    (   name(Elto, Elto_String), !  )
	;
	    (   Elto_String = Elto, !  )
	),
	name(Pre_Msg_In, Pre_Msg_In_String),
	append(Pre_Msg_In_String, Elto_String, Pre_Msg_Out_String),
	name(Pre_Msg_Out, Pre_Msg_Out_String).
	
echo_msg_for_call(Echo_Level, File_Name, Call_Level, Pre_Msg, Msg) :-
	echo_pre_msg_from_list(['call_to (L' |[ Call_Level |[ ') :: ' |[ Pre_Msg ]]]], Pre_Msg_Aux),
	echo_msg(Echo_Level, File_Name, Pre_Msg_Aux, Msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_statistics(0, _File_Name, _Msg) :- !. % No backtracking, please.
echo_statistics(1, File_Name, Msg) :-
	echo_statistics_aux(Msg),
	echo_statistics(2, File_Name, Msg),
	!. % No backtracking, please.
echo_statistics(2, File_Name, Msg) :-
	current_output(StdOut_Stream), % Save stdout stream.
	get_stream_to_file(File_Name, Statistics_Stream),
	set_output(Statistics_Stream), % Redirect stdout to stream.
	echo_statistics_aux(Msg),
	set_output(StdOut_Stream), % Recover stdout stream.
	!. % No backtracking, please.

echo_statistics_aux(Msg) :-
	nl, write(Msg), nl, nl, 
	statistics, !. % Write statistics to file.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_howto_information(Echo_Level, File_Name) :-
	echo_separation(Echo_Level, File_Name),
	echo_msg_nl(Echo_Level, File_Name),
	echo_msg(Echo_Level, File_Name, 'Info for debugging', ' '),
	echo_msg_nl(Echo_Level, File_Name),
	echo_msg(Echo_Level, File_Name, 'Basic frontier', 'frontier(Goal, Head, Body, FrontierTest)'),
	echo_msg(Echo_Level, File_Name, 'frontier_E_IE_NIE', 'frontier_E_IE_NIE(E, IE, NIE)'),
	echo_msg(Echo_Level, File_Name, 'frontier_E_IE_NIE_ied', 'frontier_E_IE_NIE_ied(E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb)'),
	echo_msg(Echo_Level, File_Name, 'Vars_Info', 'vars_info(GoalVars, UQV, ImpVars, ExpVars, RelVars, UQ_to_EQ_Vars, Dumb_Vars)'),
	echo_separation(Echo_Level, File_Name),
	echo_msg_nl(Echo_Level, File_Name).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
