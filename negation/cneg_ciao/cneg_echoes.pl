:- module(cneg_echoes,
	[
	    echo_msg/3, echo_msg_list/3, 
	    echo_msg_aux/3, echo_msg_nl/1, 
	    echo_msg_logo/1, echo_statistics/1,
	    echo_separation/1, echo_msg_for_call/4
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

:- dynamic defined_stream_to_file/1.

% Use the following sentences to enable/disable debugging.

% 0 -> no debug.
% 1 -> std output + debugging.
% 2 -> only debugging.

%%% Debug (by VPC).
get_stream_to_file(Stream) :-
	defined_stream_to_file(Stream), !.
get_stream_to_file(Stream) :-
	name(FN_Out, "debug_pkg_cneg.pl"),	% Convert string to atom.
%	open(FN_Out,write,Stream), % This removes the file !!!
	open(FN_Out,append,Stream),
	assertz_fact(defined_stream_to_file(Stream)),
	echo_howto_information.

echo_msg(Level, Msg, Clause) :-
	echo_msg_logo(Level),
	echo_msg_aux(Level, Msg, ' :: '),
	echo_msg_aux(Level, Clause, ''),
	echo_msg_nl(Level).

echo_msg_aux(0, _Msg, _Clause) :- !.
echo_msg_aux(1, Msg, Clause) :-
	echo_msg_aux(2, Msg, Clause),
	write(Msg), 
	write(Clause).
echo_msg_aux(2, Msg, Clause) :-
	get_stream_to_file(Stream),
	write(Stream, Msg), 
	write(Stream, Clause).

echo_msg_logo(Level) :-
	echo_msg_aux(Level, '% cneg', ' :: ').

echo_msg_nl(0) :- !.
echo_msg_nl(1) :-
	echo_msg_nl(2),
	nl.
echo_msg_nl(2) :-
	get_stream_to_file(Stream),
	nl(Stream).

echo_msg_list(Level, Msg, []) :-
	echo_msg_list_aux(Level, Msg, []),
	!. % No backtracking allowed.
echo_msg_list(Level, Msg, [Cl|Cls]) :- 
	!, % No backtracking allowed.
	echo_msg_list_aux(Level, Msg, Cl),
	echo_msg_list(Level, Msg, Cls).

echo_msg_list_aux(Level, Msg_Atom, Cl) :-
        atom(Msg_Atom),
        atom_codes(Msg_Atom, Msg_Chars),
	append(Msg_Chars, " (list) ", New_Msg_Chars),
        atom_codes(New_Msg_Atom, New_Msg_Chars), 
        atom(New_Msg_Atom), !,
	echo_msg(Level, New_Msg_Atom, Cl).
echo_msg_list_aux(Level, Msg_String, Cl) :-
	string(Msg_String),
	append(Msg_String, " (list) ", New_Msg_String),
	echo_msg(Level, New_Msg_String, Cl).

echo_separation(Level) :-
%	echo_msg_nl(Level), echo_separation_aux(Level), 
	echo_msg_nl(Level), 
	echo_msg_logo(Level), echo_separation_aux(Level), echo_msg_nl(Level), 
	echo_msg_logo(Level), echo_separation_aux(Level), echo_msg_nl(Level).

echo_separation_aux(Level) :-
	echo_msg_aux(Level, '-----------------------------------------------', '-----------------------------------------------').

echo_msg_for_call(Echo_Level, Level, Msg1, Msg2) :-
	echo_msg_logo(Echo_Level),
	echo_msg_aux(Echo_Level, 'call_to (L', Level),
	echo_msg_aux(Echo_Level, ') :: ', Msg1), 
	echo_msg_aux(Echo_Level, ' :: ', Msg2),
	echo_msg_nl(Echo_Level).

echo_statistics(Msg) :-
	current_output(StdOut_Stream), % Save stdout stream.
	name(FN_Statistics, "debug_pkg_cneg_statistics.pl"),	% Convert string to atom.
	open(FN_Statistics,append,Statistics_Stream),
	set_output(Statistics_Stream), % Redirect stdout to stream.
	nl, write(Msg), nl, nl, 
	statistics, % Write statistics to file.
	set_output(StdOut_Stream), % Recover stdout stream.
%	statistics, % Write statistics to stdout.
	!. % No backtracking, please.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

echo_howto_information :-
	echo_separation(2),
	echo_msg_nl(2),
	echo_msg(2, 'Info for debugging', ' '),
	echo_msg_nl(2),
	echo_msg(2, 'Basic frontier', 'frontier(Goal, Head, Body, FrontierTest)'),
	echo_msg(2, 'frontier_E_IE_NIE', 'frontier_E_IE_NIE(E, IE, NIE)'),
	echo_msg(2, 'frontier_E_IE_NIE_ied', 'frontier_E_IE_NIE_ied(E, IE_Imp, IE_Exp, IE_Dumb, NIE_Imp, NIE_Exp, NIE_Dumb)'),
	echo_msg(2, 'Vars_Info', 'vars_info(GoalVars, UQV, ImpVars, ExpVars, RelVars, UQ_to_EQ_Vars, Dumb_Vars)'),
	echo_separation(2),
	echo_msg_nl(2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
