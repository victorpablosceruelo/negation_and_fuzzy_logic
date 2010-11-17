% Debugger Package - Mon 05/07/2010
%
% Victor Pablos Ceruelo - vpablos@babel.ls.fi.upm.es
%

:- module(debugger_pkg_tr,[debugger_pkg/3]).

:- use_module(library(write)).

%:- dynamic output_file_is_open/1.
%:- dynamic output_stream/1.
:- dynamic saved/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     Options                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

filename_suffix("_rfuzzy").
filename_extension(".pl").
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                Consulting a file                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

debugger_pkg(Input, Output, _) :- 
	debugger_pkg_aux(Input, Output).

debugger_pkg_aux(end_of_file, Clauses) :- !,
	openOutputFile(Stream),
	findall(CL,(retract_fact(saved(CL))),Clauses,[end_of_file]),
	debug_sentences(Clauses),
%	write_eof,
	close_output_file(Stream).

debugger_pkg_aux(0, []) :- !. 

debugger_pkg_aux(Input, []) :-
	assertz_fact(saved(Input)).

%openOutputFile(_Stream) :-
%	output_file_is_open('Yes'), !.

openOutputFile(Stream) :-
	name(FN_Out,"debug_file.txt"),		% Convert string to atom.
	open(FN_Out,write,Stream),
	set_output(Stream).
%	assertz_fact(output_file_is_open('Yes')).

close_output_file(Stream) :-
	flush_output(Stream),
	close(Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        Switch terms to prolog sentences            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% write_eof :- write_sentence(['% DBG PKG :: '], end_of_file).

debug_sentences(Clauses) :-
	write_sentences_list(['% DBG PKG :: '], Clauses).

write_sentences_list(Pre, []) :- !, 
	write_pre_messages(Pre), 
	nl, nl.
write_sentences_list(Pre, [Sentence]) :- !,
	write_sentence(Pre, Sentence).
write_sentences_list(Pre, [Sentence|Others]) :- !,
	write_sentence(Pre, Sentence),
	write_sentences_list(Pre, Others).
write_sentences_list(Pre, Whatever) :- !,
	write_pre_messages(Pre),
	write_sentence_error(Whatever).	

write_sentence(Pre, Sentence) :- !,
	write_pre_messages(Pre),
	write('('),
	write_sentence_aux(Sentence),
	write(').'), nl, !.

write_pre_messages([]) :- !.
write_pre_messages([Msg]) :- !,
	write(Msg).
write_pre_messages([Msg|Others]) :- !,
	write(Msg),
	write_pre_messages(Others).
	
write_sentence_aux(Sent) :-		% Variables
	var(Sent), !, 
	write(Sent).

write_sentence_aux(end_of_file) :- !,
%	nl, nl.
	write(end_of_file).

write_sentence_aux(Sent) :-		% Or
	functor(Sent,';',2), !, 
	Sent =..[';'|Args],
	write('('),
	write_sentence_list_args(Args, ' ; '),
	write(')').

write_sentence_aux(Sent) :-		% And
	functor(Sent,',',2), 
	Sent=..[','|Args], !,
	write('('),
	write_sentence_list_args(Args, ', '),
	write(')').

write_sentence_aux(Sent) :-	% /
	functor(Sent,'/',2),
	Sent=..['/'|Args], !,
%	write('('),
	write_sentence_list_args(Args, '/').
%	write(')').


%write_sentence_aux(Sent) :-	% List
%	functor(Sent,Name,X), !,
%	write(functor(Sent,Name,X)), nl, 
%	is_a_list(Sent), !, 
%	write('['),
%	write_sentence_list_args(Sent, ', '),
%	write(']').
	
write_sentence_aux(Sent) :-	% List.
	functor(Sent,'.',2), 
	Sent=..['.',Arg1,Arg2], !,
	write('['),
	write_sentence_aux(Arg1),
	write_sentence_arg_being_list(Arg2),
	write(']').

write_sentence_aux(Sent) :-	% Rule.
	functor(Sent,':-',2), 
	Sent=..[':-'|Args], !,
	write('('),
	write_sentence_list_args(Args, ' :- '),
	write(')').

write_sentence_aux(Sent) :-	% Functor Ar > 1
	functor(Sent,Name,Arity),
%	write(functor(Sent,Name,Arity)), nl,
	Arity > 1, 
	Sent=..[Name|Args], !, 
	write(Name),
	write('('),
	write_sentence_list_args(Args, ', '),
	write(')').

write_sentence_aux(Sent) :-	% Functor Ar == 1
	functor(Sent,Name,Arity),
%	write(functor(Sent,Name,Arity)), nl,
	Arity == 1,
	Sent=..[Name,Arg], !, 
	write(Name),
	write('('),
	write_sentence_aux(Arg),
	write(')').

write_sentence_aux(Sent) :-	% Functor Ar == 0
	functor(Sent,Name,Arity),
%	write(functor(Sent,Name,Arity)), nl,
	Arity == 0, !, 
	write(Name).

write_sentence_aux(Sent) :-		% Others
	!, 
	write_sentence_error(Sent).

write_sentence_list_args([], _Separator) :- !.
write_sentence_list_args([Sent], _Separator) :- !,
	write_sentence_aux(Sent).
write_sentence_list_args([Sent|Others], Separator) :- !,
	write_sentence_aux(Sent),
	write(Separator),
	write_sentence_list_args(Others, Separator).
write_sentence_list_args(Sent, Separator) :- !,
	write_sentence_error(Separator), 
	write_sentence_error(Sent).

write_sentence_arg_being_list(Arg2) :- % Variable
	var(Arg2), !,
	write('|'),
	write_sentence_aux(Arg2).
write_sentence_arg_being_list(Arg2) :- % Empty
	functor(Arg2,'[]',0). 
write_sentence_arg_being_list(Arg2) :- % Sublist
	functor(Arg2,'.',2), 
	Arg2=..['.',Arg2_1,Arg2_2], !,
	write(','),
	write_sentence_aux(Arg2_1),
	write_sentence_arg_being_list(Arg2_2).
write_sentence_arg_being_list(Arg2) :- % Maybe a sentence
	write('|'),
	write_sentence_aux(Arg2).
write_sentence_arg_being_list(Arg2) :- !, % Error
	write_sentence_error(Arg2).

write_sentence_error(Sent) :-
	nl,write('% UNKNOWN SENTENCE: '), 
	write(Sent),
	nl, !. % Don't do backtracking.


