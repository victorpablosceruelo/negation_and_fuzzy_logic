:- module(plot,[generate_gnu_plot_file/2],[]).

:- use_package(assertions).

:- doc(title,"Generation of data file for gnuplot").

:- doc(author, "Claudio Ochoa").

:- doc(module," This module generates a data file that can be read
	by gnuplot, in order to generate a graphic showing the fitness
	values of the different candidate solutions").


:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(library(format),          [format/3]).
:- use_module(library(terms),           [atom_concat/2]).
:- use_module(program(itf_db),          [curr_file/2]).

:- pred generate_gnu_plot_file(+L,+Stats) # "It takes a list @var{L}of
	values and generates an output file that can be read by the
	external program gnuplot. Some stats are also left in the
	output file (as comments) in order to show more info in the
	resulting eps. Such stats are contained in a list @var{Stats}
	of pairs (description, value)".

generate_gnu_plot_file(L,Stats):-
	current_pp_flag(output_info,high),!, 
	filename(Out),
	open(Out,write,H),
	prefix(H,Stats),
	pop_file(L,H),
	close(H).
generate_gnu_plot_file(_,_).


:- pred filename(-Out) #"Generates the name of file with extension
	.dat to be generated, based on the benchmark file being
	processed".

filename(Out):-
	curr_file(F,_),
	atom_concat([Base,'.pl'],F), 
	suffix(Suf),
	current_pp_flag(poly_fitness,Fit),
	atom_concat([Base,'_',Suf,'_',Fit,'.dat'],Out). 

:- pred suffix(-) #"The output filename will have a suffix depending
	on the curring value of flag @tt{poly_modes}".

suffix(Suffix):-
	current_pp_flag(polyvar_pcpe,PV), 
	(PV == modes ->
	    current_pp_flag(poly_modes,Modes)
	;
	    Modes = ''),
	atom_concat([PV,'_',Modes],Suffix).

:- pred prefix(+,+) # "Creates a header to be outputted in the
	resulting file".

prefix(H,Stats):-
	format(H,"#This file should be used with gnuplot to generate a graphic.~n",[]),
	filename(File),
	format(H,"#Filename: ~w~n",[File]),
	current_pp_flag(poly_fitness,Fit), 
	format(H,"#Fitness: ~w~n",[Fit]),
	current_pp_flag(polyvar_pcpe,PV), 
	format(H,"#Polyvariance control: ~w",[PV]),
	(PV == modes ->
	    current_pp_flag(poly_modes,Modes),
	    format(H," + ~w",[Modes])
	;
	    true
	),
	format(H,"~n#Stats: ",[]),
	format_stats(Stats,H),
	format(H,"~n#~n ",[]).

format_stats([],_).
format_stats([(Desc,Value)|T],H):-
	format(H," ~w: ~2f~t",[Desc,Value]),
	format_stats(T,H).

:- pred pop_file(+L,+Out) # "Generates the contents of the file".

pop_file([],_).
pop_file([H|T],Out):-
	format(Out,"~w~n",[H]),
	pop_file(T,Out).
