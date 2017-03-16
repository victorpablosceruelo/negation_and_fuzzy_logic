%
%  top.pl			Nai-Wel Lin			December, 1991
%
%  This file contains the top level procedures for the system.
%

%
%  Top-level predicate of CASLOG in interactive mode.
%



caslog(Files) :- 
	nl,
	write('Caslog 1.0, April 1992.'),nl,
% Commented out PLG (28 JUl 97)
%	statistics(runtime,[_,_]),
% end 
%	read_prog(Files),
	top(Files),
% Commented out PLG (28 JUl 97)
 %% 	statistics(runtime,[_,T]),
 %% 	nl, write('{Exexution Time: '),
 %% 	write(T), write(' msec}'), nl,
% end 
	nl,
	write('{End of Caslog execution.}'),nl.

%%%%
read_prog([]).
read_prog([File|Fs]) :-
	see(File),
	r_prog,
	seen,
	read_prog(Fs).

r_prog :-
	read(Clause),
	(Clause \== end_of_file -> 
		 r_prog;
		 true).
%%%%

 %% Commented out by PLG 3 Aug 99 (ICLP99 demo). 
top(Files) :-
 	init_system(Files,BT,ST,SCCG,Error1),
 	analysis_check(SCCG,ST,Error2),
 	Error is Error1*Error2,
 	(Error =:= 1 ->
 		analysis(SCCG,BT,ST, 0, 0, TSize, TCost)
 	    %% simple_message("size analysis in ~q msec.", [TSize]), 
                 %% nl, write('{Total size analysis time: '),
 	         %% write(TSize), write(' msec}'), nl,
            %% simple_message("cost analysis in ~q msec.", [TCost]), 
                 %% nl, write('{Total cost analysis time: '),
 	        %% write(TCost), write(' msec}'), nl,
            %% TTotal is TCost + TSize
                 %% nl, 
            %% simple_message("whole cost analysis in ~q msec.", [TTotal])
                 %% write('{Total (whole) analysis time: '),
 	        %% write(TTotal), write(' msec}'), nl
 	        ;
 		true).
 %% End of commented out by PLG 3 Aug 99 (ICLP99 demo). 

 %% Commented out by PLG 3 Aug 99 (ICLP99 demo). 
 %% complexity_analysis_top(Files, TSize, TTime,  TTotal, GT)  :-
 %%         init_system(Files,BT,ST,SCCG,Error1),
 %%         analysis_check(SCCG,ST,Error2),
 %%         Error is Error1*Error2,
 %%         (Error =:= 1 ->
 %%                cs_analysis(SCCG,BT,ST, 0, 0, TSize, TTime, GT),
 %%                % nl, write('{Total size analysis time: '),
 %%                % write(TSize), write(' msec}'), nl,
 %%                % nl, write('{Total cost analysis time: '),
 %%                % write(TCost), write(' msec}'), nl,
 %%                TTotal is TTime + TSize,
 %%                % nl, write('{Total (whole) analysis time: '),
 %%                % write(TTotal), write(' msec}'), nl
 %%                output_complexity_analysis(ST) 
 %%                ;
 %%                 true).
 %% End of commented out by PLG 3 Aug 99 (ICLP99 demo). 

analysis([],_,_,ITS, ITC, ITS, ITC).
analysis([Comp|SCCG],BT,ST, ITS, ITC, OTS, OTC) :-
	%write('start dependency analysis'),nl,
        statistics(runtime,[_,_]),
	dependency_analysis(Comp,BT,ST,Adg,Ldg,Gvars,Error),
	%write(Adg),nl,
	%write(Ldg),nl,
	%write(Gvars),nl,
	(Error =:= 1 ->
		(%write('start size analysis'),nl,
                 % debug
                 % write('start size analysis? '), read(_), nl,
                 statistics(runtime,[_,_]),
		 size_analysis(Comp,BT,ST,Comp,Adg,Gvars,Size),
                 statistics(runtime,[_,TSize]),
		 print_size(Comp,ST),
	         nl, write('{Size Analysis Time: '),
	         write(TSize), write(' msec}'), nl,
		 ttyflush,
		 %write('start relation analysis'),nl,
		 relation_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_relation(Comp,ST),
		 ttyflush,
		 %write('start solution analysis'),nl,
		 determinacy_analysis(Comp,BT,ST,Adg),
		 solution_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_solution(Comp,ST),
		 ttyflush,
                 % Added by PLG
		 % debug
                 % write('start time analysis? '), read(_), nl,
                 %End added.
                 statistics(runtime,[_,_]),
                 time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 statistics(runtime,[_,TCost]),
                 NTS is ITS + TSize, 
                 NTC is ITC + TCost,
                 print_time(Comp,ST),
	         nl, write('{Cost Analysis Time: '),
	         write(TCost), write(' msec}'), nl,
                 TTotal is TCost + TSize,
                 nl, write('{Total (size + cost) analysis time: '),
	         write(TTotal), write(' msec}'), nl,
		 ttyflush,
		 analysis(SCCG,BT,ST, NTS, NTC, OTS, OTC));
		 true, ITS = OTS, ITC = OTC).


%
%  Top-level predicate of CASLOG in batch mode.
%
caslog(Complexity,File) :-
	(legal_complexity(Complexity) ->
		(init_system(File,BT,ST,SCCG,Error1), 
		 analysis_check(SCCG,ST,Error2),
%		 direct_recursive_check(SCCG,Error3),
		 Error3 = 1,
		 Error is Error1*Error2*Error3,
		 (Error =:= 1 ->
			analysis(Complexity,SCCG,BT,ST);
			true));
		true).

%
%  Check if the complexity name is legal.
%
legal_complexity(size).
legal_complexity(solution).
legal_complexity(time).
legal_complexity(all).
legal_complexity(Complexity) :-
	Complexity \== size,
	Complexity \== solution,
	Complexity \== time,
	Complexity \== all,
	error_message(comp1,Complexity,''),
	fail.

%
%  Analyze the program.
%
analysis(size,SCCG,BT,ST) :-
	size_analysis(SCCG,BT,ST).
/*
	size_analysis_check(SCCG,ST,Error),
	(Error =:= 1 ->
		size_analysis(SCCG,BT,ST);
		true).
*/
analysis(solution,SCCG,BT,ST) :-
	solution_analysis(SCCG,BT,ST).
/*
	solution_analysis_check(SCCG,ST,Error),
	(Error =:= 1 ->
		solution_analysis(SCCG,BT,ST);
		true).
*/
analysis(time,SCCG,BT,ST) :-
	time_analysis(SCCG,BT,ST).
/*
	time_analysis_check(SCCG,ST,Error),
	(Error =:= 1 ->
		time_analysis(SCCG,BT,ST);
		true).
*/
analysis(all,SCCG,BT,ST) :-
	all_analysis(SCCG,BT,ST).


%
%  Perform the argument size analysis.
%
size_analysis([],_,_).
size_analysis([Comp|SCCG],BT,ST) :-
	dependency_analysis(Comp,BT,ST,Adg,_,Gvars,Error),
	(Error =:= 1 ->
		(size_analysis(Comp,BT,ST,Comp,Adg,Gvars,_),
		 print_size(Comp,ST),
		 size_analysis(SCCG,BT,ST));
		true).

%
%  Perform the relation size and solution size analysis.
%
solution_analysis([],_,_).
solution_analysis([Comp|SCCG],BT,ST) :-
	dependency_analysis(Comp,BT,ST,Adg,Ldg,Gvars,Error),
	(Error =:= 1 ->
		(size_analysis(Comp,BT,ST,Comp,Adg,Gvars,Size),
		%print_size(Comp,ST),
		 relation_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_relation(Comp,ST),
		 solution_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_solution(Comp,ST),
		 solution_analysis(SCCG,BT,ST));
		true).

%
%  Perform the time analysis.
%
time_analysis([],_,_).
time_analysis([Comp|SCCG],BT,ST) :-
	dependency_analysis(Comp,BT,ST,Adg,Ldg,Gvars,Error),
	(Error =:= 1 ->
		(size_analysis(Comp,BT,ST,Comp,Adg,Gvars,Size),
		%print_size(Comp,ST),
		 relation_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		%print_relation(Comp,ST),
		 solution_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		%print_solution(Comp,ST),
		 time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_time(Comp,ST),
		 time_analysis(SCCG,BT,ST));
		true).

%
%  Perform all the complexity analysis.
%
all_analysis([],_,_).
all_analysis([Comp|SCCG],BT,ST) :-
	dependency_analysis(Comp,BT,ST,Adg,Ldg,Gvars,Error),
	(Error =:= 1 ->
		(size_analysis(Comp,BT,ST,Comp,Adg,Gvars,Size),
		 print_size(Comp,ST),
		 relation_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_relation(Comp,ST),
		 solution_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_solution(Comp,ST),
		 time_analysis(Comp,BT,ST,Comp,Size,Adg,Gvars,Ldg,_),
		 print_time(Comp,ST),
		 all_analysis(SCCG,BT,ST));
		true).

%
%
direct_recursive_check([],1).
direct_recursive_check([Comp|SCCG],Error) :-
	Comp = [_],
	direct_recursive_check(SCCG,Error).
direct_recursive_check([Comp|SCCG],0) :-
	Comp = [_|_],
	write('* caslog error: cannot handle indirect recursion'),
	nl,
	direct_recursive_check(SCCG,_).

























%% Control version comment prompting for the file.
%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "on"
%% End:

