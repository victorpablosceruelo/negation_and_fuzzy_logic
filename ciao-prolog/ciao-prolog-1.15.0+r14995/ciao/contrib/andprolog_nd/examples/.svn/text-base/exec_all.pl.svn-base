:- module(exec_all, 
	[
	    main/0,
	    det_time/0,
	    nondet_once_time/0,
	    nondet_time/0,
	    det_test/0,
	    nondet_test/0
	], 
	[andprolog_nd]).

 %% (true;display(fin),nl,fail),nondet_test,fail.
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(system)).
:- use_module(library(andprolog_nd(apll_nd))).
:- use_module(library(aggregates)).
:- use_module(library(between)).
:- use_module(library(write)).
:- use_module(avg).
:- use_module(library(lists),[length/2]).
:- use_module(library(arithpreds), [floor/2]).
:- use_module(library(sort),[sort/2]).

:- use_module(fibo).
:- use_module(aiakl).
:- use_module(ann).
:- use_module(mmat).
:- use_module(qsort).
:- use_module(qsort_dl).
:- use_module(iqsort).
:- use_module(iqsort_dl).
:- use_module(boyer).
:- use_module(deriv).
:- use_module(fft).
:- use_module(hamming).
:- use_module(hanoi).
:- use_module(hanoi_dl).
:- use_module(mandel).
:- use_module(pal).
:- use_module(progeom).
:- use_module(tak).   
:- use_module(queens).
:- use_module(queens10).
:- use_module(chat_parser).
:- use_module(numbers).
:- use_module(illumination).
:- use_module(illumination_no_canc).
:- use_module(check_files).
:- use_module(qsort_nd).

:- data t_seq/2.
:- data t_par/3.
:- data t_stats/5.
:- data answ_seq/2.
:- data answ_par/3.

padl2012([ann, boyer, deriv, fft, fibo, hamming, hanoi, hanoi_dl, 
	mmat, pal, qsort, qsort_dl, iqsort, iqsort_dl, tak, 
	mandel, progeom, queens, numbers, illumination, qsort_nd]).
%skip aiakl, check_files, illumination_no_can, 
%     queens10, chat_parser

programs([aiakl]).

programsND([ann, boyer, deriv, fft, fibo, hamming, hanoi, hanoi_dl, 
	mmat, pal, qsort, qsort_dl, iqsort, iqsort_dl, tak, 
	mandel, progeom, queens, illumination, qsort_nd]).

programsT([aiakl, boyer, deriv, fft, fibo, hamming, hanoi, hanoi_dl,
           mmat, pal, qsort, qsort_dl, tak]). %ann

programsNDT([fft,fibo, hanoi,hanoi_dl,mmat, pal,qsort,qsort_dl,
	iqsort,iqsort_dl,tak,qsort_nd]).
 %% programsNDT([mmat]).
 %% programsNDT([boyer, deriv, fft, fibo, hamming, hanoi, hanoi_dl, 
 %% 	mmat, pal, qsort, qsort_dl, iqsort, iqsort_dl, tak, 
 %% 	mandel, progeom, queens, numbers, illumination, qsort_nd]).
agents(8).  iter(3).

main.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% CHAT es secuencial(HACERLO - falla), pero si busco que falte una palabra
%%    y luego obligo a que esa palabra sea la misma...   
%% Falta chequear (ann), por fallo del assert.
%% Jugar el qsort_nd con comparaciones mas pesadas y listas mas pequenyas.
%% Quiza ann pueda ser no determinista.
%% TEST CALL NEEDED CHECKING THE ANSWERS AND THE STACK STATE!!!!
%% What to do with backtracking time in deterministic calls??

det_time :-
	initialize,
	programs(L),
	member(M,L),
	M:data(Data),
	iter(I),
        statistics(walltime, [T1,_]),
	call_N(I,M:seq(Data,_)),
        statistics(walltime, [T2,_]),
	T is (T2 - T1)/I,
	assertz_fact(t_seq(M,T)),
	fail.

det_time :-
	display('Executing paralellism'), nl,
	programs(L),
	agents(Ag),
	iter(I),
	between(1,Ag,A),
	display('   '), display(A), display(' agents'), nl,
	ensure_agents(A),
	member(M,L),
	display('      '), display(M), display(' program'), nl,
	between(1,I,Ii),
	release_all_for_unwinding,
	reset_stats,
	display('         '), display(Ii), display(' iteration'), nl,
	M:data(Data),
        statistics(walltime, [T1,_]),
	call_prune(M:par(Data,_)),
        statistics(walltime, [T2,_]),
	T is T2 - T1,
	get_stats(Trapped,Ttrapped,TtrappedF),
	assertz_fact(t_par(M,T,A)),
	assertz_fact(t_stats(M,Trapped,Ttrapped,TtrappedF,A)),
	fail.

det_time :- programs(L), show_results(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nondet_once_time :-
	initialize,
	programsND(L),
	member(M,L),
	M:data(Data),
	iter(I),
        statistics(walltime, [T1,_]),
	call_N(I, M:seq(Data,_)),
        statistics(walltime, [T2,_]),
	T is (T2 - T1) / I,
	assertz_fact(t_seq(M,T)),
	fail.

nondet_once_time :-
	display('Executing paralellism'), nl,
	programsND(L),
	agents(Ag),
	iter(I),
	between(1,Ag,A),
	display('   '), display(A), display(' agents'), nl,
	ensure_agents(A),
	member(M,L),
	display('      '), display(M), display(' program'), nl,
	between(1,I,Ii),
	release_all_for_unwinding,
	reset_stats,
	M:data(Data),
	display('         '), display(Ii), display(' iteration'), nl,
        statistics(walltime, [T1,_]),
	call_prune(M:par_nondet(Data,_)),
        statistics(walltime, [T2,_]),
	T is T2 - T1,
	get_stats(Trapped,Ttrapped,TtrappedF),
	assertz_fact(t_par(M,T,A)),
	assertz_fact(t_stats(M,Trapped,Ttrapped,TtrappedF,A)),
	fail.

nondet_once_time :- programsND(L), show_results(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nondet_time :-
	initialize,
	programsND(L),
	member(M,L),
	M:data(Data),
	iter(I),
        statistics(walltime, [T1,_]),
	(
	    between(1,I,_),
	    M:seq(Data,_),
	    fail
	;
	    true
	),	
        statistics(walltime, [T2,_]),
	T is (T2 - T1)/I,
	assertz_fact(t_seq(M,T)),
	fail.

nondet_time :-
	display('Executing paralellism'), nl,
	programsND(L),
	agents(Ag),
	iter(I),
	between(1,Ag,A),
	display('   '), display(A), display(' agents'), nl,
	ensure_agents(A),
	member(M,L),
	M:data(Data),
	display('      '), display(M), display(' program'), nl,
	between(1,I,Ii),
 %%   	release_all_for_unwinding,
	reset_stats,
	display('         '), display(Ii), display(' iteration'), nl,
        statistics(walltime, [T1,_]),
  	(M:par_nondet(Data,_), fail; true),
        statistics(walltime, [T2,_]),
	T is T2 - T1,
	get_stats(Trapped,Ttrapped,TtrappedF),
	assertz_fact(t_par(M,T,A)),
	assertz_fact(t_stats(M,Trapped,Ttrapped,TtrappedF,A)),
	fail.

nondet_time :- programsND(L), show_results(L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

det_test :-
	initialize,
	programsT(L),
	member(M,L),
	M:data(Data),
	call_prune(M:seq(Data,Answ)),
	assertz_fact(answ_seq(M,Answ)),
	fail.

det_test :-
	programsT(L),
	agents(Ag),
	between(1,Ag,A),
	ensure_agents(A),
	member(M,L),
	release_all_for_unwinding,
	M:data(Data),
	call_prune(M:par(Data,Answ)),
	assertz_fact(answ_par(M,Answ,A)),
	fail.

det_test :- programsT(L), check_results(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nondet_test :-
	initialize,
	programsNDT(L),
	member(M,L),
	M:data(Data),
	M:seq(Data,Answ),
	assertz_fact(answ_seq(M,Answ)),
	fail.

nondet_test :-
	programsNDT(L),
	agents(Ag),
	between(1,Ag,A),
	display(agentes(A)),nl,
	ensure_agents(A),
	iter(I),
	between(1,I,It),
	display(iteration(It)),nl,
	member(M,L),
	M:data(Data),
	display(testing(M)), nl,
 %%     	release_all_for_unwinding,
	M:par_nondet(Data,Answ),
  	assertz_fact(answ_par(M,Answ,A)),
 %% 	show_memory_usage,
	fail.

nondet_test :- programsNDT(L), check_results(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_results(L) :-
	member(M,L),
	retract_fact(t_seq(M,Seq)),
	format("~a(~3f) ~t",[M,Seq]),
	agents(Ag),
	(between(1,Ag,A);nl,fail),
	findall(TP,retract_fact(t_par(M,TP,A)),Lt),
	findall(Trapped,t_stats(M,Trapped,_,_,A),LTrapped),
	findall(TTrapped,t_stats(M,_,TTrapped,_,A),LTTrapped),
	findall(TFTrapped,retract_fact(t_stats(M,_,_,TFTrapped,A)),LTFTrapped),
	avg(Lt,Par),
	avg(LTrapped,Trappeds),
	avg(LTTrapped,TTrappeds),
	avg(LTFTrapped,TFTrappeds),
	ParFic is Par - TFTrappeds + TTrappeds,
	SpUp is 100*(Seq/ParFic),
	floor(SpUp,Sp1),
	Sp is Sp1/100,
	PercTTrapped is TTrappeds / (Par - TFTrappeds + TTrappeds),
 %% 	PercTFTrapped is TFTrappeds / Par,
	format(" (~2f,~2f,~2f)",[Sp,Trappeds,PercTTrapped]),
	fail.
show_results(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_results(L) :-
	member(M,L),
	findall(Seq,retract_fact(answ_seq(M,Seq)),LSeq),
	sort(LSeq,LSeq2),
	agents(Ag),
	format("-- ~a working: ",[M]),
	(between(1,Ag,A);nl,fail),
	findall(Par,retract_fact(answ_par(M,Par,A)),LPar),
	sort(LPar,LPar2),
 	(
	    LSeq2 = LPar2 ->
	    display('OK ')
	;
 %%     	    display(list(LSeq2,LPar2)),nl,
	    display('KO ')
	),
	fail.
check_results(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initialize :-
	set_prolog_flag(gc, off),
	retractall_fact(t_seq(_,_)),
	retractall_fact(t_par(_,_,_)).

call_N(N,Goal) :-
	between(1,N,_),
	call_prune(Goal),
	fail.
call_N(_,_).

call_prune(Goal) :- call(Goal), !.
call_prune(_).