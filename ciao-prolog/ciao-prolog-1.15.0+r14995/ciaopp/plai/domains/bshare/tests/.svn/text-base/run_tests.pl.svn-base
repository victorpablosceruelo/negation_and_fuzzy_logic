:- module(_,_,
	[assertions,isomodes]).

:- use_module(library(system), [shell/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lists), [length/2, powerset/2]).
:- use_module(library(write)).
:- use_module(library(sort)).
:- use_module(library(aggregates), [findall/3]).
:- use_module(library(messages)).
:- use_module(library(prolog_sys), [statistics/2, statistics/0, garbage_collect/0]).
:- use_module(library(strings), [get_line/2]).
% Conventional amgu implemented in C
:- use_module(domain(bshare(low_level_ops(share_c(sharing_c)))), [amgu/4, amgu_file/3]).  
% Conventional amgu implemented in Prolog
:- use_module(domain(share_amgu_aux)).
:- use_module(domain(share), [share_sort/2]).
:- use_module(domain(bshare(bshare_utils))).
:- use_module(domain(bshare(bshare_gluecode))).
:- use_module(domain(bshare(bshare))).
:- use_module(domain(bshare(config_paths)), [files_path/1]).
:- use_module(ciaopp(preprocess_flags), [set_pp_flag/2]).
:- include(domain(bshare(tests(seeds)))).

cleanup:-
     retractall_fact(which_left_side_amgu(_)),
     retractall_fact(result(_,_,_,_,_,_,_,_,_)),
     retractall_fact(label_printed).

:- multifile issue_debug_messages/1.
:- data issue_debug_messages/1.
%---------------------------------------------------------------------------------%
%% Only for debugging purposes, uncomment these lines.
%issue_debug_messages(bshare).
%issue_debug_messages(bshare_gluecode).
%issue_debug_messages(run_tests).
%---------------------------------------------------------------------------------%

%test_option(verify).
test_option(not_verify).

home_path('~/SvnReps/CiaoDE/ciaopp/plai/domains/bshare/').
%-----------------------------------------------------------------------------------%
% LIST OF EXPERIMENTS:
%-----------------------------------------------------------------------------------%
% 1) amgu implemented in C without using memory at all.
% 2) amgu implemented in any version (including C) left-side amgu  to be selected:
%    2.1) X-axis: #variables, Y-axis:#records after
%     ?- test_vars(small,8,Perc,'tNSH')
%    2.2) X-axis: #records before, Y-axis:#records after
%     ?- test_records(small,8,'tNSH').
%     ?- test_records(medium,10,'tSH').
%     ?- test_records(large,8,'bSH').
%     ?- test_records(all,8,'bSH').
%-----------------------------------------------------------------------------------%


%-----------------------------------------------------------------------------------%
% test_c_amgu_no_memory(WC,N,Beg,End)
%-----------------------------------------------------------------------------------%
% INPUT 
% WC : left side amgu
% N  : number of variables
% Beg: initial % of records before unification
% End: final   % of records before unification
% OUTPUT
% It is used to execute the C version of amgu without using memory
% in order to check how many variables are analyzable avoiding memory as 
% bottleneck. Therefore, no data are extracted from experiments.
%-----------------------------------------------------------------------------------%
test_c_amgu_no_memory(WC,N,Beg,End):-
     Total_records is 2**N -1,
     length(Vars,N),
     Vars = [X|Ts],
     select_left_side_amgu(WC,Ts,Tss),
     run_test_amgu_file(Beg,End,0.1,Total_records,X,Tss,Vars,N).

%:- push_prolog_flag(discontiguous_warnings,off).
run_test_amgu_file(Perc,Tot,_,_,_,_,_,_):-
     Perc > Tot,!. 
run_test_amgu_file(Perc,Tot,Inc,L_Pow,X,Tss,Vars,N):-
     Perc =< Tot,!,     
     L_Sh is round(Perc * L_Pow),
     seed(Seeds),
     run_one_amgu_file(Seeds,X,Tss,L_Sh,Vars,N),
     NPerc is Perc + Inc,
     run_test_amgu_file(NPerc,Tot,Inc,L_Pow,X,Tss,Vars,N).

run_one_amgu_file([],_,_,_,_,_).
run_one_amgu_file([S|Ss],X,Tss,L_Sh_beg,Vars,N):-
     create_sharing_sets(Vars,N,L_Sh_beg,S,Sh_beg,_L_Sh_Conv,_Time_Conv,'bSH',_Com),
     debug_message("Sharing groups created"),
     files_path(File),
     bshare_utils:shvars2shbits(Sh_beg,Vars,Sh_beg_bits),
     write_neg_db(File,Sh_beg_bits),
     debug_message("Sharing groups written in file"),
     run_one_amgu_file_each_ts(Tss,X,Vars),
     run_one_amgu_file(Ss,X,Tss,L_Sh_beg,Vars,N).

run_one_amgu_file_each_ts([],_X,_Vars).
run_one_amgu_file_each_ts([Ts|Tss],X,Vars):-
     sharing_c:amgu_file(X,Ts,Vars),
     run_one_amgu_file_each_ts(Tss,X,Vars).
%-----------------------------------------------------------------------------------%
% test_vars(WC,N,Perc,V)
%-----------------------------------------------------------------------------------%
% INPUT
% - N (number of variables)
% - Number of records before unification is fixed to 10%,20%,.., or 100% (Perc)
% - T is fixed, either small, medium, or large
% - V is the sharing variant (tNSH, bSH, or tSH, and also SH (Prolog))
%   Note that bSH and tSH cannot be selected from here (go to Perl scripts!)
% OUTPUT
% A file with: 
%     X-axis: N (number of variables)
%     Y-axis: Number of records after unification
%-----------------------------------------------------------------------------------%
test_vars(WC,N,Perc,V) :-
     var(Perc),!,
     Perc = 0.5,
     cleanup,
     Beg = Perc,
     End = Perc,
     simple_message("The experiment starts from 4 variables"),
     test_vars_(4,N,WC,Beg,End,V).
test_vars(WC,N,Perc,V) :-
     cleanup,
     Beg = Perc,
     End = Perc,
     simple_message("The experiment starts from 4 variables"),
     test_vars_(4,N,WC,Beg,End,V).

test_vars_(I,N,_WC,_Beg,_End,_):-
     I > N,!.
test_vars_(I,N,WC,Beg,End,V):-
     I =< N,!,
     main(WC,I,Beg,End,0.1,vars,V),
     NI is I + 1,
     test_vars_(NI,N,WC,Beg,End,V).
	
%-----------------------------------------------------------------------------------%
% test_records(WC,N,V)
%-----------------------------------------------------------------------------------%
% INPUT
% - T is fixed, (left side of amgu)
% - N is number of variables (N)
% - V is the sharing variant (tNSH, bSH or tSH, and also SH (Prolog))
%   Note that bSH and tSH canno not be selected from here (go to Perl scripts!)
% OUTPUT
% A file with 
%    X-axis: Number of records before unification (10%,20%,... of powerset)
%    Y-axis: Number of records after unification
%-----------------------------------------------------------------------------------%
test_records(WC,N,V):-
     cleanup,
     main(WC,N,0.5,1.0,0.1,records,V).


%-----------------------------------------------------------------------------------%
% main(WC,N,Beg,End,Quantum,Test,Variant)
%-----------------------------------------------------------------------------------%
% Variant := 'tNSH' | 'tSH' | 'bSH' | 'SH' | 'cSH'
main(WC,N,Beg,End,Quantum,Test,Variant):-   	
     Total_records is 2**N -1,
     length(Vars,N),
     Vars = [X|Ts],
     select_left_side_amgu(WC,Ts,Tss),
     %gen_log(Test,Beg,WC,N,Log), % be careful it's open a file	
     run_test_amgu(Beg,End,Quantum,Total_records,X,Tss,Vars,N,Variant,_Log),
     %close(Log),
     ( Test = vars ->
       fprint_main_vars(Beg,End,Quantum,Total_records,N,Variant)
     ; % Test = records
       fprint_main_records(Beg,End,Quantum,Total_records,N,Variant)
     ).


:- data which_left_side_amgu/1.
% small: |t| = 1
select_left_side_amgu(small,Ts,[NTs]):-
	retractall_fact(which_left_side_amgu(_)), 
	asserta_fact(which_left_side_amgu(small)),
	subset_len(Ts,1,NTs).
% quarter: |t| = N/4
select_left_side_amgu(quarter,Ts,[NTs]):-
	retractall_fact(which_left_side_amgu(_)), 
	asserta_fact(which_left_side_amgu(quarter)),
	length(Ts,N),
	L is round(N / 4 ),	
	subset_len(Ts,L,NTs).
% medium: |t| = N/2
select_left_side_amgu(medium,Ts,[NTs]):-
	retractall_fact(which_left_side_amgu(_)), 
	asserta_fact(which_left_side_amgu(medium)),
	length(Ts,N),
	L is round(N / 2 ),	
	subset_len(Ts,L,NTs).
% three-quarter: |t| = N/2
select_left_side_amgu('three-quarter',Ts,[NTs]):-
	retractall_fact(which_left_side_amgu(_)), 
	asserta_fact(which_left_side_amgu('three-quarter')),
	length(Ts,N),
	L is round(3* N / 4 ),	
	subset_len(Ts,L,NTs).
% large: |t| = N-1
select_left_side_amgu(large,Ts,[NTs]):-
	retractall_fact(which_left_side_amgu(_)), 
	asserta_fact(which_left_side_amgu(large)),
	length(Ts,N),
	%N1 is N -1,
	subset_len(Ts,N,NTs).
% all: |t| = powerset
select_left_side_amgu(all,Ts,Tss):-
	retractall_fact(which_left_side_amgu(_)), 
	asserta_fact(which_left_side_amgu(all)),
	powerset(Ts,Tss).

subset_len(_,0,[]).
subset_len([X|Xs],N,[X|Rs]):-
	N1 is N -1,
	subset_len(Xs,N1,Rs).

:- data result/9.

run_test_amgu(Perc,Tot,_,_,_,_,_,_,_,_Log):-
     Perc > Tot,!. 
run_test_amgu(Perc,Tot,Inc,L_Pow,X,Tss,Vars,N,Variant,Log):-
     Perc =< Tot,!,     
     L_Sh is round(Perc * L_Pow),
     seed(Seeds),
     run_one_amgu(Seeds,X,Tss,L_Sh,Vars,N,Variant,Log),
     NPerc is Perc + Inc,
     run_test_amgu(NPerc,Tot,Inc,L_Pow,X,Tss,Vars,N,Variant,Log).
   
run_one_amgu([],_,_,_,_,_,_,_).
run_one_amgu([S|Ss],X,Tss,L_Sh_beg,Vars,N,Variant,Log):-
     % L_Sh_Beg: before conversion
     % L_Sh_Conv: after conversion	
     create_sharing_sets(Vars,N,L_Sh_beg,S,Sh_beg,L_Sh_Conv,Time_Conv,Variant,_Log),
     run_one_amgu_foreach_ts(Tss,X,Sh_beg,L_Sh_beg,N,Vars,L_Sh_Conv,Time_Conv,Variant,Log),
     run_one_amgu(Ss,X,Tss,L_Sh_beg,Vars,N,Variant,Log).

run_one_amgu_foreach_ts([],_X,_Sh_beg,_L_Sh_beg,_N,_Vars,_L_Sh_Conv,_Time_Conv,_Variant,_Log).
run_one_amgu_foreach_ts([Ts|Tss],X,Sh_beg,L_Sh_beg,N,Vars,L_Sh_Conv,Time_Conv,Variant,Log):-
     %call_garbage_collector(N),
     test_amgu(X,Ts,(Sh_beg,Vars),Sh_end,Time,Variant),
     length(Sh_end,L_Sh_end),
     bshare_utils:vars_to_bits(Ts,Vars,Ts_bits),
     binary2decimal(Ts_bits,Ts_decimal),
     bshare_utils:vars_to_bits([X],Vars,X_bits),
     binary2decimal(X_bits,X_decimal),
     asserta_fact(result(N,L_Sh_beg,L_Sh_end,X_decimal,Ts_decimal,Time,L_Sh_Conv,Time_Conv,Variant)),
     %fprint_amgu_log(Sh_end,Vars,X,Ts_decimal,Log),
     run_one_amgu_foreach_ts(Tss,X,Sh_beg,L_Sh_beg,N,Vars,L_Sh_Conv,Time_Conv,Variant,Log).

%:- pop_prolog_flag(discontiguous_warnings).
create_sharing_sets(Vars,L,R,S,Sh,L_Sh_Conv,Time_Conv,Variant,Command_Log):-
     % L : number of variables
     % R : number of records
     % S : seed	
     atom_number(AL,L),
     atom_number(AR,R),
     home_path(Path),
     ( var(S) ->
       atom_concat([Path,'tests/genSH.pl -l ',AL,' -n ',AR,' -u'],Command)
     ;
       atom_number(AS,S),
       atom_concat([Path,'tests/genSH.pl -l ',AL,' -n ',AR,' -s ',AS,' -u'],Command)
     ),
     shell(Command,_),
     atom_concat([Path,'tmp/seed.txt'],File_seeds),
     open(File_seeds,read,Stream2),
     get_line(Stream2,String),
     close(Stream2),
     number_codes(Seed,String),
     atom_number(ASeed,Seed),
     atom_concat([Path,'tests/genSH.pl -l ',AL,' -n ',AR,' -s ',ASeed,' -u'],Command_Log),
     atom_concat([Path,'tmp/DB.txt'],File_DB),
     read_db(File_DB,Sh_Pos_bits0),     
     filter_allzeros(Sh_Pos_bits0,L,Sh_Pos_bits),
     create_sharing_sets_(Variant,(Sh_Pos_bits,Vars),Sh,Time_Conv),
     length(Sh,L_Sh_Conv).

create_sharing_sets_('SH',(Sh_bits,Vars),Sh,0) :-
     !,	 
     bshare_utils:bits_to_vars(Sh_bits,Vars,Sh_u),
     share_sort(Sh_u,Sh).
create_sharing_sets_('bSH',(Sh,_Vars),Sh,0):-
     !,
     set_pp_flag(bshare_option,bSH).
create_sharing_sets_(Variant,(Sh_bits,Vars),Sh,Time_Conv) :-
     show_message_if_zero(Sh_bits,Vars,before),	
     set_pp_flag(bshare_option,Variant),
     debug_message("Creating negative sharing from positive ~q",[Sh_bits]),	 
     posdb_path(4,abs,PFile_a), 
     posdb_path(4,rel,PFile_r), 
     negdb_path(4,rel,NFile_r),
     negdb_path(4,abs,NFile_a),
     mylength(Vars,0,Length),
     write_pos_db(PFile_a,Sh_bits),
     statistics(walltime,_),     
     sh2nsh(Length,PFile_r,NFile_r), 
     statistics(walltime,[_,Time_Conv]),     
     read_negdb(NFile_a,Vars,(Sh_Neg_bits,_)),
     debug_message("to negative ~q",[Sh_Neg_bits]),
     Sh = Sh_Neg_bits,
     show_message_if_zero(Sh,Vars,after).

show_message_if_zero(Sh,Vars,Flag):-
     length(Vars,L),
     paddle(1,L,[],0,Empty),	
     ( Sh == [Empty] ->
       ( Flag == before ->
         warning_message("genSH returns an empty substitution ~q",[Sh])
       ; % Flag == after
         warning_message("create_sharing_sets returns an empty substitution ~q",[Sh])
       )
     ;
       true
     ).
     
test_amgu(X,Ts,(Sh_beg,Vars),Sh_end,Time,Variant):-
     set_pp_flag(bshare_option,Variant),
     debug_message("~q = ~q and Sh=~q",[X,Ts,(Sh_beg,Vars)]),
     statistics(walltime,_),          
     bshare:bshare_amgu(X,Ts,(Sh_beg,Vars),(Sh_end,_)),
     statistics(walltime,[_,Time]),     
     ( test_option(verify) ->
       bshare_crack((Sh_end,Vars),Sh_end0), 
       bshare_crack((Sh_beg,Vars),Sh_beg0), 
       amgu(X,Ts,star,Sh_beg0,Sh_end1),     
       shvars2shbits(Sh_end0,Vars,Sh_end2),
       shvars2shbits(Sh_end1,Vars,Sh_end3),
       ( bits_sharing_equivalent(Sh_end2,Sh_end3) -> 
	 simple_message("OK"),
         true 
       ; 
	 bshare_utils:vars_to_bits(Ts,Vars,Ts_bits),
         bshare_utils:vars_to_bits([X],Vars,X_bits),
         shvars2shbits(Sh_beg0,Vars,Sh_beg1),
         warning_message("Unifying ~q = ~q and initial ~q returns ~q and should be ~q",
                         [X_bits,Ts_bits,Sh_beg1,Sh_end2,Sh_end3]) 
       )
     ; 
       true
     ),  
     debug_message("AMGU= ~q",[Sh_end]).
test_amgu(X,Ts,(Sh_beg,Vars),Sh_end,Time,Variant):-
     Variant == 'SH',!,	
     debug_message("~q = ~q and Sh=~q",[X,Ts,(Sh_beg,Vars)]),
     statistics(walltime,_),               
     amgu(X,Ts,star,Sh_beg,Sh_end),
     statistics(walltime,[_,Time]),     
     debug_message("AMGU= ~q",[Sh_end]).
% test_amgu(X,Ts,(Sh_beg,Vars),Sh_end,Time,Variant):-
%      Variant == 'cSH',!,	
%      debug_message("~q = ~q and Sh=~q",[X,Ts,(Sh_beg,Vars)]),
%      statistics(walltime,_),               
%      csharing:amgu(X,Ts,(Sh_beg,Vars),Sh_end),
%      statistics(walltime,[_,Time]),     
%      debug_message("AMGU= ~q",[Sh_end]).
test_amgu(_X,_Ts,_,_,_,_):-
     error_message("Some error in test_amgu"), halt.	
	

bits_sharing_equivalent([],_).
bits_sharing_equivalent([Xs|Xss],Yss):-
	member(Xs,Yss),
	bits_sharing_equivalent(Xss,Yss).

filter_empty([],[]).
filter_empty([X|Xs],Rs):-
	X == [],!,
	filter_empty(Xs,Rs).
filter_empty([X|Xs],[X|Rs]):-
	filter_empty(Xs,Rs).

filter_allzeros(Ss,L,Rs):-
     paddle(1,L,[],0,Empty),
     filter_allzeros_(Ss,Empty,[],Rs).

filter_allzeros_([],_E,Acc,Acc).
filter_allzeros_([S|Ss],E,Acc,Rs):-
     S == E,!,
     filter_allzeros_(Ss,E,Acc,Rs).
filter_allzeros_([S|Ss],E,Acc,Rs):-
     !,filter_allzeros_(Ss,E,[S|Acc],Rs).

% Print in a file statistics
fprint_main_vars(Perc,Tot,_,_,_,_):-
     Perc > Tot,!.
fprint_main_vars(Perc,Tot,Int,L_Pow,N,VV):-
     Perc =< Tot,!,
     Perc0 is round(Perc * 100),
     % L_Sh_Conv : # records before conversion
     % L_Beg     : # records after conversion and before amgu
     % L_End     : # records after conversion and after amgu
     % collecting data of amgu     
     findall(r(N,L_end,T),
             result(N,_L_beg,L_end,_X,_Ts,T,_L_Sh_Conv,_T_Conv,_Variant),
	     Ls_u),
     sort(Ls_u,Ls),
     % collecting data of conversion
     findall(conv(L_beg,L_Sh_Conv,T_Conv),
             result(_N,L_beg,_,_X,_Ts,_T,L_Sh_Conv,T_Conv,_Variant),
	     LConv_u),
     sort(LConv_u,LConv),
     compute_statistics(Ls,NLs),
     atom_number(APerc0,Perc0),
     which_left_side_amgu(WC),
     home_path(Path),
     atom_concat([Path,'tests/test-data/amgu-',VV,'-',WC,'-',APerc0,'p','.dat'],FileName),
     open(FileName,append,Stream),	
     fprint_statistics(NLs,LConv,Stream),
     close(Stream),
     NPerc is Perc + Int,
     fprint_main_vars(NPerc,Tot,Int,L_Pow,N,VV).

% Print in a file statistics
fprint_main_records(Perc,Tot,_,_,_,_):-
     Perc > Tot,!.
fprint_main_records(Perc,Tot,Int,L_Pow,N,VV):-
     Perc =< Tot,!,
     L_beg is round(Perc * L_Pow),
     % L_Sh_Conv : # records before conversion
     % L_Beg     : # records after conversion and before amgu
     % L_End     : # records after conversion and after amgu
     % collecting data of amgu
     findall(r(L_beg,L_end,T),
             result(_N,L_beg,L_end,_X,_Ts,T,_L_Sh_Conv,_T_Conv,_Variant),
	     Ls_u),
     sort(Ls_u,Ls),
     % collecting data of conversion
     findall(conv(L_beg,L_Sh_Conv,T_Conv),
             result(_N,L_beg,_,_X,_Ts,_T,L_Sh_Conv,T_Conv,_Variant),
	     LConv_u),
     sort(LConv_u,LConv),

     ( Ls == [] -> 
       true 
     ;    
       compute_statistics(Ls,NLs),
       atom_number(AN,N),
       which_left_side_amgu(WC),
       home_path(Path),
       atom_concat([Path,'tests/test-data/amgu-',VV,'-',AN,'-',WC,'.dat'],FileName),
       open(FileName,append,Stream),	
       fprint_label(Stream),
       fprint_statistics(NLs,LConv,Stream),
       close(Stream),
       NPerc is Perc + Int,
       fprint_main_records(NPerc,Tot,Int,L_Pow,N,VV)
     ).


compute_statistics(Rs,NRs):-
	length(Rs,N),
	compute_totals(Rs,0,Total_Records,0,Total_Time),
	compute_average(N,Total_Records,Total_Time,Avg_Records,Avg_Time),
	compute_sdeviation(Rs,N,Avg_Records,Avg_Time,Dev_Records,Dev_Time),
	Rs  = [r(NVars,_,_)|_],
	NRs = [r(NVars,Avg_Records,Dev_Records,Avg_Time,Dev_Time)].
	
compute_average(0,_Total_Records,_Total_Time,0,0).
compute_average(N,Total_Records,Total_Time,Avg_Records,Avg_Time):-
	Avg_Records is round(Total_Records / N),
	Avg_Time    is Total_Time / N.

compute_sdeviation(_Rs,0,_Avg_R,_Avg_T,0,0).
compute_sdeviation(Rs,N,Avg_R,Avg_T,Dev_R,Dev_T):-
	compute_sdeviation_(Rs,Avg_R,Avg_T,Sum_R,Sum_T),
	Dev_R is sqrt(Sum_R / N),
	Dev_T is sqrt(Sum_T / N).

compute_sdeviation_([],_Avg_R,_Avg_T,0,0).
compute_sdeviation_([r(_,R,T)|Rs],Avg_R,Avg_T,Total_R,Total_T):-
	Diff_R is (R - Avg_R)**2,
	Diff_T is (T - Avg_T)**2,
	compute_sdeviation_(Rs,Avg_R,Avg_T,Total_Rs,Total_Ts),
	Total_R is Total_Rs + Diff_R,
	Total_T is Total_Ts + Diff_T.
	
compute_totals([],R,R,T,T).
compute_totals([r(_,L_end,T)|Rs],Acc_R,Total_R,Acc_T,Total_T):-
	NAcc_R is L_end + Acc_R,
	NAcc_T is T + Acc_T,       
	compute_totals(Rs,NAcc_R,Total_R,NAcc_T,Total_T).

:- data label_printed/0.
fprint_label(_Stream):-
       current_fact(label_printed),!,
       true.
fprint_label(Stream):-
       asserta_fact(label_printed),
       write(Stream,'(1) #records before conversion'),nl(Stream),
       write(Stream,'(2) #records after conversion'),nl(Stream),
       write(Stream,'(3) (AVG,SD) #records after  amgu'),nl(Stream),
       write(Stream,'(4) time of conversion'),nl(Stream),
       write(Stream,'(5) (AVG,SD) time of amgu'),nl(Stream), nl(Stream).

fprint_statistics([],_,_).
fprint_statistics([r(N,Avg_R,Dev_R,Avg_T,Dev_T)|Rs],LConv,Stream):-
       member(conv(N,L_Conv,T_Conv),LConv),
       write(Stream,N), write(Stream,'   '), 
       write(Stream,L_Conv), write(Stream,'   '), 
       write(Stream,'('),
       write(Stream,Avg_R), write(Stream,','),
       write(Stream,Dev_R), write(Stream,')'), write(Stream,'   '), 
       write(Stream,T_Conv), write(Stream,'   '),        
       write(Stream,'('),
       write(Stream,Avg_T), write(Stream,','),
       write(Stream,Dev_T), write(Stream,')'), write(Stream,'   '), 
       nl(Stream),
       fprint_statistics(Rs,LConv,Stream).


% Print in a file the variables to be unified (x and t)
fprint_amgu_log(Sh,Vars,X,Ts_decimal,Log):-
       bshare_utils:vars_to_bits([X],Vars,X_bits),
       binary2decimal(X_bits,X_dec),
       write(Log,X_dec),write(Log,'='),write(Log,Ts_decimal),nl(Log),
       sharing2bits(Sh,Vars,Log).
gen_log(Test,Beg,WC,N,_Log):-
	home_path(Path),
	( Test = vars ->
	  Perc is round(Beg * 100),
	  atom_number(APerc,Perc),
	  atom_concat([Path,'tests/test-data/amgu_log-',WC,'-',APerc,'p'],FileName)
	; % records
	    atom_number(AN,N),
	    atom_concat([Path,'tests/test-data/amgu_log-',WC,'-',AN,'v'],FileName)
	).	  	
						%open(FileName,append,Log).
% auxiliary predicates
delete_empty_set([],[]).
delete_empty_set([[]|Ss],Ss).
delete_empty_set([S|Ss],[S|Rs]):-
	delete_empty_set(Ss,Rs).

sharing2bits([],_,_).
sharing2bits([S|Ss],Vars,Stream):-
	bshare_utils:vars_to_bits(S,Vars,S_bits),
	bshare_utils:binlist_to_atm(S_bits,S0_bits),
	write(Stream,S0_bits),nl(Stream),
	sharing2bits(Ss,Vars,Stream).

binary2decimal(Binary_List,Res):-
	length(Binary_List,L),
	L1 is L - 1,
	binary2decimal_(Binary_List,0,L1,Res0),
	Res is round(Res0).

binary2decimal_([],Res,_,Res).
binary2decimal_([X|Xs],Acc,Ind,Res):-
	Base is 2**Ind,
	Dec_Val is X * Base,
	NAcc is Dec_Val + Acc,
	NInd is Ind -1,
	binary2decimal_(Xs,NAcc,NInd,Res).


call_garbage_collector(N):-
	N > 8,!,
	garbage_collect.
call_garbage_collector(_).