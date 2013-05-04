:- module(bshare,
 	 [bshare_empty_entry/2,
 	  bshare_project/3,
 	  bshare_identical_abstract/2,
 	  bshare_compute_lub/2,
	  bshare_compute_lub_el/3,
 	  bshare_amgu/4,
	  bshare_sort/2,
	  bshare_call_to_entry/8,
	  bshare_asub_to_native/3,
	  bshare_unknown_entry/2,
          bshare_augment_two_asub/3,
	  bshare_extend_asub/3,
	  bshare_special_builtin/4,
	  bshare_success_builtin/5,
	  bshare_call_to_success_builtin/6,
	  bshare_call_to_success_fact/8,
	  % debugging
	  bshare_crack/2,
	  % output
	  bshare_output/0,
	  % experiments and csharing (C implementation)
%	  bits_to_vars/3,
%          vars_to_bits/3,
%	  binlist_to_atm/2,
	  right_side_to_bits/3
	  % csharing (C implementation)
%	  shvars2shbits/3,
%	  read_negdb/3,
	  ],
	 [assertions]).

:- use_module(library(messages)).
:- use_module(library(lists), [append/3, powerset/2]).
:- use_module(library(terms_vars), [varset/2]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(sets), [ord_intersection_diff/4]).
:- use_module(domain(share_amgu_aux), [peel_equations/3]).
:- use_module(domain(share), [share_sort/2, share_asub_to_native/3]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(domain(bshare(bshare_gluecode))).
:- use_module(domain(bshare(bshare_utils))).

:- doc(author,"Jorge Navas").

:- doc(module,"

   This module defines the abstract operations for the Jacobs and Langen's
   set-sharing domain @cite{jacobs-jlp,langen-phd} using alternative
   bit-based implementations to the set of set of variables. The first,
   @tt{bSH} , uses binary strings and it has been implemented for testing
   purposes. The second, @tt{tSH}, uses strings of 0,1 and the do not care
   symbol * to compact the sharing relationships by eliminating
   redundancies among them. Finally, @tt{tNSH} uses the same representation
   than @tt{tSH} but leveraging the complement (or negative) sharing
   relationships of the original sharing set.

   We have developed a proof-of-concept implementation split into two
   layers. @file{bshare.pl} defines all abstract operations required by a
   fixpoint algorithm and the gluecode necessary to interact with the
   low-level layer. @file{low_level_ops/*.pl} are @bf{Perl} files which
   implement each single operation such as project, amgu,
   etc. @file{low_level_ops/c_code/*} implements set-based operations
   (union, intersection, etc).

   ").

:- doc(bug,"Only operations required by the bottom-up analysis and
           CiaoPP output are defined. See @code{plai/fixpo_bu.pl}.").
:- doc(bug,"Proof-of-concept implementation is too naive.").
:- doc(bug,"Incomplete treatment of builtins.").

%------------------------------------------------------------------------%
% bshare_unknown_entry(+Qv,-Call) 
%------------------------------------------------------------------------%
bshare_unknown_entry([],([],[])):-!,
     negdb_path(0,abs,NFile_a), 
     write_neg_db(NFile_a,[]).
bshare_unknown_entry(Qv,ASub):- !,
     posdb_path(0,abs,File_a),  
     posdb_path(0,rel,File_r),
     negdb_path(0,rel,NFile_r),
     negdb_path(0,abs,NFile_a),
     powerset(Qv,Sh0),
     shvars2shbits(Sh0,Qv,Sh),
     write_pos_db(File_a,Sh),
     mylength(Qv,0,L), 
     sh2nsh(L,File_r,NFile_r),
     read_negdb(NFile_a,Qv,ASub).

%------------------------------------------------------------------------%
% bshare_empty_entry(+Qv,-Call) 
%------------------------------------------------------------------------%
bshare_empty_entry([],([],[])):-!,
     negdb_path(0,abs,NFile_a),
     write_neg_db(NFile_a,[]).
bshare_empty_entry(Qv,ASub):- 
     posdb_path(0,abs,PFile_a), 
     posdb_path(0,rel,PFile_r), 
     negdb_path(0,rel,NFile_r),
     negdb_path(0,abs,NFile_a),
     mylength(Qv,0,L), 
     empty_entry_bits(1,L,Sh), 
     write_pos_db(PFile_a,Sh),
     sh2nsh(L,PFile_r,NFile_r), 
     read_negdb(NFile_a,Qv,ASub).
%     crack_if_debug(ASub,empty_entry).
empty_entry_bits(N,Len,[]):- 
     N > Len,!.  
empty_entry_bits(N,Len,[Z|Zs]):- 
     N =< Len,!,
     empty_entry_bits_(1,N,Len,Z), 
     N1 is N + 1, 
     empty_entry_bits(N1,Len,Zs).

empty_entry_bits_(K,N,Len,[1|Zs]):- 
     K = N,!,
     K1 is K + 1,
     empty_entry_bits_(K1,N,Len,Zs).
empty_entry_bits_(K,N,Len,[0|Zs]):-
     K < N,!,
     K1 is K + 1,
     empty_entry_bits_(K1,N,Len,Zs).
empty_entry_bits_(K,_,Len,S):-
     !,
     P is Len - K +1,
     paddle(1,P,[],0,S).

%------------------------------------------------------------------------%
% bshare_project(+Vars,+ASub,-Proj)                                   
%------------------------------------------------------------------------%
bshare_project(_Vars,'$bottom','$bottom'):-!.
bshare_project([],_Proj,([],[])):-!.
bshare_project(Vars0,(NSh,Vars1),(NSh,Vars0)):-
     Vars0 == Vars1,!.
bshare_project(Vars,(Call,Univ),ASub):-      
     negdb_path(0,abs,NFile_a),
     negdb_path(0,rel,NFile_r),
     write_neg_db(NFile_a,Call),
     ord_intersection_diff(Vars,Univ,Int,_),
     vars_to_bits(Int,Univ,X),
     binlist_to_atm(X,X0),
     mylength(X,0,Len),
     nshproject(Len,X0,NFile_r,NFile_r),
     read_negdb(NFile_a,Int,ASub).
%------------------------------------------------------------------------%
% bshare_identical_abstract(+,+)                                       
% bshare_identical_abstract(ASub0,ASub)                                
%------------------------------------------------------------------------%
% PRE:  ASub,ASub0 are in MEMORY
%------------------------------------------------------------------------%
bshare_identical_abstract(ASub1,ASub2):- ASub1 == ASub2, !.
% bshare_identical_abstract((NSh0,Vars),(NSh1,Vars)):-
%      sort(NSh0,NSh0_s),
%      sort(NSh1,NSh1_s),
%      NSh0_s == NSh1_s, !.
bshare_identical_abstract((NSh0,Vars),(NSh1,Vars)):-
     negdb_path(1,rel,NFile1_r),
     negdb_path(2,rel,NFile2_r),
     negdb_path(1,abs,NFile1_a),
     negdb_path(2,abs,NFile2_a),
     write_neg_db(NFile1_a,NSh0),
     write_neg_db(NFile2_a,NSh1),
     nshcompare(_,NFile1_r,NFile2_r).
%------------------------------------------------------------------------%
% bshare_compute_lub(+,-)                                              
% bshare_compute_lub(ListASub,Lub)                                     
%------------------------------------------------------------------------%
% PRE:  ListASub is in MEMORY
% POST: Lub is in MEMORY and FILE
%------------------------------------------------------------------------%

bshare_compute_lub([ASub0,ASub1|LASub],ASub):-
     bshare_compute_lub_el(ASub0,ASub1,Rest),
     bshare_compute_lub([Rest|LASub],ASub).
bshare_compute_lub([ASub],ASub):-!.

bshare_compute_lub_el('$bottom',Yss,Yss):-!.
bshare_compute_lub_el(Xss,'$bottom',Xss):-!.
bshare_compute_lub_el(([],Vars0),([],Vars1),([],Vars)):-!,
     ( Vars0 \== Vars1 -> 
       error_message("Different variables in LUB operation\n")
     ; 
       Vars = Vars0
     ).
bshare_compute_lub_el((NSh0,Vars0),(NSh1,_Vars1),ASub):-!,
     negdb_path(1,rel,NFile1_r),
     negdb_path(2,rel,NFile2_r),
     negdb_path(1,abs,NFile1_a),
     negdb_path(2,abs,NFile2_a),
     negdb_path(0,rel,NFile0_r),
     negdb_path(0,abs,NFile0_a),
     write_neg_db(NFile1_a,NSh0),
     write_neg_db(NFile2_a,NSh1),
     nshunion(_,NFile1_r,NFile2_r,NFile0_r),
     read_negdb(NFile0_a,Vars0,ASub).

%------------------------------------------------------------------------%
% bshare_augment_two_asub(+,+,-)                                           
% bshare_augment_two_asub(ASub0,ASub1,ASub)                                    
%------------------------------------------------------------------------%
% PRE:  ASub0,ASub1 are in MEMORY
% POST: ASub is in FILE and MEMORY
%------------------------------------------------------------------------%
bshare_augment_two_asub('$bottom',ASub,ASub):-!.
bshare_augment_two_asub(ASub,'$bottom',ASub):-!.
bshare_augment_two_asub(([],[]),(NSh,Vars),(NSh,Vars)):-!,
     negdb_path(0,abs,NFile0_a),
     write_neg_db(NFile0_a,NSh).
bshare_augment_two_asub((NSh,Vars),([],[]),(NSh,Vars)):-!,
     negdb_path(0,abs,NFile0_a),
     write_neg_db(NFile0_a,NSh).
bshare_augment_two_asub(([],_),_,_):-!,
     error_message("This case should be not possible\n").
bshare_augment_two_asub(_,([],_),_):-!,
     error_message("This case should be not possible\n").
bshare_augment_two_asub((NSh0,Vars0),(NSh1,Vars1),ASub):-!,
     negdb_path(0,rel,NFile0_r),
     negdb_path(1,rel,NFile1_r),
     negdb_path(2,rel,NFile2_r),
     negdb_path(0,abs,NFile0_a),
     negdb_path(1,abs,NFile1_a),
     negdb_path(2,abs,NFile2_a),
     write_neg_db(NFile1_a,NSh0),
     write_neg_db(NFile2_a,NSh1),
     sorted_augment_two_asub(Vars0,Vars1,NFile0_r,NFile1_r,NFile2_r),
     append(Vars0,Vars1,Vars_u),
     sort(Vars_u,Vars_s),
     read_negdb(NFile0_a,Vars_s,ASub).

sorted_augment_two_asub(Vars0,Vars1,NFile0_r,NFile1_r,NFile2_r):-
     mylength(Vars0,0,L0),
     mylength(Vars1,0,L1),
     increment(L0,L1,N0),
     increment(L1,L0,N1),	
     prepare_index(Vars0,Vars1,N0,N1,NN0,NN1),
     nshaugment(L0,NN0,NFile1_r),
     nshaugment(L1,NN1,NFile2_r),
     nshunion(_,NFile1_r,NFile2_r,NFile0_r).

prepare_index(Vars0,Vars1,N0,N1,NN0,NN1):-
     Vars0 = [X|_],
     Vars1 = [Y|_],
     compare(Ord,X,Y),
     prepare_index_(Ord,N0,N1,NN0,NN1).

prepare_index_(>,N0,N1,NN0,NN1):-
     to_neg(N0,NN0),
     NN1 = N1.
prepare_index_(<,N0,N1,NN0,NN1):-
     to_neg(N1,NN1),
     NN0 = N0.
prepare_index_(=,_,_,_,_):-
     error_message("nshaugment/n").

%------------------------------------------------------------------------%
% bshare_amgu_bu(+,+,+,-)                                                 
% bshare_amgu_bu(X,T,ASub,ASub0)                                          
%------------------------------------------------------------------------%
% PRE:  ASub is in FILE and MEMORY
% POST: ASub0 is in FILE and MEMORY
% USE:  ASub is taken from FILE
%------------------------------------------------------------------------%

bshare_amgu(_,_,'$bottom','$bottom'):-!.
bshare_amgu(Sg,Head,(NSh,Vars),AMGU):-  
     negdb_path(0,abs,NFile_a),
     write_neg_db(NFile_a,NSh),
     peel_equations(Sg,Head,Eqs),     
     bshare_amgu_iterate(Eqs,Vars),
     negdb_path(0,abs,NFile0_a),
     read_negdb(NFile0_a,Vars,AMGU),!.
    
bshare_amgu_iterate([],_).
bshare_amgu_iterate([(X,Ts)|Eqs],Vars):-	
     vars_to_bits([X],Vars,V),
     binlist_to_atm(V,V0),
     right_side_to_bits(Ts,Vars,T0),
     nshamgu(_,V0,T0,_),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEGIN DEBUGGING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       negdb_path(0,abs,NFile0_a),
%       read_negdb(NFile0_a,Vars,NSh),
%       simple_message("Unifying ~q = ~q ",[X,Ts]),
%       simple_message("NSh= ~q",[NSh]),
%       crack_if_debug((NSh,Vars),amgu),
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% end DEBUGGING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
     bshare_amgu_iterate(Eqs,Vars).

right_side_to_bits([],Vars,Ts0):-!,    	
     mylength(Vars,0,L),
     paddle(1,L,[],0,T),
     binlist_to_atm(T,Ts0).
right_side_to_bits(Ts_u,Vars,Ts0):-!,
     sort(Ts_u,Ts),
     vars_to_bits(Ts,Vars,T),
     binlist_to_atm(T,Ts0).

%----------------------------------------------------------------------------%
% bshare_crack(ASub,Sh)
%----------------------------------------------------------------------------%
% Crack a negative sharing substitution to its corresponding positive
% sharing @var{Sh}
%----------------------------------------------------------------------------%
bshare_crack(([],[]),[]):-!.
bshare_crack((Sh_bits,Vars),Sh_vars):- 
     current_pp_flag(bshare_option,bSH),!,
     bits_to_vars(Sh_bits,Vars,Sh_vars).
bshare_crack((NSh,Vars),Sh):- 
     !,	
     negdb_path(3,abs,NFile_a),
     negdb_path(3,rel,NFile_r),     
     posdb_path(3,abs,PFile_a), 
     write_neg_db(NFile_a,NSh),
     nsh2sh(NFile_r,PFile_a),          
     read_db(PFile_a,Sh_bits),
     bits_to_vars(Sh_bits,Vars,Sh_u),
     share_sort(Sh_u,Sh).
bshare_crack(_,_):- 
     error_message("Failing output: sharing variant unknown"),!,fail.

%------------------------------------------------------------------------%
% bshare_special_builtin(+,+,-,-)                                         
% bshare_special_builtin(SgKey,Sg,Type,Condvars)                          
%------------------------------------------------------------------------%
bshare_special_builtin('metachoice/1',_,ground,_).
bshare_special_builtin('metacut/1',_,ground,_).
bshare_special_builtin('absolute_file_name/2',_,ground,_).
bshare_special_builtin('term_typing:atom/1',_,ground,_).     %% checked
bshare_special_builtin('term_typing:atomic/1',_,ground,_).   %% checked
bshare_special_builtin('$simplify_unconditional_cges/1',_,ground,_).
bshare_special_builtin('current_atom/1',_,ground,_).
bshare_special_builtin('current_input/1',_,ground,_).
bshare_special_builtin('current_module/1',_,ground,_).
bshare_special_builtin('current_output/1',_,ground,_).
bshare_special_builtin('current_op/3',_,ground,_).
bshare_special_builtin('close/1',_,ground,_).
bshare_special_builtin('depth/1',_,ground,_).
bshare_special_builtin('ensure_loaded/1',_,ground,_).
bshare_special_builtin('erase/1',_,ground,_).
bshare_special_builtin('float/1',_,ground,_).
bshare_special_builtin('flush_output/1',_,ground,_).
bshare_special_builtin('get_code/1',_,ground,_).
bshare_special_builtin('get1_code/1',_,ground,_).
bshare_special_builtin('get_code/2',_,ground,_).
bshare_special_builtin('get1_code/2',_,ground,_).
bshare_special_builtin('ground/1',_,ground,_).
bshare_special_builtin('int/1',_,ground,_).
bshare_special_builtin('term_typing:integer/1',_,ground,_). %%% checked
bshare_special_builtin('is/2',_,ground,_).
bshare_special_builtin('name/2',_,ground,_).
bshare_special_builtin('num/1',_,ground,_).
bshare_special_builtin('number/1',_,ground,_).
bshare_special_builtin('numbervars/3',_,ground,_).
bshare_special_builtin('nl/1',_,ground,_).
bshare_special_builtin('open/3',_,ground,_).
bshare_special_builtin('op/3',_,ground,_).
bshare_special_builtin('prolog_flag/2',_,ground,_).
bshare_special_builtin('prolog_flag/3',_,ground,_).
bshare_special_builtin('put_code/1',_,ground,_).
bshare_special_builtin('put_code/2',_,ground,_).
bshare_special_builtin('prolog_sys:statistics/2',_,ground,_). %% checked
bshare_special_builtin('seeing/1',_,ground,_).
bshare_special_builtin('see/1',_,ground,_).
bshare_special_builtin('telling/1',_,ground,_).
bshare_special_builtin('tell/1',_,ground,_).
bshare_special_builtin('tab/1',_,ground,_).
bshare_special_builtin('tab/2',_,ground,_).
bshare_special_builtin('ttyput/1',_,ground,_).
bshare_special_builtin('save_event_trace/1',_,ground,_).
bshare_special_builtin('arithmetic:=:=/2',_,ground,_). % checked
bshare_special_builtin('arithmetic:>=/2',_,ground,_).  % checked
bshare_special_builtin('arithmetic:>/2',_,ground,_).   % checked
bshare_special_builtin('arithmetic:</2',_,ground,_).   % checked
bshare_special_builtin('arithmetic:=</2',_,ground,_).  % checked
bshare_special_builtin('arithmetic:=\\=/2',_,ground,_).% checked
bshare_special_builtin('arithmetic:=\=/2',_,ground,_). % checked
%-------------------------------------------------------------------------
bshare_special_builtin('abort/0',_,bottom,_).
bshare_special_builtin('fail/0',_,bottom,_).
bshare_special_builtin('false/0',_,bottom,_).
bshare_special_builtin('halt/0',_,bottom,_).
%-------------------------------------------------------------------------
bshare_special_builtin('!/0',_,unchanged,_).           % checked
bshare_special_builtin('assert/1',_,unchanged,_).
bshare_special_builtin('asserta/1',_,unchanged,_).
bshare_special_builtin('assertz/1',_,unchanged,_).
bshare_special_builtin('debug/0',_,unchanged,_).
bshare_special_builtin('debugging/0',_,unchanged,_).
bshare_special_builtin('dif/2',_,unchanged,_).
bshare_special_builtin('display/1',_,unchanged,_).
bshare_special_builtin('flush_output/0',_,unchanged,_).
bshare_special_builtin('garbage_collect/0',_,unchanged,_).
bshare_special_builtin('gc/0',_,unchanged,_).
bshare_special_builtin('listing/0',_,unchanged,_).
bshare_special_builtin('listing/1',_,unchanged,_).
bshare_special_builtin('nl/0',_,unchanged,_).
bshare_special_builtin('nogc/0',_,unchanged,_).
bshare_special_builtin('term_typing:nonvar/1',_,unchanged,_).  % checked
bshare_special_builtin('not/1',_,unchanged,_).
bshare_special_builtin('print/1',_,unchanged,_).
bshare_special_builtin('repeat/0',_,unchanged,_).
bshare_special_builtin('start_event_trace/0',_,unchanged,_).
bshare_special_builtin('stop_event_trace/0',_,unchanged,_).
bshare_special_builtin('true/0',_,unchanged,_).               %% checked
bshare_special_builtin('ttyflush/0',_,unchanged,_).
bshare_special_builtin('otherwise/0',_,unchanged,_).
bshare_special_builtin('seen/0',_,unchanged,_).
bshare_special_builtin('told/0',_,unchanged,_).
bshare_special_builtin('ttynl/0',_,unchanged,_).
bshare_special_builtin('write/1',_,unchanged,_).
bshare_special_builtin('writeq/1',_,unchanged,_).
bshare_special_builtin('term_compare:\\==/2',_,unchanged,_).  %% checked
bshare_special_builtin('term_compare:@>=/2',_,unchanged,_).   %% checked
bshare_special_builtin('term_compare:@=</2',_,unchanged,_).   %% checked
bshare_special_builtin('term_compare:@>/2',_,unchanged,_).    %% checked
bshare_special_builtin('term_compare:@</2',_,unchanged,_).    %% checked
%-------------------------------------------------------------------------
bshare_special_builtin('assert/2',assert(_,Z),some,Vars):- varset(Z,Vars).
bshare_special_builtin('asserta/2',asserta(_,Z),some,Vars):- varset(Z,Vars).
bshare_special_builtin('assertz/2',assertz(_,Z),some,Vars):- varset(Z,Vars).
bshare_special_builtin('compare/3',compare(X,_,_),some,Vars):- varset(X,Vars).
bshare_special_builtin('format:format/2','format:format'(X,_Y),some,Vars):- 
	varset(X,Vars).       %% checked
bshare_special_builtin('format:format/3','format:format'(X,Y,_Z),some,List):- 
	varset([X,Y],List). %% checked
bshare_special_builtin('term_basic:functor/3','term_basic:functor'(_X,Y,Z),some,List):-           
	varset([Y,Z],List).  %% checked
bshare_special_builtin('lists:length/2','lists:length'(_X,Y),some,[Y]). %% checked
bshare_special_builtin('print/2',print(X,_Y),some,Vars):- varset(X,Vars).
bshare_special_builtin('recorda/3',recorda(_,_,Z),some,Vars):- varset(Z,Vars).
bshare_special_builtin('recordz/3',recordz(_,_,Z),some,Vars):- varset(Z,Vars).
bshare_special_builtin('write/2',write(X,_Y),some,Vars):- varset(X,Vars).
%-------------------------------------------------------------------------
bshare_special_builtin('=../2','=..'(X,Y),'=../2',p(X,Y)).
bshare_special_builtin('==/2','=='(X,Y),'==/2',p(X,Y)).
bshare_special_builtin('copy_term/2',copy_term(X,Y),copy_term,p(X,Y)).
bshare_special_builtin('findall/3',findall(X,_,Z),findall,p(X,Z)).
bshare_special_builtin('indep/2',indep(X,Y),'indep/2',p(X,Y)).
bshare_special_builtin('indep/1',indep(X),'indep/1',p(X)).
bshare_special_builtin('recorded/3',recorded(_,Y,Z),'recorded/3',p(Y,Z)).
bshare_special_builtin('retract/1',retract(X),'recorded/3',p(X,b)).
bshare_special_builtin('retractall/1',retractall(X),'recorded/3',p(X,b)).
bshare_special_builtin('read/1',read(X),'recorded/3',p(X,b)).
bshare_special_builtin('read/2',read(X,Y),'recorded/3',p(Y,X)).
bshare_special_builtin('term_typing:var/1','term_typing:var'(X),var,p(X)). %% checked
%% OTHERS
bshare_special_builtin(Key,_Goal,special(Key),[]):-
 	bshare_not_that_special_builtin(Key).
bshare_special_builtin(Key,_,_,_):-
	debug_message("~q is not considered a builtin",[Key]),
	!,fail.

bshare_not_that_special_builtin('=/2').
bshare_not_that_special_builtin('C/3').
bshare_not_that_special_builtin('term_basic:arg/3').  %% checked
bshare_not_that_special_builtin('keysort/2').
bshare_not_that_special_builtin('sort:sort/2').       %% checked

%------------------------------------------------------------------------%
% bshare_success_builtin(+,+,+,+,-)                                      |
% bshare_success_builtin(Type,Sv_u,Condv,Call,Succ)                      |
% Obtains the success for some particular builtins:                      |
%  * If Type = ground, it updates Call making all vars in Sv_u ground    |
%  * If Type = bottom, Succ = '$bottom'                                  |
%  * If Type = unchanged, Succ = Call                                    |
%------------------------------------------------------------------------%
bshare_success_builtin(ground,Sv_u,_,Call,Succ):-
     sort(Sv_u,Sv), 
     unify_ground(Sv,Call,Succ).
bshare_success_builtin(some,_,NewGround,Call,Succ):-
     unify_ground(NewGround,Call,Succ).
bshare_success_builtin(bottom,_,_,_,'$bottom').
bshare_success_builtin(unchanged,_,_,Call,Call).
% bshare_success_builtin('=../2',_,p(X,Y),Call,Succ):-
% bshare_success_builtin('==/2',Sv_u,p(X,Y),Call,Succ):-
% bshare_success_builtin(copy_term,_Sv_u,p(X,Y),Call,Succ):-
bshare_success_builtin(findall,_Sv_u,p(_X,_Z),Call,Succ):-
	Call = Succ. % imprecise but correct
% bshare_success_builtin('indep/2',_Sv,p(X,Y),Call,Succ):-
% bshare_success_builtin('indep/1',_Sv,p(X),Call,Succ):- 
% bshare_success_builtin('recorded/3',Sv_u,p(Y,Z),Call,Succ):-
% bshare_success_builtin(var,_Sv,p(X),Call,Succ):-

%------------------------------------------------------------------------%
% bshare_call_to_success_builtin(+,+,+,+,+,-)                             
% bshare_call_to_success_builtin(SgKey,Sg,Sv,Call,Proj,Succ)              
% Handles those builtins for which computing Prime is easier than Succ   
%------------------------------------------------------------------------%
bshare_call_to_success_builtin('=/2','='(X,Y),_Sv,Call,_Proj,Succ):-
     copy_term(X,Xterm),
     copy_term(Y,Yterm),
     Xterm = Yterm,!,
     bshare_amgu(X,Y,Call,Succ).
bshare_call_to_success_builtin('=/2',_Sg,_Sv,_Call,_Proj,'$bottom').
bshare_call_to_success_builtin('term_basic:arg/3','term_basic:arg'(X,Y,Z),_,Call,_Proj,Succ):- 
     varset(X,OldG),
     unify_ground(OldG,Call,TempCall),
%    ord_split_lists_from_list(OldG,Call,_Intersect,TempCall),
     Sg = p(Y,Z),
     Head = p(f(A,_B),A),
     varset(Sg,Sv),
     varset(Head,Hv),
%    bshare_project(Sv,TempCall,Proj),
     bshare_call_to_success_fact(Sg,Hv,Head,Sv,TempCall,_Proj,_Prime,Succ).
bshare_call_to_success_builtin('sort:sort/2','sort:sort'(X,Y),Sv,Call,Proj,Succ):- 
     bshare_call_to_success_builtin('=/2','='(X,Y),Sv,Call,Proj,Succ).
% bshare_call_to_success_builtin('C/3','C'(X,Y,Z),Sv,Call,Proj,Succ):-
% bshare_call_to_success_builtin('expand_term/2',expand_term(X,Y),Sv,Call,Proj,Succ):- 
% bshare_call_to_success_builtin('keysort/2',keysort(X,Y),Sv,Call,Proj,Succ):- 
% 	bshare_call_to_success_builtin('=/2','='(X,Y),Sv,Call,Proj,Succ).
bshare_call_to_success_builtin(SgKey,_,_,Call,_,Call):- 
     warning_message("This type of builtin (~q) is not implemented yet.",[SgKey]).

%------------------------------------------------------------------------%
%                      ABSTRACT Call to Success Fact                     %
%------------------------------------------------------------------------%
%------------------------------------------------------------------------%
% Specialized version of call_to_entry + exit_to_prime + extend for facts%
%------------------------------------------------------------------------%

bshare_call_to_success_fact(Sg,Hv,Head,_Sv,Call,_Proj,Prime,Succ) :-
% exit_to_prime
	bshare_extend_asub(Call,Hv,ASub),
	bshare_amgu(Sg,Head,ASub,ASub1),
%	bshare_project(Sv,ASub1,Prime),
	Prime = ASub1,
% extend
	Prime = Succ.
%	delete_vars_from_list_of_lists(Hv,ASub1,Succ0),
%	sort_list_of_lists(Succ0,Succ),!.	
bshare_call_to_success_fact(_Sg,_Hv,_Head,_Sv,_Call,_Proj, '$bottom','$bottom').	

%------------------------------------------------------------------------%
% intermediate predicates for handling builtins
%------------------------------------------------------------------------%
unify_ground(Sv,ASub,Succ):-
     build_gnd_unif(Sv,Sg,Head),
     bshare_amgu(Sg,Head,ASub,Succ).
    
build_gnd_unif(Vars,Sg,Head):-
     Sg =.. [p|Vars],
     mylength(Vars,0,L),
     paddle(1,L,[],a,Vars0),
     Head =.. [p|Vars0].
     
%------------------------------------------------------------------------%
% bshare_asub_to_native(+,+,-)
% bshare_asub_to_native(Succ,Qv,Info)
%------------------------------------------------------------------------%
bshare_asub_to_native('$bottom',_Qv,_ASub_user):- !, fail.
bshare_asub_to_native(ASub,Qv,Info):-
     bshare_crack(ASub,Sh),
     share_asub_to_native(Sh,Qv,Info),!.

% neg2pos_sharing((NSh,Vars),Sh):-
%      negdb_path(3,abs,NFile_a),
%      write_neg_db(NFile_a,NSh),
%      bshare_crack((NSh,Vars),Sh),!.

% if_not_nil([],_,Xs,Xs):- !.
% if_not_nil(_,X,[X|Xs],Xs).

%------------------------------------------------------------------------%
% bshare_sort(+,-)                                              
% bshare_sort(ASub,ASub_s)                                     
%------------------------------------------------------------------------%
bshare_sort(ASub,ASub).
% bshare_sort('$bottom','$bottom').
% bshare_sort((NSh,Vars),(NSh_s,Vars_s)):-
% 	sort(NSh,NSh_s),
% 	sort(Vars,Vars_s).
% %      negdb_path(0,abs,NFile_a),
% %      write_neg_db(NFile_a,NSh_s).

%------------------------------------------------------------------------%
% bshare_call_to_entry(+,+,+,+,+,+,-,-)
% bshare_call_to_entry(_Sv,_Sg,_Hv,_Head,_Fv,Proj,Proj,_ExtraInfo)
%------------------------------------------------------------------------%
bshare_call_to_entry(_Sv,Sg,Hv,Head,Fv,Proj,Entry,_ExtraInfo):-
     bshare_extend_asub(Proj,Hv,ASub),
     bshare_amgu(Sg,Head,ASub,AMGU),
     bshare_project(Hv,AMGU,Entry0),
     bshare_extend_asub(Entry0,Fv,Entry).
%     crack_if_debug(Entry,call2entry).

bshare_extend_asub(ASub,[],ASub):-!.
bshare_extend_asub(ASub,Hv,ASub1):-!,
     bshare_empty_entry(Hv,ASub0),
     bshare_augment_two_asub(ASub,ASub0,ASub1).
 
% bshare_extend_asub_if_not(ASub,[],ASub):-!.
% bshare_extend_asub_if_not(ASub,Hv,ASub1):-!,
%      ASub = (_,Vars),	
%      ord_subtract(Hv,Vars,NewVars),
%      bshare_extend_asub(ASub,NewVars,ASub1).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(plai(plai_db), [complete/7]).
bshare_output:-
	current_fact(complete(_SgKey,bshare,Sg,Proj,[Prime],_Id,_Parents)),
	bshare_crack(Proj,Proj_out),
	bshare_crack(Prime,Prime_out),
        simple_message("~q: ~q -> ~q",[Sg,Proj_out,Prime_out]),
	fail.
bshare_output.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Only for DEBUGGING purposes (amgu)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
crack_if_debug(ASub,Op):-
	ASub = (_,Vars),
	bshare_crack(ASub,Sh),
	debug_message("[~q] (~q,~q) \n",[Op,Sh,Vars]).
*/
