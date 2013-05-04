/*             Copyright (C)1990-2002 UPM-CLIP				*/

:- module(fixpo_dd,
	[ query/8,
	  init_fixpoint/0,
	  cleanup_fixpoint/1,
	  entry_to_exit/7
	],
	[
	  .(notrace), % inhibits the tracing
	  assertions
	]).

:- include(fixpo_dx_common).

:- use_module(plai(tarjan), [recursive_class/2]).

:- use_module(program(clidlist), [atom2data/5, clid2data/4]).
:- use_module(library(messages)).


%------------------------------------------------------------------------%

:- doc(bug,"Check analysis of meta_calls works after introducing
        fixpoint_reuse_prev_id/5").
:- doc(bug,"Possibly incorrect results at program point for warplan.").

%------------------------------------------------------------------------%

:- data '$change_list'/2.

:- doc(init_fixpoint/0,"Cleanups the database of analysis of
	temporary information.").

init_fixpoint:-
	retractall_fact('$change_list'(_,_)),
	trace_fixp:cleanup.


%------------------------------------------------------------------------%
% call_to_success(+,+,+,+,+,+,+,-,-,+,+)                                 %
% call_to_success(RFlag,SgKey,Call,Proj,Sg,Sv,AbsInt,Succ,F,N,NewN)      %
% It obtains the Succ for a given Sg and Call.                           %
% Before computing the Succ we check if it has already been computed.    %
% If it has but there is a $change_list associated this means that this  %
% Succ was computed using information that was not yet final and has     %
% changed. Thus we recompute as needed to have the final Succ            %
%  If no Succ has already been computed (there is no complete for it)    %
% we compute it from scratch.                                            %
%------------------------------------------------------------------------%

call_to_success(_RFlag,SgKey,Call,Proj,Sg,Sv,AbsInt,_ClId,Succ,F,N,Id) :-
	current_fact(complete(SgKey,AbsInt,Subg,Proj1,Prime1,Id,Fs),Ref),
	identical_proj(AbsInt,Sg,Proj,Subg,Proj1),!,
	reuse_complete(Ref,SgKey,Proj,Sg,Sv,AbsInt,F,N,Id,Fs,Prime1,Prime),
	each_extend(Prime,AbsInt,Sv,Call,Succ).
call_to_success(nr,SgKey,Call,Proj,Sg,Sv,AbsInt,ClId,Succ,F,N,Id):-
	(current_pp_flag(reuse_fixp_id,on) ->
	    fixpoint_id_reuse_prev(SgKey,AbsInt,Sg,Proj,Id)
	;
	    fixpoint_id(Id)),
	fixpoint_trace('non-recursive initiated',Id,N,SgKey,Sg,Proj,_),
	proj_to_prime_nr(SgKey,Sg,Sv,Call,Proj,AbsInt,ClId,Prime,Id), 
	fixpoint_trace('non-recursive completed',Id,N,SgKey,Sg,Prime,_),
	asserta_fact(complete(SgKey,AbsInt,Sg,Proj,Prime,Id,[(F,N)])),
	each_extend(Prime,AbsInt,Sv,Call,Succ).
call_to_success(r,SgKey,Call,Proj,Sg,Sv,AbsInt,_ClId,Succ,F,N,Id) :-
	init_fixpoint0(SgKey,Call,Proj,Sg,Sv,AbsInt,F,N,Id,Prime),
	each_extend(Prime,AbsInt,Sv,Call,Succ).

reuse_complete(Ref,SgKey,Proj,Sg,Sv,AbsInt,F,N,Id,Fs,Prime1,Prime):-
	each_abs_sort(Prime1,AbsInt,TempPrime),
	(
	    current_fact('$change_list'(Id,ChList),Ref2) ->
	    erase(Ref2),
	    fixpoint_compute_change(ChList,SgKey,Sg,Sv,Proj,
	                                         AbsInt,TempPrime,Prime,Id),
	    current_fact(complete(SgKey,AbsInt,A,B,C,Id,Fs2),Ref3),
	    (member((F,N),Fs2) ->
	        true
	    ;
		erase(Ref3),
		asserta_fact(complete(SgKey,AbsInt,A,B,C,Id,[(F,N)|Fs2]))
	    )
	;
	    Prime = TempPrime,
	    (member((F,N),Fs) ->
	        true
	    ; 
		erase(Ref),
		asserta_fact(complete(SgKey,AbsInt,Sg,Proj,Prime,Id,[(F,N)|Fs]))
	    )
	).


init_fixpoint0(SgKey,Call,Proj0,Sg,Sv,AbsInt,F,N,Id,Prime):-
	current_pp_flag(widen,on),
	current_pp_flag(multi_success,off),
	widen_call(AbsInt,SgKey,Sg,F,N,Proj0,Proj), !,
	init_fixpoint1(SgKey,Call,Proj,Sg,Sv,AbsInt,F,N,Id,Prime).
init_fixpoint0(SgKey,Call,Proj,Sg,Sv,AbsInt,F,N,Id,Prime):-
	init_fixpoint_(SgKey,Call,Proj,Sg,Sv,AbsInt,F,N,Id,Prime).


init_fixpoint1(SgKey,_Call,Proj,Sg,Sv,AbsInt,F,N,Id,Prime):-
	current_fact(complete(SgKey,AbsInt,Subg,Proj1,Prime1,Id,Fs),Ref),
	identical_proj(AbsInt,Sg,Proj,Subg,Proj1),!,
	reuse_complete(Ref,SgKey,Proj,Sg,Sv,AbsInt,F,N,Id,Fs,Prime1,Prime).
init_fixpoint1(SgKey,Call,Proj,Sg,Sv,AbsInt,F,N,Id,Prime):-	
	init_fixpoint_(SgKey,Call,Proj,Sg,Sv,AbsInt,F,N,Id,Prime).

init_fixpoint_(SgKey,Call,Proj,Sg,Sv,AbsInt,F,N,Id,Prime):-
	(current_pp_flag(reuse_fixp_id,on) ->
	    fixpoint_id_reuse_prev(SgKey,AbsInt,Sg,Proj,Id)
	;
	    fixpoint_id(Id)),
	fixpoint_trace('non-recursive initiated',Id,N,SgKey,Sg,Proj,_),
	proj_to_prime_r(SgKey,Sg,Sv,Call,Proj,AbsInt,TempPrime,Id), 
	fixpoint_trace('non-recursive completed',Id,N,SgKey,Sg,TempPrime,_),
	asserta_fact(complete(SgKey,AbsInt,Sg,Proj,TempPrime,Id,[])),
	bagof(X, X^(read_clause(SgKey,r,X)),Clauses),!,
	fixpoint_trace('fixpoint initiated',Id,N,SgKey,Sg,Proj,Clauses),
        compute(Clauses,SgKey,Sg,Sv,Proj,AbsInt,TempPrime,Prime1,Id),
	fixpoint_ch(SgKey,Sg,Sv,Proj,AbsInt,Prime1,Prime2,Id), % !.
	each_apply_trusted(Proj,SgKey,Sg,Sv,AbsInt,Prime2,Prime3), 
	current_fact(complete(SgKey,AbsInt,Sg,_Proj,Prime4_u,Id,Fs2),Ref),
	each_abs_sort(Prime4_u,AbsInt,Prime4),
	widen_succ(AbsInt,Prime3,Prime4,Prime),
	(Prime2 \== Prime ->
	    write('something going wrong')
	;
	    true),
	(member((F,N),Fs2) ->
	    true
	;
	    erase(Ref),
	    asserta_fact(complete(SgKey,AbsInt,Sg,Proj,Prime,Id,[(F,N)|Fs2]))
	).



%-------------------------------------------------------------------------
% proj_to_prime(+,+,+,+,+,+,-,+,+,+)                                     %
% proj_to_prime(SgKey,Sg,Sv,Call,Proj,AbsInt,ListPrime,F,N,Id)           %
% This predicate obtains the list of Prime corresponding to each non     %
% recursive clause of Sg for a given Call. It first reads those non      %
% recursive clauses by means of a bagof and then proccess each one with  %
% a loop. If there is no non recursive clause, the answer will be        %
% ['$bottom'].                                                           %
%-------------------------------------------------------------------------

proj_to_prime_nr(SgKey,Sg,Sv,Call,Proj,AbsInt,_ClId,LPrime,Id) :-
	bagof(X, X^(read_clause(SgKey,nr,X)),Clauses), !,
	proj_to_prime(Clauses,SgKey,Sg,Sv,Call,Proj,AbsInt,LPrime,Id).
proj_to_prime_nr(SgKey,Sg,Sv,_Call,Proj,AbsInt,ClId,LPrime,_Id) :-
	apply_trusted0(Proj,SgKey,Sg,Sv,AbsInt,ClId,Prime), !,
	singleton(Prime,LPrime).
proj_to_prime_nr(SgKey,_Sg,_Sv,_Call,_Proj,_AbsInt,ClId,Bot,_Id) :-
	bottom(Bot),
	inexistent(SgKey,ClId).

proj_to_prime_r(SgKey,Sg,Sv,Call,Proj,AbsInt,Prime,Id) :-
	bagof(X, X^(read_clause(SgKey,nr,X)),Clauses), !,
	proj_to_prime(Clauses,SgKey,Sg,Sv,Call,Proj,AbsInt,Prime,Id).
proj_to_prime_r(_SgKey,_Sg,_Sv,_Call,_Proj,_AbsInt,Bot,_Id):-
	bottom(Bot).

proj_to_prime(Clauses,SgKey,Sg,Sv,Call,Proj,AbsInt,Prime,Id) :-
	fixpoint_trace('non-recursive clauses',Id,_N,SgKey,Sg,Proj,Clauses),
	proj_to_prime_loop(Clauses,Sg,Sv,Call,Proj,AbsInt,ListPrime0,Id),
	reduce_equivalent(ListPrime0,AbsInt,ListPrime1),
	each_apply_trusted(Proj,SgKey,Sg,Sv,AbsInt,ListPrime1,Prime).


proj_to_prime_loop([],_,_,_,_,_,[],_).
proj_to_prime_loop([Clause|Rest],Sg,Sv,Call,Proj,AbsInt,Primes,Id):-
	do_nr_cl(Clause,Sg,Sv,Call,Proj,AbsInt,Primes,TailPrimes,Id),!,
	proj_to_prime_loop(Rest,Sg,Sv,Call,Proj,AbsInt,TailPrimes,Id).

do_nr_cl(Clause,Sg,Sv,Call,Proj,AbsInt,Primes,TailPrimes,Id):-
	Clause = clause(Head,Vars_u,K,Body),
	clause_applies(Head,Sg), !,
	varset(Head,Hv),
	sort(Vars_u,Vars),
	ord_subtract(Vars,Hv,Fv),
	process_body(Body,K,AbsInt,Sg,Hv,Fv,Vars_u,Head,Sv,Call,
	                                                Proj,LPrime,Id),
	append_(LPrime,TailPrimes,Primes).
do_nr_cl(_Clause,_Sg,_Sv,_Call,_Proj,_AbsInt,Primes,Primes,_Id).

append_([Prime],TailPrimes,Primes):- !, Primes=[Prime|TailPrimes].
append_(LPrime,TailPrimes,Primes):- append(LPrime,TailPrimes,Primes).


process_body(Body,K,AbsInt,Sg,Hv,_Fv,_,Head,Sv,Call,Proj,LPrime,Id):- 
	Body = g(_,[],'$built'(_,true,_),'true/0',true),!,
	Help=(Sv,Sg,Hv,_Fv,AbsInt),
	singleton(Prime,LPrime),
	fixpoint_trace('visit fact',Id,_N,K,Head,Proj,Help),
	call_to_success_fact(AbsInt,Sg,Hv,Head,Sv,Call,Proj,Prime,_Succ),
	(
	    current_pp_flag(fact_info,on) ->
	    call_to_entry(AbsInt,Sv,Sg,Hv,Head,[],Prime,Exit,_),
	    decide_memo(AbsInt,K,Id,no,Hv,[Exit])
	;
	    true
	),
	fixpoint_trace('exit fact',_Id,_N,_K,Head,Prime,Help).
process_body(Body,K,AbsInt,Sg,Hv,Fv,Vars_u,Head,Sv,_,Proj,Prime,Id):-
	call_to_entry(AbsInt,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo),
	fixpoint_trace('visit clause',Id,_N,K,Head,Entry,Body),
	singleton(Entry,LEntry),
	entry_to_exit(Body,K,LEntry,Exit,Vars_u,AbsInt,Id),
	fixpoint_trace('exit clause',Id,_N,K,Head,Exit,_),
	each_exit_to_prime(Exit,AbsInt,Sg,Hv,Head,Sv,ExtraInfo,Prime).

%-------------------------------------------------------------------------
% body_succ(+,+,-,+,-,+,+,+,+)                                           %
% body_succ(Call,[Key,Sv,(I1,I2,Sg)],Succ,Hv,Fv,AbsInt,NewN)             %
% First, the lub between the abstract call substitution and the already  %
% computed information for this program point (if any) is computed. Then %
% the lub is recordered.                                                 %
% If the abstract call substitution is bottom (case handled by the first %
% clause) the success abstract substitution is also bottom and nothing   %
% more is needed. Otherwise (second clause) the computation of the       %
% success abstract substitution procceeds.                               %
%-------------------------------------------------------------------------

body_succ(Call,Atom,Succ,HvFv_u,AbsInt,_ClId,ParentId,no):- 
	bottom(Call), !,
%	bottom(Succ),
	Succ = Call,
	Atom=g(Key,_Av,_I,_SgKey,_Sg),
	asserta_fact(memo_table(Key,AbsInt,ParentId,no,HvFv_u,Succ)).
body_succ(Call,Atom,Succ,HvFv_u,AbsInt,ClId,ParentId,Id):- 
	Atom=g(Key,Sv,Info,SgKey,Sg),
	fixpoint_trace('visit goal',ParentId,ClId,Key,Sg,Call,AbsInt),
	decide_memo(AbsInt,Key,ParentId,no,HvFv_u,Call),
	body_succ0(Info,SgKey,Sg,Sv,HvFv_u,Call,Succ,AbsInt,ClId,Key,ParentId,Id),
	fixpoint_trace('exit goal',Id,ParentId,(SgKey,Key),Sg,Succ,AbsInt),
 	change_son_if_necessary(Id,Key,ParentId,HvFv_u,Call,AbsInt).

change_son_if_necessary(no,_,_,_,_,_):-!.
change_son_if_necessary(NewId,Key,NewN,Vars_u,Call,AbsInt):-
        current_fact(memo_table(Key,AbsInt,NewN,Id,_,_),Ref),
        (Id = NewId ->
            true
        ;
            erase(Ref),
            decide_memo(AbsInt,Key,NewN,NewId,Vars_u,Call)).            




%-------------------------------------------------------------------------
% compute(+,+,+,+,+,+,+,-,+).                                            %
% compute(Clauses,SgKey,Sg,Sv,Proj,AbsInt,TempPrime,Prime,Id)            %
% It analyses each clause. If after the computation the                  %
% approximate abstract prime substitution changes, the Flag is changed to%
% 'notend' and erases the register ch_id(Id,Num), increases Num by one   %
% and recorders ch_id(Id,Num1), otherwise everything remains unchanged.  %
%-------------------------------------------------------------------------

compute([],_,_,_,_,_,Prime,Prime,_).
compute([Clause|Rest],SgKey,Sg,Sv,Proj,AbsInt,TempPrime,Prime,Id) :-
	do_r_cl(Clause,SgKey,Sg,Sv,Proj,AbsInt,Id,TempPrime,NewPrime),
	compute(Rest,SgKey,Sg,Sv,Proj,AbsInt,NewPrime,Prime,Id).


do_r_cl(Clause,SgKey,Sg,Sv,Proj,AbsInt,Id,TempPrime,NewPrime):-
	Clause=clause(Head,Vars_u,K,Body),
	clause_applies(Head,Sg), !, 
	varset(Head,Hv),
	sort(Vars_u,Vars),
	ord_subtract(Vars,Hv,Fv),
	call_to_entry(AbsInt,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo),
%	erase_previous_memo_tables_and_parents(Body,AbsInt,K,Id),
	fixpoint_trace('visit clause',Id,_N,K,Head,Entry,Body),
	singleton(Entry,LEntry),
	entry_to_exit(Body,K,LEntry,Exit,Vars_u,AbsInt,Id),
	fixpoint_trace('exit clause',Id,_N,K,Head,Exit,_),
	each_exit_to_prime(Exit,AbsInt,Sg,Hv,Head,Sv,ExtraInfo,Prime1),
	widen_succ(AbsInt,TempPrime,Prime1,NewPrime),
	decide_mark_parents(AbsInt,TempPrime,NewPrime,SgKey,Sg,Id,Proj).
do_r_cl(_,_,_,_,_,_,_,Prime,Prime).


decide_mark_parents(AbsInt,TempPrime,NewPrime,_SgKey,_Sg,_Id,_Proj):-
	abs_subset_(NewPrime,AbsInt,TempPrime),!.
decide_mark_parents(AbsInt,_TempPrime,NewPrime,SgKey,Sg,Id,Proj):-
	current_fact(complete(SgKey,AbsInt,Sg,_,_,Id,Fs),Ref),
	erase(Ref),
	asserta_fact(complete(SgKey,AbsInt,Sg,Proj,NewPrime,Id,Fs)),
	make_atom([SgKey,0],Clid),
	clid2data(Clid,P,A,0),  % should be more efficient
	(recursive_class(P/A,Class)->
	    mark_parents_change_list(Fs,Class,AbsInt)
	;
	    td_mark_parents_change_list(Fs,AbsInt)
	). 

% in analysis after annotation
% the recursivity classes are not updated for newly introduced predicates

%----------------------------------------------------------------------
% td_mark_parents_change_list(+)
% td_mark_parents_change_list(Parents)
%  This complete has changed. So we add the change in the $change_list
% of all parents
%----------------------------------------------------------------------
td_mark_parents_change_list([],_).
td_mark_parents_change_list([(_,0)|Rest],AbsInt):-!,
	td_mark_parents_change_list(Rest,AbsInt).
td_mark_parents_change_list([(Literal,C)|Rest],AbsInt):-
	atom2data(Literal,N,A,Cl,G),
	make_atom([N,A],Key),
	current_fact(complete(Key,AbsInt,_,_,_,C,Parents),_),!,
	add_change(C,Literal,N/A/Cl/G,Parents,AbsInt),
	td_mark_parents_change_list(Rest,AbsInt).
td_mark_parents_change_list([_|Rest],AbsInt):- % in case we have erased
	td_mark_parents_change_list(Rest,AbsInt).        % the complete
%----------------------------------------------------------------------%
% add_change(+,+,+,+,+)                                                %
% add_change(C,Lit_Key,Literal,Parents,AbsINt)                         %
%
%----------------------------------------------------------------------%
add_change(C,Lit_Key,Literal,Parents,AbsInt):-
	current_fact('$change_list'(C,ChList),Ref),!,
	insert_literal(ChList,Lit_Key,Literal,NewList,Flag),
	(Flag == yes ->
	    erase(Ref),
	    asserta_fact('$change_list'(C,NewList)),
	    td_mark_parents_change_list(Parents,AbsInt)
	;
	    true). % this avoids infinite loops (already marked)
add_change(C,Lit_Key,Literal,Parents,AbsInt):-
	asserta_fact('$change_list'(C,[(Lit_Key,Literal)])),
	td_mark_parents_change_list(Parents,AbsInt).


insert_literal([], Lit_Key, Literal,[(Lit_Key,Literal)],yes).
insert_literal([(Head_Key,Head_Lit)|Tail], Lit_Key,Literal,Set,Flag) :-
	compare(Order, Head_Lit, Literal),
	insert_literal_(Order,Head_Key,Head_Lit,Tail,Lit_Key,Literal,Set,Flag).

insert_literal_(<,_Head_Key,N/A/C/_G1,_Tail,_Lit_Key,N/A/C/_G2,_NewList,no):-!.
insert_literal_(<, Head_Key,Head_Lit, Tail,Lit_Key, Literal, [Head|Set],Flag) :-
	Head = (Head_Key,Head_Lit),
	insert_literal(Tail,Lit_Key, Literal, Set,Flag).
insert_literal_(=, _Head_Key,_Head_Lit, _Tail, _,_, _NewList,no).
insert_literal_(>,_Head_Key,N/A/C/_G1,Tail,Lit_Key,N/A/C/G2,NewList,yes):-!,
	NewList = [(Lit_Key,N/A/C/G2)|Tail].
insert_literal_(>,Head_Key,Head_Lit,Tail,Lit_Key,Literal,NewList,yes):-
	NewList = [(Lit_Key,Literal),(Head_Key,Head_Lit)|Tail].





%----------------------------------------------------------------------
% mark_parents_change_list(+,+,+)
% mark_parents_change_list(Parents,SCC,AbsInt)
%  This complete has changed. So we add the change in the $change_list
% of all parents. If the parent is in the same SCC then we recursively
% mark its parents as well.
%----------------------------------------------------------------------
mark_parents_change_list([],_,_).
mark_parents_change_list([(_,0)|Rest],SCC,AbsInt):-!,
	mark_parents_change_list(Rest,SCC,AbsInt).
mark_parents_change_list([(Literal,C)|Rest],SCC,AbsInt):-
	atom2data(Literal,N,A,Cl,G),
	(member(N/A,SCC)->
	    make_atom([N,A],Key),
	    current_fact(complete(Key,AbsInt,_,_,_,C,Parents),_),
	    add_change_(C,Literal,N/A/Cl/G,Parents,SCC,AbsInt)
	;
	    write(user,distintos),nl),
	mark_parents_change_list(Rest,SCC,AbsInt).
%----------------------------------------------------------------------%
% add_change_(+,+,+,+,+,+)                                             %
% add_change_(C,Lit_Key,Literal,Parents,SCC,AbsInt)                    %
%                                                                      %
%----------------------------------------------------------------------%
add_change_(C,Lit_Key,Literal,Parents,SCC,AbsInt):-
	current_fact('$change_list'(C,ChList),Ref),!,
	insert_literal(ChList,Lit_Key,Literal,NewList,Flag),
	(Flag == yes ->
	    erase(Ref),
	    asserta_fact('$change_list'(C,NewList)),
	    mark_parents_change_list(Parents,SCC,AbsInt)
	;
	    true).
add_change_(C,Lit_Key,Literal,Parents,SCC,AbsInt):-
	asserta_fact('$change_list'(C,[(Lit_Key,Literal)])),
	mark_parents_change_list(Parents,SCC,AbsInt).



%-------------------------------------------------------------------------
% fixpoint_compute_change(+,+,+,+,+,+,+,-,+)                             %
% fixpoint_compute_change(Changes,SgKey,Sg,Sv,Proj,AbsInt,TempPrime,Prime,Id)  
% It obtains the Prime for the recursive predicate Sg with Call (which   %
% has been assigned to node Id), and the list of nodes it depends on     %
% In doing this it performs an iteration over the recursive clauses of Sg%
% by calling to compute_change/13 and then checks if the fixpoint has    %
% been reached by calling to fixpoint/13. Fixpoint is reached if either  %
% NewList is empty (it means that Id does not depend on any node) or if  %
% Flag is a variable (it means that the information has not been         %
% modified within the iteration)                                         %
% LEntryInf is the list of (Entry,ExtraInfo) couples for each Clause. It %
% will be computed in the first iteration and used in subsequent ones    %
%-------------------------------------------------------------------------

fixpoint_compute_change(Changes,SgKey,Sg,Sv,Proj,AbsInt,
	                                               TempPrime,Prime,Id) :-
        compute_change(Changes,SgKey,Sg,Sv,Proj,AbsInt,
	                         TempPrime,Prime1,Id),
	fixpoint_ch(SgKey,Sg,Sv,Proj,AbsInt,Prime1,Prime,Id),!.

%-------------------------------------------------------------------------
% fixpoint_ch(+,+,+,+,+,+,+,+)
% fixpoint_ch(SgKey,Sg,Sv,Proj,AbsInt,Prime1,Prime,Id)
%  Decides whether we should keep on iterating (the information the 
% complete depends on has changed or not).
%-------------------------------------------------------------------------
fixpoint_ch(SgKey,Sg,Sv,Proj,AbsInt,Prime1,Prime,Id):-
	current_fact('$change_list'(Id,Changes),Ref),
	erase(Ref),
        fixpoint_compute_change(Changes,SgKey,Sg,Sv,Proj,AbsInt,
	                                               Prime1,Prime,Id).
fixpoint_ch(_,_,_,_,_,Prime,Prime,_).

%-------------------------------------------------------------------------
% compute_change(+,+,+,+,+,+,+,-,+).                                     %
% compute_change(Changes,SgKey,Sg,Sv,Proj,AbsInt,TempPrime,Prime,Id)     %
%  We compute a fixpoint iteration.                                      %
%-------------------------------------------------------------------------

compute_change([],_,_,_,_,_,Prime,Prime,_).
% the literal N/A/C/0 means that this literal has been introduced during 
% incremental addition. So the clause must be first checked to see if it 
% applies for the corresponding Subgoal. If it does, it is analyzed entirely
compute_change([(_,N/A/C/0)|Rest],SgKey,Sg,Sv,Proj,AbsInt,
	                                           TempPrime,Prime,Id) :-!,
	make_atom([N,A,C],Clid),
	read_clause(SgKey,_,clause(Head,Vars_u,Clid,Body)),
%	recorded_internal(SgKey,clause(_,Head,Vars_u,Clid,Body),_),
	(
	    clause_applies(Head,Sg)->
	    varset(Head,Hv),
	    sort(Vars_u,Vars),
	    ord_subtract(Vars,Hv,Fv),
	    call_to_entry(AbsInt,Sv,Sg,Hv,Head,Fv,Proj,Entry,ExtraInfo),
%	    erase_previous_memo_tables_and_parents(Body,AbsInt,Clid,Id),
%  not needed as it is the first time we analyse this clause
	    singleton(Entry,LEntry),
	    entry_to_exit(Body,Clid,LEntry,Exit,Vars_u,AbsInt,Id),
	    each_exit_to_prime(Exit,AbsInt,Sg,Hv,Head,Sv,ExtraInfo,Prime1),
	    widen_succ(AbsInt,TempPrime,Prime1,NewPrime),
	    decide_mark_parents(AbsInt,TempPrime,NewPrime,SgKey,Sg,Id,Proj),
	    compute_change(Rest,SgKey,Sg,Sv,Proj,AbsInt,NewPrime,Prime,Id)
	;
	    write(user,'Warning: '), write(user,Rest),nl(user)
        ).

compute_change([(Ch_Key,N/A/C/_)|Rest],SgKey,Sg,Sv,Proj,AbsInt,
	                                           TempPrime,Prime,Id) :-
%%        write(user,'Rec: '), write(user, Ch_Key), nl(user),
	current_fact(memo_table(Ch_Key,AbsInt,Id,_,Vars_u,Entry),_),
	each_abs_sort(Entry,AbsInt,S_Entry),
	make_atom([N,A,C],Clid),
	read_clause(SgKey,_,clause(Head,Vars_u,Clid,Body)),
	advance_in_body(Ch_Key,Body,NewBody),!,
	varset(Head,Hv),
	sort(Vars_u,Vars),
 	ord_subtract(Vars,Hv,Fv),
	call_to_entry(AbsInt,Sv,Sg,Hv,Head,Fv,Proj,_,ExtraInfo),
	erase_previous_memo_tables_and_parents(NewBody,AbsInt,Clid,Id),
	entry_to_exit(NewBody,Clid,S_Entry,Exit,Vars_u,AbsInt,Id),
	each_exit_to_prime(Exit,AbsInt,Sg,Hv,Head,Sv,ExtraInfo,Prime1),
	widen_succ(AbsInt,TempPrime,Prime1,NewPrime),
	decide_mark_parents(AbsInt,TempPrime,NewPrime,SgKey,Sg,Id,Proj),
	compute_change(Rest,SgKey,Sg,Sv,Proj,AbsInt,NewPrime,Prime,Id).


each_call_to_success([Call],RFlag,SgKey,Sg,Sv,HvFv_u,AbsInt,ClId,Succ,F,N,Id):- !,
	project(AbsInt,Sv,HvFv_u,Call,Proj),
	call_to_success(RFlag,SgKey,Call,Proj,Sg,Sv,AbsInt,ClId,Succ,F,N,Id).
each_call_to_success(LCall,RFlag,SgKey,Sg,Sv,HvFv_u,AbsInt,ClId,LSucc,F,N,Id):-
	each_call_to_success0(LCall,RFlag,SgKey,Sg,Sv,HvFv_u,AbsInt,ClId,
                              LSucc,F,N,Id).

each_call_to_success0([],_Flag,_SgK,_Sg,_Sv,_HvFv,_AbsInt,_ClId,[],_F,_N,_NN).
each_call_to_success0([Call|LCall],RFlag,SgKey,Sg,Sv,HvFv_u,AbsInt,ClId,
	              LSucc,F,N,NewN):-
	project(AbsInt,Sv,HvFv_u,Call,Proj),
	call_to_success(RFlag,SgKey,Call,Proj,Sg,Sv,AbsInt,ClId,LSucc0,F,N,_Id),
	append(LSucc0,LSucc1,LSucc),
	each_call_to_success0(LCall,RFlag,SgKey,Sg,Sv,HvFv_u,AbsInt,ClId,
	                      LSucc1,F,N,NewN).



widen_call(AbsInt,SgKey,Sg,F1,Id0,Proj1,Proj):-
	( current_pp_flag(widencall,off) -> fail ; true ),
	widen_call0(AbsInt,SgKey,Sg,F1,Id0,[Id0],Proj1,Proj), !,
	fixpoint_trace('result of widening',Id0,F1,SgKey,Sg,Proj,_).

widen_call0(AbsInt,SgKey,Sg,F1,Id0,Ids,Proj1,Proj):-
	widen_call1(AbsInt,SgKey,Sg,F1,Id0,Ids,Proj1,Proj).
widen_call0(AbsInt,SgKey,Sg,F1,Id0,Ids,Proj1,Proj):-
	current_pp_flag(widencall,com_child),
	widen_call2(AbsInt,SgKey,Sg,F1,Id0,Ids,Proj1,Proj).


widen_call1(AbsInt,SgKey,Sg,F1,Id0,Ids,Proj1,Proj):-
	current_fact(complete(SgKey0,AbsInt,Sg0,Proj0,_Prime0,Id0,Fs0)),
	( SgKey=SgKey0,
	  member((F1,_NewId0),Fs0)
	-> Sg0=Sg,
	   abs_sort(AbsInt,Proj0,Proj0_s),
	   abs_sort(AbsInt,Proj1,Proj1_s),
	   widencall(AbsInt,Proj0_s,Proj1_s,Proj)
	 ; member((_F1,NewId0),Fs0),
	   \+ member(NewId0,Ids),
	   widen_call1(AbsInt,SgKey,Sg,F1,NewId0,[NewId0|Ids],Proj1,Proj)
	).

widen_call2(AbsInt,SgKey,Sg,F1,_Id,_Ids,Proj1,Proj):-
	current_fact(complete(SgKey,AbsInt,Sg0,Proj0,_Prime0,_,Fs0)),
	member((F1,_Id0),Fs0),
	Sg0=Sg,
%	same_fixpoint_ancestor(Id0,[Id0],AbsInt),
	abs_sort(AbsInt,Proj0,Proj0_s),
	abs_sort(AbsInt,Proj1,Proj1_s),
	widencall(AbsInt,Proj0_s,Proj1_s,Proj).



%-------------------------------------------------------------------------
% query(+,+,+,+,+,+,+,-)
%-------------------------------------------------------------------------

:- doc(query(AbsInt,QKey,Query,Qv,RFlag,N,Call,Succ),
	"The success pattern of @var{Query} with @var{Call} is
         @var{Succ} in the analysis domain @var{AbsInt}. The predicate
         called is identified by @var{QKey}, and @var{RFlag} says if it
         is recursive or not. The goal @var{Query} has variables @var{Qv},
         and the call pattern is uniquely identified by @var{N}.").

query(AbsInt,QKey,Query,Qv,RFlag,N,Call,Succ) :-
	project(AbsInt,Qv,Qv,Call,Proj),
	fixpoint_trace('init fixpoint',N,N,QKey,Query,Proj,_),
	call_to_success(RFlag,QKey,Call,Proj,Query,Qv,AbsInt,0,Succ,N,0,_Id),
	!,
 	fixpoint_trace('exit goal',_Id,query(N),(QKey,QKey),Query,Succ,AbsInt).
query(_AbsInt,_QKey,_Query,_Qv,_RFlag,_N,_Call,_Succ):-
% should never happen, but...
	error_message("SOMETHING HAS FAILED!"),
	fail.
