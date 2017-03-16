:- module(codegen_pcpe,
	[
	    print_all_solutions/2,
	    print_solution/4,
	    print_intermediate_configuration/3
	],[]).

:- use_package(assertions).
:- use_package(api(ciaopp_api)).


:- doc(title,"Code Generation of solutions").

:- doc(author, "Claudio Ochoa").
:- doc(author, "Germ@'{a}n Puebla").

:- doc(module," This module generates code from configurations
	(both final and non-final).").


:- use_module(profiling_pcpe, [profiler_ready/2]).
:- use_module(filenames_pcpe).
:- use_module(db_pcpe).
:- use_module(arg_filtering_pcpe).
:- use_module(benchmarking, [global_time_ellapsed/3]).
:- use_module(customization, [local/2]).
:- use_module(evaluation_pcpe, [eval_solutions/3]).
:- use_module(control_pcpe, [unfold/4]).
:- use_module(stats_pcpe, [get_spec_params/3]).
:- use_module(comments_pcpe, [add_comments_to_program/1, write_sh/3]).
:- use_module(spec(spec_support), [change_call/3]).
:- use_module(ciaopp(preprocess_flags), [current_pp_flag/2]).
:- use_module(program(clidlist), 
 	[rewrite_source_all_clauses/2,
 	 cleanup_clidlist/0]).
:- use_module(library(lists)).
:- use_module(library(filenames), [no_path_file_name/2]).
:- use_module(library(terms_check), [instance/2, variant/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(program(itf_db), [current_itf/3, curr_file/2]).
:- use_module(program(p_unit), [replace_program/2]).
:- use_module(ciaopp(printer), [output/2]). 
:- use_module(spec(codegen), [create_all_dicts/2]).
:- use_module(spec(unfold_operations), [list2body/2]).
:- use_module(api(comment_db), [cleanup_comment_db/0]).
:- use_module(api(api_module), [add_action/1, remove_action/1]).
:- use_module(api(api_direc_assrt), [get_directive/1]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(api(api_predcl), [add_defined_pred/2]).
%:- use_module( library( printer ) ).
:- use_module(library(prolog_sys), [statistics/2]). 
:- use_module(spec(mem_usage), 
	[reset_mem_usage/0, ask_mem_usage/2]).

:- doc(bug,"1. defined predicates should be initialized every time
	a new program is going to be outputted, i.e., in
	print_solution. In order to do so, the corresponding
	(cleaning) predicate should be defined in itf_db or, better
	yet, in the correspoding api.").

:- doc(bug,"2. some directives, like
	push_prolog_flag(multi_arity_warnings,off) and its
	counterpart, need to be printed at specific locations of the
	code. This is not currently being done. In order to avoid
	warnings in the residual program, the predicate
	add_directives/0 checks whether this directive is in the
	source program, and in that case it will add only a push,
	without a pop").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

analysis_time([Unf,Abs,Ora]):-
	ask_time(local_ctrl,UT),
	ask_time(global_ctrl,GT),	
	ask_time(oracle,OT),
	Unf = unf_time(UT),
	Abs = abs_time(GT),
	Ora = oracle_time(OT).



:- pred print_all_solutions(+Sols,-Info) #"Generates an output file
	for every final configuration in @var{Sols}".

print_all_solutions([],[]):-!,
	inform_user(['{transformed by PCPE(0 solutions found) in 0.0 msecs.}']),
	inform_user(['{No evaluation is needed. Best solution is original program.}']).
print_all_solutions(Sols,Info):-
	Info = [analysis([lenpath(LenPath),ties(Ties),confs(States),TimeEval|AnaTime]),codegen([Time,Mem,V,C,E]),evaluation(Eval_Info)],
	(oracle_stats(LenPath,Ties,States) -> true ; LenPath=0,Ties=0,States=0),
	analysis_time(AnaTime),
	statistics(runtime,[GT1,_]),
	reset_mem_usage,
	print_solution(Sols,1,Counter,CurrInfo),
	statistics(runtime,[GT2,_]),
	global_time_ellapsed(GT2,GT1,TT),
	ask_mem_usage(Delta,_Details),!,
	ask_time(int_eval,TEv),
	Time= total_time(TT),
	TimeEval = bnb_evaltime(TEv),
	Mem = memory(Delta),
	V = versions(Counter),
 	(retract_fact(max_id(Confs))->true;Confs=0),
 	(retract_fact(evals(Evals))->true;Evals=0),
	C = states(Confs),
	(bnb ->
	    E = evals(Evals)
	;
	    E = evals(Counter)),
	inform_user(['{transformed by PCPE(',Counter,' solutions) in ', TT, ' msec.}']),
	Solutions = solutions(CurrInfo),
	eval_solutions(Counter,Solutions,[Best,Fits|Eval_Info]),
	write_sh(Sols,Fits,Best).



:- pred print_solution(+Sols,+,-,-) #"Loops over all solutions in
	@var{Sols}, generating an specialized output file for each of
	them".

print_solution([],Counter,Counter1,[]):-!,
	Counter1 is Counter - 1.
print_solution([Sol|Sols],Counter,C1,[(Counter,[type(Type),perc(Info)])|T]):-
	gen_code_and_dicts(Sol,Spec_Cls,Spec_Ds),
	replace_program(Spec_Cls,Spec_Ds),
	functor(Counter,NFunctor,_),
	atom_number(Functor,NFunctor),
	add_action(Functor),
	get_output_file_name(Counter,F),	    
	get_spec_params(Sol,Info,Type),
	add_directives,
	add_comments_to_program(Sol),
	output(F,[]),
	remove_action(Functor),
	cleanup_comment_db,
	Counter1 is Counter + 1,
	print_solution(Sols,Counter1,C1,T).


add_directives:-
	get_directive(direc(_,_,push_prolog_flag(multi_arity_warnings,off),_,_)),!,
	add_directive(push_prolog_flag(multi_arity_warnings,off)).
add_directives.	


:- pred print_intermediate_configuration(+Sols,+,+) #"Takes a closed
	intermediate configuration and prints it for later
	evaluation".

print_intermediate_configuration(Conf,Dir_type,Atoms):-
	gen_code_and_dicts(Conf,Spec_Cls,Spec_Ds),
	replace_program(Spec_Cls,Spec_Ds),
	get_tmp_file_name(F),
	extra_directives(Dir_type,Atoms,Directs),
	add_comments_to_program(Conf),
	output(F,[]),
	clean_up(Directs),!.

clean_up(D):-
	cleanup_comment_db,
	erase_all_drts(D).

erase_all_drts([]).
erase_all_drts([Body|T]):-
	erase_directive(Body),
	erase_all_drts(T).


extra_directives(none,_,[]).
extra_directives(prof,Atoms,Drt):-
	profiler_ready(Atoms,Drt).
extra_directives(imports,Atoms,Drt):-
	not_yet_defined(Atoms,Preds),
	atoms_to_imports(Preds,Imports),
	add_imports(Imports,Drt).



:- pred not_yet_defined(+Atoms,-) # "Given a list @var{Atoms} of atoms
	not yet visited, it filters those atoms that are exported".

:- push_prolog_flag(multi_arity_warnings,off).

not_yet_defined(A,P):-
	findall(Exp,exported(Exp),Exported),
	not_yet_defined(A,Exported,P).


not_yet_defined([],_Exp,[]).
not_yet_defined([A|T],Exp,R):-
	functor(A,Functor,Arity),
	functor(B,Functor,Arity),
	member(B,Exp),!,
	not_yet_defined(T,Exp,R).
not_yet_defined([A|T],Exp,[A|R]):-	
	not_yet_defined(T,Exp,R).

:- pop_prolog_flag(multi_arity_warnings).

atoms_to_imports([],[]).
atoms_to_imports([A|T],[N/Ar|R]):-
	functor(A,Name,Ar),
	atom_concat([_,':',N],Name),
	atoms_to_imports(T,R).

:- pred add_imports(+Imports,-) #" Adds a directive import of
	non-yet-specialized predicates @var{Imports} from the original
	program to the resulting program".

add_imports([],[]):-!.
add_imports(Imports,[import(Mod,Imports)]):-
	get_src_name(Orig),
	no_path_file_name(Orig,Mod),
	add_directive(import(Mod,Imports)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred gen_code_and_dicts(+Solution,-Code,-Dicts) # "Generates
	@var{Code} and its dictionary @var{Dict} for a given
	@var{Solution}".

gen_code_and_dicts(Solution,Code,Dicts):-
	codegen(Solution,Code),
	create_all_dicts(Code,Dicts).

:- pred codegen(+Conf,-Code) #"Generates @var{Code} for a given final
	configuration @var{Conf}".


codegen(e([],Visited,_Value),Code):-
	reverse(Visited,Visited_r),
	findall(Exp,exported(Exp),Exported),
	filter_exported(Exported,Visited_r,Exp_Atoms),
	compute_resultants(Visited_r,[],Vis0),
 	gen_predicate_names(Vis0,[],Renamings,Exp_Atoms,Program),
        clean_up_filters,
	(current_pp_flag(inter_opt_arg_filt,on) ->
	    create_filters(Renamings,Exp_Atoms);true),
	cleanup_clidlist,
	(bnb ->
	    assert_prednames(Renamings);true),
	rewrite_source_all_clauses(Program,NProgram),
	replace_calls(NProgram,Renamings,Code).

:- pred filter_exported(+A,+B,-C) : (list(A) , list(B) , list(C)) #
	"Takes a list @var{A} of exported atoms, and creates a list
	containing those atoms from @var{B} that are variant from any
	exported atom".

filter_exported([],_Vis,[]).
filter_exported([E|T],Vis,[A|R]):-
	functor(E,Functor,Arity),
	functor(A,Functor,Arity),
	member(t(A,_AG,_),Vis),
	variant(E,A),!,
	filter_exported(T,Vis,R).

	
:- pred compute_resultants(+A,+B,-C) : (list(A) , list(B) , list(C)) #
	"Takes a list of final configurations @var{A}, and generates
	code by using the same unfolding rule used during
	specialization".

compute_resultants([],_,[]).
compute_resultants([t(A,AG,GU)|Tuples],Coded,[t(A,AG,GU,no_resultants)|Vis0]):-
	functor(AG,Name,Arity),
	functor(BG,Name,Arity),
	member(BG,Coded),
	variant(AG,BG),!,
	compute_resultants(Tuples,Coded,Vis0).
compute_resultants([t(A,AG,GU)|Tuples],Coded,[t(A,AG,GU,Resultants)|Vis0]):-
	GU = [(_,Ind_U)|_],
	local(Ind_U,Unfold),
	unfold(Unfold,AG,Res,_UnfStats),
	flatten_bodies(Res,AG,Resultants),
	compute_resultants(Tuples,[AG|Coded],Vis0).


:- pred flatten_bodies(+Cl,+,-) #"Takes a list of clauses @var{Cl} and
	converts their bodies from list to conjunctions".

:- push_prolog_flag(multi_arity_warnings,off).


flatten_bodies([],H,[clause(H,B)]):-
	!,
	list2body(['basiccontrol:fail'],B).
flatten_bodies(R,_,Res):- 
	flatten_bodies(R,Res).

flatten_bodies([],[]).
flatten_bodies([clause(Head,Body)|Cls],[clause(Head,NBody)|Res]):-
	list2body(Body,NBody),
	flatten_bodies(Cls,Res).

:- pop_prolog_flag(multi_arity_warnings).

add_defined(Key):-
	functor( Key , F , A ),
	(current_itf( defines, F, A) ->
	    true
	;
	    curr_file(_,M),
	    add_defined_pred(Key,M)).


:- pred gen_predicate_names(+Confs,+,-,+,-) #"Perform renaming of
	specialized predicates having the same name and different
	code".

gen_predicate_names([],_,[],_,_).
gen_predicate_names([t(A,AG,GU,no_resultants)|Tuples],Counters,[t(A,AG,GU,noname)|NTuples],Exported,Vis):-
	!,
	gen_predicate_names(Tuples,Counters,NTuples,Exported,Vis).
gen_predicate_names([t(A,AG,GU,Res)|Tuples],Counters,[t(A,AG,GU,AG)|NTuples],Exported,Vis1):-	
	member(Exp,Exported),
	Exp == A,
	!,
	append(Res,MoreVis,Vis1),
	add_defined(AG),
	gen_predicate_names(Tuples,Counters,NTuples,Exported,MoreVis).
gen_predicate_names([t(A,AG,GU,Res)|Tuples],Counters,[t(A,AG,GU,NAG)|NTuples],Exp,Vis1):-	
	functor(AG,Functor,Arity),
	update_counters(Functor,Arity,Value1,Counters,NCounters),
	name(Value1,S1),
	append([95],S1,SuffixS),
	name(Suffix,SuffixS),
	atom_concat([Functor,Suffix],NewFunctor),
	change_call(AG,NAG,NewFunctor),
	replace_heads(Res,NewFunctor,NRes),	
	append(NRes,MoreVis,Vis1),
	add_defined(NAG),
	gen_predicate_names(Tuples,NCounters,NTuples,Exp,MoreVis).

update_counters(Functor,Arity,Value1,Counters,NCounters):-
	search(Counters,Functor,Arity,Value,TmpCounters),
	Value1 is Value + 1,
	NCounters = [c(Functor,Arity,Value1)|TmpCounters].

search([],_Functor,_Arity,0,[]).
search([c(Functor,Arity,Value)|Counters],Functor,Arity,Value,TmpCounters):-!,
	TmpCounters = Counters.
search([C|Counters],Functor,Arity,Value,TmpCounters):-
	TmpCounters = [C|MoreCounters],		      
	search(Counters,Functor,Arity,Value,MoreCounters).


replace_heads([],_NewFunctor,[]).
replace_heads([clause(Head,Body)|Res],NewFunctor,NRes):-
	NRes = [clause(NHead,Body)|MoreRes],
	change_call(Head,NHead,NewFunctor),
	replace_heads(Res,NewFunctor,MoreRes).


:- pred assert_prednames(+Ren) # "Assert all predicate names of
	resulting program from list of renamings @var{Ren}".

assert_prednames(Ren) :-
	retractall_fact(predname(_,_,_)),
	assert_all_prednames(Ren,1).

assert_all_prednames([],_).
assert_all_prednames([t(_A,_AG,_,noname)|T],Id):-
	assert_all_prednames(T,Id).
assert_all_prednames([t(A,_AG,_,NAG)|T],Id):-
	(current_fact(filter(_,NAG,NL)) ->
	    functor(NL,Name,Arity)
	;
	    functor(NAG,Name,Arity)
	),
 	assertz_fact(predname(Id,A,Name/Arity)),
	NId is Id + 1,
	assert_all_prednames(T,NId).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- pred replace_calls(+Orig_Prog,+Renamings,-Sp_Prog) # "Once the
	clauses for all specialized definitions are collected, we have
	to call the appropriate implementation of each predicate in
	all calls which appear in the specialized clauses.".

replace_calls([],_Renamings,[]).
replace_calls([clause(Head,true:Id):ClId|Clauses],Ren,[NewCl|NewClauses]):-!,
	NewCl = clause(NHead,NBody):ClId,
	(current_fact(filter(_,Head,NHead)) ->
	    true
	;
	    NHead = Head),
	NBody = true:Id,
	replace_calls(Clauses,Ren,NewClauses).
replace_calls([clause(Head,Body):ClId|Clauses],Ren,[NewCl|NewClauses]):-
	NewCl = clause(NHead,NBody):ClId,
	(current_fact(filter(_,Head,NHead)) ->
	    true
	;
	    NHead = Head),
	replace_calls_in_body(Body,Ren,NBody),
	replace_calls(Clauses,Ren,NewClauses).

replace_calls_in_body((L,R),Ren,(NL,NR)):-!,
	replace_calls_in_literal(L,Ren,NL),
	replace_calls_in_body(R,Ren,NR).
replace_calls_in_body(L,Ren,NL):-
	replace_calls_in_literal(L,Ren,NL).

replace_calls_in_literal(!,_,!):-!.
replace_calls_in_literal(\+($(L,B:OtherKey,C)):Key,Ren,NBody):-
	NBody = \+($(NL,B:OtherKey,C)):Key,
	locate(L,Ren,NewName,Arity),!,
	functor(NL0,NewName,Arity),
	change_call(L,NL0,NewName),
	(current_fact(filter(_,NL0,NL)) ->
	    true
	;
	    NL = NL0
	),!.
%% replace_calls_in_literal('aggregates:findall'(Var,$(L,B:SonKey,C),List):Key,Ren,NBody):-
%% 	!,
%% 	NBody = 'aggregates:findall'(Var,$(NL,B:SonKey,C),List):Key,
%% 	current_fact(memo_table(SonKey,_,_,Son,_,_)),
%% 	(Son = no ->
%% 	    NL = L
%% 	;
%% 	    locate_spec_definition(Son,NewName,_),
%% 	    change_call(L,NL0,NewName),
%% 	    (current_fact(filter(_,NL0,NL)) ->
%% 	        true
%% 	    ;
%% 		NL = NL0
%% 	    )
%% 	),!.
%% replace_calls_in_literal('aggregates:bagof'(Var,$(L,B:SonKey,C),List):Key,Ren,NBody):-
%% 	!,
%% 	NBody = 'aggregates:bagof'(Var,$(NL,B:SonKey,C),List):Key,
%% 	current_fact(memo_table(SonKey,_,_,Son,_,_)),
%% 	(Son = no ->
%% 	    NL = L
%% 	;
%% 	    locate_spec_definition(Son,NewName,_),
%% 	    change_call(L,NL0,NewName),
%% 	    (current_fact(filter(_,NL0,NL)) ->
%% 	        true
%% 	    ;
%% 		NL = NL0
%% 	    )
%% 	),!.
%% replace_calls_in_literal('aggregates:setof'(Var,$(L,B:SonKey,C),List):Key,Ren,NBody):-
%% 	!,
%% 	NBody = 'aggregates:setof'(Var,$(NL,B:SonKey,C),List):Key,
%% 	current_fact(memo_table(SonKey,_,_,Son,_,_)),
%% 	(Son = no ->
%% 	    NL = L
%% 	;
%% 	    locate_spec_definition(Son,NewName,_),
%% 	    change_call(L,NL0,NewName),
%% 	    (current_fact(filter(_,NL0,NL)) ->
%% 	        true
%% 	    ;
%% 		NL = NL0
%% 	    )
%% 	),!.
replace_calls_in_literal(L:Key,Ren,NL:Key):-
	locate(L,Ren,NewName,Arity),!,
	functor(NL0,NewName,Arity),
	change_call(L,NL0,NewName),
	(current_fact(filter(_,NL0,NL)) ->
	    true
	;
	    NL = NL0
	).
replace_calls_in_literal(Lit,_Ren,Lit):-
	debug('Hmm :'),
	debug(Lit).

:- pred locate(+L,+Ren,-,-) # "Looks up @var{L} in the list of
	renamings @var{Ren}, and returns its new name".

locate(L,Ren,NewName,Arity):-
	locate_(L,Ren,NN,Arity),
	(NN==noname ->
	    locate_noname(L,Ren,NewName,Arity)
	;
	    NewName = NN
	).

locate_(L,Ren,NewName,Arity):-
	functor(L,Name,Arity),
	functor(NL,Name,Arity),
	member(t(NL,_AG,_GU,NAG),Ren),
	variant(NL,L),!,
	functor(NAG,NewName,_).
locate_(L,Ren,NewName,Arity):-
	locate_noname(L,Ren,NewName,Arity).
locate_noname(L,Ren,NewName,Arity):-
	functor(L,Name,Arity),
	functor(NL,Name,Arity),
	member(t(NL,AG,_GU,NAG),Ren),
	NAG \== noname,
	instance(L,AG),!,
	functor(NAG,NewName,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

