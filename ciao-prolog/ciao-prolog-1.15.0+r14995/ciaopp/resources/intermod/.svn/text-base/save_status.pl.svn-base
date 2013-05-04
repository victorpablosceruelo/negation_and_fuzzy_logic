:- module(_, [
% 	save_resources_results/4,
% 	restore_resource_results/4,
% 	save_sizes_results/4,
% 	restore_size_results/4,
		save_plai_db/0,
		save_infer_db/0,
		load_plai_db/0,
		load_infer_db/0
	    ], [assertions, regtypes]).

:- doc(module, "This module saves/loads the analysis results of a module
from/into the PLAI database into/from another database. The problem is that
after a module has been loaded into CiaoPP, the PLAI database is cleaned
up. The good solution is to use the .reg files used in the PLAI
inter-modular analysis. Meanwhile, I have defined some predicates which
does it. -JNL").

:- use_module(plai(plai_db), [complete/7, lub_complete/6, memo_table/6, 
		memo_lub/5]).
:- use_module(infer(infer_db), [domain/1, inferred/3, point_info/5]).

%--------------------------------------------------------------------
% - JNL

% :- data stored_cost/4.
% :- data stored_size/4.
% save_sizes_results(Pred,Approx,Measure,Sol):-
% 	Pred = F/A,
% 	functor(Goal,F,A),
% 	asserta_fact(stored_size(Goal,Approx,Measure,Sol)).

% restore_size_results(Goal,Approx,Measure,Sols):-
% 	current_fact(stored_size(Goal,Approx,Measure,Sol)),!,
% 	Sols = [Sol].
% restore_size_results(_,_,_,[]):-!.


% save_resources_results(Pred,Resources,Costs,Approx):-	
% 	Pred = F/A,
% 	functor(Goal,F,A),
% 	asserta_costs(Resources,Costs,Goal,Approx).

% asserta_costs([],[],_Goal,_Approx).
% asserta_costs([R|Rs],[C|Cs],Goal,Approx):-
% 	asserta_fact(stored_cost(Goal,Approx,R,C)),
% 	asserta_costs(Rs,Cs,Goal,Approx).

% restore_resource_results(Goal,Approx,Res,Vals):-
% 	current_fact(stored_cost(Goal,Approx,Res,Vals)),!.
% restore_resource_results(_,_,_,[]):-!.


%%------------------------------------------------------------------------------%%
% The following code allows saving plai_db and infer_db after a new module
% is loaded. Temporary code.
%%------------------------------------------------------------------------------%%
:- data p_abs_complete/7.
:- data p_abs_lub_complete/6.
:- data p_abs_memo_table/6.
:- data p_abs_memo_lub/5.

save_plai_db :-
	save_plai_db_complete,
	save_plai_db_lub_complete,
	save_plai_db_memo_table,
	save_plai_db_memo_lub.

save_plai_db_complete :-
	current_fact(complete(SgKey, AbsInt, Sg, Proj, Prime, Id, Parents)),
	insert_p_abs(p_abs_complete(SgKey, AbsInt, Sg, Proj, Prime, Id,
		Parents)),
	fail.
save_plai_db_complete :- !.

save_plai_db_lub_complete :-
	current_fact(lub_complete(SgKey, AbsInt, Lub, Sg, Proj, Prime)),
	insert_p_abs(p_abs_lub_complete(SgKey, AbsInt, Lub, Sg, Proj, Prime)),
	fail.
save_plai_db_lub_complete :- !.

save_plai_db_memo_table :-
	current_fact(memo_table(PointKey, AbsInt, Id, Child, Vars_u, Call)),
	insert_p_abs(p_abs_memo_table(PointKey, AbsInt, Id, Child, Vars_u,
		Call)),
	fail.
save_plai_db_memo_table :- !.

save_plai_db_memo_lub :-
	current_fact(memo_lub(PointKey, AbsInt, Lub, Vars_u, Call)),
	insert_p_abs(p_abs_memo_lub(PointKey, AbsInt, Lub, Vars_u, Call)),
	fail.
save_plai_db_memo_lub :- !.

:- data p_abs_inferred/3.
:- data p_abs_domain/1.
:- data p_abs_point_info/5.

save_infer_db :-
	save_infer_db_inferred,
	save_infer_db_domain,
	save_infer_db_point_info.

save_infer_db_inferred :-
	current_fact(inferred(Analyzer, Key, Info)),
	insert_p_abs(p_abs_inferred(Analyzer, Key, Info)),
	fail.
save_infer_db_inferred :- !.

save_infer_db_domain :-
	current_fact(domain(Analyzer)),
	insert_p_abs(p_abs_domain(Analyzer)),
	fail.
save_infer_db_domain :- !.

save_infer_db_point_info :-
	current_fact(point_info(Key, AbsInt, Vars, FVars, Info)),
	insert_p_abs(p_abs_point_info(Key, AbsInt, Vars, FVars, Info)),
	fail.
save_infer_db_point_info :- !.


load_plai_db :-
	load_plai_db_complete,
	load_plai_db_lub_complete,
	load_plai_db_memo_table,
	load_plai_db_memo_lub.
% 	retractall_fact(p_abs_complete(_,_,_,_,_,_,_)),
% 	retractall_fact(p_abs_lub_complete(_,_,_,_,_,_)),
% 	retractall_fact(p_abs_memo_table(_,_,_,_,_,_)),
% 	retractall_fact(p_abs_memo_lub(_,_,_,_,_)).

load_plai_db_complete :-
	current_fact(p_abs_complete(SgKey, AbsInt, Sg, Proj, Prime, Id,
		Parents)),
	insert_p_abs(complete(SgKey, AbsInt, Sg, Proj, Prime, Id, Parents)),
	fail.
load_plai_db_complete :- !.

load_plai_db_lub_complete :-
	current_fact(p_abs_lub_complete(SgKey, AbsInt, Lub, Sg, Proj, Prime)),
	insert_p_abs(lub_complete(SgKey, AbsInt, Lub, Sg, Proj, Prime)),
	fail.
load_plai_db_lub_complete :- !.

load_plai_db_memo_table :-
	current_fact(p_abs_memo_table(PointKey, AbsInt, Id, Child, Vars_u,
		Call)),
	insert_p_abs(memo_table(PointKey, AbsInt, Id, Child, Vars_u, Call)),
	fail.
load_plai_db_memo_table :- !.

load_plai_db_memo_lub :-
	current_fact(p_abs_memo_lub(PointKey, AbsInt, Lub, Vars_u, Call)),
	insert_p_abs(memo_lub(PointKey, AbsInt, Lub, Vars_u, Call)),
	fail.
load_plai_db_memo_lub :- !.


load_infer_db :-
	load_infer_db_inferred,
	load_infer_db_domain,
	load_infer_db_point_info.
% 	retractall_fact(p_abs_inferred(_,_,_)),
% 	retractall_fact(p_abs_domain(_)),
% 	retractall_fact(p_abs_point_info(_,_,_,_,_)).

load_infer_db_inferred :-
	current_fact(p_abs_inferred(Analyzer, Key, Info)),
	insert_p_abs(inferred(Analyzer, Key, Info)),
	fail.
load_infer_db_inferred :- !.

load_infer_db_domain :-
	current_fact(p_abs_domain(Analyzer)),
	insert_p_abs(domain(Analyzer)),
	fail.
load_infer_db_domain :- !.

load_infer_db_point_info :-
	current_fact(p_abs_point_info(Key, AbsInt, Vars, FVars, Info)),
	insert_p_abs(point_info(Key, AbsInt, Vars, FVars, Info)),
	fail.
load_infer_db_point_info :- !.


insert_p_abs(p_abs_complete(SgKey, AbsInt, Sg, Proj, Prime, Id, Parents)) :-
	current_fact(p_abs_complete(SgKey, AbsInt, Sg, Proj, Prime, Id,
		Parents)),
	!.
insert_p_abs(p_abs_complete(SgKey, AbsInt, Sg, Proj, Prime, Id, Parents)) :-
	asserta_fact(p_abs_complete(SgKey, AbsInt, Sg, Proj, Prime, Id,
		Parents)),
	!.
insert_p_abs(complete(SgKey, AbsInt, Sg, Proj, Prime, Id, Parents)) :-
	current_fact(complete(SgKey, AbsInt, Sg, Proj, Prime, Id, Parents)), !.
insert_p_abs(complete(SgKey, AbsInt, Sg, Proj, Prime, Id, Parents)) :-
	asserta_fact(complete(SgKey, AbsInt, Sg, Proj, Prime, Id, Parents)), !.
insert_p_abs(p_abs_lub_complete(SgKey, AbsInt, Lub, Sg, Proj, Prime)) :-
	current_fact(p_abs_lub_complete(SgKey, AbsInt, Lub, Sg, Proj, Prime)),
	!.
insert_p_abs(p_abs_lub_complete(SgKey, AbsInt, Lub, Sg, Proj, Prime)) :-
	asserta_fact(p_abs_lub_complete(SgKey, AbsInt, Lub, Sg, Proj, Prime)),
	!.
insert_p_abs(lub_complete(SgKey, AbsInt, Lub, Sg, Proj, Prime)) :-
	current_fact(lub_complete(SgKey, AbsInt, Lub, Sg, Proj, Prime)), !.
insert_p_abs(lub_complete(SgKey, AbsInt, Lub, Sg, Proj, Prime)) :-
	asserta_fact(lub_complete(SgKey, AbsInt, Lub, Sg, Proj, Prime)), !.
insert_p_abs(p_abs_memo_table(PointKey, AbsInt, Id, Child, Vars_u, Call)) :-
	current_fact(p_abs_memo_table(PointKey, AbsInt, Id, Child, Vars_u,
		Call)),
	!.
insert_p_abs(p_abs_memo_table(PointKey, AbsInt, Id, Child, Vars_u, Call)) :-
	asserta_fact(p_abs_memo_table(PointKey, AbsInt, Id, Child, Vars_u,
		Call)),
	!.
insert_p_abs(memo_table(PointKey, AbsInt, Id, Child, Vars_u, Call)) :-
	current_fact(memo_table(PointKey, AbsInt, Id, Child, Vars_u, Call)), !.
insert_p_abs(memo_table(PointKey, AbsInt, Id, Child, Vars_u, Call)) :-
	asserta_fact(memo_table(PointKey, AbsInt, Id, Child, Vars_u, Call)), !.
insert_p_abs(p_abs_memo_lub(PointKey, AbsInt, Lub, Vars_u, Call)) :-
	current_fact(p_abs_memo_lub(PointKey, AbsInt, Lub, Vars_u, Call)), !.
insert_p_abs(p_abs_memo_lub(PointKey, AbsInt, Lub, Vars_u, Call)) :-
	asserta_fact(p_abs_memo_lub(PointKey, AbsInt, Lub, Vars_u, Call)), !.
insert_p_abs(memo_lub(PointKey, AbsInt, Lub, Vars_u, Call)) :-
	current_fact(memo_lub(PointKey, AbsInt, Lub, Vars_u, Call)), !.
insert_p_abs(memo_lub(PointKey, AbsInt, Lub, Vars_u, Call)) :-
	asserta_fact(memo_lub(PointKey, AbsInt, Lub, Vars_u, Call)), !.
insert_p_abs(p_abs_inferred(Analyzer, Key, Info)) :-
	current_fact(p_abs_inferred(Analyzer, Key, Info)), !.
insert_p_abs(p_abs_inferred(Analyzer, Key, Info)) :-
	asserta_fact(p_abs_inferred(Analyzer, Key, Info)), !.
insert_p_abs(inferred(Analyzer, Key, Info)) :-
	current_fact(inferred(Analyzer, Key, Info)), !.
insert_p_abs(inferred(Analyzer, Key, Info)) :-
	asserta_fact(inferred(Analyzer, Key, Info)), !.
insert_p_abs(p_abs_domain(Analyzer)) :-
	current_fact(p_abs_domain(Analyzer)), !.
insert_p_abs(p_abs_domain(Analyzer)) :-
	asserta_fact(p_abs_domain(Analyzer)), !.
insert_p_abs(domain(Analyzer)) :-
	current_fact(domain(Analyzer)), !.
insert_p_abs(domain(Analyzer)) :-
	asserta_fact(domain(Analyzer)), !.
insert_p_abs(p_abs_point_info(Key, AbsInt, Vars, FVars, Info)) :-
	current_fact(p_abs_point_info(Key, AbsInt, Vars, FVars, Info)), !.
insert_p_abs(p_abs_point_info(Key, AbsInt, Vars, FVars, Info)) :-
	asserta_fact(p_abs_point_info(Key, AbsInt, Vars, FVars, Info)), !.
insert_p_abs(point_info(Key, AbsInt, Vars, FVars, Info)) :-
	current_fact(point_info(Key, AbsInt, Vars, FVars, Info)), !.
insert_p_abs(point_info(Key, AbsInt, Vars, FVars, Info)) :-
	asserta_fact(point_info(Key, AbsInt, Vars, FVars, Info)), !.
