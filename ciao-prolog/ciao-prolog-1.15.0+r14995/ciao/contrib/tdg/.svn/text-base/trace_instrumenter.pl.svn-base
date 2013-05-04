:- module(trace_instrumenter,[instrument_traces/2,instrument_traces/1,instrument_traces_/1],
	                     [assertions,clpq]).

:- doc(title,"Automatic trace instrumentation for Prolog programs").

:- doc(author, "M. Zamalloa").

:- doc(module,"This module provides the operations to automatically perform trace 
	instrumentation for a given Ciao module. It can also automatically tranform
	all arithmetic operations into their clpq counterparts").


:- use_module(library(compiler), [use_module/1]).
:- use_module(library(filenames), [no_path_file_name/2, basename/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(format), [format/2]).
:- use_module(library(prolog_sys), [statistics/2]).
:- use_module(library(write), [write/1]).
:- use_module(library(lists), [length/2, append/3]).

:- use_module(library(vndict), [create_pretty_dict/2]).
:- use_module(program(p_unit), [program/2]).
:- use_module(program(clause_db), [source_clause/3]).
:- use_module(program(clidlist), 
 	[inverse_rewrite_source_program/2, 
 	 rewrite_source_clause/4,clause_key/2]).
:- use_module(program(itf_base_db), [defines/3, exports/2]).
:- use_module(program(itf_db), [curr_file/2]).
:- use_module(ciaopp(api(api_order)), [pr_order_clean/0, pr_order_get/1, pr_order_set/1]).
:- use_module(spec(unfold_operations), [body2list/2, list2body/2]).
:- use_module(library(clpq(clpqtr)), [translate_clp/2]).
:- use_module(ciaopp(api(api_module)), [add_package_to_output/1]).
:- use_module(ciaopp(driver), [module/2]).
:- use_module(ciaopp(printer), [output/1]).

:- data pr_counter/2.
:- data clpq/1.

:- pred instrument_traces/1 #"Given a Prolog program in file @var{File} it writes in another 
	module the program in @var{File} instrumented with traces. 
        Default options are [clpq(off)]".
instrument_traces(File) :- instrument_traces(File,[clpq(off)]).

:- pred instrument_traces/2 #"Given a Prolog program in file @var{File} and a set of options 
	@var{Options} it writes in another module the program in @var{File} instrumented with traces".
instrument_traces(File,Options) :-
	basename(File,Module),
% 	use_module(Module), % I do not know if this is necessary
	module(Module,_),
	statistics(runtime,[T1,_]),
	atom_concat([Module,'_trace','.pl'],OutputFile),
	statistics(runtime,[T2,_]),
	Time is T2 - T1,
	instrument_traces_(Options),
	format("Trace instrumentation: ~q\n",[Time]),
 	output(OutputFile).

:- pred instrument_traces_/1 #"This version assumes that the module to be trace instrumented is
	already loaded. It might be very appropiate when being used as a post-processing after
	a ciaopp analysis or transformation. A set of options is passed as parameter.".
instrument_traces_(Flags) :-
	cleanup,
	set_flags(Flags),
	set_fact(pr_counter(_,_)),
%	retractall_fact(source_clause(_,directive(_),_)),
	program(P,_),
	retractall_fact(source_clause(_,clause(_),_)),
	inverse_rewrite_source_program(P,P_p),
	member(clause(H,B),P_p),
	update_pr_counter(H,RuleId),
	body2list(B,BList),
	instrument_body(BList,BList_p,BodyTraces),
	instrument_head(H,RuleId,BodyTraces,H_p),
	list2body(BList_p,B_p),
	clause_key(H_p,NewClId), 
	rewrite_source_clause(H_p,B_p,NewClId,NewCl),
	create_pretty_dict(NewCl,NewDict),
	assertz_fact(source_clause(NewClId,NewCl,NewDict)),
	fail.
instrument_traces_(_) :- 
	update_pr_order,
	update_module_header,
	cleanup.

update_pr_counter(H,Id) :-
	split_atom(H,_M,F,_),
	retract_fact(pr_counter(CurrPred,LastId)),
	(F == CurrPred -> Id is LastId + 1
	                ; Id is 1),
	assertz_fact(pr_counter(F,Id)).

instrument_body([],[],[]).
instrument_body([NegAtom|B],[\+Atom_p|B_p],[Tr|Traces]) :-
	split_atom(NegAtom,_,\+,[Atom]),!,
	split_atom(Atom,M,F,Args),
	curr_file(_,CurrM),
	(M == CurrM -> (append(Args,[Tr],ArgsTr), Atom_p =..[F|ArgsTr])
	             ; (Tr = b, arith_to_clp(Atom,Atom_p))),
	instrument_body(B,B_p,Traces).
instrument_body([Atom|B],[Atom_p|B_p],[Tr|Traces]) :-
	split_atom(Atom,M,F,Args),
	curr_file(_,CurrM),
	(M == CurrM -> (append(Args,[Tr],ArgsTr), Atom_p =..[F|ArgsTr])
	             ; (Tr = b, arith_to_clp(Atom,Atom_p))),
	instrument_body(B,B_p,Traces).

instrument_head(H,Id,BTraces,H_p) :-
	split_atom(H,M,F,Args),
	atom_number(Idatom,Id),
	atom_concat([F,'_',Idatom],F_p),
	Tr =..[F_p|BTraces],
	append(Args,[Tr],ArgsPlusTr),
	build_atom(H_p,M,F,ArgsPlusTr).

arith_to_clp(A,A_p) :-
	current_fact(clpq(on)),
	split_atom(A,arithmetic,F,Args),!,
	clp_conversion(arithmetic,F,Args,A_p).
arith_to_clp(A,A_p) :-
	current_fact(clpq(on)),
	split_atom(A,term_basic,F,Args),!,
	clp_conversion(term_basic,F,Args,A_p).
arith_to_clp(A,A).

clp_conversion(_,is,[A,B],A.=.B) :- !.
clp_conversion(_,\=,[A,B],A.<>.B) :- !.
clp_conversion(_,=\=,[A,B],A.<>.B) :- !.
clp_conversion(_,\==,[A,B],A.<>.B) :- !.
clp_conversion(_,<,[A,B],A.<.B) :- !.
clp_conversion(_,>,[A,B],A.>.B) :- !.
clp_conversion(M,F,Args,A_p) :-
	build_atom(A_p,M,F,Args).

update_pr_order :-
	pr_order_get(PrOrder),
	update_pr(PrOrder,PrOrder_p),
	pr_order_set(PrOrder_p).

update_pr([],[]).
update_pr([Pr|Ps],[Pr_p|Ps_p]) :-
	Pr =..[F|Args],
	Pr_p =..[F,_|Args],
	update_pr(Ps,Ps_p).

update_module_header :-
	curr_file(_,ModName),
	retract_fact(itf_base_db:defines(QPred,Arity,ModName)),
	atom_concat([ModName,':',PredName],QPred),
	generate_qualified(ModName,PredName,Arity,QPred,QGoal),
	retract_fact(itf_base_db:exports(QGoal,ModName)),
	NewArity is Arity + 1,
	generate_qualified(ModName,PredName,NewArity,QPred_p,QGoal_p),
	assertz_fact(itf_base_db:defines(QPred_p,NewArity,ModName)),
	assertz_fact(itf_base_db:exports(QGoal_p,ModName)),
	fail.
update_module_header :-
	(current_fact(clpq(on)) -> add_package_to_output(clpq) ; true).

generate_qualified(ModName,PredName,Arity,QPred,QGoal) :-
	atom_concat([ModName,':',PredName],QPred),
	length(Args,Arity),
	QGoal =..[QPred|Args].

split_atom(Atom,M,F,Args) :-
	Atom =..[Atom_f|Args],
	atom_concat([M,:,F],Atom_f),!.
split_atom(Atom,'',Atom_f,Args) :-
	Atom =..[Atom_f|Args].

build_atom(Atom,M,F,Args) :-
	atom_concat([M,:,F],Atom_f),
	Atom =..[Atom_f|Args].

cleanup :-
	retractall_fact(pr_counter(_,_)),
	retractall_fact(clpq(_)).

set_flags([Flag|Fs]) :-
	set_flag(Flag),
	set_flags(Fs).
set_flags([]).

set_flag(clpq(V)) :- set_fact(clpq(V)).

