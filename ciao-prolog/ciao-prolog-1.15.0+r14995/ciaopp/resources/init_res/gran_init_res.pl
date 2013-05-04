:- module(gran_init_res, [cls_init_gran_system/11],
	    [assertions, resources(inferres_decl)]).


:- use_module(library(messages), [debug_message/2]).
:- use_module(program(clidlist),
	    [
		is_directive/3,
		is_clause/4
	    ]).
:- use_module(resources(resources_basic)).
:- use_module(resources(gran_res(gran_trans_res)),
	    [
		flat_seq_body/2,
		seq/2
	    ]).
:- use_module(resources(gran_res(gran_table_res)),
	    [insert_gran_field/4]).
:- use_module(resources(init_res(initsystem_res)),
	    [call_graph/5, insert_symbol_table/4]).
:- use_module(resources(init_res(compute_scc_res)),
	    [strongly_connected_component/2]).

%
%  This file contains the procedures for initializing the system.
%

%-------------------------------------------------------------------------
%  Initialize the system by initializing the built-in table and
%  reading  in the source program, and building the program table,
%  symbol table, call graph and the strongly connected components
%  of the call graph.
%


%%---------------------------------------------------------------------
% Declaration of operators.
%%---------------------------------------------------------------------

:- op(950, xfy, [(&)]).

:- op(975, xfx, [(=>)]).

% - init_buildin_table(BT): initialize the table with builtins (modes,
%   measures,...)
% - input_ca_analysis_clauses returns modes, measures, clauses, dicts, etc
%   of the different clauses.

cls_init_gran_system(Clauses, _Bound, Dicts, BT, ST, SCCG, GT, GCG, Directs,
	    DirDicts, Error) :-
%	init_buildin_table(Bound, BT),
	input_ca_analysis_clauses(Clauses, Dicts, PT, ST, GT, Directs,
	    DirDicts, Error1),
	call_graph(PT, BT, ST, CG,  Error2),
	call_graph(PT, BT, ST, GCG, Error3),
	Error is Error1 * Error2 * Error3,
	(
	    Error =:= 1 ->
	    strongly_connected_component(CG, SCCG)
	;
	    true
	).

input_ca_analysis_clauses([], [], [], _, _, [], [], 1) :- !.
input_ca_analysis_clauses([InClause|ClauseList], [Dict|RDic], PT, ST, GT,
	    Directs, DirDicts, Error) :-
	input_ca_analysis_clause(InClause, Dict, PT, ST, GT, Directs,
	    DirDicts, PT1, Directs1, DirDicts1, Error1),
	input_ca_analysis_clauses(ClauseList, RDic, PT1, ST, GT,
	    Directs1, DirDicts1, Error2),
	Error is Error1 * Error2.

% :- check comp input_ca_analysis_clause/11 + not_fails.

input_ca_analysis_clause(InClause, Dict, PT, ST, GT, Directs, DirDicts,
	    PT1, Directs1, DirDicts1, Error1) :-
% Remove keys from body literals. 
	debug_message("INPUT CLAUSE: ~q", [InClause]),
	NokeysCl:Keys = InClause,
	debug_message("REWRITED CLAUSE: ~q", [NokeysCl]),
	clause_type_ca(NokeysCl, Type, NClause),
%% filter_clause_or_directive(Type,ST,SeqClause,Error1),
	sequentialize(Type, NClause, SeqClause),
% sequentialize could be modified to report
% whether the clause is parallel or not.
	PT=[SeqClause:Keys|PT1],
	insert_symbol_table(Type, ST, SeqClause:Keys, Error1),
% It might be convenient to flatten Clause before insert it.
% PLG
%	clause_translate_to_internal(NClause, InternalNClause), % Commented out by EMM
% 	insert_gran_table(Type, GT, InternalNClause, Dict),
	insert_gran_table(Type, GT, NClause, Dict),
% was: insert_gran_table(Type,GT,NClause,Dict),
	actualize_directive_list_and_dict(InClause, Dict, Directs, Directs1,
	    DirDicts, DirDicts1).

actualize_directive_list_and_dict(InClause, _InDict, Directs, Directs1,
	    DirDicts, DirDicts1) :-
	is_clause(InClause, _H, _B, _Clid),
	!,
	Directs = Directs1,
	DirDicts = DirDicts1.
actualize_directive_list_and_dict(InClause, _InDict, Directs, Directs1,
	    DirDicts, DirDicts1) :-
	is_directive(InClause, D, _Clid),
	functor(D, F, A),
	(
	    (F/A) = (mode/2)
	;
	    (F/A) = (measure/2)
	),
	!,
	Directs = Directs1,
	DirDicts = DirDicts1.
actualize_directive_list_and_dict(InClause, InDict, Directs, Directs1,
	    DirDicts, DirDicts1) :-
	Directs = [InClause|Directs1],
	DirDicts = [InDict|DirDicts1].

clause_type_ca(directive(D), decl, (:- D)) :- !. % directive
%% clause_type_ca(Cl, 1, (:- D)):-
%%    Cl = directive(D), 
%%    !,
%%    assertz(Cl). % directive
clause_type_ca(clause(H, B), Type, Clause) :- !,
	(
	    B == true ->
	    Type = fact,
	    Clause = H % fact
	; Type = rule,
	    Clause = (H :- B) % rule
	).

% Commented by PLG on 28-Jul-99 (currently is dead code)
%% %-------------------------------------------------------------------------
%% %  Read in the source program and store the clauses in the program table,
%% %  symbol table and call graph.
%% %
%% read_par_program(Files,BT,PT,ST,GT,Error) :-
%% 	read_par_program(Files,BT,PT,[],ST,GT,Error).
%% 
%% read_par_program([],_,PT,PT,_,_,1).
%% read_par_program([File|Fs],BT,PT,NPT,ST,GT,Error) :-
%% 	see(File),
%% 	r_par_program(BT,PT,PT1,ST,GT,Error1),
%% 	seen,
%% 	read_par_program(Fs,BT,PT1,NPT,ST,GT,Error2),
%% 	Error is Error1*Error2.
%% 
%% 
%% r_par_program(BT,PT,NPT,ST,GT,Error) :-
%% 	input_clause(Clause,Dict),  % read the dictionary as well.
%%  	(Clause \== end_of_file -> 
%% 		(clause_type(Clause,Type),
%% 		 sequentialize(Type,Clause,SeqClause),
%%                  % Se puede modificar sequentialize para que informe
%% 		 % si la clausula es paralela o no.
%%                  PT=[SeqClause|PT1], 
%% 		 insert_symbol_table(Type,ST,SeqClause,Error1),
%%                  % Puede ser conveniente aplanar Clause antes de insertarla.
%%                  insert_gran_table(Type,GT,Clause,Dict),
%%                  r_par_program(BT,PT1,NPT,ST,GT,Error2),
%% 		 Error is Error1*Error2);
%% 		(NPT = PT,
%% 		 Error = 1)).

% End Commented by PLG on 28-Jul-99

%
% Read a clause and build the  variables dictionary.
%

% Commented by PLG on 28-Jul-99 (currently is dead code)
%% %---------------------------------------------------------------------
%% input_clause(Answer,Dict):- !,
%%         current_input(Stream),
%% 	ciao:read_top_level(Stream,Answer,Variables),
%% 	ciao:flatten_dic(Variables,Dict,[]).
%% 
%% %---------------------------------------------------------------------
% End Commented by PLG on 28-Jul-99

%%---------------------------------------------------------------------
% Sequentialize a clause.
%%---------------------------------------------------------------------


sequentialize(decl, Clause, Clause).
sequentialize(fact, Clause, Clause).
sequentialize(rule, Clause, SeqClause) :-
	sequentialize_(Clause, SeqClause).



%%---------------------------------------------------------------------
% Sequentialize a rule.
%%---------------------------------------------------------------------

sequentialize_((Head :- Body), (Head :- FlatBody)) :-
	seq(Body, SeqBody), flat_seq_body(SeqBody, FlatBody).

% 
%  The following procedures insert the clauses into the gran table.
% 

:- pred insert_gran_table/4 :: clause_t
	* list(gran_entry) * clause_ppkey_t * term #
	"Insert a clause and its variable dictionary into the gran table.".
insert_gran_table(decl, _,  _,      _). % A directive.
insert_gran_table(rule, ST, Clause, Dict) :- % A clause.
	insert_gran_table_clause(ST, Clause, Dict).
insert_gran_table(fact, ST, Clause, Dict) :- % A fact.
	insert_gran_table_clause(ST, Clause, Dict).

insert_gran_table_clause(ST, Clause, Dict) :-
	clause_head(Clause, Head),
	functor(Head, F, N),
	insert_gran_field(ST, F/N, par_clauses,    Clause),
	insert_gran_field(ST, F/N, var_dictionary, Dict).
