:- module(trace_fixp,
	[ fixpoint_trace/7,
	  fixpoint_trace/1,
	  trace_fixp/1,
	  cleanup/0,
	  memotable_trace/3,
	  show_spypoint_info/0,
	  show_updated_memotable/3
	],
	[ assertions
	]).

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(lists)).
:- use_module(library(messages)).
:- use_module(library(write), [numbervars/3]).

:- use_module(plai(view_fixp)).  % windowing

:- data fixpoint_trace/1,
	fixpoint_info/3.

clean:-
	end_view,
	retractall_fact(fixpoint_trace(_)),
	cleanup.

cleanup:-
	retractall_fact(fixpoint_info(_,_,_)),
	clean_fixpoint_graph.

%% --------------------------------------------------------------------
%% tracing the fixpoint:

fixpoint_trace(Mess,Id,F,SgKey,Sg,Proj,Childs):-
	( fixpoint_trace(info) ->
	  update_fixpoint_info(Mess,Id)
	; true
	),
	( fixpoint_trace(trace) ->
	  trace_fixpoint(Mess,Id,F,Sg,Proj)
	; true
	),
	( fixpoint_trace(view) ->
	  view_fixpoint(Mess,Id,F,SgKey,Sg,Proj,Childs)
	; true
	).


%% --------------------------------------------------------------------
%% toggle tracing:
:- doc(trace_fixp(X),
	"Toggle a trace of the fixpoint computation during analysis. The trace
         can add up information on the analysis (X=info), show certain relevant
        spy points during analysis (X=trace), and/or display the analysis 
	 graph which is being constructed (X=view). The information added up 
	 can be seen at end of analysis with @tt{ciao:fixpoint_info.}").
:- pred trace_fixp(X) : var(X)
	=> sublist(X,[no,info,trace,view])
	# "Mode for querying the current flag value." .
:- pred trace_fixp(X) 
	: member(X,[no,info,trace,view])
	+ not_further_inst(X)
	# "Mode for setting the current flag to a single value." .
:- pred trace_fixp(X) 
	: sublist(X,[no,info,trace,view])
	+ not_further_inst(X)
	# "Mode for setting the current flag to several values." .
:- prop sublist/2. % for documenting

trace_fixp(F):-
	var(F), !,
	findall(S,fixpoint_trace(S),F).
trace_fixp(F):-
	trace_fixp0(F).

trace_fixp0(X):-
	fixpoint_trace(X), !.
trace_fixp0(no):-
	clean.
trace_fixp0(view):-
	start_view,
	asserta_fact(fixpoint_trace(view)).
trace_fixp0(trace):-
	asserta_fact(fixpoint_trace(trace)).
trace_fixp0(info):-
	asserta_fact(fixpoint_trace(info)).
trace_fixp0([]).
trace_fixp0([F|Fs]):-
	trace_fixp0(F),
	trace_fixp0(Fs).

%% --------------------------------------------------------------------
%% trace:

:- push_prolog_flag(multi_arity_warnings,off).

trace_fixpoint(Mess,Id,_L,Sg,Proj):-
	trace_fixpoint(Mess), !,
	simple_message("~w for node ~w",[Mess,Id]),
	\+ \+ ( numbervars(p(Sg,Proj),0,_),
	        simple_message("~q   ~q",[Sg,Proj])
	      ).
trace_fixpoint(_Mess,_Id,_L,_Sg,_Proj).

trace_fixpoint('init fixpoint').
trace_fixpoint('visit goal').
trace_fixpoint('exit goal').
trace_fixpoint('non-recursive initiated').
trace_fixpoint('non-recursive completed').
trace_fixpoint('complete used').
trace_fixpoint('fixpoint used').
trace_fixpoint('approx used').
trace_fixpoint('approx unchanged').
trace_fixpoint('fixpoint initiated').
trace_fixpoint('fixpoint completed').
trace_fixpoint('fixpoint approximated').
trace_fixpoint('fixpoint iteration').
trace_fixpoint('result of widening').
trace_fixpoint('builtin completed').
trace_fixpoint('external call completed').

:- pop_prolog_flag(multi_arity_warnings).

%% --------------------------------------------------------------------
%% info:

:- push_prolog_flag(multi_arity_warnings,off).

update_fixpoint_info(Mess,Id):-
	update_fixpoint_info(Mess), !,
	update_fixpoint_info0(Mess,Id).
update_fixpoint_info(_Mess,_Id).

update_fixpoint_info0(Mess,Id):-
	retract_fact(fixpoint_info(Id,Mess,N)), !,
	N1 is N+1,
	asserta_fact(fixpoint_info(Id,Mess,N1)).
update_fixpoint_info0(Mess,Id):-
	asserta_fact(fixpoint_info(Id,Mess,1)).

update_fixpoint_info('non-recursive completed').
update_fixpoint_info('approx used').
update_fixpoint_info('approx unchanged').
update_fixpoint_info('fixpoint initiated').
update_fixpoint_info('fixpoint iteration').

:- pop_prolog_flag(multi_arity_warnings).
%------------------- MORE FIXPOINT TRACE PREDICATES -----------------------%
% Shows certain spy points of the analysis. It only shows this information
% if and only if trace_fixp/1 includes the option trace.
show_spypoint_info:-
    fixpoint_trace(info),!,
    simple_message('The following information contains certain spy points of the analysis'),
    show_spypoint_info_.
show_spypoint_info.

show_spypoint_info_:-
	current_fact(fixpoint_info(Id,Mess,N)),
	simple_message("~q ~q ~q",[Id,Mess,N]),
	fail.
show_spypoint_info_.


%------------------- MEMOTABLE TRACE PREDICATES -------------------%
:- use_module(program(itf_db), [curr_file/2]).
:- use_module(library(filenames), [basename/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(plai(fixpo_plai), [approx/6, fixpoint/6, '$depend_list'/3]).
:- use_module(plai(plai_db), [complete/7]).
:- use_module(library(write), [write/2]).

memotable_trace(IdMess,Id,SgKey):-
	(fixpoint_trace(trace), trace_memotable(IdMess,Mess)),!,	
	simple_message("~w for node ~w and ~w",[Mess,Id,SgKey]),
	show_updated_memotable(IdMess,Id,SgKey).
memotable_trace(_IdMess,_Id,_SgKey).
	

trace_memotable(1,'MT: Updated from approx to complete').
trace_memotable(2,'MT: Added non-recursive method as complete').
trace_memotable(3,'MT: Added as fixpoint').
trace_memotable(4,'MT: Updated from fixpoint to complete').
trace_memotable(5,'MT: Updated from fixpoint to approx').
trace_memotable(6,'MT: Added builtin as complete').
trace_memotable(7,'MT: Updated from fixpoint to fixpoint').
trace_memotable(8,'MT: Added external call as complete').

show_updated_memotable(Mess,Id,SgKey):-
	curr_file(Path,_),
	basename(Path,Basefile),
	Ext = '.MEMOTABLE',
	atom_concat([Basefile,Ext],Output),
	open(Output,append,Stream),!,	
	nl(Stream),
	trace_memotable(Mess,Mess0),
	write(Stream,Mess0),write(Stream,' for node '),
	write(Stream,Id),write(Stream,' and '),write(Stream,SgKey),nl(Stream),
	show_dependencies(Stream),nl(Stream),
	write(Stream,'-------------------- MEMO TABLE --------------------'), 
	nl(Stream),
	show_complete(Stream),
	show_fixpoint(Stream),
	show_approx(Stream).

show_complete(Stream):-
	current_fact(complete(_SgKey,_,Subg,Proj,Prime,_Id,_Fs),_Ref),
	display_subgoal(Stream,Subg),write(Stream,':'),
	write(Stream,' COMPLETE '),
	write(Stream,Proj),write(Stream,' -> '),
	write(Stream,Prime),nl(Stream),
	write(Stream,'----------------------------------------------------'), 
	nl(Stream),
	fail.
show_complete(_).

show_fixpoint(Stream):-
	current_fact(fixpoint(_SgKey,Subg,Proj,Prime,_Id,_Fs),_Ref),
	display_subgoal(Stream,Subg),write(Stream,':'),
	write(Stream,' FIXPOINT '),
	write(Stream,Proj),write(Stream,' -> '),
	write(Stream,Prime),nl(Stream),
	write(Stream,'----------------------------------------------------'), 
	nl(Stream),
	fail.
show_fixpoint(_).

show_approx(Stream):-
	current_fact(approx(_SgKey,Subg,Proj,Prime,_Id,_Fs),_Ref),
	display_subgoal(Stream,Subg),write(Stream,':'),
	write(Stream,' APPROX '),
	write(Stream,Proj),write(Stream,' -> '),
	write(Stream,Prime),nl(Stream),
	write(Stream,'----------------------------------------------------'), 
	nl(Stream),
	fail.
show_approx(_).

:- use_module(library(terms_vars), [varset/2]).
display_subgoal(Stream,Subgoal):-
	varset(Subgoal,Vars),
	functor(Subgoal,F,_),
	Goal =.. [F|Vars],
	write(Stream,Goal).

show_dependencies(Stream):-
	current_fact('$depend_list'(Id,SgKey,List)),
	write(Stream,'depend('),write(Stream,Id),write(Stream,'-'),
	write(Stream,SgKey),write(Stream,','),write(Stream,List),
	write(Stream,')'),nl(Stream),
	fail.
show_dependencies(_).
	