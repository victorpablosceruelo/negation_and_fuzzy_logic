:- module(debugger_support, [], [pure, assertions]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_basic)).

:- use_module(engine(hiord_rt), ['$meta_call'/1, call/1]).
:- use_module(engine(internals), ['$global_vars_get'/2, '$global_vars_set'/2]).

:- '$native_include_c_source'(.(debugger_support)).

:- export('$retry_cut'/2).
:- '$props'('$retry_cut'/2, [impnat=cbool(retry_cut)]).

:- export('$spypoint'/3).
:- '$props'('$spypoint'/3, [impnat=cbool(spypoint)]).

:- export('$debugger_mode'/0).
'$debugger_mode' :-
	'$debugger_state'(State, State),
	arg(2, State, Mode), !,
	'$set_debugger_mode'(Mode).
'$debugger_mode' :- % if debugger state is not set
	'$set_debugger_mode'(off).

:- export('$set_debugger_mode'/1).
:- '$props'('$set_debugger_mode'/1, [impnat=cbool(set_debugger_mode)]).

:- export('$debugger_state'/2).
'$debugger_state'(S0, S) :-
	'$global_vars_get'(1, S0),
	'$global_vars_set'(1, S).
	
:- export(reset_debugger/1).
reset_debugger(State) :-
	'$debugger_state'(State, s(off,off,1000000,0,[])),
	'$debugger_mode'.

:- export(srcdbg_spy/6).
:- pred srcdbg_spy/6 # "Performing source level debugging, all goals
   are expanded to this. This is currenlty done for all interpreted
   code.".
:- doc(hide,srcdbg_spy/6).
:- meta_predicate srcdbg_spy(primitive(goal),?,?,?,?,?).

srcdbg_spy(G, _, _, _, _, _) :- '$meta_call'(G).

:- export('$notrace_call'/1).
% Trace a goal body code (calling debugger:trace/1 for each literal)
% TODO: note: indeed, it calls the goal and enables tracing for all subgoals
:- '$props'('$notrace_call'/1, [impnat=cinsnp(code_notracecall1)]).

:- export('$start_trace'/0).
:- '$props'('$start_trace'/0, [impnat=cbool(code_start_trace)]).

:- export('$stop_trace'/0).
:- '$props'('$stop_trace'/0, [impnat=cbool(code_stop_trace)]).

