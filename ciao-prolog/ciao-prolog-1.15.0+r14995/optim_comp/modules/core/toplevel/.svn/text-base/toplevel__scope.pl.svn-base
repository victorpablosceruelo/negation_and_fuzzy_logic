:- use_package([]).

:- use_module(toplevel).
% TODO: temporary disabled jf
%:- use_module(library(compiler),
%        [make_po/1,
%         set_debug_mode/1, set_nodebug_mode/1]).
%:- use_module(library(compiler(exemaker)),
%        [make_actmod/2, force_lazy/1, undo_force_lazy/1,
%         dynamic_search_path/1]).
:- use_module(engine(rt_exp), [multifile/1]).
:- use_module(compiler(dynload), [unload/1]).
:- use_module(library(debugger), 
        [trace/0, notrace/0, debug/0, nodebug/0, spy/1, nospy/1,
 	nospyall/0, debugging/0, leash/1, maxdepth/1, 
	breakpt/6,nobreakpt/6,nobreakall/0,list_breakpt/0, 
	call_in_module/2]).
:- use_module(library(operators), [op/3]).
:- use_module(engine(hiord_rt), [call/1]).
:- use_module(library(debugger(debugger_support)), 
	['$start_trace'/0, '$stop_trace'/0]).

'$toplevel_module'(ThisModule) :-
        this_module(ThisModule).

:- '$pragma'(allow_runtime_expansions).
'$toplevel_call'(X) :- '$start_trace', call(X), '$stop_trace'.

aborting :- '$toplevel_abort'.
