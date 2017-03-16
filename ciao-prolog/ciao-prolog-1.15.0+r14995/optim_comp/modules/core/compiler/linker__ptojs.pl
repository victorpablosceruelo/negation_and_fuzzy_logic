% TODO: Transform into a module, mimick linker__bytecode, share code, and write an interface for them

{
:- fluid middefs :: accum.
% TODO: just for executables, not modules
% equivalent to linker__bytecode:create_init/7
emit_exec_entry(TopModuleRs, _MainModuleR) :-
	RootR = ~module_s.lookup_module(root),
%	trust(ModuleR instance_of module_s),
	InternalsR = ~RootR.lookup_nested_module('internals', any),
	% TODO: Define real static methods and modules (for predicates)
	( BootR = ~InternalsR.lookup_pred('__boot__', 0, any) ->
	    BootR2 = ~BootR.get_PU
	; throw(bug_no_boot_pred)
	),
	%
	emit_module_init(TopModuleRs),
	% Initial worker creation and call to '__boot__'/0 predicate
	% (equivalent to 'start' function in ciao/engine/start.c)
	StartR = ~module_s.query_ref_BK(~module_s.lookup_module(root), '__start__', 0),
	GWV = ~my_var_new(_,rawmem('global_worker')),
	SolveR = ~module_s.query_ref_BU('worker', 'solve', 1),
	trust(BootR2 instance_of ref_PUM),
	BootR3 = ~BootR2.query_ref_BU_buildstr,
	BootId = ~msym_modentry_id(BootR3),
	middefs.add(bcode(StartR, [], [
	  js_stats(call(mb('$r', 'prepare'), [])),
	  % TODO: do not use 'new' here...
	  move(new(ctor_lookup(class, ':'('basiccontrol', 'worker_stacks')), []), GWV),
	  bcall(SolveR, [GWV, call(mb(mb(ctor_lookup(class, 'internals'), 'prototype'), BootId), [])])
	])),
	% Declare the global worker
	GW = 'global_worker',
	MainBlockR = ~module_s.query_ref_BK(~module_s.lookup_module(root), '__mainblock__', 0),
	middefs.add(bcode(MainBlockR, [], RBCode)),
	call((
	  wcode :: accum(RBCode),
          wcode.add(js_stats(vardecl(GW))),
	  % Emit code to execute __ciao_start__() (wrapper for '__start__'/0 in ciao_runtime.js)
	  % (otherwise it has to be executed from JS by hand)
	  ( InternalsR.get_prop(shell_exec) ->
	      wcode.add(js_stats(call('__ciao_start__', [])))
	  ; true
	  )
	)).

emit_module_init([]).
emit_module_init([R|Rs]) :-
	middefs.add(module_init(R)),
	emit_module_init(Rs).
}.
