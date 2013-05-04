:- module(_, [modular_analyze/3, resource_analyze/2], [assertions,
		isomodes, regtypes, nativeprops]).

:- use_module(program(p_abs), [get_all_module_cycles/2]).
:- use_module(plai(intermod), [set_modules_analyzed/1]).
:- use_module(ciaopp(driver), [module/1, analyze/1]).

:- doc(module, "This module performs the resource bounds analysis for
  	   both lower and upper bounds for the modular fashion.").

:- doc(bug, "0. The resource modular analysis has all the pieces to be
   connected with PLAI. However, since most of these pieces are too naive
   they are not really connected. Let me know if you are interesting in and
   I can connect them quickly (JNL).").

:- doc(bug, "1. No cycles are supported among modules.").
:- doc(bug, "2. The analysis is not compositional.").

:- pred modular_analyze(+Type, +TopLevel, -Info) # "Performs the resource
   analysis of the program unit for which @var{Module} is the top-level
   module. All modules in the program unit are loaded one-by-one in a
   bottom-up traversal of the intermodule dependency graph.".

modular_analyze(resources, TopLevel, Info) :-
	set_modules_analyzed([]),
	resource_analyze(TopLevel, Info).

:- pred resource_analyze(+TopLevel, -Info).
resource_analyze(TopLevel, _Info) :-
	get_all_module_cycles(TopLevel, ModulesList),
	resource_analyze_(ModulesList).

resource_analyze_([]).
resource_analyze_([SCC|SCCs]) :-
	not_cycles(SCC, SCC_), !,
	module(SCC_),
	analyze(resources),
	resource_analyze_(SCCs).
resource_analyze_([SCC|_]) :-
	!,
	inform_user(['There is a cycle: ', SCC]).

not_cycles([Mod], Mod).
