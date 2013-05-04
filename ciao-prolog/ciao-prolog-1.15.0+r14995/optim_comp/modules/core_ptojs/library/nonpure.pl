:- package(nonpure).
% :- doc(title, "Default definitions for non-pure modules").
% :- doc(author, "Jose F. Morales").

% [Incomplete version of package library(nonpure) for pl2js]

% TODO: add a package for 'meta_predicate' support and other to enable
%       atom-based terms (enabled by default on JS backend but not in
%       the other backends since we do not have there low-level
%       module-qualified atoms and nested modules yet)
:- '$pragma'(user_term_on_undefined). % TODO: add a package for this

% TODO: Necessary for executables, do not include by default
%       (see ptojs__compiler:compile_and_link/1)
:- use_module(engine(internals)). % TODO: do not define by default

:- use_module(engine(arithmetic)).
%:- use_module(engine(atomic_basic)).
:- use_module(engine(basic_props)).
:- use_module(engine(basiccontrol)).
:- use_module(engine(data_facts)).
:- use_module(engine(exceptions)).
%:- use_module(engine(io_aux)).
%:- use_module(engine(io_basic)). % TODO: fix streams
%:- use_module(engine(prolog_flags)).
%:- use_module(engine(streams_basic)).
%:- use_module(engine(system_info)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_compare)).
:- use_module(engine(term_typing)).
%:- use_module(engine(hiord_rt), [call/1]). % TODO: define just for call/1
%:- use_module(engine(debugger_support), [srcdbg_spy/7]).

% Uncomment to use timestamps for choicepoints instead of variables
% TODO: check that timestamps for variables is working properly
%:- compilation_fact(coarse_timestamp).

% Uncomment to make arithmetic hooks basal predicates
% (faster)
:- compilation_fact(basal_arith_hooks).
