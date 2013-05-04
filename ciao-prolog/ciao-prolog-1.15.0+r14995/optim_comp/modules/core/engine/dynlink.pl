% TODO: document: low level loading of single modules
:- module(dynlink, [], [pure]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_basic)).
:- use_module(engine(data_facts)).
:- use_module(engine(internals)).

:- '$native_include_c_source'(.(dynlink)).

% TODO: merge with part of rt_exp?

:- export(module_timestamp/2).
:- '$props'(module_timestamp/2, [impnat=cbool(prolog_module_timestamp)]).

:- export(static_module/1).
:- '$props'(static_module/1, [impnat=cbool(prolog_static_module)]).

:- export(load_module__nocheck/2).
% load_module__nocheck(BytecodeName, NativeSOName)
:- '$props'(load_module__nocheck/2, [impnat=cbool(prolog_load_module)]).

:- export(unload_module__nocheck/1).
% unload_module__nocheck(Module)
:- '$props'(unload_module__nocheck/1, [impnat=cbool(prolog_abolish_module)]).

:- export(link_pack/1). 
% link_pack(BytecodePackName) 
% # Load a single file of concatenated bytecode files
% TODO: unstable if fails
% TODO: loaded code is always static, correct?
:- '$props'(link_pack/1, [impnat=cbool(prolog_load_module_pack)]).
