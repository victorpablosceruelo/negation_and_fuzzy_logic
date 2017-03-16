:- module(_, [], [pure]).

% Use this module to load translation modules and obtain meta-expanded
% predicate names. This module isolates the effects of
% meta-programming to just one module, without interfering with the
% rest of code.
%
% Author: Jose F. Morales

% TODO: use compiler_objects?
% TODO: compilation modules are never unloaded

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(term_compare)).
:- use_module(engine(arithmetic)).
:- use_module(engine(atomic_basic)).
:- use_module(engine(data_facts)).
:- use_module(engine(rt_exp)). % TODO: if I do not include this, the compiler breaks (because some predicates are not found _after_ module expansion)

:- use_module(compiler(dynload)). % use_module/1

% note: mode that does not include any additional runtime library
:- use_package(fsyntax).
:- use_package(compiler(complang_mini)).

% ---------------------------------------------------------------------------

:- '$pragma'(allow_runtime_expansions).

:- public do_use_module/1.
% load a module for semantic translation
do_use_module(CompUspec) :- use_module(CompUspec).

:- public do_meta_exp_spec/2.
% TODO: fix: out(primitive(spec)) does not work! (see compiler/frontend.pl)
:- meta_predicate do_meta_exp_spec(?, out(spec)).
do_meta_exp_spec(Spec0, Spec) :- '$meta_exp'(spec, Spec0, Spec1), Spec = Spec1.

