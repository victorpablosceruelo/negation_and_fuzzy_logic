:- package(lprolog).
:- include(library(lprolog(lprolog_ops))).
:- use_package(hiord). % for predicates with a varibale functor - G(X) 
:- use_module(library(dynamic)). % for assert/retract used in =>
:- use_module(library(odd), [undo/1]). % for "undo" used in =>
:- use_module(library(lists), [append/3]). % append used in "solve"
:- use_module(library(prolog_sys), [new_atom/1]).

:- use_module(library(lprolog(lprolog_rt))).
:- load_compilation_module(library(lprolog(lprolog_tr))).

% TODO: probably not compatible with other packages
:- add_sentence_trans(sentence_trans/3, 750). % TODO: Right priority?
