:- package(argnamesvv).
% Experimental fork of argnames package

%:- load_compilation_module(library(argnames(argnames_trans))).
:- load_compilation_module(library(argnamesvv(argnamesvv_trans))).
:- add_sentence_trans(argnames_def/3, 630). % TODO: Right priority?
:- add_term_trans(argnames_use/3, 630). % TODO: Right priority?
:- op(150, xfx, [$]).
:- op(950, xfx, (=>)).
:- op(1150, fx, [functor_class]).
