:- package(fuzzy).
:- include(library(clpr(clpr))).
:- include(library(fuzzy(ops))).
:- use_module(library(fuzzy(faggr))).
:- load_compilation_module(library(fuzzy(fuzzy_tr))).
% note: priority after clpr (750)
% TODO: Not compatible with fsyntax (uses ':=')
:- add_sentence_trans(fuzzy_pred/3, 760).
