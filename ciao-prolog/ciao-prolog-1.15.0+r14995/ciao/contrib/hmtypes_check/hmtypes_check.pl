:- package(hmtypes_check).

:- include(library(hmtypes_check(hmtypes_check_ops))).
:- include(library(hmtypes_check(hmtypes_check_runtime_db))).
:- load_compilation_module(library(hmtypes_check(hmtypes_check_tr))).
:- add_sentence_trans(hmtypes_sentence/3, 750). % TODO: Right priority?
:- add_goal_trans(hmtypes_goal/3, 750). % TODO: Right priority?
:- use_module(library(hmtypes_check(hmtypes_check_rt))).

:- new_declaration(type_check_options/1).
:- new_declaration(runtime_type_check/1).

:- include(library(hmtypes_check(hmtypes_check_prelude))).


