:- package(nomem_usage).
% This package allows eliminating calls to update_mem_usage/0,
% thus avoiding the run-time overhead introduced by memory measurements
% if not actually being used.
:- load_compilation_module(spec(nomem_usage_tr)).
% TODO: uncertain priority: just disables some decls and goals
:- add_sentence_trans(no_mem_usage/2, 9010).
