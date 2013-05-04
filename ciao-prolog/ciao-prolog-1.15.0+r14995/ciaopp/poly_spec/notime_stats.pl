:- package(notime_stats).
% This package allows eliminating calls to increment_time/2 
% thus avoiding the run-time overhead introduced
% by asserts and retracts if not actually being used.

:- load_compilation_module(poly_spec(notime_stats_tr)).
% TODO: uncertain priority: just disables some decls and goals
:- add_sentence_trans(no_time_stats/2, 9010).
