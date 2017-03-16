:- package(nounfold_stats).
% This package allows eliminating calls to inc_derivation_steps/1 and
% inc_unfold_evals/1, thus avoiding the run-time overhead introduced
% by asserts and retracts if not actually being used.

:- load_compilation_module(spec(nounfold_stats_tr)).
% TODO: uncertain priority: just disables some decls and goals
:- add_sentence_trans(no_unfold_stats/2, 9010).
