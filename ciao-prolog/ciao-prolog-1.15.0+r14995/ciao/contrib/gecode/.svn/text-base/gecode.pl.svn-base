:- package('gecode').

 %% :- push_prolog_flag(unused_pred_warnings, no).

:- include(library(gecode(gecode_syntax))).
:- include(library(gecode(gecode_translation))).

:- load_compilation_module(library(gecode((gecode_tr)))).
% note: priority after 'tabling', right?
:- add_goal_trans(gecode_tr/2, 760). % TODO: Right priority?

 %% :- pop_prolog_flag(unused_pred_warnings).
