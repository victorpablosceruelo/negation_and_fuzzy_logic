:- package(and).
:- include(library(dclp(and(syntax)))).
:- use_module(library(dclp(and(and_rt)))).

:- use_package(fd).

:- load_compilation_module(library(dclp(and(and_tr)))).
% note: priority after 'fd' (750)
:- add_goal_trans(and_tr/2, 760). % TODO: Right priority?
