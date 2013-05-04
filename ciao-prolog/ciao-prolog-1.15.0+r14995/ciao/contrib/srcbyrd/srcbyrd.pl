:- package(srcbyrd).
% TODO: Missing documentation and status

:- load_compilation_module(library(srcbyrd(srcbyrd_tr))).

% note: priority like the embedded debugger, is it right? is it compatible?
:- add_clause_trans(srcdbg_expand/4, 8510). % TODO: Right priority?

:- use_module(library(srcbyrd(srcbyrd_rt))).
