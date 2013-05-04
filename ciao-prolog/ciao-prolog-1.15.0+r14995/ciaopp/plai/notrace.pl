:- package(notrace).
:- load_compilation_module(notrace_tr).
% TODO: uncertain priority: just disables some decls and goals
:- add_sentence_trans(no_fixp_trace/2, 9010).
