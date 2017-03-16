:- module(frontend_ptojs, [], [complang]).

% Instantiate compiler/frontend_common for the JS backend.
% TODO: can this be dynamic?
:- compilation_fact(use_backend(js)).
:- include(compiler(frontend_common)).


