:- module(frontend, [], [compiler(complang)]).

% Instantiate frontend_common for the Bytecode/C backend.
% TODO: can this be dynamic?
:- compilation_fact(use_backend(bc)).
:- include(compiler(frontend_common)).

