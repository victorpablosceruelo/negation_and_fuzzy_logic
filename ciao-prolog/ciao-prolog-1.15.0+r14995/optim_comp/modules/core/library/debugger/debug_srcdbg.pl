% Package for srcdbg debugging

:- use_module(library(debugger(debugger_support)), [srcdbg_spy/6]).
% TODO: 'dynamic' is no longer needed
% :- '$default_preddef'(dynamic).

:- multifile '$mod_srcdbg'/1.
:- '$preddef'('$mod_srcdbg'/1, bytecode).

:- '$insert_debug_info'.
