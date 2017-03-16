:- package(oo_syntax).
% :- doc(title, "Syntactic definitions for the new OO system").
% :- doc(author, "Jose F. Morales").

% Infix dot notation
% TODO: unify with :/2 eventually 
:- op(40, yfx, [(.)]).
:- set_prolog_flag(read_infix_dot, on).

% Module system extensions (nested modules and classes)
:- op(1150, fx, (class)).
:- op(1150, fx, (module)).
:- op(1150, fx, (extends)).
:- op(1150, fx, (virtual)).
:- op(1150, fx, (attr)).
:- op(1150, fx, (local)).
:- op(1150, fx, (static)).
%
%:- op(700, xfy, [kind_of]).
%
:- set_prolog_flag(read_curly_blocks, on).
:- set_prolog_flag(read_postfix_blocks, on).
:- op(50, yf, ['[]']). % note: operator name inside a list, to avoid confusing it as an empty list of opcodes
:- op(50, yf, ({})).

:- '$pragma'(class_expand).
