:- package(contextcore).
:- use_package(hiord).

:- op(1150, fx,  def_context).
:- op( 410, yfx, (.)).
:- op( 250, xfy, (::)).

% Custom control structure
% todo: allow for other syntax in addition to do/2?
:- new_declaration(control/1).
:- op(1150, fx, (control)).
:- op(700, xfy, [do]).

% Submodules
:- op(1150, fx, (sub_module)).
:- op( 550, yfx, (:)). % todo: does it conflict with anything else?

:- include(library(contextcore(contextcore_ops))).

:- set_prolog_flag(read_curly_blocks, on).

:- set_prolog_flag(read_postfix_blocks, on).
:- op(50, yf, ['[]']). % note: operator name inside a list, to avoid confusing it as an empty list of opcodes
:- op(50, yf, ({})).

:- load_compilation_module(library(contextcore(contextcore_tr))).

% note: priority before fsyntax (610)
:- add_sentence_trans(contextual_sentence_tr/3, 550).


