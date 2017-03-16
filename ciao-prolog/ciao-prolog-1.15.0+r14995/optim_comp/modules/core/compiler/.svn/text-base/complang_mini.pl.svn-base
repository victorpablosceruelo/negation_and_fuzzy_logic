:- package(complang_mini).

:- use_package(fsyntax).
:- '$pragma'(class_expand).

:- op(40, yfx, [(.)]). % TODO: ':' should be used instead
:- op(980, xfx, [(<-)]). % priority between (::) and (,)
:- op(700, xfx, [instance_of]). % priority between (::) and (,)
%:- set_prolog_flag(read_string_data_type, on).
:- set_prolog_flag(read_curly_blocks, on).
:- set_prolog_flag(read_postfix_blocks, on).
:- set_prolog_flag(read_infix_dot, on).
:- op(50, yf, ({})).

% TODO: merge with absmach_def declarations
:- op(1150, fx, [(class)]).
:- op(1150, fx, [(interface)]).
:- op(1150, fx, [(mixin)]).
% 
:- op(1150, fx, [(extends)]).
%
:- op(1150, fx, [(attr)]).
:- op(1150, fx, [(fluid)]).
:- op(1150, fx, [(constructor)]).

% Several module/predicate modifiers
% TODO: There are more operator definitions in frontend_common:define_ops/0
:- op(1150, fy, [(public)]). % public visibility
:- op(1150, fy, [(static)]). % static predicate (not a method)
:- op(1150, fy, [(virtual)]). % virtual predicate (overridable)
:- op(1150, fy, [(constant)]). % does not rewrite 'self'

% TODO: Necessary until the 'kind computation on imported' problem is solved
:- '$trust_statemodel'(any, single).
:- '$trust_statemodel'(m_any, pair).
:- '$trust_statemodel'(m_int, pair).
:- '$trust_statemodel'(accum, pair).
:- '$trust_statemodel'(revaccum, pair).
:- '$trust_statemodel'(m_dic, pair).
:- '$trust_statemodel'(u_dic, single).

% (module_exp)
:- '$trust_statemodel'(predicate_x, single).

% TODO: remove
% (module_jsexp)
:- '$trust_statemodel'(module_s, single).
:- '$trust_statemodel'(predicate_s, single).

