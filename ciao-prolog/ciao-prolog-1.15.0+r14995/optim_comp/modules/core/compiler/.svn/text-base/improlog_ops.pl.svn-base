% TODO: the following syntax definitions should be shared with complang
:- op(50, fx, [(~)]).
:- op(50, fx, [(@)]). % TODO: conflict with basicmodes.pl, isomodes.pl
:- op(980, xfx, [(<-)]). % priority between (::) and (,)
:- op(1100, xfy, [('|')]).
:- op(1150, xfx, [(:=)]).

:- op(1150, fx, [(pred)]).
:- op(1150, fx, [(attr)]).
:- op(1150, fx, [(mut)]).
:- op(1150, fx, [(abstract_class)]).
:- op(1150, fx, [(class)]).
:- op(978, xfx,(::)).

:- push_prolog_flag(read_hiord, on).

% Infix dot as method/attribute accessor
:- op(40, yfx, [(.)]).
:- push_prolog_flag(read_infix_dot, on).

% Curly blocks
:- push_prolog_flag(read_curly_blocks, on).

% Postfix blocks enable X[I] syntax
:- push_prolog_flag(read_postfix_blocks, on).
% TODO: the following operator definitions allows postfix read of [...] and {...}
%       Use other way to specify it?
:- op(40, yf, ['[]']). % note: operator name inside a list, to avoid confusing it as an empty list of opcodes
:- op(40, yf, ({})).

