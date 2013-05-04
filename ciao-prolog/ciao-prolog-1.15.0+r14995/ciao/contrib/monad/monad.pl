:- package(monad).

:- load_compilation_module(library(monad(monad_tr))).
% priority: before 'functionstr'
% TODO: use relative priorities?
:- add_sentence_trans(monad_sentence_trans/3, 605).

:- use_package(hiord).
:- use_package(functional).
:- fun_eval(hiord(true)).

% Note: since Prolog is not typed, all monad operations has been
% extended with an identifer for the monad name.

:- op(700, xfx, [(<-)]).

% The bind operator
:- op(700, xfx, [(>>=)]).
% Haskell types: (>>=) :: m a -> (a -> m b) -> m b
:- discontiguous (>>=)/3.
:- fun_eval (>>=)/2.
:- meta_predicate '>>='(?, pred(2), ?).

% The >> operator
% todo: without lazy evaluation, this is wrong!!
% :- op(700, xfx, [(>>)]).
% Haskell types: (>>) :: m a -> m b -> m b
% :- fun_eval (>>)/2.
% M >> K := M >>= (''(_, R) :- R = K).

% The return operator
% Haskell types: return :: a -> m a
:- discontiguous return/3.
:- fun_eval return/2. % extended with the monad name

% The fail operator (not necessary for all monads, just for errors in
% the otherwise case for do notation)
%
% fail :: String -> m a
:- discontiguous fail/3.
:- fun_eval fail/2. % extended with the monad name

% mzero and mplus operations for MonadPlus
:- discontiguous mzero/2.
:- fun_eval mzero/1. % extended with the monad name
% todo: mplus examples need lazy evaluation...
:- discontiguous mplus/4.
:- fun_eval mplus/3. % extended with the monad name
