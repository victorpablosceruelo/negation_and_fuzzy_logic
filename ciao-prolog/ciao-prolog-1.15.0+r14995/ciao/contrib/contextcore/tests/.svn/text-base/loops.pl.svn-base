% ---------------------------------------------------------------------------
% Some predefined control patterns

% Author: Jose F. Morales

% todo: Make sure that Code is not expanded with the modenv of the
%   control!! (if not, it would be possible to call 'cond/1' from
%   Code)

% todo: optionally, it would be possible to define some predicates
%   that could be called from code (e.g. something like yield/1 in
%   python?)

% todo: implement!!
%:- control (while(cond::block) do code::block) {
%  '' :- loop.
%  loop :- ( cond -> code, loop ; true ).
%}.

% Do code(X) for each X in the list List
:- sub_module list {
  :- control (for_each(X, List) do code(X)) {
    '' :- loop(List).
    loop([]).
    loop([X|Xs]) :- code(X), loop(Xs).
  }.
}.

:- sub_module closed_range_step {
  % Do code(X) for each number X, so that Begin <= X, X mod Step = 0, X <= End
  :- control (for_each(X, Begin, End, Step) do code(X)) {
    '' :- cond(Begin).
    cond(I) :- ( I =< End -> iter(I) ; true ).
    iter(I) :- code(I), I1 is I + Step, cond(I1).
  }.
}.

:- sub_module closed_range {
  % Do code(X) for each number X, so that Begin <= X, X mod Step = 0, X <= End
  :- control (for_each(X, Begin, End) do code(X)) {
    '' :- cond(Begin).
    cond(I) :- ( I =< End -> iter(I) ; true ).
    iter(I) :- code(I), I1 is I + 1, cond(I1).
  }.
  % todo: does not work! (see the circle demo)
  % % Do code(X) for each integer X, so that Begin <= X, X <= End
  % :- control (for_each(X, Begin, End) do code(X)) {
  %   '' :- closed_range_step:for_each(A, Begin, End, 1) do code(A).
  % }.
}.

:- sub_module range {
  % Do code(X) for each integer X, so that Begin <= X, X < End
  :- control (for_each(X, Begin, End) do code(X)) {
    '' :- cond(Begin).
    cond(I) :- ( I < End -> iter(I) ; true ).
    iter(I) :- code(I), I1 is I + 1, cond(I1).
  }.
}.

% Do code(X) for each argument of the term Term
:- sub_module functor {
  :- control (for_each_arg(X, Term) do code(X)) {
    '' :- functor(Term, _, N), loop(1, N, Term).
    loop(I, N, Term) :- I > N, !.
    loop(I, N, Term) :- arg(I, Term, X), code(X), I1 is I + 1, loop(I1, N, Term).
  }.
}.

