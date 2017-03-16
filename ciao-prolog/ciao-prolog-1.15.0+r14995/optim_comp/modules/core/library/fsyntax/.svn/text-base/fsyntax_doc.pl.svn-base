:- use_package(assertions).
:- doc(nodoc,assertions).

:- doc(title,"Functional notation").

:- doc(author,"The CLIP Group").

% TODO: Differences with mainstream fsyntax:
%   - fun_return is not allowed
%   - fun_eval does not allow fun_eval syntax
%   - simpler translation for functional if-then-else (emit (_->_;_)
%     directly, because it is correctly optimized by later
%     optimizations)

:- doc(module, "This library package allows the use of functional
   notation in a Ciao module/program.

All function applications (definitions) are translated to goals
(predicate definitions) where another argument to hold the result of the
function is added to the right.  Function applications need to be
preceded by the operator @tt{~}, or the functor be declared as such by
using the declaration @pred{fun_eval/1}.  There is an exception: all
functions understood by @pred{is/2} are considered as functions by
default.  This feature can be disabled by a declaration @tt{:-
fun_eval(arith(false))} (and reverted by using @tt{true} instead of
@tt{false}).  When interpreting arithmetic functors as functions,
function uses inside @pred{is/2} are not handled to avoid loops, so it
is recommended to use @pred{(=)/2} instead.
A functor normally considered as a function call can be
escaped using the prefix operator @tt{^}.  Function definitions can be
written by using the binary operator @tt{:=}, and can have also body.
Example of use:
@begin{verbatim}
@includeverbatim{examples/fib_fun.pl}
@end{verbatim}
  ").
