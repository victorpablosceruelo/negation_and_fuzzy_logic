:- module(prolog_sys, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(bug, "[Incomplete version of library(prolog_sys) for pl2js]").

:- use_package(js_lang).

% TODO: incomplete implementation
:- export(statistics/2).
statistics(runtime, [X|_]) :-
	X = ~'$cputime'.

:- pred '$cputime'/1 :: t_num + (detfun, argsbox([unbox])).
'$cputime' := ~js_lang.stats([
  vardecl('d', new('Date', [])),
  return('d'.getTime.[])
]).


