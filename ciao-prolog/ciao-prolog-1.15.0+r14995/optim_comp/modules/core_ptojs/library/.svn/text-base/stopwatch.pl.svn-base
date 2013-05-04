:- class(stopwatch, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "A stopwatch").
:- doc(author, "Jose F. Morales").

% TODO: Only for the JavaScript backend?

% TODO: rewrite using nb_mut and define a foreign object for JS 'Date'

:- use_package(js_lang).

% A stopwatch
:- export(cons__/0).
:- pred cons__/0 + det.
cons__ :- js_lang.stats([
  (~self).time <- 0,
  (~self).accum <- 0
]).

% Reset accumulated time
:- export(reset/0).
:- pred reset/0 + det.
reset :- js_lang.stats([
  (~self).accum <- 0
]).

% Begin counting time
:- export(start/0).
:- pred start/0 + det.
start :- js_lang.stats([
  vardecl('d', new('Date', [])),
  (~self).time <- 'd'.getTime.[]
]).

% Stop counting time and return elapsed time
:- export(end/1).
:- pred end/1 :: t_num + (detfun, argsbox([unbox])).
end := ~js_lang.stats([
  vardecl('d', new('Date', [])),
  vardecl('time2', 'd'.getTime.[]),
  vardecl('delta', 'time2' - (~self).time),
  (~self).time <- 0,
  '+='((~self).accum, 'delta'),
  return('delta')
]).

% Return total accumulated time
:- export(accum/1).
:- pred accum/1 :: t_num + (detfun, argsbox([unbox])).
accum := ~js_lang.expr((~self).accum).


