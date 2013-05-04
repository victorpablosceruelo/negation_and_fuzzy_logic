:- module(string_type_rt, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Runtime for Primitive String Type").
:- doc(author, "Jose F. Morales").

:- doc(bug, "Merge with js_backend/compiler/string_type_rt.pl").
:- doc(bug, "This type is extended with hooks for arithmetic").

:- doc(module, "By default, pl2js uses t_string objects for strings
   (\"...\"), instead of lists. Those string objects are much cheaper to
   handle in Javascript than their counterparts as lists of numbers
   (codes).
").

:- class t_string {
    :- extends(simple_box('==')). % TODO: why not ===?

    :- use_package(js_lang).

    :- export(lowercase/1).
    :- pred lowercase/1 :: t_string + (detfun, argsbox([unbox])).
    lowercase := ~js_lang.expr((~self).toLowerCase.[]).

    :- export(uppercase/1).
    :- pred uppercase/1 :: t_string + (detfun, argsbox([unbox])).
    uppercase := ~js_lang.expr((~self).toUpperCase.[]).

    :- export('$to_int'/1).
    :- pred '$to_int'/1 :: t_num + (detfun, argsbox([unbox])).
    '$to_int' := ~js_lang.expr(parseInt.[~self]).

:- if(defined(basal_arith_hooks)).
    :- export('b_ge'/1).
    :- pred 'b_ge'/1 + (basal, semidet, no_worker).
    'b_ge'(B) :- js_lang.test(op('>=').[(~bself).unbox.[], B.deref.[].unbox.[]]).
    :- export('b_gt'/1).
    :- pred 'b_gt'/1 + (basal, semidet, no_worker).
    'b_gt'(B) :- js_lang.test(op('>').[(~bself).unbox.[], B.deref.[].unbox.[]]).
    :- export('b_le'/1).
    :- pred 'b_le'/1 + (basal, semidet, no_worker).
    'b_le'(B) :- js_lang.test(op('<=').[(~bself).unbox.[], B.deref.[].unbox.[]]).
    :- export('b_lt'/1).
    :- pred 'b_lt'/1 + (basal, semidet, no_worker).
    'b_lt'(B) :- js_lang.test(op('<').[(~bself).unbox.[], B.deref.[].unbox.[]]).
    %
    :- export('b_add'/2).
    :- pred 'b_add'/2 + (basal, detfun, no_worker).
    'b_add'(B) := ~js_lang.expr(box('string_type_rt:t_string', op('+').[(~bself).unbox.[], B.deref.[].unbox.[]])).
:- else.
    :- export('$lt'/1).
    :- pred '$lt'/1 :: t_string + (semidet, argsbox([unbox])).
    '$lt'(B) :- js_lang.test(op('<').[~self, B]).
    :- export('$le'/1).
    :- pred '$le'/1 :: t_string + (semidet, argsbox([unbox])).
    '$le'(B) :- js_lang.test(op('<=').[~self, B]).
    :- export('$gt'/1).
    :- pred '$gt'/1 :: t_string + (semidet, argsbox([unbox])).
    '$gt'(B) :- js_lang.test(op('>').[~self, B]).
    :- export('$ge'/1).
    :- pred '$ge'/1 :: t_string + (semidet, argsbox([unbox])).
    '$ge'(B) :- js_lang.test(op('>=').[~self, B]).
    %
    :- export('$add'/2).
    :- pred '$add'/2 :: t_string * t_string + (detfun, argsbox([unbox, unbox])).
    '$add'(B) := ~js_lang.expr(op('+').[~self, B]).
:- endif.

    :- export('$to_str'/1).
    :- pred '$to_str'/1 :: t_string + (detfun, argsbox([unbox])).
    '$to_str' := ~js_lang.expr(~self).
}.
