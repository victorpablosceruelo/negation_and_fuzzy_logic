:- module(arithmetic, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Arithmetic").
:- doc(author, "Jose F. Morales").

:- doc(bug, "Incomplete version of engine(arithmetic) for pl2js").

% TODO: is/2 is not implemented (it uses interpreted functors instead)
% TODO: make interpreted functors optional (move to the same library
%       than in Ciao?)
% TODO: arithmetic is more general than in Ciao, it can be extended
%       for any object (not just numbers).

% ===========================================================================

:- class t_num {
    :- doc(title, "Numbers (integers and floats)").
    % This is the default class for numeric data used in pl2js

    :- extends(simple_box('===')).

    % NOTE: The following methods ($...) are private and not intended
    %       to be directly used by user code. In general, we want a
    %       more precise way to control what user code can do with
    %       each object (i.e., separate the domain from the
    %       mathematical structure, the operations).

    % TODO: Implement not only private, protected, and public
    %       visibility, but also 'constant' (and probably others).

    :- use_package(js_lang).

:- if(defined(basal_arith_hooks)).
    :- export('b_eq'/1).
    :- pred 'b_eq'/1 + (basal, semidet, no_worker).
    'b_eq'(B) :- js_lang.test(op('===').[(~bself).unbox.[], B.deref.[].unbox.[]]).
    :- export('b_neq'/1).
    :- pred 'b_neq'/1 + (basal, semidet, no_worker).
    'b_neq'(B) :- js_lang.test(op('!==').[(~bself).unbox.[], B.deref.[].unbox.[]]).
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
    :- export('b_inc'/1).
    :- pred 'b_inc'/1 + (basal, detfun, no_worker).
    'b_inc' := ~js_lang.expr(box('arithmetic:t_num', op('+').[(~bself).unbox.[], 1])).
    :- export('b_dec'/1).
    :- pred 'b_dec'/1 + (basal, detfun, no_worker).
    'b_dec' := ~js_lang.expr(box('arithmetic:t_num', op('-').[(~bself).unbox.[], 1])).
    :- export('b_add'/2).
    :- pred 'b_add'/2 + (basal, detfun, no_worker).
    'b_add'(B) := ~js_lang.expr(box('arithmetic:t_num', op('+').[(~bself).unbox.[], B.deref.[].unbox.[]])).
    :- export('b_sub'/2).
    :- pred 'b_sub'/2 + (basal, detfun, no_worker).
    'b_sub'(B) := ~js_lang.expr(box('arithmetic:t_num', op('-').[(~bself).unbox.[], B.deref.[].unbox.[]])).
    :- export('b_neg'/1).
    :- pred 'b_neg'/1 + (basal, detfun, no_worker).
    'b_neg' := ~js_lang.expr(box('arithmetic:t_num', op('-').[(~bself).unbox.[]])).
    :- export('b_mul'/2).
    :- pred 'b_mul'/2 + (basal, detfun, no_worker).
    'b_mul'(B) := ~js_lang.expr(box('arithmetic:t_num', op('*').[(~bself).unbox.[], B.deref.[].unbox.[]])).
    :- export('b_div'/2).
    :- pred 'b_div'/2 + (basal, detfun, no_worker).
    'b_div'(B) := ~js_lang.expr(box('arithmetic:t_num', op('/').[(~bself).unbox.[], B.deref.[].unbox.[]])).
    :- export('b_idiv'/2).
    :- pred 'b_idiv'/2 + (basal, detfun, no_worker).
    'b_idiv'(B) := ~js_lang.expr(box('arithmetic:t_num', op('>>').[paren(op('/').[(~bself).unbox.[], B.deref.[].unbox.[]]), 0])).
    :- export('b_bitwise_and'/2).
    :- pred 'b_bitwise_and'/2 + (basal, detfun, no_worker).
    'b_bitwise_and'(B) := ~js_lang.expr(box('arithmetic:t_num', op('&').[(~bself).unbox.[], B.deref.[].unbox.[]])).
    :- export('b_bitwise_or'/2).
    :- pred 'b_bitwise_or'/2 + (basal, detfun, no_worker).
    'b_bitwise_or'(B) := ~js_lang.expr(box('arithmetic:t_num', op('|').[(~bself).unbox.[], B.deref.[].unbox.[]])).
    :- export('b_mod'/2).
    :- pred 'b_mod'/2 + (basal, detfun, no_worker).
    'b_mod'(B) := ~js_lang.expr(box('arithmetic:t_num', op('%').[(~bself).unbox.[], B.deref.[].unbox.[]])).
    :- export('b_lsh'/2).
    :- pred 'b_lsh'/2 + (basal, detfun, no_worker).
    'b_lsh'(B) := ~js_lang.expr(box('arithmetic:t_num', op('<<').[(~bself).unbox.[], B.deref.[].unbox.[]])).
    :- export('b_rsh'/2).
    :- pred 'b_rsh'/2 + (basal, detfun, no_worker).
    'b_rsh'(B) := ~js_lang.expr(box('arithmetic:t_num', op('>>').[(~bself).unbox.[], B.deref.[].unbox.[]])).
:- else.
    :- export('$eq'/1).
    :- pred '$eq'/1 :: t_num + (semidet, argsbox([unbox])).
    '$eq'(B) :- js_lang.test(op('===').[~self, B]).
    :- export('$neq'/1).
    :- pred '$neq'/1 :: t_num + (semidet, argsbox([unbox])).
    '$neq'(B) :- js_lang.test(op('!==').[~self, B]).
    :- export('$ge'/1).
    :- pred '$ge'/1 :: t_num + (semidet, argsbox([unbox])).
    '$ge'(B) :- js_lang.test(op('>=').[~self, B]).
    :- export('$gt'/1).
    :- pred '$gt'/1 :: t_num + (semidet, argsbox([unbox])).
    '$gt'(B) :- js_lang.test(op('>').[~self, B]).
    :- export('$le'/1).
    :- pred '$le'/1 :: t_num + (semidet, argsbox([unbox])).
    '$le'(B) :- js_lang.test(op('<=').[~self, B]).
    :- export('$lt'/1).
    :- pred '$lt'/1 :: t_num + (semidet, argsbox([unbox])).
    '$lt'(B) :- js_lang.test(op('<').[~self, B]).
    %
    :- export('$inc'/1).
    :- pred '$inc'/1 :: t_num + (detfun, argsbox([unbox])).
    '$inc' := ~js_lang.expr(op('+').[~self, 1]).
    :- export('$dec'/1).
    :- pred '$dec'/1 :: t_num + (detfun, argsbox([unbox])).
    '$dec' := ~js_lang.expr(op('-').[~self, 1]).
    :- export('$add'/2).
    :- pred '$add'/2 :: t_num * t_num + (detfun, argsbox([unbox, unbox])).
    '$add'(B) := ~js_lang.expr(op('+').[~self, B]).
    :- export('$sub'/2).
    :- pred '$sub'/2 :: t_num * t_num + (detfun, argsbox([unbox, unbox])).
    '$sub'(B) := ~js_lang.expr(op('-').[~self, B]).
    :- export('$neg'/1).
    :- pred '$neg'/1 :: t_num + (detfun, argsbox([unbox])).
    '$neg' := ~js_lang.expr(op('-').[~self]).
    :- export('$mul'/2).
    :- pred '$mul'/2 :: t_num * t_num + (detfun, argsbox([unbox, unbox])).
    '$mul'(B) := ~js_lang.expr(op('*').[~self, B]).
    :- export('$div'/2).
    :- pred '$div'/2 :: t_num * t_num + (detfun, argsbox([unbox, unbox])).
    '$div'(B) := ~js_lang.expr(op('/').[~self, B]).
    :- export('$idiv'/2).
    :- pred '$idiv'/2 :: t_num * t_num + (detfun, argsbox([unbox, unbox])).
    '$idiv'(B) := ~js_lang.expr(op('>>').[paren(op('/').[~self, B]), 0]).
    :- export('$/\\'/2).
    :- pred '$/\\'/2 :: t_num * t_num + (detfun, argsbox([unbox, unbox])).
    '$/\\'(B) := ~js_lang.expr(op('&').[~self, B]).
    :- export('$\\/'/2).
    :- pred '$\\/'/2 :: t_num * t_num + (detfun, argsbox([unbox, unbox])).
    '$\\/'(B) := ~js_lang.expr(op('|').[~self, B]).
    :- export('$mod'/2).
    :- pred '$mod'/2 :: t_num * t_num + (detfun, argsbox([unbox, unbox])).
    '$mod'(B) := ~js_lang.expr(op('%').[~self, B]).
    :- export('$lsh'/2).
    :- pred '$lsh'/2 :: t_num * t_num + (detfun, argsbox([unbox, unbox])).
    '$lsh'(B) := ~js_lang.expr(op('<<').[~self, B]).
    :- export('$rsh'/2).
    :- pred '$rsh'/2 :: t_num * t_num + (detfun, argsbox([unbox, unbox])).
    '$rsh'(B) := ~js_lang.expr(op('>>').[~self, B]).
:- endif.

    :- export('$to_str'/1).
    :- pred '$to_str'/1 :: t_string + (detfun, argsbox([unbox])).
    '$to_str' := ~js_lang.expr((~self).toString.[]).

    % Lexical comparison
    :- export('$compare'/2).
    '$compare'(Other, Value) :- var(Other), !, Value = 1.
    '$compare'(Other, Value) :- \+ '$kind_of'(Other, t_num), !, Value = 1.
:- if(defined(basal_arith_hooks)).
    '$compare'(Other, Value) :- ~self > Other, !, Value = 1.
    '$compare'(Other, Value) :- ~self < Other, !, Value = -1.
    '$compare'(_Other, Value) :- Value = 0.
:- else.
    '$compare'(Other, Value) :- '$gt'(Other), !, Value = 1.
    '$compare'(Other, Value) :- '$lt'(Other), !, Value = -1.
    '$compare'(Other, Value) :- '$eq'(Other), !, Value = 0.
:- endif.
}.
% TODO: idea about numeric precission:
%   Import numbers from different modules! (e.g. int32.10000?)

% ===========================================================================
% Arithmetic functions and predicates (as wrappers for term methods)
%
% NOTE: This is is based on how Python handles operators, but much powerful,
% since Prolog syntax allows the user to define its own operators.
% Even more, pl2js module system makes it possible to redefine and
% module-qualify them.

% TODO: see http://docs.python.org/reference/datamodel.html#specialnames
%       to implement arithmetic operators properly

% TODO: Allow is/2 for compatibility (and in cases where we do not
%       want those operators to be interpreted functors).

% ---------------------------------------------------------------------------
% TODO: missing numerical evaluation

:- if(defined(basal_arith_hooks)).
:- export((<)/2).
:- pred (<)/2 + (semidet, basal_builtin(term:'b_lt'/1)).
A < B :- A < B.
:- export((=<)/2).
:- pred (=<)/2 + (semidet, basal_builtin(term:'b_le'/1)).
A =< B :- A =< B.
:- export((>)/2).
:- pred (>)/2 + (semidet, basal_builtin(term:'b_gt'/1)).
A > B :- A > B.
:- export((>=)/2).
:- pred (>=)/2 + (semidet, basal_builtin(term:'b_ge'/1)).
A >= B :- A >= B.
:- export((=\=)/2).
:- pred (=\=)/2 + (semidet, basal_builtin(term:'b_neq'/1)).
A =\= B :- A =\= B.
:- else.
:- export((<)/2).
:- pred (<)/2 + unfold.
A < B :- A.'$lt'(B).
:- export((=<)/2).
:- pred (=<)/2 + unfold.
A =< B :- A.'$le'(B).
:- export((>)/2).
:- pred (>)/2 + unfold.
A > B :- A.'$gt'(B).
:- export((>=)/2).
:- pred (>=)/2 + unfold.
A >= B :- A.'$ge'(B).
:- export((=\=)/2).
:- pred (=\=)/2 + unfold.
A =\= B :- A.'$neq'(B).
:- endif.

% ---------------------------------------------------------------------------

:- export(is/2).
is(_R, _Expr) :-
	fail. % TODO: not fully implemented

% Arithmetic
:- if(defined(basal_arith_hooks)).
:- export(('$++')/2).
:- pred ('$++')/2 + (detfun, basal_builtin(term:'b_inc'/1)).
'$++'(A) := ~'$++'(A).
:- export(('$--')/2).
:- pred ('$--')/2 + (detfun, basal_builtin(term:'b_dec'/1)).
'$--'(A) := ~'$--'(A).
:- export(('$+')/3).
:- pred ('$+')/3 + (detfun, basal_builtin(term:'b_add'/2)).
'$+'(A, B) := ~'$+'(A, B).
:- export(('$-')/3).
:- pred ('$-')/3 + (detfun, basal_builtin(term:'b_sub'/2)).
'$-'(A, B) := ~'$-'(A, B).
:- export(('$-')/2).
:- pred ('$-')/2 + (detfun, basal_builtin(term:'b_neg'/1)).
'$-'(A) := ~'$-'(A).
:- export(('$*')/3).
:- pred ('$*')/3 + (detfun, basal_builtin(term:'b_mul'/2)).
'$*'(A, B) := ~'$*'(A, B).
:- export(('$/')/3).
:- pred ('$/')/3 + (detfun, basal_builtin(term:'b_div'/2)).
'$/'(A, B) := ~'$/'(A, B).
:- export(('$//')/3).
:- pred ('$//')/3 + (detfun, basal_builtin(term:'b_idiv'/2)).
'$//'(A, B) := ~'$//'(A, B).
:- export(('$/\\')/3).
:- pred ('$/\\')/3 + (detfun, basal_builtin(term:'b_bitwise_and'/2)).
'$/\\'(A, B) := ~'$/\\'(A, B).
:- export(('$\\/')/3).
:- pred ('$\\/')/3 + (detfun, basal_builtin(term:'b_bitwise_or'/2)).
'$\\/'(A, B) := ~'$\\/'(A, B).
:- export(('$mod')/3).
:- pred ('$mod')/3 + (detfun, basal_builtin(term:'b_mod'/2)).
'$mod'(A, B) := ~'$mod'(A, B).
:- export(('$<<')/3).
:- pred ('$<<')/3 + (detfun, basal_builtin(term:'b_lsh'/2)).
'$<<'(A, B) := ~'$<<'(A, B).
:- export(('$>>')/3).
:- pred ('$>>')/3 + (detfun, basal_builtin(term:'b_rsh'/2)).
'$>>'(A, B) := ~'$>>'(A, B).
:- else.
:- export(('$++')/2).
:- pred ('$++')/2 + unfold.
'$++'(A) := ~A.'$inc'.
:- export(('$--')/2).
:- pred ('$--')/2 + unfold.
'$--'(A) := ~A.'$dec'.
:- export(('$+')/3).
:- pred ('$+')/3 + unfold.
'$+'(A, B) := ~A.'$add'(B).
:- export(('$-')/3).
:- pred ('$-')/3 + unfold.
'$-'(A, B) := ~A.'$sub'(B).
:- export(('$-')/2).
:- pred ('$-')/2 + unfold.
'$-'(A) := ~A.'$neg'.
:- export(('$*')/3).
:- pred ('$*')/3 + unfold.
'$*'(A, B) := ~A.'$mul'(B).
:- export(('$/')/3).
:- pred ('$/')/3 + unfold.
'$/'(A, B) := ~A.'$div'(B).
:- export(('$//')/3).
:- pred ('$//')/3 + unfold.
'$//'(A, B) := ~A.'$idiv'(B).
:- export(('$/\\')/3).
:- pred ('$/\\')/3 + unfold.
'$/\\'(A, B) := ~A.'$/\\'(B).
:- export(('$\\/')/3).
:- pred ('$\\/')/3 + unfold.
'$\\/'(A, B) := ~A.'$\\/'(B).
:- export(('$mod')/3).
:- pred ('$mod')/3 + unfold.
'$mod'(A, B) := ~A.'$mod'(B).
:- export(('$<<')/3).
:- pred ('$<<')/3 + unfold.
'$<<'(A, B) := ~A.'$lsh'(B).
:- export(('$>>')/3).
:- pred ('$>>')/3 + unfold.
'$>>'(A, B) := ~A.'$rsh'(B).
:- endif.

% ---------------------------------------------------------------------------

:- use_package(js_lang).

:- export(('$cos')/2).
:- pred ('$cos')/2 :: t_num * t_num + (detfun, argsbox([unbox, unbox])).
'$cos'(A) := ~js_lang.expr('Math'.cos.[A]).

:- export(('$sin')/2).
:- pred ('$sin')/2 :: t_num * t_num + (detfun, argsbox([unbox, unbox])).
'$sin'(A) := ~js_lang.expr('Math'.sin.[A]).

% ---------------------------------------------------------------------------

