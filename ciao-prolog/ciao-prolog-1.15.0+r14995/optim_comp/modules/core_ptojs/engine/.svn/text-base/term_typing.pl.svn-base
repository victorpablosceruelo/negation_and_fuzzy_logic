:- module(term_typing, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Typing (and basic definitions) of Terms").
:- doc(author, "Jose F. Morales").

:- doc(bug, "[Incomplete version of engine(term_typing) for pl2js]").

:- doc(bug, "This file mimicks engine(term_typing), but it also
   contains part of the definition of terms.").

% ===========================================================================
:- doc(section, "Basic data definitions").

:- doc(bug, "missing common base for var and nonvar (term)").

:- use_package(js_lang).

% TODO: Hardwired definition
% Base for 'term_typing.var', 'attr_rt.attrvar', etc.
:- js_native([
  call(mb('$r', 'def'), [id_s('var_base'), function(['$m'], [
    function(call('var_base', []), []),
    mb('$m', 'ctor') <- 'var_base',
    mb('$m', 'base') <- 'null',
    mb('$m', 'mlink') <- function(['$c'], [])
  ])]),
  vardecl('var_base', mb(call(mb('$r', 'query'), [id_s('var_base')]), ctor))
]).

:- class var {
    :- doc(title, "Variables").
    :- extends var_base.

    :- use_package(js_lang).

    :- export('$to_str'/1).
    :- pred '$to_str'/1 :: t_string + (detfun, argsbox([unbox])).
    '$to_str' := ~js_lang.expr("_"+(~self).deref.[].get_var_id.[]).

    :- export(cons__/0).
    :- pred cons__/0 + (basal, det, no_worker).
:- if(defined(coarse_timestamp)).
    % Note: we cannot distinguish variables easily with
    %       'coarse_timestamp', and lexical ordering does not work.
    cons__ :- js_lang.stats([
      (~bself).ref <- (~bself),
      (~bself).timestamp <- 'choice_timestamp'
    ]).
:- else.
    cons__ :- js_lang.stats([
      (~bself).ref <- (~bself),
      (~bself).timestamp <- 'post++'('choice_timestamp')
    ]).
:- endif.

    :- export(deref/1).
    % Dereference the term
    :- pred deref/1 + (basal, detfun, no_worker).
    deref := ~js_lang.stats([
      if((~bself).is_unbound.[],
        return((~bself)),
        return((~bself).ref.deref.[]))
    ]).

    :- export(is_unbound/0).
    :- pred is_unbound/0 + (basal, semidet, no_worker).
    is_unbound :-
        js_lang.test(op('===').[(~bself).ref, ~bself]).

    :- export(sd_unify_nonvar/1).
    % Unify with a nonvar (invoked from 'nonvar' class)
    :- pred sd_unify_nonvar/1 + (basal, semidet).
    sd_unify_nonvar(Other) :- js_lang.stats([
      if((~bself).is_unbound.[],
        return((~bself).sd_unify.[w, Other]),
        return((~bself).ref.sd_unify_nonvar.[w, Other]))
    ]).

    % TODO: From optim_comp/modules/core/absmach_dep.pl
    % "If the two variables have the same tag, the younger variable will
    %  point to the older one.  If the variables have different tag, they
    %  points according to the priority: sva < hva < cva"
    %
    % If the variable is unbound (i.e., dereferenced and var),
    % invoke what can be called 'sd_unify_var_var'.
    :- export(sd_unify/1).
    :- pred sd_unify/1 + (basal, semidet).
    sd_unify(Other) :- js_lang.stats([
      if((~bself).is_unbound.[], [
        Other <- Other.deref.[],
        if(instanceof(Other, ctor_lookup(class, ':'(term_typing, var))), [
          % younger cell references older
          if((~bself).timestamp > Other.timestamp, [
            (~bself).ref <- Other,
            w.trail.[~bself]
          ], [
            Other.ref <- (~bself),
            w.trail.[Other]
          ])
        ], [
          (~bself).ref <- Other,
          w.trail.[~bself]
        ]),
        return('true')
      ], [
        return((~bself).ref.sd_unify.[w, Other])
      ])
    ]).

    :- export(unbind/0).
    :- pred unbind/0 + (basal, det, no_worker).
    unbind :- js_lang.stats([
      (~bself).ref <- (~bself)
    ]).

    % TODO: like var.get_id
    % 'id' is assigned to variable lazily
    % other options: use WeakMap (FF6 only?) or object pointers (not for JS)
    :- export(get_var_id/1).
    :- pred get_var_id/1 + (basal, detfun, no_worker).
    get_var_id := ~js_lang.stats([
      if(op('===').[typeof((~bself).var_id), "undefined"],
         (~bself).var_id <- 'post++'('global_var_id'), []),
      return((~bself).var_id)
    ]).

    :- export(execute/0).
    :- pred execute/0 + (basal, nondet).
    execute :- js_lang.stats([
      if((~bself).is_unbound.[], [
        return(w.halt.[])
      ], [
	w.callhead <- (~bself).ref,
        return(w.pred_enter)
	% return((~bself).ref.execute.[w])
      ])
    ]).

    % Builtins for arithmetic (in case that the variable points to an arithmetic object)
    % TODO: duplicated in attr_rt and term_typing!
    % TODO: throw exception on the 'unbound case'
    % TODO: adding 'deref' on 'self' when calling the builtins would make things a lot simpler
:- if(defined(basal_arith_hooks)).
    :- export('b_eq'/1).
    :- pred 'b_eq'/1 + (basal, semidet, no_worker).
    'b_eq'(B) :- js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_eq.[B])])]).
    :- export('b_neq'/1).
    :- pred 'b_neq'/1 + (basal, semidet, no_worker).
    'b_neq'(B) :- js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_neq.[B])])]).
    :- export('b_ge'/1).
    :- pred 'b_ge'/1 + (basal, semidet, no_worker).
    'b_ge'(B) :- js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_ge.[B])])]).
    :- export('b_gt'/1).
    :- pred 'b_gt'/1 + (basal, semidet, no_worker).
    'b_gt'(B) :- js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_gt.[B])])]).
    :- export('b_le'/1).
    :- pred 'b_le'/1 + (basal, semidet, no_worker).
    'b_le'(B) :- js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_le.[B])])]).
    :- export('b_lt'/1).
    :- pred 'b_lt'/1 + (basal, semidet, no_worker).
    'b_lt'(B) :- js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_lt.[B])])]).
    %
    :- export('b_inc'/1).
    :- pred 'b_inc'/1 + (basal, detfun, no_worker).
    'b_inc'(B) :- js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_inc.[B])])]).
    :- export('b_dec'/1).
    :- pred 'b_dec'/1 + (basal, detfun, no_worker).
    'b_dec'(B) :- js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_dec.[B])])]).
    :- export('b_add'/2).
    :- pred 'b_add'/2 + (basal, detfun, no_worker).
    'b_add'(B) := ~js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_add.[B])])]).
    :- export('b_sub'/2).
    :- pred 'b_sub'/2 + (basal, detfun, no_worker).
    'b_sub'(B) := ~js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_sub.[B])])]).
    :- export('b_neg'/1).
    :- pred 'b_neg'/1 + (basal, detfun, no_worker).
    'b_neg' := ~js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_neg.[])])]).
    :- export('b_mul'/2).
    :- pred 'b_mul'/2 + (basal, detfun, no_worker).
    'b_mul'(B) := ~js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_mul.[B])])]).
    :- export('b_div'/2).
    :- pred 'b_div'/2 + (basal, detfun, no_worker).
    'b_div'(B) := ~js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_div.[B])])]).
    :- export('b_idiv'/2).
    :- pred 'b_idiv'/2 + (basal, detfun, no_worker).
    'b_idiv'(B) := ~js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_idiv.[B])])]).
    :- export('b_bitwise_and'/2).
    :- pred 'b_bitwise_and'/2 + (basal, detfun, no_worker).
    'b_bitwise_and'(B) := ~js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.'b_bitwise_and'.[B])])]).
    :- export('b_bitwise_or'/2).
    :- pred 'b_bitwise_or'/2 + (basal, detfun, no_worker).
    'b_bitwise_or'(B) := ~js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.'b_bitwise_or'.[B])])]).
    :- export('b_mod'/2).
    :- pred 'b_mod'/2 + (basal, detfun, no_worker).
    'b_mod'(B) := ~js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_mod.[B])])]).
    :- export('b_lsh'/2).
    :- pred 'b_lsh'/2 + (basal, detfun, no_worker).
    'b_lsh'(B) := ~js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_lsh.[B])])]).
    :- export('b_rsh'/2).
    :- pred 'b_rsh'/2 + (basal, detfun, no_worker).
    'b_rsh'(B) := ~js_lang.stats([if((~bself).is_unbound.[], [return('false')], [return((~bself).ref.b_rsh.[B])])]).
:- endif.

    % Lexical comparison
    :- export('$compare'/2).
    '$compare'(Other, Value) :- '$var_compare'(~self, Other, Value).
}.

% ===========================================================================

:- use_package(js_lang).

:- pred '$var_compare'/3 :: term * term * t_num + (detfun, argsbox([box, box, unbox])).
'$var_compare'(A, B, Value) :-
	js_lang.stats([
	    vardecl(a, A.deref.[]),
	    vardecl(b, B.deref.[]),
            if(instanceof(a, ctor_lookup(class, ':'(term_typing, var))),
                if(instanceof(b, ctor_lookup(class, ':'(term_typing, var))),
	            if(op('===').[a, b],
	                return(0), % a == b
		        % TODO: fine granularity timestamp is necessary here!
		        if(a.timestamp > b.timestamp,
			    return(1), % a @> b
			    return(-1))), % a @< b
                    return(-1)), % a @< b
                return(1)) % a @> b
        ], Value).

% TODO: make sure that 'var' name does not collides with 'var' class
%       (use different module/namespaces)
:- export(var/1).
%var(X) :- '$kind_of'(X, var_base), !. % TODO: not working
:- pred var/1 + semidet.
var(Other) :- js_lang.test(instanceof(Other.deref.[], 'var_base')).

% TODO: make it a builtin (automatically)
:- export(nonvar/1).
:- pred nonvar/1 + semidet.
nonvar(Other) :- js_lang.test(op('!').[paren(instanceof(Other.deref.[], 'var_base'))]).
%nonvar(X) :- var(X), !, fail.
%nonvar(_).

% B is a copy of A where each argument is replaced with a fresh variable
%   This is equivalent to (functor(A,F,N), functor(B,F,N)).
% TODO: good for atom-based module systems?
:- export(copy_fresh/2).
copy_fresh(A, B) :- B = ~A.'$copy_fresh'.

:- export(integer/1).
integer(X) :- number(X). % TODO: wrong! those tests are slow in JS, create two t_num classes?

:- export(float/1).
float(X) :- number(X). % TODO: wrong! those tests are slow in JS, create two t_num classes?

:- export(number/1).
number(X) :- '$kind_of'(X, t_num).


