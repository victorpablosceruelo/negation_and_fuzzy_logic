:- module(attr_rt, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Runtime Support for Attributed Variables").
:- doc(author, "Jose F. Morales").

:- doc(bug, "[Incomplete version of engine(attributes),
   engine(attributed_variables), and library(attr(attr_rt)) for
   pl2js]").

% TODO: 
%  - Share definitions with @class{var_base} (or @class{var})
%  - Add multiattributes as another kind of variable? (as tags are free)

% ===========================================================================

:- use_package(js_lang).

% (this is internal)
:- pred new_attrvar/3 + detfun.
new_attrvar(M, Attr) := ~js_lang.expr(new(ctor_lookup(class, ':'(attr_rt, attrvar)), [M, Attr])).

% interface compatible with 'attr'
:- export(put_attr/3).
put_attr(X, M, Attr) :-
	( '$kind_of'(X, attrvar) ->
	    X.'$attrset'(M, Attr)
	; X = ~new_attrvar(M, Attr)
	). 

:- export(get_attr/3).
get_attr(X, M, Attr) :-
	'$kind_of'(X, attrvar),
	Attr = ~X.'$attrget'(M).

:- class trail_attr_entry {
    :- doc(title, "Trail Entry for Attributes").
    % TODO: really, this is like entries for data fields in a record
    % TODO: add a class for 'trail entries'? variables also inherit them (in our case, not in all of them)
    :- extends basal_base.

    % Note: those entries are different than entries for undoing 
    %       references of variables (the variables themselves
    %       are enough to do untrailing)

    :- use_package(js_lang).

    % TODO: like var.cons__ but with 'module' and 'attr'
    :- export(cons__/3).
    :- pred cons__/3 + (basal, det, no_worker).
    cons__(Ref, M, Attr) :- js_lang.stats([
      (~bself).ref <- Ref,
      (~bself).m <- M,
      (~bself).attr <- Attr
    ]).

    :- export(unbind/0).
    :- pred unbind/0 + (basal, det, no_worker).
    unbind :- js_lang.stats([
      vardecl(r, (~bself).ref),
      r.m <- (~bself).m,
      r.attr <- (~bself).attr
    ]).
}.

:- class attrvar {
    :- doc(title, "Attributed Variables").
    :- extends var_base.

    :- use_package(js_lang).

    % TODO: similar to var.'$to_str', but uses a different prefix
    :- export('$to_str'/1).
    :- pred '$to_str'/1 :: t_string + (detfun, argsbox([unbox])).
    '$to_str' := ~js_lang.expr("_CV" + self.deref.[].get_var_id.[]).

    % TODO: like var.cons__ but with 'module' and 'attr'
    :- export(cons__/2).
    :- pred cons__/2 + (basal, det, no_worker).
:- if(defined(coarse_timestamp)).
    cons__(M, Attr) :- js_lang.stats([
      (~bself).ref <- ~bself,
      (~bself).timestamp <- 'choice_timestamp',
      (~bself).m <- M,
      (~bself).attr <- Attr
    ]).
:- else.
    cons__(M, Attr) :- js_lang.stats([
      (~bself).ref <- ~bself,
%      console.log.["created attrvar with timestamp: " + 'choice_timestamp'],
      (~bself).timestamp <- 'post++'('choice_timestamp'),
      (~bself).m <- M,
      (~bself).attr <- Attr
    ]).
:- endif.

    % TODO: like var.deref
    :- export(deref/1).
    :- pred deref/1 + (basal, detfun, no_worker).
    deref := ~js_lang.stats([
      if((~bself).is_unbound.[],
        return(~bself),
        return((~bself).ref.deref.[]))
    ]).

    % TODO: like var.is_unbound
    :- export(is_unbound/0).
    :- pred is_unbound/0 + (basal, semidet, no_worker).
    is_unbound :- js_lang.test(op('===').[(~bself).ref, ~bself]).

    % TODO: like var.sd_unify_nonvar
    :- export(sd_unify_nonvar/1).
    :- pred sd_unify_nonvar/1 + (basal, semidet).
    sd_unify_nonvar(Other) :- js_lang.stats([
      if((~bself).is_unbound.[],
        return((~bself).sd_unify.[w, Other]),
        return((~bself).ref.sd_unify_nonvar.[w, Other]))
    ]).

    % TODO: Like var.sd_unify, but modified to support attr_rt.attrvar.
    %       It seems that Bvar does not need to be modified (which is good)
    % TODO: Implement a constraint queue and a wake operation, sometimes
    %       we want to reorder constraints or even create choice points.
    % TODO: support multiple attributes? (or add as other kind of variable)
    :- export(sd_unify/1).
    :- pred sd_unify/1 + (basal, semidet).
    sd_unify(Other) :- js_lang.stats([
  %    console.log(\"{log: sd_unify}\");
      if((~bself).is_unbound.[], [
        Other <- Other.deref.[],
        if(instanceof(Other, ctor_lookup(class, ':'(term_typing, var))), [
          % term_typing.var has more priority, switch
          return(Other.sd_unify.[w, ~bself])
        ], [
          if(instanceof(Other, ctor_lookup(class, ':'(attr_rt, attrvar))), [
            % younger cell references older
            if((~bself).timestamp > Other.timestamp, 
              return((~bself).trail_bind_and_wake.[w, Other]),
              return(Other.trail_bind_and_wake.[w, ~bself]))
          ], [
            return((~bself).trail_bind_and_wake.[w, Other])
          ])
        ])
      ], [
        % TODO: share with var.sd_unify
        return((~bself).ref.sd_unify.[w, Other])
      ])
    ]).
    % TODO: Write an equivalent for 'var'?
    % TODO: We can wake in place or delay (both are interesting)
    % Unify an attributed variable with @var{Other}, another attributed variable or
    % nonvar:
    %  - bind 'this' with 'other'
    %  - trail 'this' (so that on backtracking it returns to its
    %    previous state)
    %  - ...
    % Precondition: 'self' must be unbound
    :- export(trail_bind_and_wake/1).
    :- pred trail_bind_and_wake/1 + (basal, semidet).
    trail_bind_and_wake(Other) :- js_lang.stats([
      (~bself).ref <- Other,
      w.trail.[~bself],
      % TODO: remove attributes from the variable? <- good for GC
      %
      % wake constraint
      vardecl(md, (~bself).m.deref.[]),
      % TODO: like 'unk_method_tr.tr_exec'
      return(w.sd_solve.[~ref_PUM__buildstr(md, 'attr_unify_hook', [(~bself).attr, Other])])
    ]).
    :- export(trail_set_attr/2).
    :- pred trail_set_attr/2 + (basal, det).
    trail_set_attr(M, Attr) :- js_lang.stats([
      % TODO: pick attribute
      vardecl(u, (~bself).deref.[]),
      vardecl(v, new(ctor_lookup(class, ':'(attr_rt, trail_attr_entry)), [u, M, u.attr])),
      u.m <- M,
      u.attr <- Attr,
      w.trail0.[v] % unconditional trailing
    ]).
    % TODO: like var.unbind
    :- export(unbind/0).
    :- pred unbind/0 + (basal, det, no_worker).
    % Note: unbinds only the reference, when the variable points to other attrvar
    % (see trail_attr_entry for untrailing attributes)
    unbind :- js_lang.stats([
      (~bself).ref <- ~bself
    ]).
    % TODO: like var.execute
    :- export(execute/0).
    :- pred execute/0 + (basal, nondet).
    execute :- js_lang.stats([
      if((~bself).is_unbound.[],
        return(w.halt.[]),
        return((~bself).ref.execute.[w]))
    ]).
    % Operations on the attribute
    % TODO: multi-attributes are not yet supported
    :- export('$attrset'/2).
    :- pred '$attrset'/2.
    '$attrset'(M, Attr) :- '$trail_set_attr'(~self, M, Attr). % wrapper (for built-in)
% TODO: we would like this, but the worker is not available (?!)
%     js_lang.stats([(~self).trail_set_attr.[w, M, Attr]]).

    :- export('$attrget'/2).
    :- pred '$attrget'/2 + detfun.
    '$attrget'(_M) := ~js_lang.expr((~self).deref.[].attr).

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

% TODO: missing
%    % Lexical comparison
%    :- export('$compare'/2).
%    '$compare'(Other, Value) :- '$var_compare'(~self, Other, Value).
}.

