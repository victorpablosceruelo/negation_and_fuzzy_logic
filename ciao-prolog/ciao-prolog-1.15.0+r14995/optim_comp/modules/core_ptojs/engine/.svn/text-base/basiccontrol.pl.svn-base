:- module(basiccontrol, [], [oo_syntax, hiord, assertions, fsyntax, mutables, string_type]).

:- doc(title, "Basiccontrol").
:- doc(author, "Jose F. Morales").

:- doc(bug, "[Incomplete version of engine(basiccontrol) for pl2js]").

:- use_module(library(arithpreds)). % TODO: include it? (at least for improlog-like code)

% Note: Like in optim_comp, this file provides the starting point for
%   the definition of most of abstract machine runtime.

% ===========================================================================

% TODO:
%  det, semidet, and nondet are indeed a low-level notation for
%  special continuations (basal predicates do not really 'fail', this
%  is just notation to make coding simpler)

:- class worker_stacks {
    :- doc(title, "Worker stacks").
    :- extends basal_base.

    :- use_package(js_lang).

    :- export(cons__/0).
    :- pred cons__/0 + (basal, det, no_worker).
    cons__ :- js_lang.stats([
      % Note: this order of attributes makes examples faster
      (~bself).callhead <- 'null', % currently executed predicate
      (~bself).cont <- 'null',
      (~bself).frame <- 'null',
      (~bself).undo <- '[]', % empty stack
      (~bself).choice <- 'null', % filled later
      (~bself).previous_choice <- 'null',
      %
      % TODO: rewrite as globals or static?
      % choice when solver failed
%      'choice_timestamp' <- -1,
%      (~bself).failed_choice <- new(ctor_lookup(class, ':'(basiccontrol, choice)), [
%        -1, 'null', 'null', 'null', 'null', 'null', 'null'
%      ]),
      (~bself).push_choice.['null'],
      (~bself).failed_choice <- (~bself).choice,
      % choice when solver succeeded
%      (~bself).ok_choice <- new(ctor_lookup(class, ':'(basiccontrol, choice)), [
%        0, 'null', 'null', 'null', 'null', 'null', (~bself).failed_choice
%      ]),
      (~bself).push_choice.['null'],
      (~bself).ok_choice <- (~bself).choice,
      %
      (~bself).choice <- (~bself).ok_choice,
      (~bself).rmode <- 'false'
%      'choice_timestamp' <- (~bself).choice.timestamp
    ]).

    :- export(reset/0).
    % Reset some registers (before solve/1 can be called)
    :- pred reset/0 + (basal, det, no_worker).
    reset :- js_lang.stats([
      (~bself).callhead <- 'null', % currently executed predicate
      (~bself).cont <- 'null',
      (~bself).frame <- 'null',
      (~bself).undo <- '[]', % empty stack
      (~bself).choice <- (~bself).ok_choice,
      'choice_timestamp' <- (~bself).choice.timestamp
    ]).

    :- export(halt/0).
    :- pred halt/0 + (basal, nondet, no_worker).
    halt :- js_lang.stats([
      console.error.["internal error: halt"],
      (~bself).frame <- 'null',
      (~bself).choice <- 'null',
      return('null')
    ]).

    :- export(nodef/0).
    :- pred nodef/0 + (basal, nondet, no_worker).
    nodef :- js_lang.stats([
      console.error.["internal error: predicate not defined"],
      (~bself).frame <- 'null',
      (~bself).choice <- 'null',
      return('null')
    ]).

    :- export(error/1).
    :- pred error/1 + (basal, nondet, no_worker).
    error(Msg) :- js_lang.stats([
      console.error.["internal error: " + Msg],
      (~bself).frame <- 'null',
      (~bself).choice <- 'null',
      return('null')
    ]).

    % TODO: this is not worth expanding
    % TODO: define patch_choice
    :- export(push_choice/1).
    :- pred push_choice/1 + (basal, det, no_worker).
    push_choice(AltCont) :- js_lang.stats([
      'choice_timestamp' <- 'choice_timestamp' + 1,
%      (~bself).choice <- new(ctor_lookup(class, ':'(basiccontrol, choice)), [
%                            'choice_timestamp',
%                            AltCont,
%			    (~bself).callhead,
%			    (~bself).cont,
%			    (~bself).frame,
%			    (~bself).undo.length,
%			    (~bself).choice
%                          ])
      (~bself).choice <- new(ctor_lookup(class, ':'(basiccontrol, choice)), [(~bself), AltCont])
    ]).

    :- export(trail0/1).
    :- pred trail0/1 + (basal, det, no_worker).
    % unconditional trailing
    trail0(X) :- js_lang.stats([
      (~bself).undo.push.[X]
    ]).

    :- export(trail/1).
    :- pred trail/1 + (basal, det, no_worker).
:- if(fail).
    trail(X) :- js_lang.stats([
      (~bself).undo.push.[X]
    ]).
:- else.
    % Using conditional trailing
    trail(X) :- js_lang.stats([
%      console.log.["[trailing: var.timestamp=" + X.timestamp + "], [(~bself).choice.timestamp=" + (~bself).choice.timestamp + "]"],
      if(X.timestamp < (~bself).choice.timestamp,
         (~bself).undo.push.[X],
         [])
    ]).
:- endif.

    :- export(untrail/1).
    :- pred untrail/1 + (basal, det, no_worker).
    untrail(TrailTop) :- js_lang.stats([
      vardecl('i', (~bself).undo.length),
      while('i' > TrailTop, [
        'post--'('i'),
        (~bself).undo.pop.[].unbind.[]
      ])
    ]).

    :- export(cut/1).
    :- pred cut/1 + (basal, det, no_worker).
    cut(C) :- js_lang.stats([
      C.deref.[].cut.[~bself]
    ]).

    :- export(dealloc/0).
    :- pred dealloc/0 + (basal, det, no_worker).
    dealloc :- js_lang.stats([
      (~bself).cont <- (~bself).frame.cont,
      (~bself).frame <- (~bself).frame.prev
    ]).

    :- export(pred_enter/0).
    % TODO: this should be static (do not depend on '~bself')
    % (returns continuation)
    :- pred pred_enter/0 + (basal, det).
    pred_enter :- js_lang.stats([
      w.previous_choice <- w.choice,
      return(w.callhead.execute.[w])
    ]).

    :- export(suspend/0).
    :- pred suspend/0 + (basal, nondet, no_worker).
    suspend :- js_lang.stats([return('null')]).

    :- export(solve/1).
    :- pred solve/1 + (basal, det, no_worker).
    solve(Goal) :- js_lang.stats([
%KK      vardecl('old_choice_timestamp', 'choice_timestamp'), % TODO: do not make global
      (~bself).callhead <- Goal,
      (~bself).continuation_loop.[(~bself).pred_enter]
      % Goal.execute.[~bself]
%KK      'choice_timestamp' <- 'old_choice_timestamp'
    ]).
  % //    console.log('finish continuation loop');
  % //    console.log('this:'+util.inspect(this.choice, true, null));

    :- export(continuation_loop/1).
    :- pred continuation_loop/1 + (basal, det, no_worker).
    % Note: I tried "try { while(true) ... } catch(e) {}", but it 
    %       didn't show to be faster at all. Maybe in the C backend?
    continuation_loop(Next0) :- js_lang.stats([
      vardecl('next', Next0),
      while(op('!==').['next', 'null'], [
        'next' <- 'next'.[~bself]
      ])
    ]).

    :- export(resume/0).
    :- pred resume/0 + (basal, det, no_worker).
    resume :- js_lang.stats([
      (~bself).continuation_loop.[(~bself).cont]
    ]).

    :- export(restart/0).
    :- pred restart/0 + (basal, det, no_worker).
    restart :- js_lang.stats([
      (~bself).untrail.[0],
      (~bself).frame <- 'null',
      (~bself).undo <- '[]', % empty stack
      (~bself).choice <- (~bself).ok_choice,
      'choice_timestamp' <- (~bself).choice.timestamp
    ]).

    :- export(is_failed/0).
    % the worker goal has failed (no more solutions)
    % TODO: this is wrong
    :- pred is_failed/0 + (basal, semidet, no_worker).
    is_failed :-
        js_lang.test(op('===').[(~bself).choice, (~bself).failed_choice]).

    :- export(sd_solve/1).
    % execute a goal from a forced semidet context
    :- pred sd_solve/1 + (basal, semidet, no_worker).
    % TODO: 'choice_timestamp' is SHARED, which is obviously a mistake
    % TODO: this predicate may be buggy; probably I can optimize it a lot
    % TODO: add preconditions/postconditions (they can be cheaply removed in nondebug versions easily)
    sd_solve(Goal) :- js_lang.stats([
%      console.log.["Enter sd_solve: " + 'choice_timestamp'],
%      vardecl('prev_timestamp', 'choice_timestamp'),
%?      vardecl('prev_timestamp', (~bself).choice.timestamp),
      vardecl('w2', new(ctor_lookup(class, ':'(basiccontrol, worker_stacks)), [])),
%KK      'choice_timestamp' <- 'prev_timestamp', % TODO: incorrectly reset by worker creation
      'w2'.undo <- (~bself).undo, % copy w trail into w2
%      'w2'.ok_choice.timestamp <- 'choice_timestamp',
%?      'w2'.ok_choice.timestamp <- 'prev_timestamp', % TODO: avoid unnecessary trailing
      'w2'.ok_choice.trail_top <- (~bself).undo.length,
      'w2'.solve.[Goal],
%      console.log.["Exit sd_solve:" + 'choice_timestamp'],
%      console.log.["log: sd_solve:" + 'w2'.undo.length],
      (~bself).undo <- 'w2'.undo, % update w trail
%      console.log.["log: sd_solve:" + (~bself).undo],
%      console.log.["log: sd_solve:" + util.inspect.[(~bself).undo]],
%      console.log.['\"sd_solve success: \"' + paren(op('==').['w2'.is_failed.[], 'false'])],
      return(op('===').['w2'.is_failed.[], 'false'])
    ]).
}.

% ===========================================================================

:- use_package(js_lang).

:- if(defined(coarse_timestamp)).
% A time-stamp for choicepoints
:- else.
% A time-stamp for variables
:- endif.
% TODO: make it a parameter in the worker
% TODO: this seems like an old idea, what other systems use it?
:- js_native([vardecl('choice_timestamp', 0)]).

:- if(true). % TODO: make conditional
% Variable identifiers
:- js_native([vardecl('global_var_id', 0)]).
:- endif.

% ===========================================================================

:- class choice {
    :- doc(title, "A Choice Point").
    % TODO: We base choice on 'nonvar' so that it can be used directly
    %       in cut (!/0). Anyway we may want to box it in a different
    %       way and avoid this.
    :- extends basalnv_base.

    :- use_package(js_lang).

    :- export(cons__/2).
    :- pred cons__/2 + (basal, det, no_worker).
    % 'W' is a worker
    cons__(W, AltCont) :- js_lang.stats([
      (~bself).timestamp <- 'choice_timestamp',
      (~bself).altcont <- AltCont,
      (~bself).callhead <- W.callhead,
      (~bself).cont <- W.cont,
      (~bself).frame <- W.frame,
      (~bself).trail_top <- W.undo.length,
      (~bself).choice <- W.choice
    ]).
  
    :- export(sd_unify_nonvar/1).
    :- pred sd_unify_nonvar/1 + (basal, semidet).
    sd_unify_nonvar(Other) :-
        js_lang.test(op('===').[Other, ~bself]).

    :- export(cut/0).
    :- pred cut/0 + (basal, det).
    cut :- js_lang.stats([
      w.choice <- ~bself,
      w.previous_choice <- ~bself
      % TODO: not correct, you should reset all variables in the segment
%      'choice_timestamp' <- w.choice.timestamp
    ]).

    :- export(fail/0).
    :- pred fail/0 + (basal, nondet).
    fail :- js_lang.stats([
      % untrail (undo changes)
      w.untrail.[(~bself).trail_top],
      % execute the alternative
      w.callhead <- (~bself).callhead,
      w.cont <- (~bself).cont,
      w.frame <- (~bself).frame,
      % Reset timestamp (the related portion of cells must be deactivated)
      'choice_timestamp' <- w.choice.timestamp - 1, % TODO: check that it is correct
      % TODO: unfolded 'cut'
      w.choice <- (~bself).choice,
      w.previous_choice <- (~bself).choice,
      return((~bself).altcont)
    ]).
}.

% ===========================================================================

% TODO: This class wraps a 'basal' class. Find a solution that works
%       both for the 'choice' and the 'worker' class.

% TODO: what happens with this constructor?
worker := ~(':'(':'(basiccontrol, worker), new__)).
:- class worker {
    :- doc(title, "Application-side Workers").
    :- extends(simple_box('===')).

    :- use_package(js_lang).

    % TODO: This could mimick library(concurrency)

    % Note: In the near future, it could be possible to create threads
    %       in standard JS implementations. However, there seem to be
    %       no plans for threads with shared memory. All communication
    %       is done by message passing, where data is copied.

    % TODO: new__/1 should be static?!
    :- export(new__/1).
    :- pred new__/1 :: worker + (detfun, argsbox([unbox])).
    new__ := ~js_lang.expr(new(ctor_lookup(class, ':'(basiccontrol, worker_stacks)), [])).

    :- export(execute/1).
    :- pred execute/1 + det.
    execute(Goal) :- js_lang.stats([(~self).solve.[Goal]]).

    :- export(resume/0).
    :- pred resume/0 + det.
    resume :- js_lang.stats([(~self).resume.[]]).

    :- export(restart/0).
    :- pred restart/0 + det.
    restart :- js_lang.stats([(~self).restart.[]]).
}.

% ===========================================================================

% A simple reflection class for modules (for '$owner_module'/1 method)
% Note: classes are a particular kind of modules
:- class t_module {
    :- doc(title, "Class for Modules (reflection)").
    :- extends(simple_box('===')).
    % 'name' and 'enclosing_module' are attributes of the prototype of
    % the class assigned in 'self'.

    :- use_package(js_lang).

    :- export('$enclosing_module'/1).
    :- pred '$enclosing_module'/1 :: t_module + (detfun, argsbox([unbox])).
    '$enclosing_module' := ~js_lang.expr((~self).prototype.enclosing_module).

    :- export('$is_root'/0).
    :- pred '$is_root'/0 + semidet.
    % The class that contains all the classes in the system
    '$is_root' :- js_lang.test(op('===').[(~self), 'null']).

    :- export('$name'/1).
    :- pred '$name'/1 :: t_string + (detfun, argsbox([unbox])).
    '$name' := ~js_lang.expr((~self).prototype.name).

    :- export('$to_str'/1).
    % TODO: use streams to be faster?
    '$to_str' := S :-
	( '$is_root' ->
	    S = "\6\root"
	; Sname = ~'$name',
	  Owner = ~'$enclosing_module',
	  S = ~'$to_str_q'(Owner, Sname)
	).

    % TODO: Add methods here to define new atoms, functors, etc.
}.

% (common for nonvar and t_module)
'$to_str_q'(Owner, Sname) := S0 :-
	( Owner.'$is_root' ->
	    % TODO: for module names?
	    S0 = Sname
	; S00 = ~Owner.'$to_str',
	  ( hide_module(S00) ->
	      S0 = Sname % default scope for print
	  ; S0 = S00 + "." + Sname
	  )
	).

% Those modules are imported by default, do not show the module in those cases
% TODO: customize like operators
hide_module("user").
hide_module("basiccontrol").

% ===========================================================================

:- class nonvar {
    :- doc(title, "Base class for nonvar terms").
    :- doc(comment, "The class from which all terms which are not
       variables are derived.").

    % The properties of 'nonvar' objects are:
    %  - they are dereferenced
    %  - [finish]
    % TODO: rename 'nonvar' by 'nonref'?
    :- extends nonvar_base.

    :- use_package(js_lang).

    :- export(cons__/0).
    :- pred cons__/0 + (basal, det, no_worker).
    cons__ :- js_lang.stats([]).

    :- export(sd_unify/1).
    :- pred sd_unify/1 + (basal, semidet).
    sd_unify(Other) :-
        js_lang.test(Other.sd_unify_nonvar.[w, ~bself]).

    :- export(unbind/0).
    :- pred unbind/0 + (basal, det, no_worker).
    unbind :- js_lang.stats([
      console.error.["internal error: cannot unbind a nonvar term"]
    ]).

    :- export(deref/1).
    % Dereference the term
    :- pred deref/1 + (basal, detfun, no_worker).
    deref := ~js_lang.expr(~bself).

    :- export(execute/0).
    :- pred execute/0 + (basal, nondet).
    execute :- js_lang.stats([
      return(w.error.["non executable functor"])
    ]).

    :- export('$owner_module'/1).
    % TODO: I am not sure that t_module is necessary here (it is similar to an atom...)
    :- pred '$owner_module'/1 :: t_module + (detfun, argsbox([unbox])).
    '$owner_module' := ~js_lang.expr((~self).owner_module).

    % TODO: '$name'/1 and '$arity'/1 are probably methods for functors, not every class
    :- export('$name'/1).
    :- pred '$name'/1 :: t_string + (detfun, argsbox([unbox])).
    '$name' := ~js_lang.expr((~self).name).

    :- export('$arity'/1).
    :- pred '$arity'/1 :: t_num + (detfun, argsbox([unbox])).
    '$arity' := ~js_lang.expr((~self).arity).

    % TODO: this implementation of '$arg' may be very slow, why not generate a switch?
    :- export('$arg'/2).
    :- pred '$arg'/2 :: t_num * term + (detfun, argsbox([unbox, box])).
    % Note: Parenthesis are necessary around I-1 because '+' is
    %       working on strings, not numbers.
    % TODO: this MUST be a builtin (it depends on strarg_array flag)
    '$arg'(I) := ~js_lang.expr((~self).elem("a" + paren(I-1))).

    :- export('$copy_fresh'/1).
    :- pred '$copy_fresh'/1 + detfun.
    '$copy_fresh' := ~js_lang.expr((~self).copy_fresh.[]).

    % A class instance, not a normal structure
    % TODO: special, has no arity -- fix?
    :- pred '$is_obj'/0 + semidet.
    '$is_obj' :-
        js_lang.test(op('===').[typeof((~self).arity), "undefined"]). 

    :- export('$to_str'/1).
    % ('$to_str'/1 hook)
    %
    % Fast '$to_str' method for functors
    % TODO: define as a fmap with a HO arg so that we can reuse this
    %       code for multiple things (not only I/O); use a nbmut or
    %       stream to generate the output.
    '$to_str' := S :-
        '$is_obj', !,
	% Cannot enumerate attributes of objects by default, just show
	% some internal name.
        % TODO: define hooks (merge with hooks for attributed variables)
	Sname = ~'$name',
	S = "<#" + Sname + ">".
    '$to_str' := S :-
	Sname = ~'$name',
	Owner = ~'$owner_module',
	S0 = ~'$to_str_q'(Owner, Sname),
	( ~'$arity' = 0 ->
	    S = S0
        ; S1 = S0 + "(",
          args_str(1, S1, S2),
          S = S2 + ")"
        ).
    args_str(I, S0, S) :- 
        ( I > ~'$arity' -> S = S0
        ; I = ~'$arity' ->
            S = S0 + (~((~'$arg'(I)).'$to_str'))
        ; S1 = S0 + (~((~'$arg'(I)).'$to_str')) + ",",
	  args_str(I + 1, S1, S)
        ).

    % Lexical comparison
    :- export('$compare'/2).
    '$compare'(Other, Value) :- var(Other), !, Value = -1.
    '$compare'(Other, Value) :- '$kind_of'(Other, t_num), !, Value = -1.
    '$compare'(Other, Value) :-
        Arity = ~'$arity',
        OArity = ~Other.'$arity',
	( Arity < OArity -> Value = -1
	; Arity > OArity -> Value = 1
	; '$compare_'(Other, Value0),
	  '$compare_args'(Value0, 1, Arity, Other, Value)
	).

    '$compare_'(Other, Value) :- ~Other.'$name' < ~'$name', !, Value = 1.
    '$compare_'(Other, Value) :- ~Other.'$name' > ~'$name', !, Value = -1.
    '$compare_'(Other, Value) :- ~Other.'$name' = ~'$name', !, Value = 0.

    '$compare_args'(0, I, N, Other, Value) :- I =< N, !,
        % same value, more arguments, continue comparing
        Arg = ~'$arg'(I),
	OArg = ~Other.'$arg'(I),
	Arg.'$compare'(OArg, Value1),
        '$compare_args'(Value1, I + 1, N, Other, Value).
    '$compare_args'(Value, _, _, _, Value). % no more args or found diff
}.

% ===========================================================================
:- doc(section, "Builtins treated by the compiler").

:- export(','/2).
(A, B) :- A, B. % (wrapper for built-in)

:- export((;)/2).
(A ; B) :- A ; B. % (wrapper for built-in)

:- export('$caller_choice'/1).
'$caller_choice'(_) :- true. % TODO: special builtin (not definable)

:- export('$get_choice'/1).
'$get_choice'(A) :- '$get_choice'(A). % (wrapper for built-in)

:- export('$cut'/1).
'$cut'(A) :- '$cut'(A). % (wrapper for built-in)

:- export(true/0).
true :- true.

:- export(fail/0).
fail :- fail. % (wrapper for built-in)

:- export(repeat/0).
repeat.
repeat :- repeat.

% ---------------------------------------------------------------------------
% TODO: Those are predicate that are not in basiccontrol, but could be

:- module sys {
    :- export(suspend/0).
    suspend :- '$suspend'. % (this is a builtin)
}.
