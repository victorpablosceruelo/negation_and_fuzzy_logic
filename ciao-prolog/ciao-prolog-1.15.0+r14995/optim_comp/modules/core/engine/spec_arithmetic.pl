% This file includes declarations to specialize arithmetic and builtins
% TODO: not all cases are contemplated
% TODO: it should be a module (implement exported properties) or be included in other modules
% TODO: add alternative syntax (similar to cbool, etc.) to simplify all those declarations
% TODO: use :- '$pragma'(unbox_cons) ?? always? as part of unbox? independently?

:- op(50, fx, [(~)]).
:- op(50, fx, [(@)]).
:- op(40, yfx, [(.)]).
:- set_prolog_flag(read_infix_dot, on).
:- op(980, xfx, [(<-)]). % priority between (::) and (,)
:- op(1100, xfy, [('|')]).
:- op(1150, xfx, [(:=)]).
:- op(1150, fx, [(pred)]).

:- '$native_weak_inline'(include('math.h')).
% :- '$native_weak_inline'(declare(myfloor, function([flt64_t], flt64_t), lambda([X], [return(call(floor, [X]))]))).

:- '$forceprops'(term_typing:var/1, [
	specialize = [on([any], rename('$is_var'/1))]
]).
:- '$props'('$is_var'/1, [
	imp = semidet,
	argmodes = [in],
	argderefs = [true],
	argmems = [cvar],
	sht_usermemo = shtdef([any], [var]),
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([X],
	  is_var(@X)
	))]).
:- '$forceprops'(term_typing:nonvar/1, [
	specialize = [on([any], rename('$is_nonvar'/1))]
]).
:- '$props'('$is_nonvar'/1, [
	imp = semidet,
	argmodes = [in],
	argderefs = [true],
	argmems = [cvar],
	sht_usermemo = shtdef([any], [nonvar]),
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([X],
	  \+ is_var(@X)
	))]).

:- '$props'('$=smallintunbox'/2, [
	imp = semidet,
	argmodes = [in, in],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
	argunboxs = [true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([X, Y],
	  @X == @Y
	))]).

:- '$forceprops'(term_basic:'$unify'/2, [
	specialize = [on([smallint, smallint], rename('$=smallint'/2))]
]).
:- '$props'('$=smallint'/2, [
	imp = semidet,
	argmodes = [in, in],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
	argunboxs = [false, false],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([X, Y],
	  @X == @Y
	))]).

:- '$forceprops'(arithmetic:'>'/2, [
	specialize = [on([smallint, smallint], rename('$>smallint'/2))]
]).
:- '$props'('$>smallint'/2, [
	imp = semidet,
	argmodes = [in, in],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
	argunboxs = [false, false],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([X, Y],
	  @X > @Y
	))]).

:- '$forceprops'(arithmetic:'>='/2, [
	specialize = [on([smallint, smallint], rename('$>=smallint'/2))]
]).
:- '$props'('$>=smallint'/2, [
	imp = semidet,
	argmodes = [in, in],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
	argunboxs = [false, false],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([X, Y],
	  @X >= @Y
	))]).

:- '$forceprops'(arithmetic:'<'/2, [
	specialize = [on([smallint, smallint], rename('$<smallint'/2))]
]).
:- '$props'('$<smallint'/2, [
	imp = semidet,
	argmodes = [in, in],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
	argunboxs = [false, false],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([X, Y],
	  @X < @Y
	))]).

:- '$forceprops'(arithmetic:'=<'/2, [
	specialize = [on([smallint, smallint], rename('$=<smallint'/2))]
]).
:- '$props'('$=<smallint'/2, [
	imp = semidet,
	argmodes = [in, in],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
	argunboxs = [false, false],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([X, Y],
	  @X =< @Y
	))]).

:- '$forceprops'(arithmetic:'=\\='/2, [
	specialize = [on([smallint, smallint], rename('=\\=smallint'/2))]
]).
:- '$props'('=\\=smallint'/2, [
	imp = semidet,
	argmodes = [in, in],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
	argunboxs = [false, false],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impnat = ptoc_macro(imacro_def([X, Y],
	  \+ @X == @Y
	))]).

:- '$ptoc_typeprop'(float, cons, float_new/2).
:- '$ptoc_typeprop'(float, box, float_box/2).
:- '$ptoc_typeprop'(float, unbox, float_unbox/2).
:- '$ptoc_typeprop'(float, imptype, flt64).

:- '$props'(float_box/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [false, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([float, var], [float, float]),
	argunboxs = [true, false],
	saveregs = all, noderefmod = true,
	heap_usage = max(4), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- ~box_float(@X)
        ))]).
:- '$pragma'(ip((:- pred box_float/2 + foreignfun([flt64], tagged, 'BoxFloat') + prop(no_worker)))).
% TODO: use a specialized TaggedToFloat that assumes that the number is a float?
:- '$props'(float_unbox/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([any, var], [any, float]),
	argunboxs = [false, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- ~tagged_to_float(@X)
        ))]).
:- '$pragma'(ip((:- pred tagged_to_float/2 + foreignfun([tagged], flt64, 'TaggedToFloat') + prop(no_worker)))).
:- '$props'(float_new/2, [
	imp = det,
	argmodes = [param, out],
	argderefs = [false, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([any, var], [any, float]),
	argunboxs = [false, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([Value, Var],
	  Var <- ~'$cast'(Value, flt64)
        ))]).

:- '$ptoc_typeprop'(smallint, box, smallint_box/2).
:- '$ptoc_typeprop'(smallint, unbox, smallint_unbox/2).
:- '$ptoc_typeprop'(smallint, imptype, intval).

:- '$props'(smallint_box/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [false, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, var], [smallint, smallint]),
	argunboxs = [true, false],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- ~make_small(@X) % TODO: trunc nonsmall bits? here or in operations? raise error on overflow?
        ))]).
:- '$props'(smallint_unbox/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([any, var], [any, smallint]),
	argunboxs = [false, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- ~get_small(@X)
        ))]).
:- '$props'(smallint_new/2, [
	imp = det,
	argmodes = [param, out],
	argderefs = [false, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([any, var], [any, smallint]),
	argunboxs = [false, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([Value, Var],
	  Var <- ~'$trust_typed'(Value, intval)
        ))]).

:- '$props'(cast_smallint_to_float/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, var], [smallint, float]),
	argunboxs = [true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- ~'$cast'(@X, flt64)
        ))]).
:- '$props'(cast_float_to_smallint/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([float, var], [float, smallint]),
	argunboxs = [true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- ~'$cast'(@X, intval)
        ))]).
:- '$props'(float_round_and_cast_to_smallint/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([float, var], [float, smallint]),
	argunboxs = [true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y], [
	  % TODO: use c99 and lround?? 
          % TODO: breaks if I use floor here instead of myfloor!!!
%	  Y = call(myfloor, [X + 0.5])
%	  Y = call(floor, [X + 0.5])
	  Y <- ~rint(@X)
        ]))]).
:- '$pragma'(ip((:- pred rint/2 + foreignfun([flt64], intval, 'rint') + prop(no_worker)))).

:- '$props'(float_div/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [true, true, true],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([float, float, var], [float, float, float]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- @X / @Y
        ))]).
:- '$props'(div_sma_sma/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [true, true, true],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, float]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- ~'$cast'(@X, flt64) / ~'$cast'(@Y, flt64)
        ))]).
:- '$props'(idiv_sma_sma/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [true, true, true],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- @X / @Y
        ))]).
:- '$props'(div_flo_sma/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [true, true, true],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([float, smallint, var], [float, smallint, float]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- X / ~'$cast'(@Y, flt64)
        ))]).

:- '$props'(float_mul/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [true, true, true],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([float, float, var], [float, float, float]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- @X * @Y
        ))]).
:- '$props'(mul_sma_flo/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [true, true, true],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([smallint, float, var], [smallint, float, float]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- @X * @Y
        ))]).
:- '$props'(mul_flo_sma/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [true, true, true],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([float, smallint, var], [float, smallint, float]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- @X * @Y
        ))]).
:- '$props'(mul_sma_sma/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [true, true, true],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- @X * @Y
        ))]).

:- '$props'(float_add/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [true, true, true],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([float, float, var], [float, float, float]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- @X + @Y
        ))]).

:- '$props'(float_sub/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [true, true, true],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([float, float, var], [float, float, float]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- @X - @Y
        ))]).

:- '$props'(float_fmod/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [true, true, true],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([float, float, var], [float, float, float]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- ~fmod(@X, @Y)
        ))]).
:- '$pragma'(ip((:- pred fmod/3 + foreignfun([flt64, flt64], flt64, 'fmod') + prop(no_worker)))).

:- '$props'(float_floor/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([float, var], [float, float]),
	argunboxs = [true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- ~floor(@X)
        ))]).
:- '$pragma'(ip((:- pred floor/2 + foreignfun([flt64], flt64, 'floor') + prop(no_worker)))).
:- '$props'(float_fabs/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([float, var], [float, float]),
	argunboxs = [true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- ~fabs(@X)
        ))]).
:- '$pragma'(ip((:- pred fabs/2 + foreignfun([flt64], flt64, 'fabs') + prop(no_worker)))).
:- '$props'(float_atan/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([float, var], [float, float]),
	argunboxs = [true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- ~atan(@X)
        ))]).
:- '$pragma'(ip((:- pred atan/2 + foreignfun([flt64], flt64, 'atan') + prop(no_worker)))).
:- '$props'(float_sin/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([float, var], [float, float]),
	argunboxs = [true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- ~sin(@X)
        ))]).
:- '$pragma'(ip((:- pred sin/2 + foreignfun([flt64], flt64, 'sin') + prop(no_worker)))).

:- '$props'(smallint_add/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [false, false, false],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- @X + @Y
	))]).
:- '$props'(smallint_sub/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [false, false, false],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- @X - @Y
	))]).
:- '$props'(smallint_neg/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [false, false],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, var], [smallint, smallint]),
	argunboxs = [true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- - @X
	))]).
:- '$props'(smallint_add/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [false, false, false],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- @X + @Y
	))]).
:- '$props'(smallint_plus/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [false, false],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, var], [smallint, smallint]),
	argunboxs = [true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- @X
	))]).
:- '$props'(smallint_mod/3, [
	imp = det,
	argmodes = [in, in, out],
	argderefs = [false, false, false],
	argmems = [cvar, cvar, cvar],
	sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
	argunboxs = [true, true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y, Z],
	  Z <- @X mod @Y
	))]).

% TODO: include a property to define native functions
:- '$props'(smallint_dec/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, var], [smallint, smallint]),
	argunboxs = [true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- @X - ~'$trust_typed'(1, intval)
        ))]).
:- '$props'(smallint_inc/2, [
	imp = det,
	argmodes = [in, out],
	argderefs = [true, true],
	argmems = [cvar, cvar],
	sht_usermemo = shtdef([smallint, var], [smallint, smallint]),
	argunboxs = [true, true],
	saveregs = all, noderefmod = true,
	heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impnat = ptoc_macro(imacro_def([X, Y],
	  Y <- @X + ~'$trust_typed'(1, intval)
        ))]).

:- '$forceprops'(arithmetic:'$*'/3, [
	specialize = [on([float, float, var], rename(float_mul/3))]
]).
:- '$forceprops'(arithmetic:'$*'/3, [
	specialize = [on([smallint, float, var], rename(mul_sma_flo/3))]
]).
:- '$forceprops'(arithmetic:'$*'/3, [
	specialize = [on([float, smallint, var], rename(mul_flo_sma/3))]
]).
:- '$forceprops'(arithmetic:'$*'/3, [
	specialize = [on([smallint, smallint, var], rename(mul_sma_sma/3))]
]).
:- '$forceprops'(arithmetic:'$/'/3, [
	specialize = [on([float, float, var], rename(float_div/3))]
]).
:- '$forceprops'(arithmetic:'$/'/3, [
	specialize = [on([float, smallint, var], rename(div_flo_sma/3))]
]).
:- '$forceprops'(arithmetic:'$/'/3, [
	specialize = [on([smallint, smallint, var], rename(div_sma_sma/3))]
]).
:- '$forceprops'(arithmetic:'$//'/3, [
	specialize = [on([smallint, smallint, var], rename(idiv_sma_sma/3))]
]).
:- '$forceprops'(arithmetic:'$sin'/2, [
	specialize = [on([float, var], rename(float_sin/2))]
]).
:- '$forceprops'(arithmetic:'$float'/2, [
	specialize = [on([smallint, var], rename(cast_smallint_to_float/2))]
]).
% TODO: this is not correct because we can exit smallint boundaries
:- '$forceprops'(arithmetic:'$--'/2, [
	specialize = [on([smallint, var], rename(smallint_dec/2))]
]).
% TODO: this is not correct because we can exit smallint boundaries
:- '$forceprops'(arithmetic:'$++'/2, [
	specialize = [on([smallint, var], rename(smallint_inc/2))]
]).
% TODO: this is not correct because we can exit smallint boundaries
:- '$forceprops'(arithmetic:'$+'/2, [
	specialize = [on([smallint, var], rename(smallint_plus/2))]
]).
% TODO: this is not correct because we can exit smallint boundaries
:- '$forceprops'(arithmetic:'$+'/3, [
	specialize = [on([smallint, smallint, var], rename(smallint_add/3))]
]).
:- '$forceprops'(arithmetic:'$+'/3, [
	specialize = [on([float, float, var], rename(float_add/3))]
]).
% TODO: this is not correct because we can exit smallint boundaries
:- '$forceprops'(arithmetic:'$-'/2, [
	specialize = [on([smallint, var], rename(smallint_neg/2))]
]).
% TODO: this is not correct because we can exit smallint boundaries
:- '$forceprops'(arithmetic:'$-'/3, [
	specialize = [on([smallint, smallint, var], rename(smallint_sub/3))]
]).
:- '$forceprops'(arithmetic:'$-'/3, [
	specialize = [on([float, float, var], rename(float_sub/3))]
]).
:- '$forceprops'(arithmetic:'$mod'/3, [
	specialize = [on([smallint, smallint, var], rename(smallint_mod/3))]
]).
% TODO: this is not correct since a float can be translated to a bigint
:- '$forceprops'(arithmetic:'$integer'/2, [
	specialize = [on([float, var], rename(cast_float_to_smallint/2))]
]).
% TODO: this is not correct since a float can be translated to a bigint
:- '$forceprops'(arithmetic:'$round'/2, [
	specialize = [on([float, var], rename(float_round_and_cast_to_smallint/2))]
]).
