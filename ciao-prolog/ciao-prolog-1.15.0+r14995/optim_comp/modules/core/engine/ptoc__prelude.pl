% impcode: external code that implements the predicate
% impclass: builtin or (default) | builtin 
% argmodes: list of in or out (out needs a var type)
% argmems: list of push or x(R)

% ---------------------------------------------------------------------------
% internal definitions
:- '$forceprops'(basiccontrol:'fail'/0, [
	imp = semidet, saveregs = all, heap_usage = max(0), frame_usage = max(0), should_trim_frame = false
	]).
:- '$forceprops'(basiccontrol:'$cut'/1, [
%	impnat = ptoc_builtin,
	sht_usermemo = shtdef([smallint], [smallint]),
	argderefs = [true], imp = detcut, saveregs = all, argmems = [cvar], heap_usage = max(0), frame_usage = max(0), should_trim_frame = false,
	impmacro = imacro_def([X],
	  v__inline_cut(X)
	)
	]).
:- '$forceprops'(basiccontrol:'$choice'/1, [
	sht_usermemo = shtdef([var], [smallint]),
	argderefs = [true], imp = det, saveregs = all, argmodes = [out], argmems = [cvar], heap_usage = max(0), frame_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impmacro = imacro_def([X],
	  '<-'(Y, '~'(v__inline_choice))
	)
	]).
% TODO: are all props correct or useful?
:- '$forceprops'(basiccontrol:'$caller_choice'/1, [
	sht_usermemo = shtdef([var], [smallint]),
	argderefs = [true], imp = det, saveregs = all, argmodes = [out], argmems = [default_choice], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true,
	impmacro = imacro_def([_], true) % TODO: just uses the mem to do a copy (strange)
	]).

:- '$forceprops'(term_basic:'$unify'/2, [
	impcode = cunify,
	imp = semidet, saveregs = all, argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), should_trim_frame = false
	]).
:- '$forceprops'(term_basic:'$instance'/2, [
	imp = semidet, saveregs = all, argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), should_trim_frame = false
	]).
:- '$forceprops'(term_basic:'$equal'/2, [
	imp = semidet, saveregs = all, argmems = [cvar, cvar], argderefs = [true, true], heap_usage = max(0), frame_usage = max(0), should_trim_frame = false,
	impmacro = imacro_def([X,Y],
	  v__inline_equal('@'(X), '@'(Y))
	)
	]).

:- '$forceprops'(term_basic:'$varmem'/2, [
	imp = det, saveregs = all, argmodes = [in, param], argmems = [cvar, cvar], argderefs = [false, false], argimptypes = [unknown, unknown], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true
	]).
:- '$forceprops'(term_basic:'$trust_type'/2, [
	imp = det, saveregs = all, argmodes = [in, param], argmems = [cvar, cvar], argderefs = [false, false], argimptypes = [unknown, unknown], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true
	]).
:- '$forceprops'(term_basic:'$trust_imptype'/2, [
	imp = det, saveregs = all, argmodes = [in, param], argmems = [cvar, cvar], argderefs = [false, false], argimptypes = [unknown, unknown], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true
	]).
:- '$forceprops'(term_basic:'$box'/2, [
	imp = det, noderefmod = true, saveregs = all, argmodes = [in, out], argmems = [cvar, cvar], argderefs = [false, false], argimptypes = [unknown, tagged], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true
	]).
:- '$forceprops'(term_basic:'$unbox'/2, [
	imp = det, noderefmod = true, saveregs = all, argmodes = [in, out], argmems = [cvar, cvar], argderefs = [false, false], argimptypes = [tagged, unknown], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	nosideeffdet = true, nosideeff = true
	]).
:- '$forceprops'(term_basic:'$check_test_str'/2, [
	imp = semidet, noderefmod = true, saveregs = all, argmodes = [in, param], argmems = [cvar, cvar], argderefs = [true, false], argimptypes = [tagged, unknown], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false,
	impmacro = imacro_def([X,Y],
	  v__inline_test('@'(X),Y)
	)
	]).
:- '$forceprops'(term_basic:'$bind'/2, [
	imp = det, saveregs = all, argmodes = [in, in], argmems = [cvar, cvar], argderefs = [true, true], argimptypes = [tagged, tagged], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
	]).
% only works if the argument is the Y register that we want to trail
:- '$forceprops'(term_basic:'$trail_if_conditional'/1, [
	imp = det, saveregs = all, argmodes = [in], argmems = [cvar], argderefs = [false], argimptypes = [tagged], heap_usage = max(0), frame_usage = max(0), trail_usage = max(1), should_trim_frame = false,
	impmacro = imacro_def([X],
	  v__inline_trail_if_conditional(X)
	)
	]).
% ---------------------------------------------------------------------------

% Some predefined types
:- '$forceprops'(term_typing:'atom'/1, [
	sht_usermemo = shtdef([any], [atom]),
	specialize = [on([atom], true)]
	]).
:- '$forceprops'(term_typing:'atomic'/1, [
	sht_usermemo = shtdef([any], [atomic]),
	specialize = [on([atomic], true)]
	]).
:- '$forceprops'(term_typing:'nonvar'/1, [
	sht_usermemo = shtdef([any], [nonvar]),
	specialize = [on([nonvar], true)]
	]).
:- '$forceprops'(term_typing:'number'/1, [
	sht_usermemo = shtdef([any], [number]),
	specialize = [on([number], true)]
	]).
:- '$forceprops'(term_typing:'var'/1, [
	sht_usermemo = shtdef([any], [var]),
	specialize = [on([var], true)]
	]).


%:- if(use_ptoc_spec).
% begin ptocspec
% :- '$forceprops'(arithmetic:'=:='/2, [specialize = [on([smallint, smallint], rename('arithmetic:=:=__small'))]]).
% :- '$forceprops'(arithmetic:'=\\='/2, [specialize = [on([smallint, smallint], rename('arithmetic:=\\=__small'))]]).
% :- '$forceprops'(arithmetic:'<'/2, [specialize = [on([smallint, smallint], rename('arithmetic:<__small'))]]).
% :- '$forceprops'(arithmetic:'>='/2, [specialize = [on([smallint, smallint], rename('arithmetic:>=__small'))]]).
% :- '$forceprops'(arithmetic:'>'/2, [specialize = [on([smallint, smallint], rename('arithmetic:>__small'))]]).
% :- '$forceprops'(arithmetic:'=<'/2, [specialize = [on([smallint, smallint], rename('arithmetic:=<__small'))]]).
% :- '$forceprops'(arithmetic:'$-'/2, [specialize = [on([smallint, var], rename('arithmetic:$-__small'))]]).
% :- '$forceprops'(arithmetic:'$+'/2, [specialize = [on([smallint, var], rename('arithmetic:$+__small'))]]).
% :- '$forceprops'(arithmetic:'$--'/2, [specialize = [on([smallint, var], rename('arithmetic:$--__small'))]]).
% :- '$forceprops'(arithmetic:'$++'/2, [specialize = [on([smallint, var], rename('arithmetic:$++__small'))]]).
% :- '$forceprops'(arithmetic:'$\\'/2, [specialize = [on([smallint, var], rename('arithmetic:$\\__small'))]]).
% :- '$forceprops'(arithmetic:'$abs'/2, [specialize = [on([smallint, var], rename('arithmetic:$abs__small'))]]).
% :- '$forceprops'(arithmetic:'$sign'/2, [specialize = [on([smallint, var], rename('arithmetic:$sign__small'))]]).
% :- '$forceprops'(arithmetic:'$+'/3, [specialize = [on([smallint, smallint, var], rename('arithmetic:$+__small'))]]).
% :- '$forceprops'(arithmetic:'$-'/3, [specialize = [on([smallint, smallint, var], rename('arithmetic:$-__small'))]]).
% :- '$forceprops'(arithmetic:'$*'/3, [specialize = [on([smallint, smallint, var], rename('arithmetic:$*__small'))]]).
% :- '$forceprops'(arithmetic:'$/'/3, [specialize = [on([smallint, smallint, var], rename('arithmetic:$/__small'))]]).
% :- '$forceprops'(arithmetic:'$//'/3, [specialize = [on([smallint, smallint, var], rename('arithmetic:$//__small'))]]).
% :- '$forceprops'(arithmetic:'$rem'/3, [specialize = [on([smallint, smallint, var], rename('arithmetic:$rem__small'))]]).
% :- '$forceprops'(arithmetic:'$#'/3, [specialize = [on([smallint, smallint, var], rename('arithmetic:$#__small'))]]).
% :- '$forceprops'(arithmetic:'$/\\'/3, [specialize = [on([smallint, smallint, var], rename('arithmetic:$/\\__small'))]]).
% :- '$forceprops'(arithmetic:'$\\/'/3, [specialize = [on([smallint, smallint, var], rename('arithmetic:$\\/__small'))]]).
% :- '$forceprops'(arithmetic:'$<<'/3, [specialize = [on([smallint, smallint, var], rename('arithmetic:$<<__small'))]]).
% :- '$forceprops'(arithmetic:'$>>'/3, [specialize = [on([smallint, smallint, var], rename('arithmetic:$>>__small'))]]).
% :- '$forceprops'(arithmetic:'$mod'/3, [specialize = [on([smallint, smallint, var], rename('arithmetic:$mod__small'))]]).

% :- '$forceprops'(arithmetic:'=:=__small'/2,
% 	[
%             impcode = q(arith_small, numeq_2),
% 	    sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
% 	    saveregs = all, imp = semidet, argderefs = [true, true], argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'=\\=__small'/2,
% 	[
%             impcode = q(arith_small, numne_2),
% 	    sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
% 	    saveregs = all, imp = semidet, argderefs = [true, true], argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'<__small'/2,
% 	[
%             impcode = q(arith_small, numlt_2),
% 	    sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
% 	    saveregs = all, imp = semidet, argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'>=__small'/2,
% 	[
%             impcode = q(arith_small, numge_2),
% 	    sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
% 	    saveregs = all, imp = semidet, argderefs = [true, true], argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'>__small'/2,
% 	[
%             impcode = q(arith_small, numgt_2),
% 	    sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
% 	    saveregs = all, imp = semidet, argderefs = [true, true], argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'=<__small'/2,
% 	[
%             impcode = q(arith_small, numle_2),
% 	    sht_usermemo = shtdef([smallint, smallint], [smallint, smallint]),
% 	    saveregs = all, imp = semidet, argderefs = [true, true], argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$-__small'/2,
% 	[
%             impcode = q(arith_small, minus_1),
% 	    sht_usermemo = shtdef([smallint, var], [smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, out], argderefs = [true, true], argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$+__small'/2,
% 	[
%             impcode = q(arith_small, plus_1),
% 	    sht_usermemo = shtdef([smallint, var], [smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, out], argderefs = [true, true], argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$--__small'/2,
% 	[
%             impcode = q(arith_small, sub1_1),
% 	    sht_usermemo = shtdef([smallint, var], [smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, out], argderefs = [true, true], argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$++__small'/2,
%         [
%             impcode = q(arith_small, add1_1),
% 	    sht_usermemo = shtdef([smallint, var], [smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, out], argderefs = [true, true], argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$\\__small'/2,
% 	[
%             impcode = q(arith_small, not_1),
% 	    sht_usermemo = shtdef([smallint, var], [smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, out], argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$abs__small'/2,
% 	[
%             impcode = q(arith_small, abs_1),
% 	    sht_usermemo = shtdef([smallint, var], [smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, out], argderefs = [true, true], argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$sign__small'/2,
% 	[
%             impcode = q(arith_small, sign_1),
% 	    sht_usermemo = shtdef([smallint, var], [smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, out], argderefs = [true, true], argmems = [cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$+__small'/3,
% 	[
%             impcode = q(arith_small, plus_2),
% 	    sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, in, out], argderefs = [true, true, true], argmems = [cvar, cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$-__small'/3,
% 	[
%             impcode = q(arith_small, minus_2),
% 	    sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, in, out], argderefs = [true, true, true], argmems = [cvar, cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$*__small'/3,
% 	[
%             impcode = q(arith_small, times_2),
% 	    sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, in, out], argderefs = [true, true, true], argmems = [cvar, cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$/__small'/3,
% 	[
%             impcode = q(arith_small, fdivide_2),
% 	    sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, in, out], argderefs = [true, true, true], argmems = [cvar, cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$//__small'/3,
% 	[
%             impcode = q(arith_small, idivide_2),
% 	    sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, in, out], argderefs = [true, true, true], argmems = [cvar, cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$rem__small'/3,
% 	[
%             impcode = q(arith_small, rem_2),
% 	    sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, in, out], argderefs = [true, true, true], argmems = [cvar, cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$#__small'/3,
% 	[
%             impcode = q(arith_small, xor_2),
% 	    sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, in, out], argderefs = [true, true, true], argmems = [cvar, cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$/\\__small'/3,
% 	[
%             impcode = q(arith_small, and_2),
% 	    sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, in, out], argderefs = [true, true, true], argmems = [cvar, cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% :- '$forceprops'(arithmetic:'$\\/__small'/3,
% 	[
%             impcode = q(arith_small, or_2),
% 	    sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, in, out], argderefs = [true, true, true], argmems = [cvar, cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% % :- '$forceprops'(arithmetic:'$<<__small'/3,
% % 	[
% %             impcode = q(arith_small, lsh_2),
% % 	    sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
% % 	    saveregs = all, imp = det, argmodes = [in, in, out], argderefs = [true, true, true], argmems = [cvar, cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
% %         ]).
% % :- '$forceprops'(arithmetic:'$>>__small'/3,
% % 	[
% %             impcode = q(arith_small, rsh_2),
% % 	    sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
% % 	    saveregs = all, imp = det, argmodes = [in, in, out], argderefs = [true, true, true], argmems = [cvar, cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
% %         ]).
% :- '$forceprops'(arithmetic:'$mod__small'/3,
% 	[
%             impcode = q(arith_small, mod_2),
% 	    sht_usermemo = shtdef([smallint, smallint, var], [smallint, smallint, smallint]),
% 	    saveregs = all, imp = det, argmodes = [in, in, out], argderefs = [true, true, true], argmems = [cvar, cvar, cvar], heap_usage = max(0), frame_usage = max(0), trail_usage = max(0), should_trim_frame = false
%         ]).
% end ptocspec
%:- endif.
