:- load_compilation_module(library(rtchecks(rtchecks_inline_tr))).

:- add_goal_trans(transform_icheck/3).
:- add_sentence_trans(transform_as_saved/3).

:- use_module(library(rtchecks(rtchecks_mod)), [pop_parent_goal/1,
		push_parent_goal/1, dispatch_error/3]).


:- multifile '$saved_assertion'/3.


% '$process_throw_info'( _ , as(ErrorInfo, Loc, Goal) ) :-
% %	num( ErrorInfo ),
% 	'$saved_assertion'( ErrorInfo , MessageInfo ),
% 	!,
% 	throw( rtcheck( assrt , MessageInfo , Loc ) ).
% % Case 2: it is user program check point => we only have the locator
% '$process_throw_info'( Goal , Loc ) :-
% 	%Loc = loc( _ , _ , _ ),
% 	functor( Loc , loc , _ ),
% 	!,
% 	throw( rtcheck( pp_check, Goal, Loc ) ).
% % Case 3: None of the above options => ERROR, something was added and
% %         we do not consider it here.
% '$process_throw_info'( _ , ErrorInfo ) :-
% 	message( error, [ 'Program check point failed. INTERNAL ERROR: ',
% 			 'Unconsidered case of error ', ErrorInfo ] ),
%         halt.
