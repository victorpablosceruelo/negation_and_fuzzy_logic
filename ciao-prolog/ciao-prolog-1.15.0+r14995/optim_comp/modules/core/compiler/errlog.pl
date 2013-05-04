:- class(errlog, [], [pure, compiler(complang)]).

% An error log for the compilation
%
% The compilation phases annotates through this class the location
% information of active compilation unit. Errors and warnings reported
% from this module automatically uses this location information.
%
% Note that each instance of this class keeps separate information.
%
% Author: Jose F. Morales (based on previous code)

:- use_module(engine(basiccontrol)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(exceptions)).
:- use_module(engine(hiord_rt)).
:- use_module(engine(rt_exp), [rt_exp/6]).
:- use_module(engine(arithmetic)).
:- use_module(engine(streams_basic)).
:- use_module(engine(io_basic)).
:- use_module(engine(io_aux)).

:- use_module(engine(internals), ['$quiet_flag'/2]).

:- use_module(compiler(error_messages)).
:- use_module(compiler(store), [denormalized_spec/2]).

:- public data verbose/1.

:- data printed_depths/1.

:- data depth/1.
:- data depth__action/2.
:- data depth__src/2.
:- data depth__location/3.
:- data depth__error/1.

:- data errors/1.
:- data warnings/1.

:- constructor new_/0.
new_ :- init.

init :-
	add(depth(0)),
	add(errors(0)),
	add(warnings(0)),
	add(printed_depths([])).

% TODO: verbose is not cleaned... is that correct? (document)
:- public clear/0.
clear :-
	del(depth__src(_, _)),
	del(depth__location(_, _, _)),
	del(depth__action(_, _)),
	del(depth__error(_)),
	del(depth(_)),
	del(errors(_)),
	del(warnings(_)),
	del(printed_depths(_)),
	init.

:- public delete/0.
delete :-
	'$inst_destroy'(~self).

:- public get1_module_error/0.
get1_module_error :-
	depth(Depth),
	depth__error(Depth), !.

:- public add_module_error/0.
add_module_error :-
	depth(Depth), !,
	add(depth__error(Depth)).

% Protect a goal between push_action and pop_action preds
:- public protect/2.
%:- use_module(engine(io_basic)).
%:- use_module(library(prolog_sys)).
:- meta_predicate protect(?, goal).
protect(Action, Goal) :-
%	display(Action), nl,
%	statistics,
	push_action(Action),
	Ok = ( Goal ? yes | no ), % TODO: make it 'exception safe'
	pop_action(HadErrors),
	( HadErrors = no, Ok = no ->
            bug([Action, ' failed for unknown reason'])
	; HadErrors = yes, Ok = yes ->
            bug([Action, ' had errors but did not failed'])
        ; true
        ),
	Ok = yes.

push_action(Action) :-
	depth(Depth), !,
	Depth1 is Depth + 1,
	add(depth__action(Depth1, Action)),
	del(depth(_)),
	add(depth(Depth1)),
	del(depth__error(Depth1)),
        ( verbose(on) -> show_action ; true ).

pop_action(HadErrors) :-
	depth(Depth1),
	%
	del(depth__action(Depth1, _)),
	del(depth__src(Depth1, _)),
        del(depth__location(Depth1, _, _)),
	Depth is Depth1 - 1,
	del(depth(_)),
	add(depth(Depth)),
	%
	% propagate errors to parent actions
	( depth__error(Depth1) ->
	    del(depth__error(Depth1)),
	    add(depth__error(Depth)),
            HadErrors = yes
	; HadErrors = no
	),
	%
	( printed_depths(PrintedDepths) -> true ),
	( PrintedDepths = [LastPrintedDepth|PrintedDepths0],
	  Depth1 =< LastPrintedDepth ->
	    end_brace,
	    del(printed_depths(_)),
	    add(printed_depths(PrintedDepths0))
	; true
	).

:- public add_src/1.
add_src(Src) :-
	depth(Depth), !, add(depth__src(Depth, Src)).
:- public del_src/0.
del_src :-
	depth(Depth), !, del(depth__src(Depth, _)).
:- public add_location/2.
add_location(Ln0, Ln1) :-
	depth(Depth), !, add(depth__location(Depth, Ln0, Ln1)).
:- public del_location/0.
del_location :-
	depth(Depth), !, del(depth__location(Depth, _, _)).

:- public add_loc/1.
% TODO: loc VarNames are just ignored... (need to use logical vars)
add_loc(loc(_, Src, Ln0, Ln1)) :- !,
	add_src(Src),
	add_location(Ln0, Ln1).
add_loc('').

:- public del_loc/0.
% TODO: loc VarNames are just ignored... (need to use logical vars)
del_loc :-
	del_src,
	del_location, !.
del_loc.

:- public get1_loc/2.
% TODO: loc VarNames are just ignored... (need to use logical vars)... VarNames is inserted in the loc 
get1_loc(VarNames, loc(VarNames, Src, Ln0, Ln1)) :-
	depth(Depth),
	depth__src(Depth, Src),
	depth__location(Depth, Ln0, Ln1),
	!.
get1_loc(_, '').

:- public get1_location/2.
get1_location(Ln0, Ln1) :-
	depth(Depth),
	depth__location(Depth, Ln0, Ln1),
	!.
get1_location(_, '').

:- public static loc_dict/2.
loc_dict(loc(Dict, _, _, _), Dict).
loc_dict('', []).

:- public static empty_loc/1.
% (Used also for location of automatically generated code)
%empty_loc(loc([], '', '', '')).
% TODO: this is a special representation for empty loc, remove the previous one if everything works
empty_loc('').

% TODO: change name to reflect that it support other things, not only the compiler... and braces if there is no stacked action
:- public compiler_error/1.
compiler_error(Error) :-
        ( error_messages:error(Error, Type, Message) ->
	    true
	; % TODO: this should be a bug!
	  Type = error, Message = ['undefined error: ', Error]
	),
	compiler_error__show(Type, Message),
	( Type = error ->
	    ( depth(Depth) -> true ),
	    add(depth__error(Depth)),
	    ( errors(N) -> true ), del(errors(_)), N1 is N + 1, add(errors(N1))
	; Type = warning ->
	    ( warnings(N) -> true ),
	    del(warnings(_)), N1 is N + 1, add(warnings(N1))
	; true % anything?
	).

compiler_error__show(Type, Message) :-
	'$quiet_flag'(Q, Q),
	allowed_message_type(Q, Type), % only show allowed messages
	!,
	show_action,
	brace_item,
	( depth(Depth) -> true ),
	( Depth = 0 -> show('{') ; true ), % rt msg
	show(Type),
	show(': '),
	( depth__location(Depth, Ln0, Ln1) ->
	    show('(lns '),
	    show(Ln0),
	    show('-'),
	    show(Ln1),
	    show(') ')
	; true
	),
	showmsg(Message), show_nl,
	( Depth = 0 -> show('}'), show_nl ; true ). % rt msg
compiler_error__show(_, _).

% Show current action
:- public show_action/0.
show_action :-
	'$quiet_flag'(Q, Q),
	% TODO: works but... it is not nice
	allowed_message_type(Q, error), % only show if errors are showed
	!,
	( depth(Depth) -> true ),
	( printed_depths(PrintedDepths) -> true ),
	( PrintedDepths = [LastPrintedDepth|_], Depth = LastPrintedDepth ->
	    true
	; PrintedDepths = [LastPrintedDepth|_], Depth < LastPrintedDepth ->
	    errlog:bug('depth is smaller than last printed depth!')
	; depth__action(Depth, Action) ->
	    ( action_message(Action, ActionMsg) ->
	        true
	    ; ActionMsg = ['unknown ', Action]
	    ),
	    begin_brace(ActionMsg),
	    del(printed_depths(_)),
	    add(printed_depths([Depth|PrintedDepths]))
	; % ok, no action...
	  true
        ).
show_action.

:- public summary/0.
summary :-
	( errors(Errors) -> true ),
	( warnings(Warnings) -> true ),
	( Errors = 0, Warnings = 0 ->
	    true
	; begin_brace([Errors, ' errors and ', Warnings, ' warnings']),
	  end_brace
	).

% ---------------------------------------------------------------------------

:- data queued_nl/0.

begin_brace(X) :-
	print_queued_nl,
	show('{'),
	showmsg(X),
	add(queued_nl).

end_brace :-
	del(queued_nl),
	show('}'), show_nl.

brace_item :-
	print_queued_nl.

print_queued_nl :- % (if needed)
	( queued_nl ->
	    del(queued_nl),
	    show_nl
	; true
	).

% ---------------------------------------------------------------------------

:- public static bug/1.
bug(X) :- labeled_message(bug, X).

:- public static trace/1.
trace(X) :- labeled_message(trace, X).

% TODO: this should not be used!!
:- public static temperror/1.
temperror(Error) :-
	( error_messages:error(Error, Type, Message) ->
	    true
	; Type = error, Message = ['undefined error: ', Error]
	),
	labeled_message(temperror, Message).

% TODO: this thing surely is reimplemented somewhere else...
:- static labeled_message/2.
labeled_message(Label, Message0) :-
        Message1 = ( Message0 = [_|_] ? Message0 | [Message0] ),
	show('{'),
	show(Label),
	show(': '),
	showmsg(Message1),
	show('}'),
	show_nl.

:- static show/1.
show(M) :-
	display(user_error, M).

:- static showmsg/1.
showmsg(M) :-
        current_output(S),
        set_output(user_error),
        output_message(M),
        set_output(S).

:- static show_nl/0.
show_nl :- nl(user_error).
