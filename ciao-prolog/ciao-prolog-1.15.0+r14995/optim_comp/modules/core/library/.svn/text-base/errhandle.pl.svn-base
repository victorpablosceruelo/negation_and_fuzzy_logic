:- module(errhandle,
	[
	    error_protect/1, handle_error/2, default_error_message/1
	], [pure, assertions]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_basic)).
:- use_module(engine(term_typing)).
:- use_module(engine(io_basic)).
:- use_module(engine(io_aux)).
:- use_module(engine(streams_basic)).
:- use_module(engine(exceptions)).

:- use_module(engine(rt_exp), [rt_modexp/4]).

:- meta_predicate(error_protect(goal)).

error_protect(Goal) :-
        catch(Goal, error(Error,Where), handle_error(Error, Where)).

handle_error(Error, Where) :-
	default_error_message(error(Error, Where)),
        fail.

default_error_message(error(Error, Where)) :- !,
        current_output(S),
        set_output(user_error),
        display('{ERROR: '),
        display_where(Where),
        display(' - '),
        display_error(Error),
        display('}'),
        nl,
        set_output(S).
default_error_message(Error) :-
        display(user_error, '{ERROR: No handle found for thrown error '),
        display(user_error, Error),
        display(user_error, '}'),
        nl(user_error).

display_where(P/N-A) :- !,
        display_list([P,'/',N,', arg ',A]). 
display_where(P/N) :- !,
        display_list([P,'/',N]). 
display_where(W) :-
        display(W).

% OGRAMA: OLD VERSION ---------------------------------------------------
%display_error(instantiation_error) :- !,
%        display('instantiation error').
%display_error(existence_error(procedure,_P)) :- !,
%        display('undefined predicate').
%display_error(existence_error(source_sink, S)) :- !,
%        display_list(['file ',S,' not found']).
%display_error(permission_error(open, source_sink, S)) :- !,
%        display_list(['cannot open file ',S]).
%display_error(permission_error(input,stream,S)) :- !,
%        display_list(['no input permission from ',S]).
%display_error(permission_error(output,stream,S)) :- !,
%        display_list(['no output permission to ',S]).
%display_error(permission_error(input,past_end_of_stream,S)) :- !,
%        display_list(['attempt to read past end of stream ',S]).
% OGRAMA: END OLD VERSION -----------------------------------------------
% error code=128

display_error(system_error) :- !, display('system error').
%display_error(no_access_permission) :- !,
%	display('no access permission').
display_error(syntax_error) :- !, display('syntax error').
display_error(resource_error):- !, display('resource error').
display_error(user_error):- !, display('user error (C interface?)').
display_error(evaluation_error(Type ,Culprit)):- !,
        display_list(['evaluation error ', Type, ' in ', Culprit]).
display_error(representation_error(Type, Culprit)) :- !,
	display_list(['representation error ',Type,' ', Culprit]).
display_error(permission_error(Object,Operation,Culprit)) :- !,
         display_list(['permission error: ', Operation, 
                       ' not allowed on ',Object,
                       ' with value ',Culprit]).
display_error(existence_error(Type,Culprit)) :- !,
        display_list(['existence error: ', Type,':',
                      Culprit,' does not exist']).
display_error(type_error(Type, Culprit)) :- !,
        display_list(['expected ',Type,', found ',Culprit]).
display_error(domain_error(Domain, Culprit)) :- !,
        display_list(['expected ',Domain,', found ',Culprit]).
display_error(syntax_error(L0,L1,Msg,ErrorLoc)) :- !,
        display('syntax error (lns '),
	display(L0),
	display('-'),
	display(L1),
	display(')'),
        message(['\n',[](Msg),':']),
        message(ErrorLoc).
display_error(X) :-
        display(X).
