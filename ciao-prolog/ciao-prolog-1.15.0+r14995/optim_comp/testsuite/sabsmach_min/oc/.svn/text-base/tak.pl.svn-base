:- module(_, [main/0], [pure]).

:- use_module(engine(internals)).
:- use_module(engine(arithmetic)).
%:- use_module(engine(atomic_basic)).
%:- use_module(engine(attributes)).
%:- use_module(engine(basic_props)).
:- use_module(engine(basiccontrol)).
%:- use_module(engine(interpreter)).
%:- use_module(engine(data_facts)).
%:- use_module(engine(exceptions)).
%:- use_module(engine(io_aux)).
%:- use_module(engine(io_basic)).
%:- use_module(engine(prolog_flags)).
%:- use_module(engine(streams_basic)).
%:- use_module(engine(system_info)).
:- use_module(engine(term_basic)).
%:- use_module(engine(term_compare)).
%:- use_module(engine(term_typing)).

main :-
	X = 18, Y = 12, Z = 6,
        tak(X, Y, Z, _Out).

/*:- ctocprop(tak/4, [imp=det, argimptypes=[int, int, int, int], call_types=[int, int, int, var], exit_types=[int, int, int, int], argmodes=[in,in,in,out], argmems=[push,push,push,push], argderefs=[true,true,true,true], should_trim_frame=no]). THIS WILL NOT WORK!! CHOICE POINTS CANNOT STORE UNTAGGED VALUES (at this moment) */
tak(X,Y,Z,A) :-
        X =< Y, !,
        Z = A.
tak(X,Y,Z,A) :-
        % X > Y,
        X1 is X - 1,
        tak(X1,Y,Z,A1),
        Y1 is Y - 1,
        tak(Y1,Z,X,A2),
        Z1 is Z - 1,
        tak(Z1,X,Y,A3),
        tak(A1,A2,A3,A).
