:- module(_, [main/0],[]).
:- use_module(library(prolog_sys)).
%:- use_module(library(format)).

% TODO: chech w & w/o meta_predicate, etc....
% (see mails Bug in Logtalk benchmarks...)

main :-
	% Empty loop
        statistics(runtime, _),
        bn(500000),
        statistics(runtime, [_|Tn]),
	%
        statistics(runtime, _),
        b0(500000, true),
        statistics(runtime, [_|T0]),
        dtime('b0 - metaexp', T0, Tn),
	%
        statistics(runtime, _),
        b1(500000, true),
        statistics(runtime, [_|T1]),
        dtime('b1 - with meta pred', T1, Tn),
	%
        statistics(runtime, _),
        b0(500000, mytrue),
        statistics(runtime, [_|T0l]),
        dtime('b0local - metaexp', T0l, Tn),
	%
        statistics(runtime, _),
        b1(500000, mytrue),
        statistics(runtime, [_|T1l]),
        dtime('b1local - with meta pred', T1l, Tn),
	%
        statistics(runtime, _),
        b2(500000, 'basiccontrol:true'),
        statistics(runtime, [_|T2]),
        dtime('b2 - direct metacall', T2, Tn),
	%
        statistics(runtime, _),
        b3(500000),
        statistics(runtime, [_|T3]),
        dtime('b3 - no hiord', T3, Tn).

mytrue.

dtime(B, T, T0) :-
        display(B), display(': '),
	T1 is T - T0,
	display(T1), display(' ms'), nl.

bn(N) :-
        repeat(N),
        fail.
bn(_).

b0(N, Goal) :-
        repeat(N),
          call(Goal),
        fail.
b0(_, _).

:- meta_predicate b1(_, goal).
b1(N, Goal) :-
        repeat(N),
          call(Goal),
        fail.
b1(_, _).

:- use_module(engine(hiord_rt), ['$meta_call'/1]).
b2(N, Goal) :-
        repeat(N),
          '$meta_call'(Goal),
        fail.
b2(_, _).

b3(N) :-
        repeat(N),
          t,
        fail.
b3(_).
t.

repeat(0) :- !.
repeat(N) :- true ; N1 is N - 1, repeat(N1).
