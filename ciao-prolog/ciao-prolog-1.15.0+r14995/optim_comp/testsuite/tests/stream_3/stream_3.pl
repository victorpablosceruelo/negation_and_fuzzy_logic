:- module(_, [main/0], [fsyntax]).

%% Naive version of the stream interpreter for ptoc
%% (modified by Jose Morales <jfran@clip.dia.fi.upm.es>)
%% (initial version by Manuel Carro <mcarro@fi.upm.es>)

:- '$pragma'(analyze_all).

% ---------------------------------------------------------------------------
% Native operations

:- include(engine(spec_arithmetic)).

% ---------------------------------------------------------------------------
% Parameters
% TODO: do unfolding!
%:- '$props'(audio_sps/1, [impnat = ptoc]).
%audio_sps(4410).
%:- '$props'(compass_sps/1, [impnat = ptoc]).
%compass_sps(10).

num_data(150000).
%num_data(10000).
extra_data(X):- X is ~num_data * 2.

% ---------------------------------------------------------------------------
% Process data (naive)

:- '$props'(stream/5, [impnat = ptoc, indexed = false,
	argderefs = [true, false, false, false, false]]).
stream(0, _, _, _, AOs) :- !,
	AOs = [].
stream(N, [Compass|Cs], [G0|Gs], [Aud|As], [AOut|AOs]):-
	G0 = gps(GLeft, GRight),
	%% GPS data is constant for every second -> during AUDIO_SAMPLES_PER_SEC
        %% the value of t will not change; therefore we could calculate T just
        %% once per second (i.e., once every AUDIO_SAMPLES_PER_SEC samples).
        %% Note that it involves a costly atan2 operation!
        At0 = ~float_div(GRight, GLeft),
        At = ~float_atan(At0),
        T = ~float_mul(~float_new(55), At),
        Angle0 = ~float_sub(~float_fmod(~float_floor(~float_add(T, Compass)), ~float_new(360)), ~float_new(180)),
	Angle = ~cast_float_to_smallint(Angle0),
        Offset = ~float_floor(~float_fabs(~float_div(Angle0, ~float_new(6.0)))),
        RealOffset = ~cast_float_to_smallint(~float_add(Offset, ~float_new(2))),
        Aud = audio(One, _),
        %% Give a 3-D feeling by changing the phase of the sound received by
        %% every ear.  Note: assuming the compass is centered in the head, this
        %% should probably be 
        %%    one = data[pos - offset / 2][AUDIO].left;
        %%    other = data[pos + offset / 2 ][AUDIO].left;
        nth(RealOffset, [Aud|As], Aud2),
	Aud2 = audio(Other, _),
        %% I guess that if the head has turned around, the 'left' sound source
        %% goes into the right ear and the other way around.
        (
            Angle < 0 ->
            AOut = p(One, Other)
        ;
            AOut = p(Other, One)
        ),
        N1 = ~smallint_dec(N),
	'$trust_type'(AOs, var),
        stream(N1, Cs, Gs, As, AOs).

% TODO: how can the derefs be inferred? they give a good performance increase
:- '$props'(nth/3, [impnat = ptoc, imp = semidet,
	argderefs = [true, false, false]
%	argmems = [cvar, cvar, cvar],
%	argmodes = [in, in, out]
]).
nth(1, List, Elem) :- !,
	List = [Elem|_].
nth(N, [_|List], Elem) :-
        N1 = ~smallint_dec(N),
	'$trust_type'(Elem, var),
        nth(N1, List, Elem).

% ---------------------------------------------------------------------------
% Gen data eager

:- '$props'(generate_data/5, [impnat = ptoc, indexed = false,
	argderefs = [true, true, false, false, false]]).
generate_data(N, N, Cs, Gs, As) :- !, % TODO: choice point is generated here!! fix!
	Cs = [], Gs = [], As = [].
generate_data(N, M, Cs, Gs, As) :-
	Au = ~float_new(4410),
	Comp = ~float_new(10),
        C = ~float_fmod(~float_div(~cast_smallint_to_float(N), ~float_div(Au, Comp)), ~float_new(360)),
	Cs = [C|Cs0],
	Gs = [gps(~float_new(50.0), ~float_new(50.0))|Gs0],
        N1 = ~smallint_inc(N),
	As = [audio(~cast_smallint_to_float(N1), 0)|As0],
	'$trust_type'(Cs0, var),
	'$trust_type'(Gs0, var),
	'$trust_type'(As0, var),
        generate_data(N1, M, Cs0, Gs0, As0).

% ---------------------------------------------------------------------------

:- use_module(library(prolog_sys)).
'$cputime'(X) :- statistics(runtime, [X|_]).

main :-
	'$cputime'(T1),
        extra_data(E),
        do_generate_data(0, E, Compass, Gps, AudioIn),
        process_data(Compass, Gps, AudioIn),
	'$cputime'(T2),
	Time is T2-T1,
	display(Time), nl.

process_data(Compass, Gps, AudioIn):-
        num_data(N),
        dostream(N, Compass, Gps, AudioIn).

:- '$props'(dostream/4, [impnat = ptoc]).
dostream(NSamples, Compass, Gps, AudioIn):-
        stream(NSamples, Compass, Gps, AudioIn, _).

:- '$props'(do_generate_data/5, [impnat = ptoc]).
do_generate_data(N, M, Cs, Gs, As) :-
	generate_data(N, M, Cs, Gs, As).



