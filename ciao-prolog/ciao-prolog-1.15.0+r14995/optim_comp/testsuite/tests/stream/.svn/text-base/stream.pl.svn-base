:- module(_, [main/0], [fsyntax]).

%% Naive version of the stream interpreter for ptoc
%% (modified by Jose Morales <jfran@clip.dia.fi.upm.es>)
%% (initial version by Manuel Carro <mcarro@fi.upm.es>)

:- '$pragma'(analyze_all).

% ---------------------------------------------------------------------------
% Parameters
audio_sps(4410).
compass_sps(10).
gps_sps(1).

num_data(150000).
extra_data(X) :- X is ~num_data * 2.

% ---------------------------------------------------------------------------
% Process data (naive)

process_data(Compass, Gps, AudioIn) :-
        num_data(N),
        dostream(N, Compass, Gps, AudioIn).

dostream(NSamples, Compass, Gps, AudioIn) :-
        stream(NSamples, Compass, Gps, AudioIn, _), !.

:- '$props'(stream/5, [impnat = ptoc]).
stream(0, _, _, _, []).
stream(N, [Compass|Cs], [gps(GLeft, GRight)|Gs], [Aud|As], [AOut|AOs]) :-
        N > 0,
	%% GPS data is constant for every second -> during AUDIO_SAMPLES_PER_SEC
        %% the value of t will not change; therefore we could calculate T just
        %% once per second (i.e., once every AUDIO_SAMPLES_PER_SEC samples).
        %% Note that it involves a costly atan2 operation!
        atan2(GRight, GLeft, At),
        T is 55 * At,
        %% COMPASS data changes 10 times (i.e., COMPASS_SAMPLES_PER_SEC) every
        %% second.  Therefore we need to calculate angle and offset only once
        %% every 441 (for this case) audio samples.
        Angle is (integer(T+Compass) mod 360) - 180,
%        Offset is integer(abs(float(Angle)/6)),
        Offset is integer(abs(float(Angle)/6.0)),
        Aud = audio(One, _),
        %% Give a 3-D feeling by changing the phase of the sound received by
        %% every ear.  Note: assuming the compass is centered in the head, this
        %% should probably be 
        %%    one = data[pos - offset / 2][AUDIO].left;
        %%    other = data[pos + offset / 2 ][AUDIO].left;
        RealOffset is Offset+2,
        nth(RealOffset, [Aud|As], audio(Other, _)),
        %% I guess that if the head has turned around, the 'left' sound source
        %% goes into the right ear and the other way around.
        (
            Angle < 0 ->
            AOut = [One, Other]
        ;
            AOut = [Other, One]
        ),
%	display(AOut), nl,
        N1 is N - 1,
        stream(N1, Cs, Gs, As, AOs).

:- '$props'(atan2/3, [impnat = ptoc]).
atan2(X, Y, Z) :-  %% Needs to be changed -- atan is NOT atan2!!!
        Z is atan(X/Y).

:- '$props'(nth/3, [impnat = ptoc]).
nth(N, List, Elem) :-
        integer(N), !, N >= 1, nthfunc(N, List, Elem).

:- '$props'(nthfunc/3, [impnat = ptoc]).
nthfunc(1, [Elem|_], Elem) :- !.
nthfunc(N, [_|List], Elem) :-
        N1 is N-1,
        nthfunc(N1, List, Elem).

% ---------------------------------------------------------------------------
% Gen data eager

generate_data(N, N, [], [], []).
generate_data(N, M, [C|Cs], [gps(50, 50)|Gs], [audio(N1, 0)|As]) :-
        N < M,
        audio_sps(Au),
        compass_sps(Com),
        C is (N / (Au / Com)) mod 360,
        N1 is N + 1,
        generate_data(N1, M, Cs, Gs, As).

% ---------------------------------------------------------------------------

main :-
        extra_data(E),
        generate_data(0, E, Compass, Gps, AudioIn),
        process_data(Compass, Gps, AudioIn).

