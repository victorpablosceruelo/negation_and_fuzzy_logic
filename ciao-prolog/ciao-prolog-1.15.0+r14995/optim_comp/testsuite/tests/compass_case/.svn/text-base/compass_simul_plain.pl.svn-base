:- module(_, [main/1], [fsyntax,assertions,nativeprops]).

:- use_module(.(io_access)).

% :- '$pragma'(unbox_cons). % use unboxed constant constructors (and box them if necessary)
% :- include(engine(spec_arithmetic)).

% :- use_package(compiler(ciaopp_info)).
% :- '$pragma'(treat_assertions). % propagate ciaopp assertions as properties
% :- '$pragma'(analyze_imp). % determine imp property automatically (at this point, using ciaopp info)
% :- '$pragma'(no_unfold_subpreds). % do not detect and expand subpredicates
% :- '$pragma'(analyze_all). % analyze all predicates (necessary if some predicates use ptoc)
% :- '$default_preddef'(ptoc). % use ptoc in all predicates by default

audio_sps(44100). %% This _has_ to match the actual sampling frequency

%:- '$props'(main/1, [impnat = bytecode]).
main([AudioFile]) :-
%% Instead, we receive a file / device name.
	catch(main__2(AudioFile), _E, close_write_stereo).
%:- '$trust_entry'(main__2/1, sht, [any]). % TODO: fix!

%:- '$props'(main__2/1, [impnat = bytecode]).
main__2(AudioFile) :-
        open(AudioFile, read, InStream),
	open_read_mono(InStream, InId),
%%	OutStream = user_output,
%%	open_write_stereo(OutStream),
        open_write_stereo,
        Skip = 0, % Just for documentation: looking North
        %% N.B.: we should calibrate to determine the initial position
        %% of the sound source!
        play_stereo(1, InId, Sample, Sample, Skip).

play_stereo(Samples_Remaining, InId, SampleL, SampleR, CurrSkip) :-
	Samples_Remaining > -1,
        %% Skip the necessary number of samples according to the
        %% orientation of the compass.
        new_sample_cycle(Samples_Remaining, NewCycle, InId,
                         CurrSkip, NewSkip,
                         SampleL, SampleR, NewSampleL, NewSampleR),
        %% Get two new samples for the right and left channels
        new_sample(InId, NewSampleR, R, RestSampleRight),
        new_sample(InId, NewSampleL, L, RestSampleLeft),
        %% Play this sample and continue.
        write_stereo(R, L),
        play_stereo(NewCycle, InId,
                    RestSampleLeft, RestSampleRight, 
                    NewSkip).

compass_sps(10).

audio_per_compass(X) :-
	X is integer(~audio_sps / ~compass_sps).

sound_speed(340).   % In meters per second
head_radius(0.1).   % In meters
pi(3.141592).


%% How many samples left and right ear differ, according to the angle
%% the head has turned w.r.t. the North.
find_skip(Angle, SamplesDif) :-
	SamplesDif is round(~samples_per_meter * 2 * ~ear_dif(Angle)).

%% How many meters is some ear displaced North with respect to the
%% center of the head.
ear_dif(Angle, Diff) :-
	Diff is ~head_radius * sin((Angle * ~pi) / 180).

%% How many samples are needed to fill in a meter of air.
samples_per_meter(S) :-
	S is ~audio_sps / ~sound_speed.

%% When we have read enough audio samples, read the compass and find
%% out whether we have to skip or not samples from some channel.
new_sample_cycle(Cycle, APC, InId,
                 CurrSkip, NewSkip, SL, SR, NSL, NSR) :-
        Cycle = 1,
	!,
        audio_per_compass(APC),
	read_compass(Angle),
	find_skip(Angle, NewSkip),
	Diff is NewSkip - CurrSkip,
        skip(Diff, InId, SL, SR, NSL, NSR).
new_sample_cycle(Cycle, NewCycle, _InId, Sk, Sk, SL, SR, NSL, NSR) :-
	SL = NSL,
	SR = NSR,
        Cycle > 1,
        NewCycle is Cycle - 1.

new_sample(InId, [Sample|Rest], Sample, Rest) :-
	read_it(Sample, InId).

%% Advance (and instantiate, if needed) the input list.
read_it(RealSample, InId) :-
	var(RealSample),
	!,
        RealSample = ~read_mono(InId).
read_it(RealSample, _) :- nonvar(RealSample).

%% This predicate is only called from time to time (e.g., only when
%% we need read from the compass).  But we might want to read from the
%% compass very often.
skip(0, _InId, L, R, NL, NR) :- !,
	NL = L,
	NR = R.
skip(Disp, InId, L, R, NL, NR) :-
        Disp =\= 0,
	( Disp > 0 ->
            L = NL,
            skip_nth(Disp, InId, R, NR)
        ; 
	  D is -Disp,
	  R = NR,
	  skip_nth(D, InId, L, NL)
        ).

skip_nth(1, InId, Samples, Rest) :- !,
        new_sample(InId, Samples, _Elem, Rest).
skip_nth(N, InId, Samples, Rest) :-
        N > 1,
        N1 is N-1,
        new_sample(InId, Samples, _, List),
        skip_nth(N1, InId, List, Rest).


