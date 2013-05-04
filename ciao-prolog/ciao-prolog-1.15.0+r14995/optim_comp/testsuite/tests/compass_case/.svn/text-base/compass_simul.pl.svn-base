/* This program is preprocessed using CPP */
/* All parameters are passed as PARAM_* or USE_* */

:- module(_, [main/1], [fsyntax,assertions,nativeprops]).

:- use_module(.(io_access)).

% #define USE_ARGMODES 1
% #define REDUNDANT 1

#if !defined(PARAM_FREQUENCY)
#error "No PARAM_FREQUENCY. E.g. use -DPARAM_FREQUENCY=44100"
#endif

#if !defined(PARAM_COMPASS)
#error "No PARAM_COMPASS. E.g. use -DPARAM_COMPASS=10"
#endif

#if !defined(PARAM_AUDIO_PER_COMPASS)
#error "No PARAM_AUDIO_PER_COMPASS!"
#endif

#if (PARAM_OPTIMIZE == 0)
/* bytecode */
#elif (PARAM_OPTIMIZE == 1)
/* plain ptoc + cheap analysis */ 
#  define USE_PTOC 1
#elif (PARAM_OPTIMIZE == 2)
/* plain ptoc + semidet predicates */ 
#  define USE_PTOC 1
#  define USE_IMPDET 1
#elif (PARAM_OPTIMIZE == 3)
/* plain ptoc + semidet predicates + ciaopp annotations */ 
#  define USE_PTOC 1
#  define USE_CIAOPPTYPEMOD0 1
#  define USE_CIAOPPTYPEMOD 1
#  define USE_IMPDET 1
#elif (PARAM_OPTIMIZE == 4)
/* plain ptoc + semidet predicates + ciaopp annotations + specialized arithmetic */ 
#  define USE_PTOC 1
#  define USE_CIAOPPTYPEMOD0 1
#  define USE_CIAOPPTYPEMOD 1
#  define USE_IMPDET 1
#  define USE_SPECBLT 1
#else
#  error "Unknown PARAM_OPTIMIZE"
#endif

#if (PARAM_UNFOLD == 0)
/* no unfolding */
#elif (PARAM_UNFOLD == 1)
#  define USE_CIAOPPSPEC 1
#else
#  error "Unknown PARAM_UNFOLD"
#endif

#if defined(USE_SPECBLT)
:- '$pragma'(unbox_cons). % use unboxed constant constructors (and box them if necessary)
:- include(engine(spec_arithmetic)).
#endif

#if defined(USE_PTOC)
% bytecode version
:- use_package(compiler(ciaopp_info)).
:- '$pragma'(treat_assertions). % propagate ciaopp assertions as properties
:- '$pragma'(analyze_imp). % determine imp property automatically (at this point, using ciaopp info)
% :- '$pragma'(no_unfold_subpreds). % do not detect and expand subpredicates
:- '$pragma'(analyze_all). % analyze all predicates (necessary if some predicates use ptoc)
:- '$default_preddef'(ptoc). % use ptoc in all predicates by default
#endif

#if defined(USE_IMPDET)
% :- '$props'(audio_sps/1, [imp = semidet]).
:- true pred audio_sps/1 + is_det.
#endif
#if defined(USE_ARGMODES)
:- '$props'(audio_sps/1, [argmodes = [out]]).
#endif
audio_sps(PARAM_FREQUENCY). %% This _has_ to match the actual sampling frequency

:- '$props'(main/1, [impnat = bytecode]).
main([AudioFile]) :-
%% Instead, we receive a file / device name.
	catch(main__2(AudioFile), _E, close_write_stereo).
%	main__2(AudioFile).
#if defined(USE_PTOC)
:- '$trust_entry'(main__2/1, sht, [any]). % TODO: fix!
#endif

:- '$props'(main__2/1, [impnat = bytecode]).
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

#if defined(USE_IMPDET)
% :- '$props'(play_stereo/5, [imp = semidet]).
:- true pred play_stereo/5 + is_det.
#endif
play_stereo(Samples_Remaining, InId, SampleL, SampleR, CurrSkip) :-
#if defined(REDUNDANT)
	Samples_Remaining > -1,
#endif
        %% Skip the necessary number of samples according to the
        %% orientation of the compass.
#if defined(USE_CIAOPPTYPEMOD)
 	true((
		 '$trust_type'(Samples_Remaining, smallint),
		 '$trust_type'(NewCycle, var),
		 '$trust_type'(NewSampleL, var),
		 '$trust_type'(NewSampleR, var)
	     )),
#endif
        new_sample_cycle(Samples_Remaining, NewCycle, InId,
                         CurrSkip, NewSkip,
                         SampleL, SampleR, NewSampleL, NewSampleR),
        %% Get two new samples for the right and left channels
#if defined(USE_CIAOPPSPEC)
	NewSampleR = [R|RestSampleRight],
	read_it(R, InId),
	NewSampleL = [L|RestSampleLeft],
	read_it(L, InId),
#else
#if defined(USE_CIAOPPTYPEMOD0)
 	true((
		 '$trust_type'(R, var),
		 '$trust_type'(RestSampleRight, var)
	     )),
#endif
        new_sample(InId, NewSampleR, R, RestSampleRight),
#if defined(USE_CIAOPPTYPEMOD0)
 	true((
		 '$trust_type'(L, var),
		 '$trust_type'(RestSampleLeft, var)
	     )),
#endif
        new_sample(InId, NewSampleL, L, RestSampleLeft),
#endif
        %% Play this sample and continue.
        write_stereo(R, L),
        play_stereo(NewCycle, InId,
                    RestSampleLeft, RestSampleRight, 
                    NewSkip).

#if defined(USE_IMPDET)
% :- '$props'(compass_sps/1, [imp = semidet]).
:- true pred compass_sps/1 + is_det.
#endif
#if defined(USE_ARGMODES)
:- '$props'(compass_sps/1, [argmodes = [out]]).
#endif
compass_sps(PARAM_COMPASS).

#if defined(USE_IMPDET)
% :- '$props'(audio_per_compass/1, [imp = semidet]).
:- true pred audio_per_compass/1 + is_det.
#endif
#if defined(USE_ARGMODES)
:- '$props'(audio_per_compass/1, [argmodes = [out]]).
#endif
audio_per_compass(X) :-
	X is integer(~audio_sps / ~compass_sps).

%% This decides how many samples we have to skip for every angle.
%% We can give a negative amount of samples to skip.  I hope I got the
%% formulae right.  The results match my experimental feeling with the
%% earphone. 

#if defined(USE_IMPDET)
% :- '$props'(sound_speed/1, [imp = semidet]).
:- true pred sound_speed/1 + is_det.
#endif
#if defined(USE_ARGMODES)
:- '$props'(sound_speed/1, [argmodes = [out]]).
#endif
sound_speed(340).   % In meters per second
#if defined(USE_IMPDET)
% :- '$props'(head_radius/1, [imp = semidet]).
:- true pred head_radius/1 + is_det.
#endif
#if defined(USE_ARGMODES)
:- '$props'(head_radius/1, [argmodes = [out]]).
#endif
head_radius(0.1).   % In meters
#if defined(USE_IMPDET)
% :- '$props'(pi/1, [imp = semidet]).
:- true pred pi/1 + is_det.
#endif
#if defined(USE_ARGMODES)
:- '$props'(pi/1, [argmodes = [out]]).
#endif
pi(3.141592).


%% How many samples left and right ear differ, according to the angle
%% the head has turned w.r.t. the North.
#if defined(USE_IMPDET)
% :- '$props'(find_skip/2, [imp = semidet]).
:- true pred find_skip/2 + is_det.
#endif
find_skip(Angle, SamplesDif) :-
#if defined(USE_CIAOPPSPEC)
        C is 0.1*sin(Angle*0.01745328888888889),
        SamplesDif is round(259.4117647058824*C).
#else
	SamplesDif is round(~samples_per_meter * 2 * ~ear_dif(Angle)).
#endif


%% How many meters is some ear displaced North with respect to the
%% center of the head.
#if defined(USE_IMPDET)
% :- '$props'(ear_dif/2, [imp = semidet]).
:- true pred ear_dif/2 + is_det.
#endif
ear_dif(Angle, Diff) :-
	Diff is ~head_radius * sin((Angle * ~pi) / 180).

%% How many samples are needed to fill in a meter of air.
#if defined(USE_IMPDET)
% :- '$props'(samples_per_meter/1, [imp = semidet]).
:- true pred samples_per_meter/1 + is_det.
#endif
samples_per_meter(S) :-
	S is ~audio_sps / ~sound_speed.

#if defined(USE_IMPDET)
% :- '$props'(new_sample_cycle/9, [imp = semidet]).
:- true pred new_sample_cycle/9 + is_det.
#endif
%% When we have read enough audio samples, read the compass and find
%% out whether we have to skip or not samples from some channel.
new_sample_cycle(Cycle, APC, InId,
                 CurrSkip, NewSkip, SL, SR, NSL, NSR) :-
        Cycle = 1,
	!,
#if defined(USE_CIAOPPSPEC)
        APC = PARAM_AUDIO_PER_COMPASS,
#else
        audio_per_compass(APC),
#endif
	read_compass(Angle),
#if defined(USE_CIAOPPTYPEMOD)
	true('$trust_type'(Angle, smallint)),
#endif
	find_skip(Angle, NewSkip),
#if defined(USE_CIAOPPTYPEMOD)
 	true((
		 '$trust_type'(NewSkip, smallint),
		 '$trust_type'(CurrSkip, smallint)
	     )),
#endif
	Diff is NewSkip - CurrSkip,
#if defined(USE_CIAOPPTYPEMOD)
 	true((
		 '$trust_type'(Diff, smallint),
		 '$trust_type'(NSL, var),
		 '$trust_type'(NSR, var)
	     )),
#endif
        skip(Diff, InId, SL, SR, NSL, NSR).
new_sample_cycle(Cycle, NewCycle, _InId, Sk, Sk, SL, SR, NSL, NSR) :-
#if defined(USE_CIAOPPTYPEMOD0)
 	true((
		 '$trust_type'(NSL, var),
		 '$trust_type'(NSR, var)
	     )),
#endif
	SL = NSL,
	SR = NSR,
#if defined(REDUNDANT)
        Cycle > 1,
#endif
#if defined(USE_CIAOPPTYPEMOD)
 	true('$trust_type'(NewCycle, var)),
#endif
        NewCycle is Cycle - 1,
#if defined(USE_CIAOPPTYPEMOD)
 	true('$trust_type'(NewCycle, smallint)).
#else
	true.
#endif

#if defined(USE_IMPDET)
% :- '$props'(new_sample/4, [imp = semidet]).
:- true pred new_sample/4 + is_det.
#endif
#if defined(USE_ARGMODES)
:- '$props'(new_sample/4, [argmodes = [in, in, out, out]]).
#endif
new_sample(InId, [Sample|Rest], Sample, Rest) :-
	read_it(Sample, InId).

#if defined(USE_IMPDET)
% :- '$props'(read_it/2, [imp = semidet]).
:- true pred read_it/2 + is_det.
#endif
%% Advance (and instantiate, if needed) the input list.
read_it(RealSample, InId) :-
	var(RealSample),
	!,
        RealSample = ~read_mono(InId).
#if defined(REDUNDANT)
read_it(RealSample, _) :- nonvar(RealSample).
#else
read_it(_, _).
#endif

#if defined(USE_IMPDET)
:- '$props'(skip/6, [imp = semidet]).
:- true pred skip/6 + is_det.
#endif
%% This predicate is only called from time to time (e.g., only when
%% we need read from the compass).  But we might want to read from the
%% compass very often.
skip(0, _InId, L, R, NL, NR) :- !,
#if defined(USE_CIAOPPTYPEMOD)	
	true('$trust_type'(NL, var)),
#endif
	NL = L,
#if defined(USE_CIAOPPTYPEMOD)	
	true('$trust_type'(NR, var)),
#endif
	NR = R.
skip(Disp, InId, L, R, NL, NR) :-
#if defined(REDUNDANT)
        Disp =\= 0,
#endif
	( Disp > 0 ->
#if defined(USE_CIAOPPTYPEMOD)	
	    true('$trust_type'(NL, var)),
#endif
            L = NL,
#if defined(USE_CIAOPPTYPEMOD)	
	    true('$trust_type'(NR, var)),
#endif
            skip_nth(Disp, InId, R, NR)
        ; 
	  D is -Disp,
#if defined(USE_CIAOPPTYPEMOD)	
	  true('$trust_type'(NR, var)),
#endif
	  R = NR,
#if defined(USE_CIAOPPTYPEMOD)	
	  true('$trust_type'(NL, var)),
#endif
	  skip_nth(D, InId, L, NL)
        ).

#if defined(USE_IMPDET)
% :- '$props'(skip_nth/4, [imp = semidet]).
:- true pred skip_nth/4 + is_det.
#endif
skip_nth(1, InId, Samples, Rest) :- !,
#if defined(USE_CIAOPPSPEC)
	Samples = [_Elem|Rest],
	read_it(_Elem, InId).
#else
        new_sample(InId, Samples, _Elem, Rest).
#endif
skip_nth(N, InId, Samples, Rest) :-
#if defined(REDUNDANT)
        N > 1,
#endif
        N1 is N-1,
#if defined(USE_CIAOPPSPEC)
	Samples = [_Elem|List],
	read_it(_Elem, InId),
#else
        new_sample(InId, Samples, _, List),
#endif
        skip_nth(N1, InId, List, Rest).


