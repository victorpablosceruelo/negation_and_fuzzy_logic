:- module(process_data_dynamic, [process_data/2], []).

%% By Manuel Carro <mcarro@fi.upm.es>

 %% :- use_package(functions).
 %% :- use_package(hiord).
 %% :- use_module(library(hiordlib)).

:- use_module(.(parameters)).
 %% :- use_module(library(lists), [nth/3]).
 %% :- use_module(library(prolog_sys), [statistics/2]).


%% Streams is a list of str(Type, SamplingRate, ListOfSamples)
%% This module should not use any knowledge about the streams other
%% than that passed as arguments.
 %% process_data(NSamples, Streams):-
 %%         display(user_error, 'Reserving memory'), 
 %%         nl(user_error), 
 %%         dostream(NSamples, Streams),  %% Warm up
 %%         fail.
process_data(NSamples, Streams):-
 %%         display(user_error, 'Executing'),  nl(user_error),
 %%         statistics(runtime, _),
 %%         statistics(walltime, _),
        dostream(NSamples, Streams).
 %%         statistics(runtime, [_,RunTime]),
 %%         statistics(walltime, [_,WallTime]),
 %%         display(user_error, runtime(RunTime)), nl(user_error), 
 %%         display(user_error, walltime(WallTime)), nl(user_error).
%        map(AudioOut, (_(X, _):- display(X), display(',')), _).

dostream(NSamples, Streams):-
        stream(NSamples, Streams, _AudioOut), !.

%% We have to decide when every piece of the resulting expression  has
%% to be recomputed.  The relative sampling rate interacts with the
%% dependencies in the arithmetics: the GPS is needed to calculate the
%% angle, and this is used to offset the index in the audio sample in
%% order to create a phase difference in the headphones.  So the only
%% sensible loop creation would take the GPS part to the outside and
%% leave the audio part in the inner part.  But this should be
%% automatically discovered by the compiler -- that's not easy.

mcm([str(Sampling, _)], Sampling).
mcm([str(Sampling, _), B|Bs], MCM):-
        mcm([B|Bs], MCM1),
        MCM is (Sampling * MCM1) // gcd(MCM1, Sampling).

enrich([], _MCM, []).
enrich([str(Rt, S)|Ss], MCM, [str(0,R1,S)|SOs]):-
        R1 is integer(MCM)/Rt,
        enrich(Ss, MCM, SOs).

%% We enrich the streams, because we're going to use their sampling
%% rate to decide when a new sample has to be generated.
stream(NSamples, Streams, SignalOut):-
 %%         map(Streams, 
 %%             (_(str(Rt,S), str(0,R1,S)):- R1 is integer(~mcm(Streams)/Rt)),
 %%              EStreams),
        mcm(Streams, MCM),
        enrich(Streams, MCM, EStreams),
        stream_loop(NSamples, EStreams, stin(_, _, _), SignalOut).
        
%% T is to be generated only when we get to a new sample
%% of the GPS.  Same with the compass and the audio.  We need,
%% therefore, to carry around the partial results.  This is encoded in
%% the State argument, which is threaded through the process.
stream_loop(0, _EStream, _State, []).
stream_loop(N, EStream, StIn, [Aud|AudioOut]):-
        N > 0,
        new_partial(EStream, EStreamOut, StIn, StOut, Aud),
        N1 is N - 1,
        stream_loop(N1, EStreamOut, StOut, AudioOut).

%% Go through the streams, do whatever has to be done whenever we
%% reach to the end of one sample of them.  This is similar to a
%% run-length encoding of the stream.
%% If the stream has not finished, then increment the run-length
%% count and do not update any state variable.
%% This might be left to a kind of fold?
new_partial([], [], S, S, _).
new_partial([Str|Strs], [NStr|NStrs], Si, So, A):-
        treat_stream(Str, NStr, Si, Sm, A),
        new_partial(Strs, NStrs, Sm, So, A).

%% For each type of sample
%%  
treat_stream(str(0, Cy, [gps(L,R)|Ss]), str(Mo, Cy, Ss), 
             stin(_, A, O), stin(Audio, A, O), _):-
            Mo is 1 mod Cy,
            atan2(R, L, At),
            Audio is 55 * At.
%% 
treat_stream(str(0, Cy, [cmp(Compass)|Ss]), str(Mo, Cy, Ss), 
             stin(T, _, _), stin(T, A, O), _):-
%     display(user_error, Compass),
     Mo is 1 mod Cy,
     A is (integer(T+Compass) mod 360) - 180,
     O is integer(abs(float(A)/6.0)).
%% 
treat_stream(str(0, Cy, [audio(One, _)|As]), str(Mo, Cy, As), 
             stin(T, A, O), stin(T, A, O), AOut):-
        Mo is 1 mod Cy,
        Ro is O + 1,
        nth(Ro, As, audio(Other, _)),
        (A < 0 -> AOut = [One, Other] ; AOut = [Other, One] ).
%% 
treat_stream(str(C, Cy, As), str(C1, Cy, As), S, S, _):- 
        C > 0,
        C1 is (C + 1) mod Cy.

%% Needs to be changed -- atan is NOT atan2!!!
%% (But it suffices for our case, since the coords are (50, 50)
atan2(X, Y, Z):- Z is atan(X/Y).


nth(N, List, Elem) :-
        integer(N), !, N >= 1, nthfunc(N, List, Elem).

nthfunc(1, [Elem|_], Elem) :- !.
nthfunc(N, [_|List], Elem) :-
        N1 is N-1,
        nthfunc(N1, List, Elem).
