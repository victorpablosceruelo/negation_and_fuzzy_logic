:- module(process_data_layered, [process_data/3], []).

%% By Manuel Carro <mcarro@fi.upm.es>

 %% :- use_package(functions).
 %% :- use_package(hiord).
 %% :- use_module(library(hiordlib)).

:- use_module(.(parameters)).
 %% :- use_module(library(lists), [nth/3]).
 %% :- use_module(library(prolog_sys), [statistics/2]).



 %% process_data(Compass, Gps, AudioIn):-
 %%         display(user_error, 'Reserving memory'), 
 %%         nl(user_error), 
 %%         dostream(Compass, Gps, AudioIn),
 %%         fail.
process_data(Compass, Gps, AudioIn):-
 %%         display(user_error, 'Executing'),  nl(user_error),
 %%         statistics(runtime, _),
 %%         statistics(walltime, _),
        dostream(Compass, Gps, AudioIn).
 %%         statistics(runtime, [_,RunTime]),
 %%         statistics(walltime, [_,WallTime]),
 %%         display(user_error, runtime(RunTime)), nl(user_error), 
 %%         display(user_error, walltime(WallTime)), nl(user_error).

dostream(Compass, Gps, AudioIn):-
        Gps = [gps(GLeft, GRight)|_],
        atan2(GRight, GLeft, At),
        T is 55 * At,
        num_data(Num),
        stream(Num, Compass, T, AudioIn, _AudioOut),
        !.
        

stream(N, _, _, _, []):-
        N =< 0, !.
stream(N, [cmp(Compass)|Cs], T, AudioIn, AudioOut):-
        N > 0,
 %% There is a dirty trick here: this division is exact!  Otherwise
 %% we'd have to use a MCM to find out how many ticks we should wait
 %% until deciding we need to issue a sample.
        audio_sps(AudioRate),
        compass_sps(CompassRate),
        CompWL is integer(AudioRate / CompassRate),
        Angle is (integer(T+Compass) mod 360) - 180,
        Offset is integer(abs(float(Angle)/6.0)),
        stream_1(CompWL,Angle, Offset, AudioIn, AudioIn_R, AudioOut, AudioO_R),
        NextComp is N-CompWL,
        stream(NextComp, Cs, T, AudioIn_R, AudioO_R).

stream_1(0, _, _, AudioIn, AudioIn, AudioOut, AudioOut).
stream_1(N, Angle, Offset, [Aud|AudioIn], A_In_R, [AOut|AudioOut], A_O_R):-
        N > 0,
        Aud = audio(One, _),
        RealOffset is Offset+2,
        nth(RealOffset, [Aud|AudioIn], audio(Other, _)),
        (
            Angle < 0 ->
            AOut = [One, Other]
        ;
            AOut = [Other, One]
        ),
        N1 is N - 1,
        stream_1(N1, Angle, Offset, AudioIn, A_In_R, AudioOut, A_O_R).


atan2(X, Y, Z):-  %% Needs to be changed -- atan is NOT atan2!!!
        Z is atan(X/Y).


nth(N, List, Elem) :-
        integer(N), !, N >= 1, nthfunc(N, List, Elem).

nthfunc(1, [Elem|_], Elem) :- !.
nthfunc(N, [_|List], Elem) :-
        N1 is N-1,
        nthfunc(N1, List, Elem).
