:- module(gen_data_layer_eager, [gen_gps/3, gen_compass/3, gen_audio/3], []).

%% By Manuel Carro <mcarro@fi.upm.es>

%:- use_package(functions).
%:- use_module(parameters).
%:- use_module(library(when)).


%% I am wrapping every single sample in its own container for two
%% reasons: one is that it makes the code for the generic processing a
%% bit more homogeneous, and the other is that that would presumably
%% make it easier to merge all samples in a single stream, instead of
%% having different explicit sources.  I do not know how this would
%% actually affect the code of the data processing.
gen_compass(N, N, []).
gen_compass(N, M, [cmp(C)|Cs]):-
        N < M, 
        C is N mod 360,
        N1 is N +1,
        gen_compass(N1, M, Cs).

gen_gps(N, N, []).
gen_gps(N, M, [gps(50, 50)|Gs]):- 
        N < M, 
        N1 is N + 1,
        gen_gps(N1, M, Gs).


gen_audio(N, N, []).
gen_audio(N, M, [audio(N1, 0)|As]):- 
        N < M,
        N1 is N +1,
        gen_audio(N1, M, As).
