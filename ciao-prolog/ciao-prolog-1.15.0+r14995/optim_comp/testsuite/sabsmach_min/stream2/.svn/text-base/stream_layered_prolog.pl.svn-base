:- module(stream_layered_prolog, [main/0], []).

%% By Manuel Carro <mcarro@fi.upm.es>

 %% :- use_package(functions).

:- use_module(.(parameters)).

%:- use_module(gen_data_eager).
 %% :- use_module(gen_data_layer_lazy).
:- use_module(.(gen_data_layer_eager)).
:- use_module(.(process_data_layered)).


%% None of the two implementations below have the optimizations which
%% come from considering the different signals constant during some time.

%% Input lists: they should actually be generated on demand.  My
%% educated guess is that lists of this size and with this audio
%% frequency should be processed in slightly more than 1 second.  My
%% laptop processes them in 0.23 seconds with low-quality audio (4410
%% samples per second, just enough to have reasoble voice quality).
%% We could probably make sound sampling go up to 20000 samples per
%% second and still meet the deadlines.

%% It is possible that a partial evaluator realizes that Gps is
%% constant.  Some computations can then be moved outside the loop.

%% Generate the input lists, then process them.
 %% main([eager]):-
 %%         gen_data_eager:generate_data(0, ~extra_data, Compass, Gps, AudioIn),
 %%         process_data(Compass, Gps, AudioIn).
 %% 

%% Process the input lists, which are being generated on demand.
 %% main([lazy]):-
 %%         process_data(
 %%                         ~(gen_data_layer_lazy:gen_gps(0)), 
 %%                         ~(gen_data_layer_lazy:gen_compass(0)),
 %%                         ~(gen_data_layer_lazy:gen_audio(0))
 %%                     ).


main:-
        extra_data(ED),
        audio_sps(AudioRate),
        compass_sps(CompassRate),
        gps_sps(GPSRate),
        gen_audio(0, ED, Audio),
        AudioSamplesPerCompass is ceiling(ED/(AudioRate / CompassRate)),
        gen_compass(0, AudioSamplesPerCompass, Compass),
        AudioSamplesPerGPS is ceiling(ED/(AudioRate / GPSRate)),
        gen_gps(0, AudioSamplesPerGPS, GPS),
        process_data(Compass, GPS, Audio).
