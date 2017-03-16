:- module(stream_dynamic_prolog, [main/0], []).

%% By Manuel Carro <mcarro@fi.upm.es>

%:- use_package(functions).
:- use_module(.(parameters)).
%:- use_module(gen_data_layer_lazy).
:- use_module(.(gen_data_layer_eager)).
:- use_module(.(process_data_dynamic)).


main:-
        num_data(ProcSamples),
        extra_data(NumSamples),
    
        gps_sps(GPSRate),
        compass_sps(CompassRate),
        audio_sps(AudioRate),

        gen_gps(0, NumSamples, GPSSamples),
        gen_compass(0, NumSamples, CompassSamples),
        gen_audio(0, NumSamples, AudioSamples),

        process_data(ProcSamples, [
                                      str(GPSRate, GPSSamples),
                                      str(CompassRate, CompassSamples),
                                      str(AudioRate, AudioSamples)
                                  ]).
