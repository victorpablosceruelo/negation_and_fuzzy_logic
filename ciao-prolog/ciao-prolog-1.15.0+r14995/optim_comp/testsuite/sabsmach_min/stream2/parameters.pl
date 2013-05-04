:- module(parameters, _, [functions]).

%% By Manuel Carro <mcarro@fi.upm.es>

audio_sps(4410).
compass_sps(10).
gps_sps(1).

num_data(150000).
extra_data(X):- X is ~num_data * 2.
