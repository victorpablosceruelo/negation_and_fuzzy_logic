:- module(gen_data_eager, [generate_data/5], []).

%% By Manuel Carro <mcarro@fi.upm.es>

 %% :- use_package(functions).
:- use_module(.(parameters)).

generate_data(N, N, [], [], []).
generate_data(N, M, [C|Cs], [gps(50, 50)|Gs], [audio(N1, 0)|As]):-
        N < M ,
        audio_sps(Au),
        compass_sps(Com),
        C is (N / (Au / Com)) mod 360,
        N1 is N + 1,
        generate_data(N1, M, Cs, Gs, As).
