:- module(stream_naive_prolog, [main/0], []).

%% By Manuel Carro <mcarro@fi.upm.es>

 %% :- use_package(functions).

:- use_module(.(parameters)).

:- use_module(.(gen_data_eager)).
%:- use_module(gen_data_lazy).
:- use_module(.(process_data_naive)).


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



%% The two gen_data modules generate lists with the data needed for the stream
%% interpreter.  The _eager one generates the lists before processing
%% them, and the _lazy module generates them 'on demand', as it is needed
%% by the consumer process.  They have 'hardwired' the number of streams
%% / clocks: one argument per stream.  They do not give information about
%% the clocks.  However, the the corresponding processing filter does not
%% need them, because data samples are generated at the speed of the
%% fastest one.  This knowledge could be extracted from the
%% implementation of the data generation and propagated to the data
%% processing.  A more general implementation would not assume any number
%% of clocks fixed beforehand, or any sample rate for them; these would
%% come in the data structures themselves.  But then specialization would
%% be _much_ harder.


%% Generate the input lists, then process them.
main:-
        extra_data(E),
        gen_data_eager:generate_data(0, E, Compass, Gps, AudioIn),
        process_data(Compass, Gps, AudioIn).

%% Process the input lists, which are being generated on demand.
 %% main([lazy]):-
 %%         gen_data_lazy:generate_data(0, Compass, Gps, AudioIn),
 %%         process_data(Compass, Gps, AudioIn).
