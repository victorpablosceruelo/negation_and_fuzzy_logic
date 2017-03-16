:- module(simple_server,[population/2],[]).

% A simple server (an active module)

:- use_module(library(system)).
:- use_module(library(write)).
:- use_module(library(concurrency)).

population(belgium,9).
population(france,52).
population(germany,80).
population(italy,60).
population(spain,42).
population(sweden,8).
population(united_kingdom,55).

 %% shutdown_server :- 
 %%         write('Server received shutdown message'), nl,
 %%         pause(3),
 %%         %% We cannot just halt here! 
 %%         %% If we do, the server will never receive the control back!
 %%         %% halt.
 %%         eng_call(do_halt, true, true).
 %% 
 %% do_halt:-
 %%         write('Shutting down'),
 %%         nl,
 %%         pause(1),
 %%         halt.
