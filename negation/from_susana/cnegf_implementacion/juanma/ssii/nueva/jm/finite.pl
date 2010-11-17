%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  finite.pl
%%  Package  FINITE
%%                                 Juan Manuel Martinez Barrena
%%                                                  version 1.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% To execute this the line:
%% :- use_package(.(finite)).
%% must be included in the file 
%% and we will obtain... 
%%  not_fails Y steps_ub(_) -> finite

:- load_compilation_module(.(finite_tr)).

:- add_sentence_trans(add_finite/3).

