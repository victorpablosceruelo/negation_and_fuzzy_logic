%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Package  FINITE
%  not_fails Y steps_ub(_) -> finite
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% To execute this the line:
%% :- use_package(.(finite)).
%% must be included in the file 
%% and we will obtain... 

:- load_compilation_module(.(finite_tr)).


%Prueba
:- add_sentence_trans(add_finite/3).

