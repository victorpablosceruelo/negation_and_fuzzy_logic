%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Package  CNEGF
%  neg -> cnegf 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% To execute this the line:
%% :- use_package(.(cnegf)).
%% must be included in the file 
%% and we will obtain... 

:- load_compilation_module(.(cnegf_tr)).


%%%%%%%%%%%%%%%%%%%
%% add_cnegf/3 
%%    neg -> cnegf
%%%%%%%%%%%%%%%%%%%
:- add_term_trans(add_cnegf/3).
