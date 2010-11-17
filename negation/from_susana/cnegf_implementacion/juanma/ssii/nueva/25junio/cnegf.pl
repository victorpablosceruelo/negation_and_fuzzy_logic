%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  cnegf.pl
%%  Package  CNEGF
%%                                 Juan Manuel Martinez Barrena
%%                                                  version 1.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% To execute this the line:
%% :- use_package(.(cnegf)).
%% must be included in the file 
%% and we will obtain... 
%%    neg -> cnegf

:- load_compilation_module(.(cnegf_tr)).

:- add_term_trans(add_cnegf/3).

