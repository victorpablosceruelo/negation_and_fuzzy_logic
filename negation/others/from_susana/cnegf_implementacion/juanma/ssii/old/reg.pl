%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Package  REG
%   elimina los regedit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% To execute this the line:
%% :- use_package(.(reg)).
%% must be included in the file 
%% and we will obtain... 

:- load_compilation_module(.(reg_tr)).


%Prueba
:- add_sentence_trans(del_reg/3).

