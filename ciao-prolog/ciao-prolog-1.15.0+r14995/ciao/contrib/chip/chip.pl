:- package(chip).

%% Definitions for CHIP programs to run on CIAO

:- use_package(pure).        % so that no ciao libs are included
:- use_module(engine(basic_props)).  % so as not to complain about basic props

:- use_package(assertions).       % assertions
:- use_package(regtypes).         % types
% this is directly done in chipre
%:- include(library(chip(chip_ops))).
:- include(library(chip(chip_decl))). % chip declarations

:- load_compilation_module(library(chip(chip_exp))).
:- add_sentence_trans(expand_chip/2, 750).

% TODO: No documentation