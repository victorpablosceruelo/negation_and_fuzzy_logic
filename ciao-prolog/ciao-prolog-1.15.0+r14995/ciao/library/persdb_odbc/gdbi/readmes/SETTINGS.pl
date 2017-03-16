:- module(_, _, [ciaopaths, fsyntax, assertions]).

% TODO: THIS SETTINGS.pl FILE IS OUTDATED!

:- use_module(library(bundle_registry(bundle_registry_load))).
:- use_module(library(system)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lpdist(ciao_config_options))).
:- use_module(library(lpdist(ciao_configure))).
:- use_module(library(lpdist(makedir_aux)), [fsR/2]).

:- reexport(ciaosrc(doc(common('LPDOCCOMMON')))).
:- reexport(library(lpdist(ciao_config_options)), [docdir/1,mandir/1,infodir/1,htmldir/1]).

% TODO: Use standalone?
% :- redefining(component/1).
% 
% component := 'INSTALLATION'.
% component := 'README'.
% 
% :- redefining(mainfile/1).
% 
% %mainfile := ~current_env('MAINFILE').
% mainfile(_) :- fail.

:- redefining(index/1).

index(_) :- fail.

:- redefining(filepath/1).

filepath := '.' | '../doc'.

htmlstyle := ~fsR(~lpdoclib/'default.css').

