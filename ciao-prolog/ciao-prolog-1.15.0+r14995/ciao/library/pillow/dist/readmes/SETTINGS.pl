:- module(_, _, [ciaopaths, fsyntax, assertions]).

% TODO: THIS SETTINGS.pl FILE IS OUTDATED!

:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lpdist(ciao_configure))).
:- use_module(library(bundle_registry(bundle_registry_load))).
:- use_module(library(lpdist(makedir_aux)), [fsR/2]).

:- reexport(ciaosrc(doc(common('LPDOCCOMMON')))).
:- reexport(library(lpdist(ciao_config_options)), [docdir/1,mandir/1,infodir/1,htmldir/1]).

% TODO: use standalone?
% :- redefining(component/1).
%
%component := 'INSTALL_pillow'.
%component := 'README_pillow'.
%
%:- redefining(mainfile/1).
%
%%mainfile := ~current_env('MAINFILE').
%mainfile(_) :- fail.

:- redefining(index/1).

index(_) :- fail.

:- redefining(filepath/1).

filepath := ~fsR(bundle_src(ciao)/'lib').
filepath := ~fsR(bundle_src(ciao)/'lib'/'assertions').
filepath := ~fsR(bundle_src(ciao)/'library').
filepath := ~fsR(bundle_src(ciao)/'library'/'pillow'/'dist'/'doc').
filepath := ~fsR(bundle_src(ciao)/'library'/'pillow'/'dist'/'readmes').

htmlstyle := ~fsR((~lpdoclib)/'default.css').

