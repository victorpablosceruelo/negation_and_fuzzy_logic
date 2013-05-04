:- module(_, _, [ciaopaths, assertions, regtypes, fsyntax]).

:- include(lpdoclib('SETTINGS_schema')).
% ****************************************************************************
% This is an LPdoc configuration file. See SETTINGS_schema for documentation *
% ****************************************************************************

:- doc(title, "Settings for the Compile Farm Manual").
:- doc(author, "Jose F. Morales").
:- doc(filetype, user).

:- use_module(library(system)).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(lpdist(makedir_aux)), [fsR/2]).
:- reexport(library(lpdist(ciao_config_options)), 
    [bibfile/1, htmldir/1, docdir/1, infodir/1, mandir/1, lpdoclib/1]).
datamode(_) :- fail.
execmode(_) :- fail.

filepath := ~fsR(bundle_src(ciaode)/ciaobot/doc).
systempath := ~fsR(bundle_src(ciao)/lib).
systempath := ~fsR(bundle_src(ciao)/lib/assertions).
systempath := ~fsR(bundle_src(ciao)/lib/metaprops).
systempath := ~fsR(bundle_src(ciao)/lib/regtypes).
systempath := ~fsR(bundle_src(ciao)/lib/engine).
systempath := ~fsR(bundle_src(ciao)/lib/rtchecks).
systempath := ~fsR(bundle_src(ciao)/lib/unittest).
systempath := ~fsR(bundle_src(ciao)/library).
systempath := ~fsR(bundle_src(ciao)/contrib).
systempath := ~fsR(bundle_src(ciao)/doc/common).

pathsfile(_) :- fail.  % kludge to avoid compilation error

output_name := _ :- fail.

doc_structure := 'ciaobot'.

commonopts := no_bugs|no_patches.
doc_mainopts := ~commonopts.
doc_compopts := ~commonopts.

index := concept|lib|pred|prop|regtype|decl|author|global.

startpage := 1.

papertype := afourpaper.

libtexinfo := 'yes'.

docformat := texi|ps|pdf|manl|info|html.

