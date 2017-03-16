:- module(_, _, [ciaopaths, fsyntax, assertions, regtypes]).
:- use_module(library(terms)).
:- use_module(library(lpdist(makedir_aux)), [fsR/2]).
:- use_module(library(lpdist(ciao_configure))).

:- reexport(ciaosrc(doc(common('LPDOCCOMMON')))).
:- reexport(library(lpdist(ciao_config_options)), [lpdoclib/1]).

:- include(lpdoclib('SETTINGS_schema')).
% ****************************************************************************
% This is an LPdoc configuration file. See SETTINGS_schema for documentation *
% ****************************************************************************

output_name := 'main'.

filepath := ~fsR(bundle_src(ciao)/'lib'/'assertions'/'test'/'doc').

systempath := ~fsR(bundle_src(ciao)/'lib').
systempath := ~fsR(bundle_src(ciao)/'library').

% Document structure
doc_structure := 'main'-['my_native_props'].

libtexinfo := 'yes'.

docformat := 'texi' | 'dvi' | 'ps' | 'ascii' | 'manl' | 'info' | 'html'.

index := 'concept' | 'pred' | 'prop' | 'regtype' | 'modedef' | 'global'.

startpage := 1.

htmlstyle := ~fsR((~lpdoclib)/'default.css').

datamode := perm(rw, rw, r).
execmode := perm(rwx, rwx, rx).


