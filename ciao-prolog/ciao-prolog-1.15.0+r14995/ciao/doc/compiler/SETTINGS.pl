:- module(_, _, [ciaopaths, regtypes, fsyntax, assertions]).

:- include(lpdoclib('SETTINGS_schema')).
% ****************************************************************************
% This is an LPdoc configuration file. See SETTINGS_schema for documentation *
% ****************************************************************************

:- use_module(library(lpdist(makedir_aux)), [fsR/2]).
:- use_module(library(lpdist(ciao_config_options))). % TODO: used?

:- reexport(ciaosrc(doc(common('LPDOCCOMMON')))).
libtexinfo(_) :- fail.
datamode(_) :- fail.
execmode(_) :- fail.

output_name := 'ciao_compiler'.

% TODO: use parent_bundle to share those defs
filepath := ~fsR(bundle_src(ciao)/'doc'/'compiler').
filepath := ~fsR(bundle_src(ciao)/'ciaoc').
filepath := ~fsR(bundle_src(ciao)/'lib'/'compiler').
filepath := ~ciaofilepath_common.

doc_structure := 
    'ciao-compiler'-[
      'ciaoc',
      'compiler',
      'exemaker',
%      'c_itf',
      'c_itf_internal'
    ].
%            callback.pl

doc_mainopts := no_patches.
% TODO: Added no_propuses because texindex breaks with very large
%       indices (due to internal, maybe arbitrary, limitations) --JF.
doc_compopts := no_isoline|no_engmods|no_propmods|no_changelog|no_propuses.
