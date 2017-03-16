:- module(_, _, [ciaopaths, regtypes, fsyntax, assertions]).

:- include(lpdoclib('SETTINGS_schema')).
% ****************************************************************************
% This is an LPdoc configuration file. See SETTINGS_schema for documentation *
% ****************************************************************************

:- use_module(library(lpdist(makedir_aux)), [fsR/2]).

% :- use_module(library(lpdist(ciao_config_options))).
% :- use_module(library(lpdist(ciao_configure))).

:- reexport(ciaosrc(doc(common('LPDOCCOMMON')))).
output_name(_) :- fail.
libtexinfo(_) :- fail.
datamode(_) :- fail.
execmode(_) :- fail.

htmldir := ''.

filepath := ~fsR(bundle_src(ciaode)/'lpdoc'/'tests'/'ciaotest')
	  | ~fsR(bundle_src(ciao)/'shell')
	  | ~fsR(bundle_src(ciao)/'ciaoc')
%	  | ~fsR(bundle_src(ciao)/'lib')
%	  | ~fsR(bundle_src(ciao)/'engine')
	  | ~fsR(bundle_src(ciao)/'etc')
	  | ~fsR(bundle_src(ciao)/'etc_contrib')
	  | ~fsR(bundle_src(ciao))
	  | ~fsR(bundle_src(ciao)/'doc'/'common')
	  | ~fsR(bundle_src(ciao)/'doc'/'readmes')
          | ~fsR(bundle_src(ciaode)/'emacs-mode').

doc_structure := 
        'ciao'-[
          ~docstr_getstarted,
	  'IsoProlog'-['write'],
	  'Append'-[~docstr_installation],
	  'DevEnv'-[~docstr_devenv]
        ].
docstr_getstarted := ['GetStartUnix',
	'GetStartWin32'].
docstr_installation := ['Install',
	'InstallWin32bin',
	'BeyondInstall'].
docstr_devenv :=
	['ciaoc',
	 'toplevel/toplevel_doc',
	 'debugger/debugger_doc'-['debugger/debugger'],
	 'ciao-shell',
	 'libpaths',
	 'chartlib/chartlib'-[
%	   'chartlib/bltclass',
%	   'chartlib/chartlib_errhandle',
%	   'chartlib/color_pattern',
%	   'chartlib/genbar1',
%	   'chartlib/genbar2',
%	   'chartlib/genbar3',
%	   'chartlib/genbar4',
%	   'chartlib/gengraph1',
%	   'chartlib/gengraph2',
%	   'chartlib/genmultibar',
%	   'chartlib/table_widget1',
%	   'chartlib/table_widget2',
%	   'chartlib/table_widget3',
%	   'chartlib/table_widget4',
	   'chartlib/test_format'
         ],
%	 'chartlib/bltclass',
         'dht/dht_doc'-[
	   'dht/dht_client',
	   'dht/dht_server'-[
	     'dht/dht_s2c',
	     'dht/dht_s2s',
	     'dht/dht_logic'-[
	       'dht/dht_routing',
	       'dht/dht_logic_misc',
	       'dht/dht_rpr',
	       'dht/dht_storage'
             ]
           ],
	   % Common modules for both DHT server and client
	   'dht/dht_config',
	   'dht/dht_misc'
         ],
	 'make/make_doc',
	 'zeromq/zeromq',
	 'CiaoMode'].

doc_mainopts := no_patches|no_versioned_output.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog.

docformat := texi|ps|pdf|manl|info|html.
