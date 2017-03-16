:- module(_, _, [ciaopaths, regtypes, fsyntax, assertions]).

:- include(lpdoclib('SETTINGS_schema')).
% ****************************************************************************
% This is an LPdoc configuration file. See SETTINGS_schema for documentation *
% ****************************************************************************

:- reexport(ciaoppsrc(doc(common('LPDOCCOMMON')))).
pathsfile(_) :- fail.
doc_mainopts(_) :- fail.
doc_compopts(_) :- fail.
datamode(_) :- fail.
execmode(_) :- fail.

% the bundle that contains this manual
% TODO: This could be inferred (looking for a Manifest.pl in a parent dir)
parent_bundle := 'ciaopp'.

output_name := 'ciaopp_internals'.

doc_structure :=
        'ciaopp_doc'-[
	  'driver',
	  'preprocess_flags',
	  'printer',
	  'Punit'-[ % in component
	    'Punit0',% in component
	    'p_unit',
	    'native',
	    'clidlist',
	    % (not in p_unit dir)
	    'tr_syntax'
          ],
	  'Infer1'-[ % in component
	    'infer'
	  ],
          'Plai1'-[ % in component
	    'Plai0',% in component
	    'plai',
            'fixpo_plai',
	    'domains',
	    'gr',
	    'plai_db',
	    'Trust' % in component
	  ]
         ].

index := concept|lib|pred|prop|regtype|decl|author|global.

% index := concept.
% index := global.
% index := pred.
% index := regtype.
% index := prop.
% index := modedef.


