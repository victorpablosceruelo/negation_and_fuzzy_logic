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

output_name := 'ciaopp'.

doc_structure :=
        ciaopp_ref_man-[
	  part_usage-[
	    auto_interface,
	    ciaopp,
	    ciaoppcl
          ],
	  part_assertions-[
	    debugging_in_ciaopp,
	    assertions_doc,
	    assertions_props,
	    regtypes_doc,
	    basic_props,
	    native_props,
%           unittest_doc,
	    rtchecks_doc % Needed also for cross referencing
          ],
	  part_extensions-[
	    adding_new_domain,
	    domains,
	    gr
          ]
%	  'tutorial'
        ].

index := concept|lib|pred|prop|regtype|decl|author|global.
%
% index := prop.
% index := modedef.

