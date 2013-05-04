:- module(_, _, [ciaopaths, fsyntax, assertions]).

:- use_module(library(lpdist(makedir_aux)), [fsR/2]).

:- reexport(library(lpdist(ciao_config_options)), 
    [bibfile/1, htmldir/1, docdir/1, infodir/1, mandir/1, lpdoclib/1]).

% the component that contains this manual
% TODO: This could be inferred (looking for a Manifest.pl in a parent dir)
:- export(parent_bundle/1).
parent_bundle := 'ciaopp'.

% Ciao System docs
filepath := ~fsR(bundle_ins(ciaode)/doc/common).
filepath := ~fsR(bundle_ins(ciaode)/doc/readmes).
% CiaoPP docs
filepath := ~fsR(bundle_ins(ciaopp)/doc/readmes).
filepath := ~fsR(bundle_ins(ciaopp)/doc/common).
filepath := ~fsR(bundle_ins(ciaopp)/doc/reference).
filepath := ~fsR(bundle_ins(ciaopp)/doc/tutorials).
filepath := ~fsR(bundle_ins(ciaopp)/doc/figs).
filepath := ~fsR(bundle_ins(ciaopp)/doc/components).
% CiaoPP modules
filepath := ~fsR(bundle_ins(ciaopp)/auto_interface).
filepath := ~fsR(bundle_ins(ciaopp)/plai).
filepath := ~fsR(bundle_ins(ciaopp)/plai/domains).
filepath := ~fsR(bundle_ins(ciaopp)/command_line).
filepath := ~fsR(bundle_ins(ciaopp)/p_unit).
filepath := ~fsR(bundle_ins(ciaopp)/tr_syntax).
filepath := ~fsR(bundle_ins(ciaopp)/plai).
filepath := ~fsR(bundle_ins(ciaopp)/plai/domains).
filepath := ~fsR(bundle_ins(ciaopp)/plai/domains/typeslib).
filepath := ~fsR(bundle_ins(ciaopp)/infer).
filepath := ~fsR(bundle_ins(ciaopp)/infernf).
filepath := ~fsR(bundle_ins(ciaopp)/infercost).
filepath := ~fsR(bundle_ins(ciaopp)/resources).
filepath := ~fsR(bundle_ins(ciaopp)).
% Ciao
filepath := ~fsR(bundle_ins(ciao)).
filepath := ~fsR(bundle_ins(ciao)/lib/condcomp).
filepath := ~fsR(bundle_ins(ciao)/contrib/regexp).
filepath := ~fsR(bundle_ins(ciao)/lib/assertions).
filepath := ~fsR(bundle_ins(ciao)/lib/regtypes).
filepath := ~fsR(bundle_ins(ciao)/lib/rtchecks).
filepath := ~fsR(bundle_ins(ciao)/lib/unittest).

systempath := ~fsR(bundle_ins(ciao)/lib).
systempath := ~fsR(bundle_ins(ciao)/library).
systempath := ~fsR(bundle_ins(ciao)/contrib).
systempath := ~fsR(bundle_ins(ciao)/lib/engine).
systempath := ~fsR(bundle_ins(ciao)/doc/common).

% default values
startpage := 1.
papertype := afourpaper.
libtexinfo := yes.

docformat := texi|ps|pdf|manl|info|html.
