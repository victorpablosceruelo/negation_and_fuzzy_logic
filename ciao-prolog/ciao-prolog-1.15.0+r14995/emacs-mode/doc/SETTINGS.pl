:- module(_, _, [ciaopaths, fsyntax, assertions]).

% TODO: THIS SETTINGS.pl FILE IS OUTDATED!

:- use_module(library(lpdist(makedir_aux)), [fsR/2]).
:- use_module(library(lpdist(ciao_configure))).

:- reexport(ciaosrc(doc(common('LPDOCCOMMON')))).

:- reexport(library(lpdist(ciao_config_options)), [docformat/1,
		docdir/1, mandir/1, infodir/1, htmldir/1]).
:- redefining(_).

doc_structure :=
    'CiaoMode'-[
      'write'
    ].

filepath := ~fsR(bundle_src(ciao)).
filepath := ~fsR(bundle_src(ciao)/doc/common).
filepath := ~fsR(bundle_src(ciao)/doc/readmes).
filepath := ~fsR(bundle_src(ciao)/doc/reference').
filepath := ~fsR(bundle_src(ciao)/shell').
filepath := ~fsR(bundle_src(ciao)/ciaoc').
filepath := ~fsR(bundle_src(ciao)/engine').
filepath := ~fsR(bundle_src(ciao)/etc').
filepath := ~fsR(bundle_src(ciao)/library/pillow/dist/doc).
filepath := ~fsR(bundle_src(ciaode)/'emacs-mode').

doc_mainopts := no_patches.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog.

