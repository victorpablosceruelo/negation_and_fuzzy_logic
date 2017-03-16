:- module(_, _, [ciaopaths, fsyntax]).

% TODO: THIS SETTINGS.pl FILE IS OUTDATED!

:- use_module(library(terms)).
:- use_module(library(bundle_registry(bundle_registry_load))).
:- use_module(library(lpdist(ciao_configure))).
:- use_module(library(lpdist(makedir_aux)), [fsR/2]).

:- reexport(ciaosrc(doc(common('LPDOCCOMMON')))).
:- reexport(library(lpdist(ciao_config_options)), [lpdoclib/1]).

:- redefining(_).

% -*- mode: Makefile; -*-
% ----------------------------------------------------------------------------
% 
% ****             lpdoc document generation SETTINGS                  *****
% 
%         These SETTINGS should be changed to suit your application
% 
% The defaults listed are suggestions and/or the ones used for local 
% installation in the CLIP group machines.
% ----------------------------------------------------------------------------
% Makes lpdoc use texinfo file in current dir
libtexinfo := 'no'.
% ----------------------------------------------------------------------------
% List of all directories where the .pl files to be documented can be found
% (separated by spaces; put explicit paths, i.e., do not use any variables)
% You also need to specify all the paths of files used by those files!

filepath := ~fsR(bundle_src(ciao)/'library'/'pillow').
filepath := ~fsR(bundle_src(ciao)/'library'/'pillow'/'dist'/'doc').

% ----------------------------------------------------------------------------
% List of all the *system* directories where .pl files used are
% (separated by spaces; put explicit paths, i.e., do not use any variables)
% You also need to specify all the paths of files used by those files!
% You can put these in FILEPATHS instead. Putting them here only
% means that the corresponding files will be assumed to be "system"
% files in the documentation.

systempath := ~fsR(bundle_src(ciao)/'lib').
systempath := ~fsR(bundle_src(ciao)/'library').
systempath := ~fsR(bundle_src(ciao)/'doc'/'common').

% ----------------------------------------------------------------------------
% Document structure

doc_structure := 
    'pillow_doc'-[
      'html',
      'http',
      'pillow_types'
    ].

% ----------------------------------------------------------------------------

doc_mainopts := no_changelog|no_sysmods|no_engmods.
doc_compopts := no_changelog|no_sysmods|no_engmods.

docformat := texi|ps|info|html.

index := pred|regtype|global.

% ----------------------------------------------------------------------------
% If you are using bibliographic references, define this to be the list 
% (separated by commas, full paths, no spaces) of .bib files to be used 
% to find them. 
% 
:- reexport(library(lpdist(ciao_config_options)), [bibfile/1]).
% ----------------------------------------------------------------------------
% Setting this to a different value allows changing the page number of
% the first page of the manual. This can be useful if the manual is to
% be included in a larger document or set of manuals.
% Typically, this should be an odd number.
%
startpage := 1.
% ----------------------------------------------------------------------------
% Selects the type of paper/format for printed documents
% See also the -onesided and -twosided options for the main file
% 
% letterpaper smallbook afourpaper afourlatex afourwide afourthesis 
papertype := 'afourpaper'.
% ----------------------------------------------------------------------------
% Only need to change these if you will be installing the docs somewhere else
% ----------------------------------------------------------------------------
% Define this to be the dir in which you want the document(s) installed. 
% 
:- reexport(library(lpdist(ciao_config_options)),[docdir/1,mandir/1,infodir/1,htmldir/1]).
% ----------------------------------------------------------------------------
% Uncomment this for .dvi and .ps files to be compressed on installation
% If uncommented, set it to path for gzip
% 
% COMPRESSDVIPS = gzip -f
% ----------------------------------------------------------------------------
% Define this to be the style sheet to copy for the html pages
% Uncomment this to avoid using style sheets
%
htmlstyle := ~fsR((~lpdoclib)/'default.css').
% ----------------------------------------------------------------------------
% Define this to be the permissions for automatically generated data files
% 
datapermissions := perm(rw, rw, r).
% ----------------------------------------------------------------------------
% Define this to be the perms for automatically generated dirs and exec files
% 
execpermissions := perm(rwx, rwx, rx).

% ----------------------------------------------------------------------------
% end of SETTINGS
