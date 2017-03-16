%%------------------------------------------------------------------------
%%
%% O'Ciao: Object Oriented Programming in Ciao/Prolog
%%
%% SYNTAX FILE FOR CLASSES
%%
%% AUTHOR : Angel Fernandez Pineda
%% DATE   : July 1999
%%
%%------------------------------------------------------------------------

:- use_module(library(class(class_rt))).

%%------------------------------------------------------------------------

:- new_declaration(inherit_class/1,public).
:- new_declaration(implements/1,public).
:- new_declaration(inheritable/1,public).
:- new_declaration(public/1,public).
:- new_declaration(virtual/1,public).

% The following declarations are for internal use:

:- new_declaration(method/1,public).
:- new_declaration(attribute/1,public).
:- new_declaration(super/1,public).

%%------------------------------------------------------------------------

:- op(1150,fx,[(public),(inheritable),(virtual),(method)]).

:- op(900,fy,[(inherited)]).

%%------------------------------------------------------------------------

:- load_compilation_module(library(class(class_tr))).

:- add_sentence_trans(class_sentence_trans/3, 8110).
:- add_clause_trans(class_clause_trans/3, 8110).
