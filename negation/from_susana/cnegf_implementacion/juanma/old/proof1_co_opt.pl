:- module(_proof1_co,[impar/1,digito_par1/1,digito_par2/1,digito_par3/1],ciaopp).

:- new_declaration(comment/2).

:- op(975,xfx,=>).

:- op(978,xfx,::).

:- new_declaration(decl/1).

:- op(1150,fx,decl).

:- new_declaration(decl/2).

:- op(1150,xfx,decl).

:- new_declaration(pred/1).

:- op(1150,fx,pred).

:- new_declaration(pred/2).

:- op(1150,xfx,pred).

:- new_declaration(prop/1).

:- op(1150,fx,prop).

:- new_declaration(prop/2).

:- op(1150,xfx,prop).

:- new_declaration(modedef/1).

:- op(1150,fx,modedef).

:- new_declaration(calls/1).

:- op(1150,fx,calls).

:- new_declaration(calls/2).

:- op(1150,xfx,calls).

:- new_declaration(success/1).

:- op(1150,fx,success).

:- new_declaration(success/2).

:- op(1150,xfx,success).

:- new_declaration(comp/1).

:- op(1150,fx,comp).

:- new_declaration(comp/2).

:- op(1150,xfx,comp).

:- new_declaration(entry/1).

:- op(1150,fx,entry).

:- include(library(assertions)).

:- use_module(library('assertions/native_props')).

:- include(library(nativeprops)).

:- redefining(indep/1).

:- redefining(indep/2).

:- op(950,xf,[&]).

:- op(975,xfx,[=>]).

:- use_module(library(andprolog)).

:- include(library(cges)).

:- use_module('.'(neg)).

impar(X) :-
        Y is X rem 2,
        Y\==0.

mi_member(X,[X|_L]).

mi_member(X,[_Y|L]) :-
        mi_member(X,L).

digito_par1(X) :-
        mi_member(X,[0,1,2,3,4,5,6,7,8,9]),
        neg(impar(X)).

digito_par2(X) :-
        neg(impar(X)),
        mi_member(X,[0,1,2,3,4,5,6,7,8,9]).

digito_par3(X) :-
        neg(impar(X)).

