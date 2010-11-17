:- module(_qsort_typesfd_shfr_upper_nf_co_new_co_new,[qsort/2],ciaopp).

:- new_declaration(comment/2).

:- op(975,xfx,=>).

:- op(978,xfx,::).

:- new_declaration((decl)/1).

:- op(1150,fx,decl).

:- new_declaration((decl)/2).

:- op(1150,xfx,decl).

:- new_declaration((pred)/1).

:- op(1150,fx,pred).

:- new_declaration((pred)/2).

:- op(1150,xfx,pred).

:- new_declaration((prop)/1).

:- op(1150,fx,prop).

:- new_declaration((prop)/2).

:- op(1150,xfx,prop).

:- new_declaration((modedef)/1).

:- op(1150,fx,modedef).

:- new_declaration((calls)/1).

:- op(1150,fx,calls).

:- new_declaration((calls)/2).

:- op(1150,xfx,calls).

:- new_declaration((success)/1).

:- op(1150,fx,success).

:- new_declaration((success)/2).

:- op(1150,xfx,success).

:- new_declaration((comp)/1).

:- op(1150,fx,comp).

:- new_declaration((comp)/2).

:- op(1150,xfx,comp).

:- new_declaration((entry)/1).

:- op(1150,fx,entry).

:- include(library(assertions)).

:- use_module(library('assertions/native_props')).

:- include(library(nativeprops)).

:- redefining(indep/1).

:- redefining(indep/2).

:- op(950,xf,[&]).

:- op(975,xfx,[=>]).

:- use_module(library('andprolog/andprolog_rt')).

:- include(library(cges)).

:- new_declaration(comment/2).

:- op(975,xfx,=>).

:- op(978,xfx,::).

:- new_declaration((decl)/1).

:- op(1150,fx,decl).

:- new_declaration((decl)/2).

:- op(1150,xfx,decl).

:- new_declaration((pred)/1).

:- op(1150,fx,pred).

:- new_declaration((pred)/2).

:- op(1150,xfx,pred).

:- new_declaration((prop)/1).

:- op(1150,fx,prop).

:- new_declaration((prop)/2).

:- op(1150,xfx,prop).

:- new_declaration((modedef)/1).

:- op(1150,fx,modedef).

:- new_declaration((calls)/1).

:- op(1150,fx,calls).

:- new_declaration((calls)/2).

:- op(1150,xfx,calls).

:- new_declaration((success)/1).

:- op(1150,fx,success).

:- new_declaration((success)/2).

:- op(1150,xfx,success).

:- new_declaration((comp)/1).

:- op(1150,fx,comp).

:- new_declaration((comp)/2).

:- op(1150,xfx,comp).

:- new_declaration((entry)/1).

:- op(1150,fx,entry).

:- new_declaration(comment/2).

:- op(975,xfx,=>).

:- op(978,xfx,::).

:- new_declaration((decl)/1).

:- op(1150,fx,decl).

:- new_declaration((decl)/2).

:- op(1150,xfx,decl).

:- new_declaration((pred)/1).

:- op(1150,fx,pred).

:- new_declaration((pred)/2).

:- op(1150,xfx,pred).

:- new_declaration((prop)/1).

:- op(1150,fx,prop).

:- new_declaration((prop)/2).

:- op(1150,xfx,prop).

:- new_declaration((modedef)/1).

:- op(1150,fx,modedef).

:- new_declaration((calls)/1).

:- op(1150,fx,calls).

:- new_declaration((calls)/2).

:- op(1150,xfx,calls).

:- new_declaration((success)/1).

:- op(1150,fx,success).

:- new_declaration((success)/2).

:- op(1150,xfx,success).

:- new_declaration((comp)/1).

:- op(1150,fx,comp).

:- new_declaration((comp)/2).

:- op(1150,xfx,comp).

:- new_declaration((entry)/1).

:- op(1150,fx,entry).

:- include(library(assertions)).

:- use_module(library('assertions/native_props')).

:- use_module(library('assertions/native_props')).

:- include(library(nativeprops)).

:- redefining(indep/1).

:- redefining(indep/2).

:- op(950,xf,[&]).

:- op(975,xfx,[=>]).

:- use_module(library('andprolog/andprolog_rt')).

:- op(950,xf,[&]).

:- op(975,xfx,[=>]).

:- use_module(library('andprolog/andprolog_rt')).

:- include(library(cges)).

:- new_declaration(comment/2).

:- op(975,xfx,=>).

:- op(978,xfx,::).

:- new_declaration((decl)/1).

:- op(1150,fx,decl).

:- new_declaration((decl)/2).

:- op(1150,xfx,decl).

:- new_declaration((pred)/1).

:- op(1150,fx,pred).

:- new_declaration((pred)/2).

:- op(1150,xfx,pred).

:- new_declaration((prop)/1).

:- op(1150,fx,prop).

:- new_declaration((prop)/2).

:- op(1150,xfx,prop).

:- new_declaration((modedef)/1).

:- op(1150,fx,modedef).

:- new_declaration((calls)/1).

:- op(1150,fx,calls).

:- new_declaration((calls)/2).

:- op(1150,xfx,calls).

:- new_declaration((success)/1).

:- op(1150,fx,success).

:- new_declaration((success)/2).

:- op(1150,xfx,success).

:- new_declaration((comp)/1).

:- op(1150,fx,comp).

:- new_declaration((comp)/2).

:- op(1150,xfx,comp).

:- new_declaration((entry)/1).

:- op(1150,fx,entry).

:- use_module('.'(neg)).

:- true pred qsort(_96512,_96539)
         : ( list(_96512,num), var(_96539) )
        => ( list(_96512,num), list(_96539,num), size_ub(_96512,length(_96512)), size_ub(_96539,exp(2,length(_96512))-1.0) )
         + ( not_fails(qsort(_96512,_96539),qsort(_96512,_96539)), covered(qsort(_96512,_96539),qsort(_96512,_96539)), steps_ub(qsort(_96512,_96539),qsort(_96512,_96539),sum($(j),1,length(_96512),exp(2,length(_96512)- $(j))* $(j))+exp(2,length(_96512)-1)*length(_96512)+2.0*exp(2,length(_96512))-1.0) ).

:- true pred qsort(_98154,_98181)
         : ( ground(_98154), var(_98181), mshare([[_98181]]) )
        => ( ground(_98154), ground(_98181) ).

:- entry qsort(_98619,_98646)
         : ( list(_98619,num), var(_98646), ground(_98619) ).

qsort([_98944|_98971],_99000) :-
        partition(_98971,_98944,_99099,_99126),
        qsort(_99126,_99191),
        qsort(_99099,_99256),
        append(_99256,[_98944|_99191],_99000).

qsort([],[]).

:- true pred partition(_99672,_99699,_99726,_99753)
         : ( list(_99672,num), num(_99699), var(_99726), var(_99753) )
        => ( list(_99672,num), num(_99699), list(_99726,num), list(_99753,num), size_ub(_99672,length(_99672)), size_ub(_99699,int(_99699)), size_ub(_99726,length(_99672)), size_ub(_99753,length(_99672)) )
         + ( not_fails(partition(_99672,_99699,_99726,_99753),partition(_99672,_99699,_99726,_99753)), covered(partition(_99672,_99699,_99726,_99753),partition(_99672,_99699,_99726,_99753)), steps_ub(partition(_99672,_99699,_99726,_99753),partition(_99672,_99699,_99726,_99753),length(_99672)+1) ).

:- true pred partition(_101338,_101365,_101392,_101419)
         : ( ground(_101338), ground(_101365), var(_101392), var(_101419), mshare([[_101392],[_101419]]), indep(_101392,_101419) )
        => ( ground(_101338), ground(_101365), ground(_101392), ground(_101419) ).

partition([],_102148,[],[]).

partition([_102287|_102314],_102343,[_102287|_102392],_102421) :-
        _102287<_102343,
        !,
        partition(_102314,_102343,_102392,_102421).

partition([_102752|_102779],_102808,_102835,[_102752|_102884]) :-
        _102752>=_102808,
        partition(_102779,_102808,_102835,_102884).

:- true pred append(_103234,_103261,_103288)
         : ( list(_103234,num), list1(_103261,num), var(_103288) )
        => ( list(_103234,num), list1(_103261,num), list1(_103288,num), size_ub(_103234,length(_103234)), size_ub(_103261,length(_103261)), size_ub(_103288,length(_103261)+length(_103234)) )
         + ( not_fails(append(_103234,_103261,_103288),append(_103234,_103261,_103288)), covered(append(_103234,_103261,_103288),append(_103234,_103261,_103288)), steps_ub(append(_103234,_103261,_103288),append(_103234,_103261,_103288),length(_103234)+1) ).

:- true pred append(_104678,_104705,_104732)
         : ( ground(_104678), ground(_104705), var(_104732), mshare([[_104732]]) )
        => ( ground(_104678), ground(_104705), ground(_104732) ).

append([],_105259,_105259).

append([_105393|_105420],_105449,[_105393|_105498]) :-
        append(_105420,_105449,_105498).

prueba_1(_105728,_105755) :-
        cnegf(qsort(_105728,_105755)).

prueba_2(_105972,_105999,_106026) :-
        cnegf(append(_105972,_105999,_106026)).

