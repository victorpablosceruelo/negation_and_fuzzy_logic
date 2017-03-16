:- module(_qsort6,[qsort/2],ciaopp).

:- new_declaration(doc/2).

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


:- check success qsort(A,B)
        => sorted_num_list(B).

:- entry qsort(A,B)
         : ( list(A,num), ground(A) ).

qsort([X|L],R) :-
        partition(L,X,L1,L2),
        qsort(L2,R2),
        qsort(L1,R1),
        append(R2,[X|R1],R).

qsort([],[]).

partition([],_B,[],[]).

partition([E|R],C,[E|Left1],Right) :-
        E<C,
        !,
        partition(R,C,Left1,Right).

partition([E|R],C,Left,[E|Right1]) :-
        E>=C,
        partition(R,C,Left,Right1).

append([],X,X).

append([H|X],Y,[H|Z]) :-
        append(X,Y,Z).

:- prop sorted_num_list(_1).

sorted_num_list([]).

sorted_num_list([X]) :-
        number(X).

sorted_num_list([X,Y|Z]) :-
        number(X),
        number(Y),
        X<Y,
        sorted_num_list([Y|Z]).
