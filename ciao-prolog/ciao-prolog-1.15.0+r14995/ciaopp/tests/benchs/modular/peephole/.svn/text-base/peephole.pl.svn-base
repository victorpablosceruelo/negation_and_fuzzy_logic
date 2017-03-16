:- module( _peephole, [
	peephole_opt/2,
	popt1/2, popt2/2, popt3/2, popt4/3
		      ], [assertions,nativeprops] ).

:- use_module(peephole2, [popt11/3, popt21/3, popt31/3, member1/2, popt_chkmember/3]).

:- entry peephole_opt(X,Y)
         : ( term_typing:var(Y), term_typing:ground(X) ).

peephole_opt(Pil,OptPil) :-
        popt1(Pil,Pil1),
        popt2(Pil1,Pil2),
        popt3(Pil2,Pil3),
        popt4(Pil3,_N,OptPil).

popt1([],[]).
popt1([Inst|Rest],Pil1) :-
        popt11(Inst,Rest,Pil1).
popt1([Inst|Rest],Pil1) :-
        \+popt11(Inst,Rest,Pil),
        term_basic:(Pil1=[Inst|Pil]),
        popt1(Rest,Pil).


popt2([],[]).
popt2([Inst|PilRest],Pil1) :-
        popt21(Inst,PilRest,Pil1).
popt2([Inst|PilRest],Pil1) :-
        \+popt21(Inst,PilRest,Pil1),
        term_basic:(Pil1=[Inst|Pil1Rest]),
        popt2(PilRest,Pil1Rest).

popt3([],[]).
popt3([Inst|Rest],Pil) :-
        popt31(Inst,Rest,Pil).
popt3([Inst|Rest],Pil) :-
        \+popt31(Inst,Rest,Pil),
        term_basic:(Pil=[Inst|Pil1]),
        popt3(Pil,Pil1).

popt4([],_1,[]).
popt4([label((P,N,K))|Rest],Seen,[label((P,N,K))|ORest]) :-
        !,
        'popt4/3/2/$disj/1'(Seen,P,N),
        popt4(Rest,Seen,ORest).
popt4([execute((P,N)),label((P,N,K))|Rest],Seen,OList) :-
        !,
        'popt4/3/3/$disj/1'(Seen,OList,P,N,K,ORest),
        popt4(Rest,Seen,ORest).
popt4([Inst|Rest],Seen,[Inst|ORest]) :-
        popt4(Rest,Seen,ORest).

'popt4/3/2/$disj/1'(Seen,P,N) :-
        arithmetic:(N>=0),
        member1((P,N),Seen).
'popt4/3/2/$disj/1'(Seen,P,N) :-
        arithmetic:(N<0).

'popt4/3/3/$disj/1'(Seen,OList,P,N,K,ORest) :-
        arithmetic:(N>=0),
        popt_chkmember((P,N),Seen,SFlag),
        'popt4/3/3/$disj/1/6/1/$disj/1'(OList,P,N,K,ORest,SFlag).
'popt4/3/3/$disj/1'(Seen,OList,P,N,K,ORest) :-
        arithmetic:(N<0),
        term_basic:(OList=[execute((P,N)),label((P,N,K))|ORest]).

'popt4/3/3/$disj/1/6/1/$disj/1'(OList,P,N,K,ORest,SFlag) :-
        arithmetic:(SFlag=:=1),
        term_basic:(OList=[execute((P,N)),label((P,N,K))|ORest]).
'popt4/3/3/$disj/1/6/1/$disj/1'(OList,P,N,K,ORest,SFlag) :-
        arithmetic:(SFlag=\=1),
        term_basic:(OList=[label((P,N,K))|ORest]).



