:- module(peephole2, [ 
	popt11/3, popt21/3, popt31/3, member1/2, popt_chkmember/3,
	peep_chk/2, popt1a/2
		     ],[]).

:- use_module(peephole, [popt1/2, popt2/2, popt3/2, popt4/3]).
:- use_module(peephole3, [popt1a1/3, peep_use/2, term_or_chk/2]).


popt11(puttvar(T,R),[getstr(S,R)|PilRest],[putstr(S,T)|OptPilRest]) :-
        popt1a(PilRest,OptPilRest).
popt11(movreg(T,R),[getstr(S,R)|PilRest],OptInstList) :-
        'popt11/3/2/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest),
        popt1(PilRest,OptPilRest).
popt11(movreg(T,R),[puttbreg(R)|PilRest],OptInstList) :-
        'popt11/3/3/$disj/1'(OptInstList,T,R,PilRest,OptPilRest),
        popt1(PilRest,OptPilRest).
popt11(movreg(T,R),[addreg(R,S)|PilRest],OptInstList) :-
        'popt11/3/4/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest),
        popt1(PilRest,OptPilRest).
popt11(movreg(T,R),[subreg(R,S)|PilRest],OptInstList) :-
        'popt11/3/5/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest),
        popt1(PilRest,OptPilRest).
popt11(movreg(T,R),[mulreg(R,S)|PilRest],OptInstList) :-
        'popt11/3/6/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest),
        popt1(PilRest,OptPilRest).
popt11(movreg(T,R),[divreg(R,S)|PilRest],OptInstList) :-
        'popt11/3/7/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest),
        popt1(PilRest,OptPilRest).
popt11(putpvar(V,R),[getpval(V,R)|PilRest],[putpvar(V,R)|OptPilRest]) :-
        popt1(PilRest,OptPilRest).
popt11(putpval(V,R),[getstr(Str,R)|PilRest],[getstrv(Str,V)|OptPilRest]) :-
        \+term_basic:(Str=('.',2)),
        popt1(PilRest,OptPilRest).
popt11(putpvar(V,R),[getstr(Str,R)|PilRest],[putstrv(Str,V)|OptPilRest]) :-
        \+term_basic:(Str=('.',2)),
        popt1a(PilRest,OptPilRest).
popt11(gettval(R,R),PRest,OptPRest) :-
        popt1(PRest,OptPRest).
popt11(movreg(R,R),PRest,OptPRest) :-
        popt1(PRest,OptPRest).
popt11(jump(L),[label(L)|PRest],[label(L)|OptPRest]) :-
        popt1(PRest,OptPRest).
popt11(jump(Addr),[jump(_N)|PRest],[jump(Addr)|OptPRest]) :-
        popt1(PRest,OptPRest).
popt11(jumpz(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
        popt1(PRest,OptPRest).
popt11(jumpnz(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
        popt1(PRest,OptPRest).
popt11(jumplt(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
        popt1(PRest,OptPRest).
popt11(jumple(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
        popt1(PRest,OptPRest).
popt11(jumpgt(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
        popt1(PRest,OptPRest).
popt11(jumpge(_N,L),[label(L)|PRest],[label(L)|OptPRest]) :-
        popt1(PRest,OptPRest).

'popt11/3/2/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
        peep_chk(PilRest,R),
        term_basic:(OptInstList=[getstr(S,T)|OptPilRest]).
'popt11/3/2/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
        \+peep_chk(PilRest,R),
        term_basic:(OptInstList=[movreg(T,R),getstr(S,R)|OptPilRest]).

'popt11/3/3/$disj/1'(OptInstList,T,R,PilRest,OptPilRest) :-
        peep_chk(PilRest,R),
        term_basic:(OptInstList=[puttbreg(T)|OptPilRest]).
'popt11/3/3/$disj/1'(OptInstList,T,R,PilRest,OptPilRest) :-
        \+peep_chk(PilRest,R),
        term_basic:(OptInstList=[movreg(T,R),puttbreg(R)|OptPilRest]).

'popt11/3/4/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
        peep_chk(PilRest,R),
        term_basic:(OptInstList=[addreg(T,S)|OptPilRest]).
'popt11/3/4/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
        \+peep_chk(PilRest,R),
        term_basic:(OptInstList=[movreg(T,R),addreg(R,S)|OptPilRest]).

'popt11/3/5/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
        peep_chk(PilRest,R),
        term_basic:(OptInstList=[subreg(T,S)|OptPilRest]).
'popt11/3/5/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
        \+peep_chk(PilRest,R),
        term_basic:(OptInstList=[movreg(T,R),subreg(R,S)|OptPilRest]).

'popt11/3/6/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
        peep_chk(PilRest,R),
        term_basic:(OptInstList=[mulreg(T,S)|OptPilRest]).
'popt11/3/6/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
        \+peep_chk(PilRest,R),
        term_basic:(OptInstList=[movreg(T,R),mulreg(R,S)|OptPilRest]).

'popt11/3/7/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
        peep_chk(PilRest,R),
        term_basic:(OptInstList=[divreg(T,S)|OptPilRest]).
'popt11/3/7/$disj/1'(OptInstList,T,R,PilRest,S,OptPilRest) :-
        \+peep_chk(PilRest,R),
        term_basic:(OptInstList=[movreg(T,R),divreg(R,S)|OptPilRest]).

popt1a([],[]).
popt1a([Inst|PilRest],Pil1) :-
        popt1a1(Inst,PilRest,Pil1).
popt1a([Inst|PilRest],Pil1) :-
        \+popt1a1(Inst,PilRest,Pil1),
        term_basic:(Pil1=[Inst|Pil1Rest]),
        popt1a(PilRest,Pil1Rest).

% -----------------------------------------------------------

popt21(getstr(('.',2),R),PilRest,[getlist(R)|OptPilRest]) :-
        popt2(PilRest,OptPilRest).
popt21(putstr(('.',2),R),PilRest,[putlist(R)|OptPilRest]) :-
        popt2(PilRest,OptPilRest).
popt21(getcon([],R),PilRest,[getnil(R)|OptPilRest]) :-
        popt2(PilRest,OptPilRest).
popt21(putcon([],R),PilRest,[putnil(R)|OptPilRest]) :-
        popt2(PilRest,OptPilRest).
popt21(unicon([]),PilRest,[uninil|OptPilRest]) :-
        popt2(PilRest,OptPilRest).
popt21(bldcon([]),PilRest,[bldnil|OptPilRest]) :-
        popt2(PilRest,OptPilRest).

% -----------------------------------------------------------

popt31(getlist(R0),[unitvar(R1),unitvar(R2)|Rest],[getlist_tvar_tvar(R0,R1,R2)|OptRest]) :-
        popt3(Rest,OptRest).
popt31(getcomma(R0),[unitvar(R1),unitvar(R2)|Rest],[getcomma_tvar_tvar(R0,R1,R2)|OptRest]) :-
        popt3(Rest,OptRest).

% -----------------------------------------------------------

popt_chkmember(P,L,Flag) :-
        term_typing:var(L),
        term_basic:(L=[P|_N]),
        term_basic:(Flag=0).
popt_chkmember(P,L,Flag) :-
        term_typing:nonvar(L),
        term_basic:(L=[P1|L1]),
        'popt_chkmember/3/2/$disj/1'(P,Flag,P1,L1).

'popt_chkmember/3/2/$disj/1'(P,Flag,P1,L1) :-
        term_basic:(P=P1),
        !,
        term_basic:(Flag=1).
'popt_chkmember/3/2/$disj/1'(P,Flag,P1,L1) :-
        popt_chkmember(P,L1,Flag).


peep_chk([],_1).
peep_chk([Inst|Rest],R) :-
        \+peep_use(Inst,R),
        term_or_chk([Inst|Rest],R).

member1(X,[X|_Xs]).
member1(X,[_N|Xs]) :-
        member1(X,Xs).

