:- module(_proof_cneg_co,[stored_clause/2,stored_pred/2,cneg/1,cneg_aux/2,compound_meta_to_term/2,compound_to_term/2,is_eq_diseq/1,frontier/2,filter_frontier/3,conjunction/3,distributive/3,conj_clauses/3,negate_conj_frontier/4,negate_conj/4,organization_conj/6,get_eq_head/3,obtain_eq_head/5,organization_body/4,detach_attribute_all/1,detach_att_if_needed/1,organization_body_diseq/6,organization_body_diseq_aux/6,replace_dist/2,remove_duplicates/2,obtain_diseq/2,normalization_conj/8,difference/3,union/3,eliminate_redundant_var/3,eliminate_repeated_eq/2,eliminate_irrelevant_disequalities/4,negation_conj/7,divide_formula/4,negation_formula/9,negation_formula1/7,negate_I/3,negate_eq/3,replace_free_vars/3,replace/4,replace_all/4,replace_list/4,rm_fA/2,rm_fA_all/2,negate_Dimp/2,negate_Rimp/2,negate_Dexp_Rexp/4,get_conjunction/2,combine/1,call_all/1,odd/1,not_odd_number/1,parent/2,grandparent/2,boole/1,binary_list/1,dist_list/1,p/4,p1/1,p2/1,p3/1,p4/2,q/1,r/2,positive/1,natural/1,pred2/2,pred1/1,no_boole/1,no_binary_list/1,no_dist_list/1,no_p/4,no_q/1,no_r/2,no_p1/1,no_p2/1,no_p3/1,no_p4/2,no_positive/1,no_natural/1,no_parent/2,no_grandparent/2,no_pred2/2,no_pred1/1],ciaopp).

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

:- meta_predicate cneg(goal).

:- meta_predicate stored_pred(goal,?).

:- use_module(library(metaterms),[varset/2,ask/2]).

:- use_module(library(lists),[append/3]).

:- use_module(library(idlists),[memberchk/2]).

:- use_module(library(aggregates),[setof/3]).

:- use_module(engine(internals),[term_to_meta/2]).

:- use_module(dist,[dist/2]).

:- data stored_clause/2.

:- data stored_pred/2.

stored_pred('dist:dist'(X,Y),dist(X,Y)).

cneg(Goal) :-
        varset(Goal,GoalVars),
        cneg_aux(Goal,GoalVars).

cneg_aux(Meta,GoalVars) :-
        compound_meta_to_term(Meta,GoalI),
        rm_fA(GoalI,Goal),
        frontier(Goal,Frontier),
        negate_conj_frontier(Frontier,Goal,GoalVars,LSolutions),
        combine(LSolutions).

compound_meta_to_term(M,G) :-
        term_to_meta(T,M),
        !,
        compound_to_term(T,G).

compound_meta_to_term(T,G) :-
        compound_to_term(T,G).

compound_to_term((M1;M2),(G1;G2)) :-
        !,
        compound_meta_to_term(M1,G1),
        compound_meta_to_term(M2,G2).

compound_to_term((M1,M2),(G1,G2)) :-
        !,
        compound_meta_to_term(M1,G1),
        compound_meta_to_term(M2,G2).

compound_to_term(T,T) :-
        is_eq_diseq(T),
        !.

compound_to_term(T,G) :-
        stored_pred(T,G).

is_eq_diseq(dist(_1,_2)).

is_eq_diseq(_1=_2).

frontier((G1;G2),Frontier) :-
        !,
        frontier(G1,F1),
        frontier(G2,F2),
        append(F1,F2,Front),
        filter_frontier(Front,(G1;G2),Frontier).

frontier((G1,G2),Frontier) :-
        !,
        frontier(G1,F1),
        frontier(G2,F2),
        conjunction(F1,F2,Front),
        filter_frontier(Front,(G1,G2),Frontier).

frontier(dist(X,Y),[(dist(X,Y),[dist(X,Y)])]) :- !.

frontier(X=_Y,[(X=X,[])]) :- !.

frontier(Goal,Frontier) :-
        setof((Goal,Body),stored_clause(Goal,Body),Frontier).

filter_frontier([],_G,[]).

filter_frontier([(H,B)|Front],G,[(H,B)|Frontier]) :-
        ask(H,G),
        !,
        filter_frontier(Front,G,Frontier).

filter_frontier([(H,B)|Front],G,[(H,B)|Frontier]) :-
        ask(G,H),
        !,
        filter_frontier(Front,G,Frontier).

filter_frontier([(_H,_B)|Front],G,Frontier) :-
        filter_frontier(Front,G,Frontier).

conjunction([],_F2,[]) :- !.

conjunction([Conj|F1],F2,F5) :-
        distributive(F2,Conj,F3),
        conjunction(F1,F2,F4),
        append(F3,F4,F5).

distributive([],_Conj,[]).

distributive([Conj2|L],Conj1,[Conj3|L3]) :-
        conj_clauses(Conj1,Conj2,Conj3),
        distributive(L,Conj1,L3).

conj_clauses((H1,B1),(H2,B2),((H1,H2),B3)) :-
        append(B1,B2,B3).

negate_conj_frontier([],_Goal,_GoalVars,[]).

negate_conj_frontier([Conj|Frontier],Goal,GoalVars,[SolConj|LSolutions]) :-
        negate_conj(Conj,Goal,GoalVars,SolConj),
        negate_conj_frontier(Frontier,Goal,GoalVars,LSolutions).

negate_conj(C,G,GoalVars,SolC) :-
        organization_conj(C,G,GoalVars,I,D,R),
        normalization_conj(I,D,R,GoalVars,In,Dn,ImpVars,ExpVars),
        negation_conj(In,Dn,R,GoalVars,ImpVars,ExpVars,SolC).

organization_conj((Head,BodyList),G,GoalVars,I,D,R) :-
        setof(GoalVars,G=Head,[Values]),
        get_eq_head(GoalVars,Values,Ih),
        organization_body(BodyList,Ib,D,R),
        append(Ih,Ib,I).

get_eq_head(GoalVars,Values,Ih) :-
        obtain_eq_head(GoalVars,Values,_Values1,_RepVars,Ih).

obtain_eq_head([],[],[],[],[]).

obtain_eq_head([Var|GoalVars],[X|Vs],[X|NewVs],RepVars,[Var=X|I]) :-
        var(X),
        obtain_eq_head(GoalVars,Vs,NewVs,RepVars,I),
        memberchk(X,RepVars),
        !.

obtain_eq_head([Var|GoalVars],[X|Vs],[Var|NewVs],[Var|RepVars],I) :-
        var(X),
        !,
        Var=X,
        obtain_eq_head(GoalVars,Vs,NewVs,RepVars,I).

obtain_eq_head([Var|GoalVars],[X|Vs],[X|NewVs],RepVars,[Var=X|I]) :-
        obtain_eq_head(GoalVars,Vs,NewVs,RepVars,I).

organization_body([],[],[],[]).

organization_body([T1=T2|BodyList],I,D,R) :-
        !,
        varset(T1,T1Vars),
        varset(T2,T2Vars),
        append(T1Vars,T2Vars,T12Vars),
        remove_duplicates(T12Vars,Vars),
        setof(Vars,T1=T2,[Values]),
        get_eq_head(Vars,Values,Ih),
        organization_body(BodyList,Ib,D,R),
        append(Ih,Ib,I).

organization_body([dist(T1,T2)|BodyList],I,D,R) :-
        !,
        varset(dist(T1,T2),VarsI),
        copy_term(dist(T1,T2),DNew),
        varset(DNew,VarsNew),
        detach_attribute_all(VarsNew),
        remove_duplicates(VarsNew,Vars),
        setof(VarsNew,DNew,Values),
        organization_body_diseq(Values,Vars,BodyList,I1,D1,R1),
        replace_list(VarsNew,VarsI,(I1,D1,R1),(I,D,R)).

organization_body([Other|BodyList],I,D,[Other|R]) :-
        organization_body(BodyList,I,D,R).

detach_attribute_all([]).

detach_attribute_all([V|Vars]) :-
        detach_att_if_needed(V),
        detach_attribute_all(Vars).

detach_att_if_needed(Var) :-
        get_attribute(Var,_1),
        !,
        detach_attribute(Var).

detach_att_if_needed(_1).

organization_body_diseq([],_Vars,BodyList,I,D,R) :-
        !,
        organization_body(BodyList,I,D,R).

organization_body_diseq(Values,Vars,BodyList,I,D,R) :-
        member(ValueVars,Values),
        organization_body_diseq_aux(ValueVars,Vars,BodyList,I,D,R).

organization_body_diseq_aux([V|ValueVars],Vars,BodyList,I,D,R) :-
        obtain_diseq(V,D1),
        replace_list([V|ValueVars],Vars,D1,Dh),
        replace_dist(Dh,Daux),
        organization_body(BodyList,I,Db,R),
        append(Daux,Db,D).

replace_dist([],[]).

replace_dist([X/Y|List],[dist(X,Y)|ListD]) :-
        replace_dist(List,ListD).

remove_duplicates([],[]).

remove_duplicates([X|L1],L2) :-
        memberchk(X,L1),
        !,
        remove_duplicates(L1,L2).

remove_duplicates([X|L1],[X|L2]) :-
        remove_duplicates(L1,L2).

obtain_diseq(Value,D) :-
        get_attribute(Value,formula(Value,Form)),
        member(D,Form).

normalization_conj(I,D,R,GoalVars,I1,D1,ImpVars,ExpVars) :-
        varset(R,RVars),
        difference(RVars,GoalVars,RelVars),
        varset(I,IVars),
        union(IVars,GoalVars,ImpVars),
        difference(RVars,ImpVars,ExpVars),
        eliminate_redundant_var(I,GoalVars,Iv),
        eliminate_repeated_eq(Iv,I1),
        eliminate_irrelevant_disequalities(D,ImpVars,RelVars,D1).

difference([],_NFVars,[]).

difference([Var|Vars],NFVars,FreeVars) :-
        memberchk(Var,NFVars),
        !,
        difference(Vars,NFVars,FreeVars).

difference([Var|Vars],NFVars,[Var|FreeVars]) :-
        difference(Vars,NFVars,FreeVars).

union([],Vars2,Vars2).

union([Var|Vars1],Vars2,Vars) :-
        memberchk(Var,Vars2),
        !,
        union(Vars1,Vars2,Vars).

union([Var|Vars1],Vars2,[Var|Vars]) :-
        union(Vars1,Vars2,Vars).

eliminate_redundant_var([],_GoalVars,[]).

eliminate_redundant_var([Var=Value|I],GoalVars,Iv) :-
        var(Var),
        \+memberchk(Var,GoalVars),
        !,
        Var=Value,
        eliminate_redundant_var(I,GoalVars,Iv).

eliminate_redundant_var([Var=Value|I],GoalVars,Iv) :-
        var(Value),
        \+memberchk(Value,GoalVars),
        !,
        Var=Value,
        eliminate_redundant_var(I,GoalVars,Iv).

eliminate_redundant_var([Var=Value|I],GoalVars,[Var=Value|Iv]) :-
        eliminate_redundant_var(I,GoalVars,Iv).

eliminate_repeated_eq([],[]).

eliminate_repeated_eq([X=Y|L1],L2) :-
        X==Y,
        !,
        eliminate_repeated_eq(L1,L2).

eliminate_repeated_eq([X=Y|L1],L2) :-
        memberchk(X=Y,L1),
        !,
        eliminate_repeated_eq(L1,L2).

eliminate_repeated_eq([X=Y|L1],L2) :-
        memberchk(Y=X,L1),
        !,
        eliminate_repeated_eq(L1,L2).

eliminate_repeated_eq([E|L1],[E|L2]) :-
        eliminate_repeated_eq(L1,L2).

eliminate_irrelevant_disequalities([],_ImpVars,_RelVars,[]).

eliminate_irrelevant_disequalities([Diseq|D],ImpVars,RelVars,D1) :-
        varset(Diseq,Vars),
        member(V,Vars),
        \+memberchk(V,ImpVars),
        \+memberchk(V,RelVars),
        !,
        eliminate_irrelevant_disequalities(D,ImpVars,RelVars,D1).

eliminate_irrelevant_disequalities([Diseq|D],ImpVars,RelVars,[Diseq|D1]) :-
        eliminate_irrelevant_disequalities(D,ImpVars,RelVars,D1).

negation_conj(I,D,R,GoalVars,ImpVars,ExpVars,SolC) :-
        divide_formula(D,ExpVars,Dimp,Dexp),
        divide_formula(R,ExpVars,Rimp,Rexp),
        negation_formula(I,Dimp,Dexp,Rimp,Rexp,GoalVars,ImpVars,ExpVars,SolC).

divide_formula([],_ExpVars,[],[]).

divide_formula([Term|F],ExpVars,Fimp,[Term|Fexp]) :-
        varset(Term,Vars),
        member(V,Vars),
        memberchk(V,ExpVars),
        !,
        divide_formula(F,ExpVars,Fimp,Fexp).

divide_formula([Term|F],ExpVars,[Term|Fimp],Fexp) :-
        divide_formula(F,ExpVars,Fimp,Fexp).

negation_formula(I,_Dimp,_Dexp,_Rimp,_Rexp,GoalVars,_ImpV,_ExpV,[Sol]) :-
        negate_I(I,GoalVars,Sol).

negation_formula(I,Dimp,_Dexp,_Rimp,_Rexp,_GoalV,_ImpV,_ExpV,SolC) :-
        negate_Dimp(Dimp,Sol),
        append(I,Sol,SolC).

negation_formula(I,Dimp,Dexp,Rimp,Rexp,_GoalV,ImpVars,ExpsVars,SolC) :-
        append(I,Dimp,I_Dimp),
        negation_formula1(I_Dimp,Dexp,Rimp,Rexp,ImpVars,ExpsVars,SolC).

negation_formula1(I_Dimp,_Dexp,Rimp,_Rexp,_ImpsV,_ExpsV,SolC) :-
        negate_Rimp(Rimp,Sol),
        append(I_Dimp,Sol,SolC).

negation_formula1(I_Dimp,Dexp,Rimp,Rexp,ImpVars,ExpVars,SolC) :-
        append(I_Dimp,Rimp,I_Dimp_Rimp),
        append(Dexp,Rexp,DRexp),
        negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,Sol),
        append(I_Dimp_Rimp,Sol,SolC).

negate_I(I,GoalVars,Sol) :-
        member(Eq,I),
        negate_eq(Eq,GoalVars,Sol).

negate_eq(T1=T2,GoalVars,dist(T1f,T2f)) :-
        varset(T1=T2,Vars),
        difference(Vars,GoalVars,FreeVars),
        replace_free_vars(FreeVars,T1,T1f),
        replace_free_vars(FreeVars,T2,T2f).

replace_free_vars([],T,T).

replace_free_vars([X|FreeVars],T,Tf) :-
        replace(T,X,fA(X),Tf1),
        replace_free_vars(FreeVars,Tf1,Tf).

replace(Term,_Var,_Value,Term) :-
        ground(Term),
        !.

replace(Term,Var,Value,Value) :-
        var(Term),
        Term==Var,
        !.

replace(Term,Var,_Value,Term) :-
        var(Term),
        Term\==Var,
        !.

replace(Term,Var,Value,Term1) :-
        Term=..[Functor|Args],
        replace_all(Args,Var,Value,Args1),
        Term1=..[Functor|Args1].

replace_all([],_1,_2,[]).

replace_all([Arg|Rest],Var,Value,[Arg1|Rest1]) :-
        replace(Arg,Var,Value,Arg1),
        replace_all(Rest,Var,Value,Rest1).

replace_list([],[],Term,Term).

replace_list([Var|LVars],[Value|LValues],Term1,Term2) :-
        replace(Term1,Var,Value,TermAux),
        replace_list(LVars,LValues,TermAux,Term2).

rm_fA(Term,Term) :-
        ground(Term),
        !.

rm_fA(Term,Term) :-
        var(Term),
        !.

rm_fA(fA(Var),Var) :-
        var(Var),
        !.

rm_fA(Term,Term1) :-
        Term=..[Functor|Args],
        rm_fA_all(Args,Args1),
        Term1=..[Functor|Args1].

rm_fA_all([],[]).

rm_fA_all([Arg|Args],[Arg1|Args1]) :-
        rm_fA(Arg,Arg1),
        rm_fA_all(Args,Args1).

negate_Dimp([D|_Dimp],[T1=T2]) :-
        replace(D,fA(X),X,dist(T1,T2)).

negate_Dimp([D|Dimp],[D|SolC]) :-
        negate_Dimp(Dimp,SolC).

negate_Rimp([R|_Rimp],[cneg(R)]).

negate_Rimp([R|Rimp],[R|SolC]) :-
        negate_Rimp(Rimp,SolC).

negate_Dexp_Rexp([],_1,_2,_3) :-
        !,
        fail.

negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,[cneg_aux(DR,ImpVars)]) :-
        replace_free_vars(ExpVars,DRexp,DRList),
        get_conjunction(DRList,DR).

get_conjunction([X],X).

get_conjunction([X|L],(X,C)) :-
        get_conjunction(L,C).

combine([]).

combine([Sol|Rest]) :-
        call_all(Sol),
        combine(Rest).

call_all([]).

call_all([G|L]) :-
        call(G),
        call_all(L).

:- use_module(dist,[dist/2]).

odd(s(0)).

odd(s(s(X))) :-
        odd(X).

not_odd_number(X) :-
        cneg(odd(X)).

parent(d,r).

parent(r,s).

grandparent(X,Y) :-
        parent(X,Z),
        parent(Z,Y).

boole(0).

boole(1).

binary_list([]).

binary_list([Y|L]) :-
        boole(Y),
        binary_list(L).

dist_list([]).

dist_list([(X,Y)|L]) :-
        dist(X,Y),
        dist_list(L).

p(1,2,X,X) :-
        dist(X,3),
        dist(X,Z),
        q(Z).

p1(X) :-
        dist(X,3),
        dist(X,Z),
        q(Z).

p2(X) :-
        dist(X,3),
        dist(X,_Z).

p3(X) :-
        dist(X,3),
        dist(X,5).

p4(X,Y) :-
        dist(X,3),
        dist(Y,5).

q(Z) :-
        dist(Z,0).

q(Z) :-
        r(Z,_W).

r(8,9).

positive(0).

positive(s(X)) :-
        positive(X).

natural(X) :-
        dist(X,0),
        positive(X).

pred2(7,_1).

pred2(9,Y) :-
        dist(Y,5),
        pred1(_X).

pred1(2).

no_boole(X) :-
        cneg(boole(X)).

no_binary_list(X) :-
        cneg(binary_list(X)).

no_dist_list(X) :-
        cneg(dist_list(X)).

no_p(V1,V2,VX,VY) :-
        cneg(p(V1,V2,VX,VY)).

no_q(Z) :-
        cneg(q(Z)).

no_r(X,Y) :-
        cneg(r(X,Y)).

no_p1(X) :-
        cneg(p1(X)).

no_p2(X) :-
        cneg(p2(X)).

no_p3(X) :-
        cneg(p3(X)).

no_p4(X,Y) :-
        cneg(p4(X,Y)).

no_positive(X) :-
        cneg(positive(X)).

no_natural(X) :-
        cneg(natural(X)).

no_parent(X,Y) :-
        cneg(parent(X,Y)).

no_grandparent(X,Y) :-
        cneg(grandparent(X,Y)).

no_pred2(X,Y) :-
        cneg(pred2(X,Y)).

no_pred1(X) :-
        cneg(pred1(X)).

