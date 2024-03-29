Ciao-Prolog 1.6 #3: Sun Nov 12 11:31:04 CET 2000
?- use_module('/home/susana/tesis/micodigo/ciao/cneg/version4/proof_cneg.pl').
{Compiling /home/susana/tesis/micodigo/ciao/cneg/version4/proof_cneg.pl
WARNING: (lns 97-168) Predicate dist:dist/2 undefined in source
}

yes


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      DE  PROOF_CNEG.PL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
?
?- no_boole(X).

attach_attribute(X,formula(X,[[X/1,X/0]])) ? ;

no



?- no_binary_list(X).

attach_attribute(_A,formula(_A,[[X/[fA(_A)|fA(_B)]]])),
attach_attribute(_B,formula(_B,[[X/[fA(_A)|fA(_B)]]])),
attach_attribute(X,formula(X,[[X/[fA(_A)|fA(_B)],X/[]]])) ? ;

X = [_A|_],
attach_attribute(_A,formula(_A,[[_A/1,_A/0]])) ? ;

X = [0|_A],
attach_attribute(_B,formula(_B,[[_A/[fA(_B)|fA(_C)]]])),
attach_attribute(_C,formula(_C,[[_A/[fA(_B)|fA(_C)]]])),
attach_attribute(_A,formula(_A,[[_A/[fA(_B)|fA(_C)],_A/[]]])) ? ;

X = [0,_A|_],
attach_attribute(_A,formula(_A,[[_A/1,_A/0]])) ? ;

X = [0,0|_A],
attach_attribute(_B,formula(_B,[[_A/[fA(_B)|fA(_C)]]])),
attach_attribute(_C,formula(_C,[[_A/[fA(_B)|fA(_C)]]])),
attach_attribute(_A,formula(_A,[[_A/[fA(_B)|fA(_C)],_A/[]]])) ? ;

X = [0,0,_A|_],
attach_attribute(_A,formula(_A,[[_A/1,_A/0]])) ? 

yes



?- no_dist_list(X).

attach_attribute(_C,formula(_C,[[X/[(fA(_B),fA(_A))|fA(_C)]]])),
attach_attribute(_B,formula(_B,[[X/[(fA(_B),fA(_A))|fA(_C)]]])),
attach_attribute(_A,formula(_A,[[X/[(fA(_B),fA(_A))|fA(_C)]]])),
attach_attribute(X,formula(X,[[X/[(fA(_B),fA(_A))|fA(_C)],X/[]]])) ? ;

X = [(_A,_A)|_] ? ;

X = [(_B,_C)|_A],
attach_attribute(_B,formula(_B,[[_B/_C]])),
attach_attribute(_C,formula(_C,[[_B/_C]])),
attach_attribute(_F,formula(_F,[[_A/[(fA(_E),fA(_D))|fA(_F)]]])),
attach_attribute(_E,formula(_E,[[_A/[(fA(_E),fA(_D))|fA(_F)]]])),
attach_attribute(_D,formula(_D,[[_A/[(fA(_E),fA(_D))|fA(_F)]]])),
attach_attribute(_A,formula(_A,[[_A/[(fA(_E),fA(_D))|fA(_F)],_A/[]]])) ? ;

X = [(_A,_B),(_C,_C)|_],
attach_attribute(_A,formula(_A,[[_A/_B]])),
attach_attribute(_B,formula(_B,[[_A/_B]])) ? ;

X = [(_A,_B),(_D,_E)|_C],
attach_attribute(_A,formula(_A,[[_A/_B]])),
attach_attribute(_B,formula(_B,[[_A/_B]])),
attach_attribute(_D,formula(_D,[[_D/_E]])),
attach_attribute(_E,formula(_E,[[_D/_E]])),
attach_attribute(_H,formula(_H,[[_C/[(fA(_G),fA(_F))|fA(_H)]]])),
attach_attribute(_G,formula(_G,[[_C/[(fA(_G),fA(_F))|fA(_H)]]])),
attach_attribute(_F,formula(_F,[[_C/[(fA(_G),fA(_F))|fA(_H)]]])),
attach_attribute(_C,formula(_C,[[_C/[(fA(_G),fA(_F))|fA(_H)],_C/[]]])) ? 

yes



?- no_p(V1,V2,VX,VY).

attach_attribute(V1,formula(V1,[[V1/1]])) ? ;

attach_attribute(V2,formula(V2,[[V2/2]])) ? ;

attach_attribute(VX,formula(VX,[[VX/VY]])),
attach_attribute(VY,formula(VY,[[VX/VY]])) ? ;

V1 = 1,
V2 = 2,
VX = 3,
VY = 3 ? ;

no



?- no_q(Z).

X = 0 ? ;

no



?- no_r(X,Y).

attach_attribute(X,formula(X,[[X/8]])) ? ;

attach_attribute(Y,formula(Y,[[Y/9]])) ? ;

no



?- no_pp1(X).

X = 3 ? ;

no



?- no_pp2(X).

X = 3 ? ;

no



?- no_pp3(X).

X = 3 ? ;

X = 5 ? ;

no



?- no_p4(X,Y).

X = 3 ? ;

Y = 5,
attach_attribute(X,formula(X,[[X/3]])) ? ;

no



?- no_positive(X).

attach_attribute(_A,formula(_A,[[X/s(fA(_A))]])),
attach_attribute(X,formula(X,[[X/s(fA(_A)),X/0]])) ? ;

X = s(_A),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[_A/s(fA(_B)),_A/0]])) ? ;

X = s(s(_A)),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[_A/s(fA(_B)),_A/0]])) ? ;

X = s(s(s(_A))),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[_A/s(fA(_B)),_A/0]])) ? 

yes



?- no_natural(X).

X = 0 ? ;

attach_attribute(_A,formula(_A,[[X/s(fA(_A))]])),
attach_attribute(X,formula(X,[[X/s(fA(_A)),X/0]])) ? ;

X = s(_A),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[_A/s(fA(_B)),_A/0]])) ? ;

X = s(s(_A)),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[_A/s(fA(_B)),_A/0]])) ? ;

X = s(s(s(_A))),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[_A/s(fA(_B)),_A/0]])) ? ;

X = s(s(s(s(_A)))),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[_A/s(fA(_B)),_A/0]])) ? 

yes



?- no_pred2(X,Y).

attach_attribute(X,formula(X,[[X/9,X/7]])) ? ;

X = 9,
Y = 5 ? ;

no



?- no_pred1(X).

attach_attribute(X,formula(X,[[X/2]])) ? ;

no



% ?- no_parent(X,Y).

% attach_attribute(X,formula(X,[[X/r,X/d]])) ? ;

% attach_attribute(X,formula(X,[[X/d]])),
% attach_attribute(Y,formula(Y,[[Y/s]])) ? ;

% attach_attribute(Y,formula(Y,[[Y/r]])),
% attach_attribute(X,formula(X,[[X/r]])) ? ;

% attach_attribute(Y,formula(Y,[[Y/s,Y/r]])) ? ;

% no



% ?- no_grandparent(X,Y).

% attach_attribute(X,formula(X,[[X/d]])) ? ;

% attach_attribute(Y,formula(Y,[[Y/s]])) ? ;

% no



?- no_parent(X,Y).


attach_attribute(Y,formula(Y,[[Y/h2,Y/p,Y/h1]])) ? ;

attach_attribute(Y,formula(Y,[[Y/h1,Y/p]])),
attach_attribute(X,formula(X,[[X/p]])) ? ;

attach_attribute(X,formula(X,[[X/p]])),
attach_attribute(Y,formula(Y,[[Y/h2,Y/p]])) ? ;

attach_attribute(Y,formula(Y,[[Y/p]])),
attach_attribute(X,formula(X,[[X/p]])) ? ;

attach_attribute(X,formula(X,[[X/a2]])),
attach_attribute(Y,formula(Y,[[Y/h2,Y/p,Y/h1]])) ? ;

attach_attribute(Y,formula(Y,[[Y/h1,Y/p]])),
attach_attribute(X,formula(X,[[X/p,X/a2]])) ? ;

attach_attribute(X,formula(X,[[X/p,X/a2]])),
attach_attribute(Y,formula(Y,[[Y/h2,Y/p]])) ? ;

attach_attribute(Y,formula(Y,[[Y/p]])),
attach_attribute(X,formula(X,[[X/a2,X/p]])) ? ;

attach_attribute(X,formula(X,[[X/a1]])),
attach_attribute(Y,formula(Y,[[Y/h2,Y/p,Y/h1]])) ? ;

attach_attribute(Y,formula(Y,[[Y/h1,Y/p]])),
attach_attribute(X,formula(X,[[X/p,X/a1]])) ? ;

attach_attribute(X,formula(X,[[X/p,X/a1]])),
attach_attribute(Y,formula(Y,[[Y/h2,Y/p]])) ? ;

attach_attribute(Y,formula(Y,[[Y/p]])),
attach_attribute(X,formula(X,[[X/a1,X/p]])) ? ;

attach_attribute(X,formula(X,[[X/a2,X/a1]])),
attach_attribute(Y,formula(Y,[[Y/h2,Y/h1]])) ? ;

attach_attribute(Y,formula(Y,[[Y/h1]])),
attach_attribute(X,formula(X,[[X/p,X/a1,X/a2]])) ? ;

attach_attribute(X,formula(X,[[X/p,X/a1,X/a2]])),
attach_attribute(Y,formula(Y,[[Y/h2]])) ? ;

attach_attribute(X,formula(X,[[X/a2,X/a1,X/p]])) ? ;

no


?- no_grandparent(X,Y).
attach_attribute(Y,formula(Y,[[Y/h2,Y/h1]])) ? ;

attach_attribute(Y,formula(Y,[[Y/h1,Y/h2]])),
attach_attribute(X,formula(X,[[X/a2]])) ? ;

attach_attribute(X,formula(X,[[X/a2]])),
attach_attribute(Y,formula(Y,[[Y/h1,Y/h2]])) ? ;

attach_attribute(Y,formula(Y,[[Y/h2,Y/h1]])),
attach_attribute(X,formula(X,[[X/a2]])) ? ;

attach_attribute(X,formula(X,[[X/a1]])),
attach_attribute(Y,formula(Y,[[Y/h2,Y/h1]])) ? ;

attach_attribute(Y,formula(Y,[[Y/h1]])),
attach_attribute(X,formula(X,[[X/a2,X/a1]])) ? ;

attach_attribute(X,formula(X,[[X/a2,X/a1]])),
attach_attribute(Y,formula(Y,[[Y/h2,Y/h1]])) ? ;

attach_attribute(Y,formula(Y,[[Y/h1]])),
attach_attribute(X,formula(X,[[X/a1,X/a2]])) ? ;

attach_attribute(X,formula(X,[[X/a1]])),
attach_attribute(Y,formula(Y,[[Y/h2,Y/h1]])) ? ;

attach_attribute(Y,formula(Y,[[Y/h1,Y/h2]])),
attach_attribute(X,formula(X,[[X/a2,X/a1]])) ? ;

attach_attribute(X,formula(X,[[X/a2,X/a1]])),
attach_attribute(Y,formula(Y,[[Y/h2]])) ? ;

.
attach_attribute(Y,formula(Y,[[Y/h2]])),
attach_attribute(X,formula(X,[[X/a1,X/a2]])) ? ;

attach_attribute(X,formula(X,[[X/a1]])),
attach_attribute(Y,formula(Y,[[Y/h2,Y/h1]])) ? ;

attach_attribute(Y,formula(Y,[[Y/h1]])),
attach_attribute(X,formula(X,[[X/a2,X/a1]])) ? ;

attach_attribute(X,formula(X,[[X/a2,X/a1]])),
attach_attribute(Y,formula(Y,[[Y/h2]])) ? ;

attach_attribute(X,formula(X,[[X/a1,X/a2]])) ? ;

no

?- no_iguales(X,Y).

attach_attribute(X,formula(X,[[Y/X]])),
attach_attribute(Y,formula(Y,[[Y/X]])) ? ;

no
?- no_r1(X,Y).

attach_attribute(Y,formula(Y,[[Y/2,Y/1]])) ? ;

attach_attribute(Y,formula(Y,[[Y/1]])),
attach_attribute(X,formula(X,[[X/2]])) ? ;

attach_attribute(X,formula(X,[[X/1]])),
attach_attribute(Y,formula(Y,[[Y/2]])) ? ;

attach_attribute(X,formula(X,[[X/2,X/1]])) ? ;

no
?- no_r2(X,Y).

attach_attribute(Y,formula(Y,[[Y/9]])) ? ;

attach_attribute(X,formula(X,[[X/8]])) ? ;

no
?- no_r3(X,Y).

attach_attribute(Y,formula(Y,[[Y/3,Y/2]])) ? ;

attach_attribute(Y,formula(Y,[[Y/2]])),
attach_attribute(X,formula(X,[[X/2]])) ? ;

attach_attribute(X,formula(X,[[X/1]])),
attach_attribute(Y,formula(Y,[[Y/3]])) ? ;

attach_attribute(X,formula(X,[[X/2,X/1]])) ? ;

no
?- no_r4(X,Y).

attach_attribute(Y,formula(Y,[[Y/3]])) ? ;

attach_attribute(X,formula(X,[[X/1]])) ? ;

no
?-  no_r5(X,Y,Z).

attach_attribute(Y,formula(Y,[[Y/3,Y/2]])) ? ;

attach_attribute(Y,formula(Y,[[Y/2]])),
attach_attribute(X,formula(X,[[X/2]])) ? ;

attach_attribute(X,formula(X,[[X/1]])),
attach_attribute(Y,formula(Y,[[Y/3]])) ? ;

attach_attribute(X,formula(X,[[X/2,X/1]])) ? ;

X = 1,
Y = 2,
attach_attribute(Z,formula(Z,[[Z/3]])) ? ;

X = 2,
Y = 3 ? ;

no

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      DE QUEENSPEANO.PL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
?- no_p9(0).

yes
?- no_p91(0).

yes
?- no_greater(X,Y).

attach_attribute(_B,formula(_B,[[X/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[X/s(fA(_A))]])),
attach_attribute(X,formula(X,[[X/s(fA(_A)),X/s(fA(_B))]])) ? ;

attach_attribute(X,formula(X,[[X/s(fA(_A))]])),
attach_attribute(_A,formula(_A,[[X/s(fA(_A))]])),
attach_attribute(Y,formula(Y,[[Y/0]])) ? ;

attach_attribute(Y,formula(Y,[[Y/s(fA(_A))]])),
attach_attribute(_A,formula(_A,[[Y/s(fA(_A))]])),
attach_attribute(X,formula(X,[[X/s(fA(_B))]])),
attach_attribute(_B,formula(_B,[[X/s(fA(_B))]])) ? ;

attach_attribute(_A,formula(_A,[[Y/s(fA(_A))]])),
attach_attribute(Y,formula(Y,[[Y/0,Y/s(fA(_A))]])) ? ;

X = s(_A),
Y = s(_),
attach_attribute(_C,formula(_C,[[_A/s(fA(_C))]])),
attach_attribute(_D,formula(_D,[[_A/s(fA(_D))]])),
attach_attribute(_B,formula(_B,[[_A/fA(_B)]])),
attach_attribute(_A,formula(_A,[[_A/fA(_B),_A/s(fA(_C)),_A/s(fA(_D))]])) ? ;

X = s(_B),
Y = s(_A),
attach_attribute(_D,formula(_D,[[_B/s(fA(_D))]])),
attach_attribute(_A,formula(_A,[[_A/0]])),
attach_attribute(_C,formula(_C,[[_B/fA(_C)]])),
attach_attribute(_B,formula(_B,[[_B/fA(_C),_B/s(fA(_D))]])) ? 

yes
?- no_greater(0,0).

yes
?- select(X,[1],L).

L = [],
X = 1 ? ;

no
?- no_select(X,[1],L).   

attach_attribute(L,formula(L,[[L/[1|fA(_A)]]])),
attach_attribute(_A,formula(_A,[[L/[1|fA(_A)]]])),
attach_attribute(X,formula(X,[[X/1]])) ? ;

attach_attribute(_A,formula(_A,[[L/[1|fA(_A)]]])),
attach_attribute(L,formula(L,[[L/[],L/[1|fA(_A)]]])) ? ;

L = [1|_],
attach_attribute(X,formula(X,[[X/1]])) ? ;

L = [1|_] ? ;

no

?-  no_select(X,[1],L), L=[1].

L = [1] ? ;

L = [1],
attach_attribute(X,formula(X,[[X/1]])) ? ;

L = [1] ? ;

L = [1],
attach_attribute(X,formula(X,[[X/1]])) ? ;

no

?- no_select(X,[1],L), L=[1,2].

L = [1,2] ? ;

L = [1,2],
attach_attribute(X,formula(X,[[X/1]])) ? ;

L = [1,2] ? ;

L = [1,2],
attach_attribute(X,formula(X,[[X/1]])) ? ;

no

?- no_select(X,[1],L), X=1.  

X = 1,
attach_attribute(_A,formula(_A,[[L/[1|fA(_A)]]])),
attach_attribute(L,formula(L,[[L/[],L/[1|fA(_A)]]])) ? ;

L = [1|_],
X = 1 ? ;

no



?- no_attack1([],fA(X),s(0)).

yes
?-  cneg_aux(no_attack1([],X,s(0)),[Z],[X]).

no

?-   negate_conj(((select([0|Y])),[select(1)]),(select(B)),[Z],[],Sol).

B = fA(_B),
Sol = [dist(fA(_B),[0|fA(_A)])],
Y = fA(_A) ? ;

Sol = [B=[0|Y],cneg(select(1))] ? ;

no

?- no_queens(0,Qs).          

attach_attribute(Qs,formula(Qs,[[Qs/[]]])) ? ;

no
?- no_select(H,[s(0)],J). 

attach_attribute(_A,formula(_A,[[J/[s(0)|fA(_A)]]])),
attach_attribute(J,formula(J,[[J/[],J/[s(0)|fA(_A)]]])) ? ;

attach_attribute(J,formula(J,[[J/[s(0)|fA(_A)]]])),
attach_attribute(_A,formula(_A,[[J/[s(0)|fA(_A)]]])),
attach_attribute(H,formula(H,[[H/s(0)]])) ? ;

J = [s(0)|_] ? ;

J = [s(0)|_],
attach_attribute(H,formula(H,[[H/s(0)]])) ? ;

no

?- no_queens2([],[s(0)],X).

attach_attribute(X,formula(X,[[X/[s(0)]]])) ? ;

no

?- no_queens4([s(0)],[s(s(s(0)))],X).

attach_attribute(X,formula(X,[[X/[s(0),s(s(s(0)))]]])) ? ;

no


?- no_p3(Y).

yes

?- no_p2(Y).

no

?- no_p1(Y). 

no

?- frontier((dist(Y,s(0)),
          add(s(s(s(0))),s(0),Y),
          add(s(0),s(0),X),
          no_attack1([],s(0),X)),F).
         
F = [((dist(s(_A),s(0)),add(s(s(s(0))),s(0),s(_A)),add(s(0),s(0),s(_B)),no_attack1([],s(0),s(_B))),[dist(s(_A),s(0)),add(s(s(0)),s(0),_A),add(0,s(0),_B)])] ? ;

no


?- no_queens3([s(0)],[s(s(s(0)))],X).

attach_attribute(X,formula(X,[[X/[s(0),s(s(s(0)))]]])) ? ;

no

?-  no_queens4([s(0)],[s(s(s(0)))],X).

attach_attribute(X,formula(X,[[X/[s(0),s(s(s(0)))]]])) ? ;

no
?- no_queens5([s(0)],[s(s(s(0)))],X).

attach_attribute(X,formula(X,[[X/[s(0),s(s(s(0)))]]])) ? ;

no

?-  no_queens8([s(0)],[s(s(s(0)))],X).

attach_attribute(X,formula(X,[[X/[s(0),s(s(s(0)))]]])) ? ;

no

?-  no_select2(Q,[s(0)],N).

attach_attribute(N,formula(N,[[N/[]]])) ? ;

attach_attribute(Q,formula(Q,[[Q/s(0)]])) ? ;

no
?-  no_select3(Q,[s(0)],N).

attach_attribute(N,formula(N,[[N/[s(0)|fA(_A)]]])),
attach_attribute(_A,formula(_A,[[N/[s(0)|fA(_A)]]])) ? ;

N = [s(0)|_] ? ;

no


?- negate_conj((select(A,[e],[e|L]),[select(A,[],L)]),select(Q,[e],F),[Q],[],S).

F = fA(_B),
L = fA(_A),
Q = A,
S = [dist(fA(_B),[e|fA(_A)])] ? ;

Q = A,
S = [F=[e|L],cneg(select(A,[],L))] ? ;

no
%Q = A,
%S = [dist(fA(F),[e|fA(L)])] ? ;

%Q = A,
%S = [F=[e|L],cneg(select(A,[],L))] ? ;

%no



?- cneg_aux(select(Q,[s(0)],N),[Q],[]).

N = [s(0)|_],
attach_attribute(Q,formula(Q,[[Q/s(0)]])) ? ;

no
?- no_queens6([s(0)],[s(s(s(0)))],X).  

attach_attribute(_A,formula(_A,[[X/[fA(_A),s(s(s(0)))]]])),
attach_attribute(X,formula(X,[[X/[s(0),s(s(s(0)))],X/[fA(_A),s(s(s(0)))]]])) ? ;

attach_attribute(X,formula(X,[[X/[fA(_A),s(s(s(0)))]]])),
attach_attribute(_A,formula(_A,[[X/[fA(_A),s(s(s(0)))]]])) ? ;

attach_attribute(X,formula(X,[[X/[fA(_A),s(s(s(0)))]]])),
attach_attribute(_A,formula(_A,[[X/[fA(_A),s(s(s(0)))]]])) ? ;

attach_attribute(X,formula(X,[[X/[s(0),s(s(s(0)))]]])) ? ;

no 

?- no_queens9([s(0)],[s(s(s(0)))],X). 

attach_attribute(X,formula(X,[[X/[fA(_A),s(s(s(0)))]]])),
attach_attribute(_A,formula(_A,[[X/[fA(_A),s(s(s(0)))]]])) ? ;

X = [_A,s(s(s(0)))],
attach_attribute(_A,formula(_A,[[_A/s(0)]])) ? ;

no
?- no_queens2([s(0)],[s(s(s(0)))],X).  

attach_attribute(X,formula(X,[[X/[s(0),s(s(s(0)))]]])) ? ;

no 
?- no_queens1([s(0)],[s(s(s(0)))],X).  

attach_attribute(X,formula(X,[[X/[s(0),s(s(s(0)))]]])) ? ;

no 

?- no_p17(X).   

attach_attribute(X,formula(X,[[X/[s(0)]]])) ? ;

no 
?- no_queens(s(0),Qs).        

attach_attribute(Qs,formula(Qs,[[Qs/[s(0)]]])) ? ;

yes

?-  no_add(s(s(0)),s(0),S).

attach_attribute(S,formula(S,[[S/s(fA(_A))]])),
attach_attribute(_A,formula(_A,[[S/s(fA(_A))]])) ? ;

S = s(_A),
attach_attribute(_A,formula(_A,[[_A/s(fA(_B))]])),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])) ? ;

S = s(s(_A)),
attach_attribute(_A,formula(_A,[[_A/s(0)]])) ? ;

no
?- no_no_attack_down(s(s(0)),s(0),s(0)).

yes
?- no_no_attack1([s(s(0))],s(0),s(0)).

yes
?- no_no_attack(s(0),[s(s(0))]).

yes

?- cneg_aux(queens1([s(0)],[s(s(0))],Q),[Q],[]). 

attach_attribute(Q,formula(Q,[[Q/[s(0),s(s(0))]]])) ? ;

yes   
?- cneg_aux(queens1([s(s(0))],[s(0)],Q),[Q],[]).  

attach_attribute(Q,formula(Q,[[Q/[s(s(0)),s(0)]]])) ? ;

yes
?- no_p(X).

X = s(0) ? ;

X = s(_A),
attach_attribute(_A,formula(_A,[[_A/s(0),_A/0]])) ? ;

no

?- no_exist_list.

no
?- no_zero_list.

yes
?-  no_q3(i). 

yes
?-  no_q3(1).

no
?- no_queens(0,Qs). 

attach_attribute(Qs,formula(Qs,[[Qs/[]]])) ? ;

yes
?- no_queens([],Qs). 

yes
?- no_queens1([s(s(0)),s(0)],[],Qs). 

yes
?- no_no_attack1([],s(0),s(0)).

no
?-   no_no_attack(s(0),[s(s(0))]).

yes

?- no_no_attack(s(0),[s(s(s(0)))]).

no
?- no_queens6([s(0)],[s(s(s(0)))],a).

yes
?- no_queens6([s(0)],[s(s(s(0)))],[s(0),s(s(s(0)))]).

no
