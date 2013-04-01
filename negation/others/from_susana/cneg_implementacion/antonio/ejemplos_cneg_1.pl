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
%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?- no_boole(X).

attach_attribute(X,formula(X,[[X/1,X/0]])) ? ;

no

%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_boole(X).

attach_attribute(X,formula(X,[[X/0,X/1]])) ? .

no

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
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

%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%

?-  no_binary_list(X).

attach_attribute(_A,formula(_A,[[X/[fA(_A)|fA(_B)]]])),
attach_attribute(_B,formula(_B,[[X/[fA(_A)|fA(_B)]]])),
attach_attribute(X,formula(X,[[X/[],X/[fA(_A)|fA(_B)]]])) ? .

X = [_A|_],
attach_attribute(_A,formula(_A,[[_A/0,_A/1]])) ? .

X = [0|_A],
attach_attribute(_B,formula(_B,[[_A/[fA(_B)|fA(_C)]]])),
attach_attribute(_C,formula(_C,[[_A/[fA(_B)|fA(_C)]]])),
attach_attribute(_A,formula(_A,[[_A/[],_A/[fA(_B)|fA(_C)]]])) ? .

X = [0,_A|_],
attach_attribute(_A,formula(_A,[[_A/0,_A/1]])) ? .

X = [0,0|_A],
attach_attribute(_B,formula(_B,[[_A/[fA(_B)|fA(_C)]]])),
attach_attribute(_C,formula(_C,[[_A/[fA(_B)|fA(_C)]]])),
attach_attribute(_A,formula(_A,[[_A/[],_A/[fA(_B)|fA(_C)]]])) ? .

X = [0,0,_A|_],
attach_attribute(_A,formula(_A,[[_A/0,_A/1]])) ? .

X = [0,0,0|_A],
attach_attribute(_B,formula(_B,[[_A/[fA(_B)|fA(_C)]]])),
attach_attribute(_C,formula(_C,[[_A/[fA(_B)|fA(_C)]]])),
attach_attribute(_A,formula(_A,[[_A/[],_A/[fA(_B)|fA(_C)]]])) ? 

yes


%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
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

%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?- no_dist_list(X).

attach_attribute(_C,formula(_C,[[X/[(fA(_B),fA(_A))|fA(_C)]]])),
attach_attribute(_B,formula(_B,[[X/[(fA(_B),fA(_A))|fA(_C)]]])),
attach_attribute(_A,formula(_A,[[X/[(fA(_B),fA(_A))|fA(_C)]]])),
attach_attribute(X,formula(X,[[X/[],X/[(fA(_B),fA(_A))|fA(_C)]]])) ? .

X = [(_A,_A)|_] ? .

X = [(_B,_C)|_A],
attach_attribute(_B,formula(_B,[[_B/_C]])),
attach_attribute(_C,formula(_C,[[_B/_C]])),
attach_attribute(_F,formula(_F,[[_A/[(fA(_E),fA(_D))|fA(_F)]]])),
attach_attribute(_E,formula(_E,[[_A/[(fA(_E),fA(_D))|fA(_F)]]])),
attach_attribute(_D,formula(_D,[[_A/[(fA(_E),fA(_D))|fA(_F)]]])),
attach_attribute(_A,formula(_A,[[_A/[],_A/[(fA(_E),fA(_D))|fA(_F)]]])) ? .

X = [(_A,_B),(_C,_C)|_],
attach_attribute(_A,formula(_A,[[_A/_B]])),
attach_attribute(_B,formula(_B,[[_A/_B]])) ? .

X = [(_A,_B),(_D,_E)|_C],
attach_attribute(_A,formula(_A,[[_A/_B]])),
attach_attribute(_B,formula(_B,[[_A/_B]])),
attach_attribute(_D,formula(_D,[[_D/_E]])),
attach_attribute(_E,formula(_E,[[_D/_E]])),
attach_attribute(_H,formula(_H,[[_C/[(fA(_G),fA(_F))|fA(_H)]]])),
attach_attribute(_G,formula(_G,[[_C/[(fA(_G),fA(_F))|fA(_H)]]])),
attach_attribute(_F,formula(_F,[[_C/[(fA(_G),fA(_F))|fA(_H)]]])),
attach_attribute(_C,formula(_C,[[_C/[],_C/[(fA(_G),fA(_F))|fA(_H)]]])) ? 

yes


%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
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

%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?- no_p(V1,V2,VX,VY).

attach_attribute(V1,formula(V1,[[V1/1]])) ? .

attach_attribute(V2,formula(V2,[[V2/2]])) ? .

attach_attribute(VX,formula(VX,[[VY/VX]])),
attach_attribute(VY,formula(VY,[[VY/VX]])) ? .

V1 = 1,
V2 = 2,
VX = 3,
VY = 3 ? .

V1 = 1,
V2 = 2,
VY = VX,
attach_attribute(VX,formula(VX,[[VX/3]])) ? .

V1 = 1,
V2 = 2,
VY = VX,
attach_attribute(_A,formula(_A,[[_A/0,VX/0]])),
attach_attribute(VX,formula(VX,[[_A/0,VX/0]])) ? .

no


%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?- no_q(Z).

X = 0 ? ;

no

%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?- no_q(Z).

Z = 0 ? .

no


%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?- no_r(X,Y).

attach_attribute(X,formula(X,[[X/8]])) ? ;

attach_attribute(Y,formula(Y,[[Y/9]])) ? ;

no

%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_r(X,Y).

attach_attribute(X,formula(X,[[X/8]])) ? .

attach_attribute(Y,formula(Y,[[Y/9]])) ? .

no


%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?- no_p1(X).

X = 3 ? ;

no

%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?- no_p1(X).

X = 3 ? .

attach_attribute(X,formula(X,[[X/3]])) ? .

attach_attribute(_A,formula(_A,[[_A/0,X/0]])),
attach_attribute(X,formula(X,[[_A/0,X/0]])) ? .

no

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?- no_p2(X).

X = 3 ? ;

no
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?- no_p2(X).

X = 3 ? .

no

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?-

X = 3 ? ;

X = 5 ? ;

no
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_p3(X).

X = 3 ? .

X = 5 ? .

no

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?- no_p4(X,Y).

X = 3 ? ;

Y = 5,
attach_attribute(X,formula(X,[[X/3]])) ? ;

no
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_p4(X,Y).

X = 3 ? .

Y = 5,
attach_attribute(X,formula(X,[[X/3]])) ? .

no

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?-

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
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_positive(X).

attach_attribute(_A,formula(_A,[[X/s(fA(_A))]])),
attach_attribute(X,formula(X,[[X/0,X/s(fA(_A))]])) ? .

X = s(_A),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[_A/0,_A/s(fA(_B))]])) ? .

X = s(s(_A)),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[_A/0,_A/s(fA(_B))]])) ? .

X = s(s(s(_A))),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[_A/0,_A/s(fA(_B))]])) ? 

yes

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
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
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_natural(X).

X = 0 ? .

attach_attribute(_A,formula(_A,[[X/s(fA(_A))]])),
attach_attribute(X,formula(X,[[X/s(fA(_A)),X/0]])) ? .

X = s(_A),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[_A/0,_A/s(fA(_B))]])) ? .

X = s(s(_A)),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[_A/0,_A/s(fA(_B))]])) ? .

X = s(s(s(_A))),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[_A/0,_A/s(fA(_B))]])) ? .

X = s(s(s(s(_A)))),
attach_attribute(_B,formula(_B,[[_A/s(fA(_B))]])),
attach_attribute(_A,formula(_A,[[_A/0,_A/s(fA(_B))]])) ? 

yes

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?- no_pred2(X,Y).

attach_attribute(X,formula(X,[[X/9,X/7]])) ? ;

X = 9,
Y = 5 ? ;

no
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_pred2(X,Y).

attach_attribute(X,formula(X,[[X/7,X/9]])) ? .

X = 9,
Y = 5 ? .

no

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?- no_pred1(X).

attach_attribute(X,formula(X,[[X/2]])) ? ;

no
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_pred1(X).

attach_attribute(X,formula(X,[[X/2]])) ? .

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


%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
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
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_parent(X,Y).

attach_attribute(X,formula(X,[[X/a1,X/p,X/a2]])) ? .

attach_attribute(X,formula(X,[[X/a2,X/p]])),
attach_attribute(Y,formula(Y,[[Y/p]])) ? .

attach_attribute(Y,formula(Y,[[Y/p]])),
attach_attribute(X,formula(X,[[X/a1,X/p]])) ? .

attach_attribute(X,formula(X,[[X/p]])),
attach_attribute(Y,formula(Y,[[Y/p]])) ? .

attach_attribute(Y,formula(Y,[[Y/h1]])),
attach_attribute(X,formula(X,[[X/a1,X/p,X/a2]])) ? .

attach_attribute(X,formula(X,[[X/a2,X/p]])),
attach_attribute(Y,formula(Y,[[Y/p,Y/h1]])) ? .

attach_attribute(Y,formula(Y,[[Y/p,Y/h1]])),
attach_attribute(X,formula(X,[[X/a1,X/p]])) ? .

attach_attribute(X,formula(X,[[X/p]])),
attach_attribute(Y,formula(Y,[[Y/h1,Y/p]])) ? .

attach_attribute(Y,formula(Y,[[Y/h2]])),
attach_attribute(X,formula(X,[[X/a1,X/p,X/a2]])) ? .

attach_attribute(X,formula(X,[[X/a2,X/p]])),
attach_attribute(Y,formula(Y,[[Y/p,Y/h2]])) ? .

attach_attribute(Y,formula(Y,[[Y/p,Y/h2]])),
attach_attribute(X,formula(X,[[X/a1,X/p]])) ? .

attach_attribute(X,formula(X,[[X/p]])),
attach_attribute(Y,formula(Y,[[Y/h2,Y/p]])) ? .

attach_attribute(Y,formula(Y,[[Y/h1,Y/h2]])),
attach_attribute(X,formula(X,[[X/a1,X/a2]])) ? .

attach_attribute(X,formula(X,[[X/a2]])),
attach_attribute(Y,formula(Y,[[Y/p,Y/h2,Y/h1]])) ? .

attach_attribute(Y,formula(Y,[[Y/p,Y/h2,Y/h1]])),
attach_attribute(X,formula(X,[[X/a1]])) ? 

yes

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
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
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_grandparent(X,Y).

no

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?- no_iguales(X,Y).

attach_attribute(X,formula(X,[[Y/X]])),
attach_attribute(Y,formula(Y,[[Y/X]])) ? ;

no
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_iguales(X,Y).

attach_attribute(X,formula(X,[[Y/X]])),
attach_attribute(Y,formula(Y,[[Y/X]])) ? .

no

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?- no_r1(X,Y).

attach_attribute(Y,formula(Y,[[Y/2,Y/1]])) ? ;

attach_attribute(Y,formula(Y,[[Y/1]])),
attach_attribute(X,formula(X,[[X/2]])) ? ;

attach_attribute(X,formula(X,[[X/1]])),
attach_attribute(Y,formula(Y,[[Y/2]])) ? ;

attach_attribute(X,formula(X,[[X/2,X/1]])) ? ;

no
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_r1(X,Y).

attach_attribute(X,formula(X,[[X/1,X/2]])) ? .

attach_attribute(X,formula(X,[[X/2]])),
attach_attribute(Y,formula(Y,[[Y/1]])) ? .

attach_attribute(Y,formula(Y,[[Y/2]])),
attach_attribute(X,formula(X,[[X/1]])) ? .

attach_attribute(Y,formula(Y,[[Y/1,Y/2]])) ? .

no

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?- no_r2(X,Y).

attach_attribute(Y,formula(Y,[[Y/9]])) ? ;

attach_attribute(X,formula(X,[[X/8]])) ? ;

no
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_r2(X,Y).

attach_attribute(X,formula(X,[[X/8]])) ? .

attach_attribute(Y,formula(Y,[[Y/9]])) ? .

no

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?- no_r3(X,Y).

attach_attribute(Y,formula(Y,[[Y/3,Y/2]])) ? ;

attach_attribute(Y,formula(Y,[[Y/2]])),
attach_attribute(X,formula(X,[[X/2]])) ? ;

attach_attribute(X,formula(X,[[X/1]])),
attach_attribute(Y,formula(Y,[[Y/3]])) ? ;

attach_attribute(X,formula(X,[[X/2,X/1]])) ? ;

no
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_r3(X,Y).

attach_attribute(X,formula(X,[[X/1,X/2]])) ? .

attach_attribute(X,formula(X,[[X/2]])),
attach_attribute(Y,formula(Y,[[Y/2]])) ? .

attach_attribute(Y,formula(Y,[[Y/3]])),
attach_attribute(X,formula(X,[[X/1]])) ? .

attach_attribute(Y,formula(Y,[[Y/2,Y/3]])) ? .

no

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
?- no_r4(X,Y).

attach_attribute(Y,formula(Y,[[Y/3]])) ? ;

attach_attribute(X,formula(X,[[X/1]])) ? ;

no
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_r4(X,Y).

attach_attribute(X,formula(X,[[X/2,X/1]])) ? .

attach_attribute(Y,formula(Y,[[Y/3]])),
attach_attribute(X,formula(X,[[X/1,X/2]])) ? .

attach_attribute(Y,formula(Y,[[Y/3]])),
attach_attribute(X,formula(X,[[X/2,X/1]])) ? .

attach_attribute(Y,formula(Y,[[Y/3]])),
attach_attribute(X,formula(X,[[X/1,X/2]])) ? .

no

%%%%%%%%CON EL CNEG DE SUSANA%%%%%%%%%
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
%%%%%%%%CON EL CNEG DE ANTONIO%%%%%%%%%
?-  no_r5(X,Y,Z).

attach_attribute(X,formula(X,[[X/1,X/2]])) ? .

attach_attribute(X,formula(X,[[X/2]])),
attach_attribute(Y,formula(Y,[[Y/2]])) ? .

attach_attribute(Y,formula(Y,[[Y/3]])),
attach_attribute(X,formula(X,[[X/1]])) ? .

attach_attribute(Y,formula(Y,[[Y/2,Y/3]])) ? .

X = 1,
Y = 2,
attach_attribute(Z,formula(Z,[[Z/3]])) ? .

X = 2,
Y = 3 ? .

no

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      DE QUEENSPEANO.PL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
