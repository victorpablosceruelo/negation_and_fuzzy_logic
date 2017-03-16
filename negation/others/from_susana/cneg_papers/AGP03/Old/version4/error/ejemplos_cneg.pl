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
attach_attribute(_A,formula(_A,[[_A/1,_A/0]])) ? ;

X = [0,0,0|_A],
attach_attribute(_B,formula(_B,[[_A/[fA(_B)|fA(_C)]]])),
attach_attribute(_C,formula(_C,[[_A/[fA(_B)|fA(_C)]]])),
attach_attribute(_A,formula(_A,[[_A/[fA(_B)|fA(_C)],_A/[]]])) ? ;

X = [0,0,0,_A|_],
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
attach_attribute(_C,formula(_C,[[_C/[(fA(_G),fA(_F))|fA(_H)],_C/[]]])) ? ;

X = [(_A,_B),(_C,_D),(_E,_E)|_],
attach_attribute(_A,formula(_A,[[_A/_B]])),
attach_attribute(_B,formula(_B,[[_A/_B]])),
attach_attribute(_C,formula(_C,[[_C/_D]])),
attach_attribute(_D,formula(_D,[[_C/_D]])) ? 

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

V1 = 1,
V2 = 2,
VY = VX,
attach_attribute(VX,formula(VX,[[VX/3]])) ? ;

no



?- no_q(Z).

no




?- no_r(X,Y).

attach_attribute(X,formula(X,[[X/8]])) ? ;

attach_attribute(Y,formula(Y,[[Y/9]])) ? ;

no




?- no_p1(X).

X = 3 ? ;

no



?-  no_p2(X).

X = 3 ? ;

no



?-  no_p3(X).

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
attach_attribute(_A,formula(_A,[[_A/s(fA(_B)),_A/0]])) ? 

yes



?- no_parent(X,Y).

attach_attribute(X,formula(X,[[X/r,X/d]])) ? ;

attach_attribute(X,formula(X,[[X/d]])),
attach_attribute(Y,formula(Y,[[Y/s]])) ? ;

attach_attribute(Y,formula(Y,[[Y/r]])),
attach_attribute(X,formula(X,[[X/r]])) ? ;

attach_attribute(Y,formula(Y,[[Y/s,Y/r]])) ? ;

no




?- no_grandparent(X,Z).

attach_attribute(X,formula(X,[[X/d]])) ? ;

attach_attribute(Z,formula(Z,[[Z/s]])) ? ;

no





?- no_pred2(X,Y).

attach_attribute(X,formula(X,[[X/9,X/7]])) ? ;

X = 9,
Y = 5 ? ;

no


?- no_pred1(X).

attach_attribute(X,formula(X,[[X/2]])) ? ;

no

