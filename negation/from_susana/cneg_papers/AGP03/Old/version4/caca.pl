boole(0),
boole(1),

binary_list([]),
binary_list([_B|_A]):-boole(_B),binary_list(_A),

stored_pred(boole(_C),boole(_C)),
stored_pred(boole(_D),boole(_D)),
stored_pred(binary_list(_E),binary_list(_E)),
stored_pred(binary_list(_F),binary_list(_F)),

stored_clause(boole(0),[]),
stored_clause(boole(1),[]),
stored_clause(binary_list([]),[]),
stored_clause(binary_list([_G|_H]),[boole(_G),binary_list(_H)]),
end_of_file]

negate_conj_frontier(
[(dist_list([(_903,_904)|_901]),[dist(_903,_904),dist_list(_901)])],
dist_list(_162),
[_162],
[[dist(_162,[(fA(_903),fA(_904))|fA(_901)])]]) 
99999999999999999999999999999999999999999999999999999999

  225  12  Call: organization_body([dist(X,fA(A)),r(fA(A),Z)],_14996,_14649,_14650)
          [attach_attribute(X,formula(X,[[X/3]]))] ? 
   226  13  Call: varset(X,_16551)
          [attach_attribute(X,formula(X,[[X/3]]))] ? s
   226  13  Exit: varset(X,[X])
          [attach_attribute(X,formula(X,[[X/3]]))] ? 
   227  13  Call: varset(fA(A),_16566) ? s
   227  13  Exit: varset(fA(A),[A]) ? 
   228  13  Call: lists:append([X],[A],_16582)
          [attach_attribute(X,formula(X,[[X/3]]))] ? 
   228  13  Exit: lists:append([X],[A],[X,A])
          [attach_attribute(X,formula(X,[[X/3]]))] ? 
   229  13  Call: remove_duplicates([X,A],_16597)
          [attach_attribute(X,formula(X,[[X/3]]))] ? s
   229  13  Exit: remove_duplicates([X,A],[X,A])
          [attach_attribute(X,formula(X,[[X/3]]))] ? 
   232  13  Call: aggregates:setof([X,A],$:(dist:dist(X,fA(A))),_16613)
          [attach_attribute(X,formula(X,[[X/3]]))] ? s
   232  13  Exit: aggregates:setof([X,A],$:(dist:dist(X,fA(A))),[[_17991,_17976]])
          [attach_attribute(X,formula(X,[[X/3]]))]
          [attach_attribute(_17976,formula(_17976,[[_17991/fA(_17976)]]))]
          [attach_attribute(_17991,formula(_17991,[[_17991/fA(_17976),_17991/3]]))] ? 
   233  13  Call: organization_body_diseq([[_17991,_17976]],[X,A],[r(fA(A),Z)],_14996,_14649,_14650)
          [attach_attribute(X,formula(X,[[X/3]]))]
          [attach_attribute(_17976,formula(_17976,[[_17991/fA(_17976)]]))]
          [attach_attribute(_17991,formula(_17991,[[_17991/fA(_17976),_17991/3]]))] ? 
   234  14  Call: member(_18581,[[_17991,_17976]])
          [attach_attribute(_17976,formula(_17976,[[_17991/fA(_17976)]]))]
          [attach_attribute(_17991,formula(_17991,[[_17991/fA(_17976),_17991/3]]))] ? s
   234  14  Exit: member([_17991,_17976],[[_17991,_17976]])
          [attach_attribute(_17976,formula(_17976,[[_17991/fA(_17976)]]))]
          [attach_attribute(_17991,formula(_17991,[[_17991/fA(_17976),_17991/3]]))] ? 
   235  14  Call: organization_body_diseq_aux([_17991,_17976],[X,A],[r(fA(A),Z)],_14996,_14649,_14650)
          [attach_attribute(X,formula(X,[[X/3]]))]
          [attach_attribute(_17976,formula(_17976,[[_17991/fA(_17976)]]))]
          [attach_attribute(_17991,formula(_17991,[[_17991/fA(_17976),_17991/3]]))] ? 
   236  15  Call: member(_19313,[_17991,_17976])
          [attach_attribute(_17976,formula(_17976,[[_17991/fA(_17976)]]))]
          [attach_attribute(_17991,formula(_17991,[[_17991/fA(_17976),_17991/3]]))] ? s
   236  15  Exit: member(_17991,[_17991,_17976])
          [attach_attribute(_17976,formula(_17976,[[_17991/fA(_17976)]]))]
          [attach_attribute(_17991,formula(_17991,[[_17991/fA(_17976),_17991/3]]))] ? 
   237  15  Call: obtain_diseq(_17991,_19329)
          [attach_attribute(_17991,formula(_17991,[[_17991/fA(_17976),_17991/3]]))] ? s
   237  15  Exit: obtain_diseq(_17991,[_17991/fA(_17976),_17991/3])
          [attach_attribute(_17976,formula(_17976,[[_17991/fA(_17976)]]))]
          [attach_attribute(_17991,formula(_17991,[[_17991/fA(_17976),_17991/3]]))] ? a
