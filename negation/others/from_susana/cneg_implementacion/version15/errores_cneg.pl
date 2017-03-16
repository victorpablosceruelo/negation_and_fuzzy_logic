Sale este warning siempre para todos los predicados pero espero que
cuando haga bien la expansion del paquete deje de salir.

WARNING: Predicate proof_cneg:dist_list/1 undefined in module proof_cneg

&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


Con :

:- module(proof_cneg,_,[.(cneg)]).
  
%:- module(proof_cneg,[odd/1,not_odd_number/1],[cneg]).

:- use_module(dist,[dist/2]). 
...


Me sale:
?-   use_module('/home/susana/tesis/micodigo/ciao/cneg/version4/proof_cneg.pl').
{Compiling /home/susana/tesis/micodigo/ciao/cneg/version4/proof_cneg.pl
WARNING: (lns 101-105) Predicate (:-)/1 undefined in source
WARNING: (lns 101-105) Predicate dist:dist/2 undefined in source
}

yes

Mientras que si comento:
:- module(proof_cneg,_,[.(cneg)]).
  
%:- module(proof_cneg,[odd/1,not_odd_number/1],[cneg]).

%:- use_module(dist,[dist/2]). 

Me sale:
?-   use_module('/home/susana/tesis/micodigo/ciao/cneg/version4/proof_cneg.pl').
{Compiling /home/susana/tesis/micodigo/ciao/cneg/version4/proof_cneg.pl
WARNING: (lns 101-105) Predicate dist:dist/2 undefined in source
}

yes

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      DE  PROOF_CNEG.PL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      DE QUEENSPEANO.PL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


?- queens(s(s(0)),Q).

no
?- queens(s(s(s(s(0)))),Q).

Q = [s(s(0)),s(s(s(s(0)))),s(0),s(s(s(0)))] ? ;

Q = [s(s(s(0))),s(0),s(s(s(s(0)))),s(s(0))] ? ;

no
?- no_queens(s(s(s(s(0)))),[Y2,s(s(s(s(0)))),Y1,s(s(s(0)))]).

Ciao interruption (h for help)? a
{ Execution aborted }
?- no_queens(s(s(s(s(0)))),[s(s(0)),s(s(s(s(0)))),s(0),s(s(s(X)))]).

Ciao interruption (h for help)? t
{The debugger will first creep -- showing everything (trace)}
   1  1  Call: queensPeano:replace_all([],_1668,_233502,_234718) ? 
   1  1  Exit: queensPeano:replace_all([],_1668,_233502,[]) ? 
   2  1  Call: queensPeano:replace_all([],_1668,_233502,_234319) ? s
   2  1  Exit: queensPeano:replace_all([],_1668,_233502,[]) ? 
   3  1  Call: queensPeano:replace_list([_1669,_1670,_1671,_1672,_1673,_1674,_1675,_1676,_1677],[_233504,_233506,_233508,_233510,_233512,_233514,_233516,_233518,_233520],_233556,(_232400,_232401)) ? s
   3  1  Exit: queensPeano:replace_list([_1669,_1670,_1671,_1672,_1673,_1674,_1675,_1676,_1677],[_233504,_233506,_233508,_233510,_233512,_233514,_233516,_233518,_233520],(_232400,_232401),(_232400,_232401)) ? 
   49  1  Call: queensPeano:union(_232416,[_20],_232441) ? s
   49  1  Exit: queensPeano:union([],[_20],[_20]) ? 
   50  1  Call: queensPeano:rm_redundant_eqs([],[_20],_232457) ? s
   50  1  Exit: queensPeano:rm_redundant_eqs([],[_20],[]) ? 
   51  1  Call: queensPeano:negation_formula([],[],_232400,[_20],[greater(s(s(0)),0),add(0,s(0),s(s(0)))],_232401,[_20],[_18092,_18100,_18141,_18159,_18254,_18279,_18319,_18321],_40355) ? s
   51  1  Exit: queensPeano:negation_formula([],[],_232400,[_20],[greater(s(s(0)),0),add(0,s(0),s(s(0)))],_232401,[_20],[_18092,_18100,_18141,_18159,_18254,_18279,_18319,_18321],[cneg(greater(s(s(0)),0))]) ? 
   56  1  Call: queensPeano:negate_conj_frontier([((add(0,s(s(s(0))),s(s(s(0)))),greater(s(s(s(0))),s(0)),add(s(0),s(0),s(s(s(...)))),greater(s(0),0),subst(s(...),s(...),0),queens_list(...),...,...),[greater(s(s(0)),0),add(0,s(0),s(s(0))),select(_18465,[],_18473),add(_18465,s(0),_18639),dist(s(s(...)),_18639),no_attack_down(_18465,s(...),s(...)),add(...)|...]),((add(0,s(s(s(0))),s(s(s(0)))),greater(s(s(s(0))),s(0)),add(s(0),s(0),s(s(s(...)))),greater(s(0),0),subst(s(...),s(...),0),queens_list(...),...,...),[greater(s(s(0)),0),add(0,s(0),s(s(0))),select(_18871,[],_18879),add(s(0),s(0),_19012),dist(_18871,_19012),no_attack_down(s(...),s(...),_18871),add(...)|...]),((add(0,s(s(s(0))),s(s(s(0)))),greater(s(s(s(...))),s(0)),add(s(0),s(0),s(s(...))),greater(s(...),0),subst(...),...,...),[greater(s(s(0)),0),add(0,s(0),s(s(0))),select(_19234,[],_19242),add(s(0),s(0),_19387),dist(_19234,_19387),no_attack_down(...)|...]),((add(0,s(s(s(...))),s(s(s(...)))),greater(s(s(...)),s(0)),add(s(...),s(...),s(...)),greater(...),...,...),[greater(s(s(0)),0),add(0,s(0),s(s(0))),add(s(0),s(0),_19752),dist(s(...),_19752),no_attack_down(...)|...])],(add(0,s(_1668),s(s(s(0)))),greater(s(_1668),s(0)),add(s(0),_1669,s(_1668)),greater(_1669,0),subst(_1669,s(0),_1675),queens_list(_1675,_1670),select(_1671,[_1669|_1670],_1672),select(...),...,...),[_20],[_1668,_1669,_1670,_1671,_1672,_1673,_1674,_1675,_1676|...],_40356) ? s

Ciao interruption (h for help)? a
{ Execution aborted }
?- 
