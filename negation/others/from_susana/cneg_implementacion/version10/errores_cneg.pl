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
?- no_queens(s(s(0)),Q).

attach_attribute(Q,formula(Q,[[Q/[s(s(0))]]])) ? ;

yes



?- queens1([s(s(0)),s(0)],[],Qs).

no
?- no_queens1([s(s(0)),s(0)],[],Qs).

no

?- cneg_aux((select(fA(A),[s(0),s(s(0))],fA(B)),no_attack(fA(A),[]),queens1(fA(B),[fA(A)],Q)),[Q]).

no


frontier((select(_318,[s(0),s(s(0))],_393),
	no_attack(_318,[]),
	queens1(_393,[_318],_536)),

	[((select(_7372,[s(0),s(s(0))],[s(0)|_7386]),
	no_attack(_7372,[]),
	queens1([s(0)|_7386],[_7372],_7398)),

	         [select(_7372,[s(s(0))],_7386),
		  no_attack1([],_7372,s(0)),
		  select(_7426,[s(0)|_7386],_7428),
		  no_attack(_7426,[_7372]),
		  queens1(_7428,[_7426,_7372],_7398)]),


	((select(s(0),[s(0),s(s(0))],[s(s(0))]),
	  no_attack(s(0),[]),
	  queens1([s(s(0))],[s(0)],_7761)),

	           [no_attack1([],s(0),s(0)),
		    select(_7777,[s(s(0))],_7779),
		    no_attack(_7777,[s(0)]),
		    queens1(_7779,[_7777,s(0)],_7761)])]) 

%%%%%% Solucion
   1152  2  Call: queensPeano:combine([
[_31=[s(0)|_3268],
 cneg_aux((select(fA(_21),[s(s(0))],_3268),
           no_attack1([],fA(_21),s(0)),
           select(fA(_3308),[s(...)|_3268],fA(_3310)),
           no_attack(fA(...),[...]),
           queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],

[_31=[s(s(0))],
 _21=s(0),
 cneg(no_attack1([],s(0),s(0)))]]) ? s
   %%% Empezamos con la primera
    ?- no_no_attack1([],s(0),s(0)).
    no
    
%%% Luego esta no es solucion.

%%%% Otra solucion

   1426  2  Call: queensPeano:combine([
[_31=[s(0)|_3268],
 cneg_aux((select(fA(_21),[s(s(0))],_3268),
           no_attack1([],fA(_21),s(0)),
           select(fA(_3308),[s(...)|_3268],fA(_3310)),
           no_attack(fA(...),[...]),
           queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],
 [_31=[s(s(0))],
 _21=s(0),
 no_attack1([],s(0),s(0)),
 cneg_aux((select(fA(_3435),[s(...)],fA(_3437)),
 no_attack(fA(...),[...]),
 queens1(fA(...),[...|...],_41)),[_21,_31,_41])]]) ? s


%%% Empezamos con la primera
?- no_no_attack1([],s(0),s(0)).
   1  1  Call: queensPeano:no_no_attack1([],s(0),s(0)) ? s
   1  1  Fail: queensPeano:no_no_attack1([],s(0),s(0)) ? 

no
{trace}



%%% La traza total

?- cneg_aux((select(fA(A),[s(0),s(s(0))],fA(B)),no_attack(fA(A),[]),queens1(fA(B),[fA(A)],Q)),[Q]).
   1  1  Call: queensPeano:cneg_aux((select(fA(_318),[s(0),s(s(0))],fA(_393)),no_attack(fA(_318),[]),queens1(fA(_393),[fA(_318)],_536)),[_536]) ? 
   2  2  Call: queensPeano:rm_fA((select(fA(_318),[s(0),s(s(0))],fA(_393)),no_attack(fA(_318),[]),queens1(fA(_393),[fA(_318)],_536)),_939) ? s
   2  2  Exit: queensPeano:rm_fA((select(fA(_318),[s(0),s(s(0))],fA(_393)),no_attack(fA(_318),[]),queens1(fA(_393),[fA(_318)],_536)),(select(_318,[s(0),s(s(0))],_393),no_attack(_318,[]),queens1(_393,[_318],_536))) ? 
   58  2  Call: metaterms:varset([_536],_954) ? s
   58  2  Exit: metaterms:varset([_536],[_536]) ? 
   59  2  Call: queensPeano:frontier((select(_318,[s(0),s(s(0))],_393),no_attack(_318,[]),queens1(_393,[_318],_536)),_969) ? s
   59  2  Exit: queensPeano:frontier((select(_318,[s(0),s(s(0))],_393),no_attack(_318,[]),queens1(_393,[_318],_536)),[((select(_7372,[s(0),s(s(0))],[s(0)|_7386]),no_attack(_7372,[]),queens1([s(0)|_7386],[_7372],_7398)),[select(_7372,[s(s(0))],_7386),no_attack1([],_7372,s(0)),select(_7426,[s(0)|_7386],_7428),no_attack(_7426,[_7372]),queens1(_7428,[_7426,_7372],_7398)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_7761)),[no_attack1([],s(0),s(0)),select(_7777,[s(s(0))],_7779),no_attack(_7777,[s(0)]),queens1(_7779,[_7777,s(0)],_7761)])]) ? 
   116  2  Call: queensPeano:negate_conj_frontier([((select(_7372,[s(0),s(s(0))],[s(0)|_7386]),no_attack(_7372,[]),queens1([s(0)|_7386],[_7372],_7398)),[select(_7372,[s(s(0))],_7386),no_attack1([],_7372,s(0)),select(_7426,[s(0)|_7386],_7428),no_attack(_7426,[_7372]),queens1(_7428,[_7426,_7372],_7398)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_7761)),[no_attack1([],s(0),s(0)),select(_7777,[s(s(0))],_7779),no_attack(_7777,[s(0)]),queens1(_7779,[_7777,s(0)],_7761)])],(select(_318,[s(0),s(s(0))],_393),no_attack(_318,[]),queens1(_393,[_318],_536)),[_536],_986) ? s
   116  2  Exit: queensPeano:negate_conj_frontier([((select(_318,[s(0),s(s(0))],[s(0)|_7386]),no_attack(_318,[]),queens1([s(0)|_7386],[_318],_536)),[select(_318,[s(s(0))],_7386),no_attack1([],_318,s(0)),select(_7426,[s(0)|_7386],_7428),no_attack(_7426,[_318]),queens1(_7428,[_7426,_318],_536)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_536)),[no_attack1([],s(0),s(0)),select(_7777,[s(s(0))],_7779),no_attack(_7777,[s(0)]),queens1(_7779,[_7777,s(0)],_536)])],(select(_318,[s(0),s(s(0))],_393),no_attack(_318,[]),queens1(_393,[_318],_536)),[_536],[[dist(fA(_393),[s(0)|fA(_7386)])],[dist(fA(_393),[s(s(0))])]]) ? 
   635  2  Call: queensPeano:combine([[dist(fA(_393),[s(0)|fA(_7386)])],[dist(fA(_393),[s(s(0))])]]) ? s
   635  2  Fail: queensPeano:combine([[dist(fA(_393),[s(0)|fA(_7386)])],[dist(fA(_393),[s(s(0))])]]) ? 
   116  2  Redo: queensPeano:negate_conj_frontier([((select(_318,[s(0),s(s(0))],[s(0)|_7386]),no_attack(_318,[]),queens1([s(0)|_7386],[_318],_536)),[select(_318,[s(s(0))],_7386),no_attack1([],_318,s(0)),select(_7426,[s(0)|_7386],_7428),no_attack(_7426,[_318]),queens1(_7428,[_7426,_318],_536)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_536)),[no_attack1([],s(0),s(0)),select(_7777,[s(s(0))],_7779),no_attack(_7777,[s(0)]),queens1(_7779,[_7777,s(0)],_536)])],(select(_318,[s(0),s(s(0))],_393),no_attack(_318,[]),queens1(_393,[_318],_536)),[_536],[[dist(fA(_393),[s(0)|fA(_7386)])],[dist(fA(_393),[s(s(0))])]]) ? s
   116  2  Exit: queensPeano:negate_conj_frontier([((select(_318,[s(0),s(s(0))],[s(0)|_7386]),no_attack(_318,[]),queens1([s(0)|_7386],[_318],_536)),[select(_318,[s(s(0))],_7386),no_attack1([],_318,s(0)),select(_7426,[s(0)|_7386],_7428),no_attack(_7426,[_318]),queens1(_7428,[_7426,_318],_536)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_536)),[no_attack1([],s(0),s(0)),select(_7777,[s(s(0))],_7779),no_attack(_7777,[s(0)]),queens1(_7779,[_7777,s(0)],_536)])],(select(_318,[s(0),s(s(0))],_393),no_attack(_318,[]),queens1(_393,[_318],_536)),[_536],[[dist(fA(_393),[s(0)|fA(_7386)])],[dist(fA(_318),s(0))]]) ? 
   620  2  Call: queensPeano:combine([[dist(fA(_393),[s(0)|fA(_7386)])],[dist(fA(_318),s(0))]]) ? s
   620  2  Fail: queensPeano:combine([[dist(fA(_393),[s(0)|fA(_7386)])],[dist(fA(_318),s(0))]]) ? 
   116  2  Redo: queensPeano:negate_conj_frontier([((select(_318,[s(0),s(s(0))],[s(0)|_7386]),no_attack(_318,[]),queens1([s(0)|_7386],[_318],_536)),[select(_318,[s(s(0))],_7386),no_attack1([],_318,s(0)),select(_7426,[s(0)|_7386],_7428),no_attack(_7426,[_318]),queens1(_7428,[_7426,_318],_536)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_536)),[no_attack1([],s(0),s(0)),select(_7777,[s(s(0))],_7779),no_attack(_7777,[s(0)]),queens1(_7779,[_7777,s(0)],_536)])],(select(_318,[s(0),s(s(0))],_393),no_attack(_318,[]),queens1(_393,[_318],_536)),[_536],[[dist(fA(_393),[s(0)|fA(_7386)])],[dist(fA(_318),s(0))]]) ? s
   116  2  Exit: queensPeano:negate_conj_frontier([((select(_318,[s(0),s(s(0))],[s(0)|_7386]),no_attack(_318,[]),queens1([s(0)|_7386],[_318],_536)),[select(_318,[s(s(0))],_7386),no_attack1([],_318,s(0)),select(_7426,[s(0)|_7386],_7428),no_attack(_7426,[_318]),queens1(_7428,[_7426,_318],_536)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_536)),[no_attack1([],s(0),s(0)),select(_7777,[s(s(0))],_7779),no_attack(_7777,[s(0)]),queens1(_7779,[_7777,s(0)],_536)])],(select(_318,[s(0),s(s(0))],_393),no_attack(_318,[]),queens1(_393,[_318],_536)),[_536],[[dist(fA(_393),[s(0)|fA(_7386)])],[_393=[s(s(0))],_318=s(0),cneg(no_attack1([],s(0),s(0)))]]) ? s
{Skip not applicable at this port, creeping ...}
   603  2  Call: queensPeano:combine([[dist(fA(_393),[s(0)|fA(_7386)])],[_393=[s(s(0))],_318=s(0),cneg(no_attack1([],s(0),s(0)))]]) ? s
   603  2  Fail: queensPeano:combine([[dist(fA(_393),[s(0)|fA(_7386)])],[_393=[s(s(0))],_318=s(0),cneg(no_attack1([],s(0),s(0)))]]) ? 
   116  2  Redo: queensPeano:negate_conj_frontier([((select(_318,[s(0),s(s(0))],[s(0)|_7386]),no_attack(_318,[]),queens1([s(0)|_7386],[_318],_536)),[select(_318,[s(s(0))],_7386),no_attack1([],_318,s(0)),select(_7426,[s(0)|_7386],_7428),no_attack(_7426,[_318]),queens1(_7428,[_7426,_318],_536)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_536)),[no_attack1([],s(0),s(0)),select(_7777,[s(s(0))],_7779),no_attack(_7777,[s(0)]),queens1(_7779,[_7777,s(0)],_536)])],(select(_318,[s(0),s(s(0))],_393),no_attack(_318,[]),queens1(_393,[_318],_536)),[_536],[[dist(fA(_393),[s(0)|fA(_7386)])],[_393=[s(s(0))],_318=s(0),cneg(no_attack1([],s(0),s(0)))]]) ? s
   116  2  Exit: queensPeano:negate_conj_frontier([((select(_318,[s(0),s(s(0))],[s(0)|_7386]),no_attack(_318,[]),queens1([s(0)|_7386],[_318],_536)),[select(_318,[s(s(0))],_7386),no_attack1([],_318,s(0)),select(_7426,[s(0)|_7386],_7428),no_attack(_7426,[_318]),queens1(_7428,[_7426,_318],_536)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_536)),[no_attack1([],s(0),s(0)),select(_7777,[s(s(0))],_7779),no_attack(_7777,[s(0)]),queens1(_7779,[_7777,s(0)],_536)])],(select(_318,[s(0),s(s(0))],_393),no_attack(_318,[]),queens1(_393,[_318],_536)),[_536],[[dist(fA(_393),[s(0)|fA(_7386)])],[_393=[s(s(0))],_318=s(0),no_attack1([],s(0),s(0)),cneg_aux((select(fA(_7777),[s(...)],fA(_7779)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_536)),[_318,_393,_536])]]) ? 
   877  2  Call: queensPeano:combine([[dist(fA(_393),[s(0)|fA(_7386)])],[_393=[s(s(0))],_318=s(0),no_attack1([],s(0),s(0)),cneg_aux((select(fA(_7777),[s(...)],fA(_7779)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_536)),[_318,_393,_536])]]) ? s
   877  2  Fail: queensPeano:combine([[dist(fA(_393),[s(0)|fA(_7386)])],[_393=[s(s(0))],_318=s(0),no_attack1([],s(0),s(0)),cneg_aux((select(fA(_7777),[s(...)],fA(_7779)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_536)),[_318,_393,_536])]]) ? 
   116  2  Redo: queensPeano:negate_conj_frontier([((select(_318,[s(0),s(s(0))],[s(0)|_7386]),no_attack(_318,[]),queens1([s(0)|_7386],[_318],_536)),[select(_318,[s(s(0))],_7386),no_attack1([],_318,s(0)),select(_7426,[s(0)|_7386],_7428),no_attack(_7426,[_318]),queens1(_7428,[_7426,_318],_536)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_536)),[no_attack1([],s(0),s(0)),select(_7777,[s(s(0))],_7779),no_attack(_7777,[s(0)]),queens1(_7779,[_7777,s(0)],_536)])],(select(_318,[s(0),s(s(0))],_393),no_attack(_318,[]),queens1(_393,[_318],_536)),[_536],[[dist(fA(_393),[s(0)|fA(_7386)])],[_393=[s(s(0))],_318=s(0),no_attack1([],s(0),s(0)),cneg_aux((select(fA(_7777),[s(...)],fA(_7779)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_536)),[_318,_393,_536])]]) ? s
   116  2  Exit: queensPeano:negate_conj_frontier([((select(_318,[s(0),s(s(0))],[s(0)|_7386]),no_attack(_318,[]),queens1([s(0)|_7386],[_318],_536)),[select(_318,[s(s(0))],_7386),no_attack1([],_318,s(0)),select(_7426,[s(0)|_7386],_7428),no_attack(_7426,[_318]),queens1(_7428,[_7426,_318],_536)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_536)),[no_attack1([],s(0),s(0)),select(_7777,[s(s(0))],_7779),no_attack(_7777,[s(0)]),queens1(_7779,[_7777,s(0)],_536)])],(select(_318,[s(0),s(s(0))],_393),no_attack(_318,[]),queens1(_393,[_318],_536)),[_536],[[_393=[s(0)|_7386],cneg_aux((select(fA(_318),[s(s(0))],_7386),no_attack1([],fA(_318),s(0)),select(fA(_7426),[s(...)|_7386],fA(_7428)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_536)),[_393,_7386,_536])],[dist(fA(_393),[s(s(0))])]]) ? 
   1184  2  Call: queensPeano:combine([[_393=[s(0)|_7386],cneg_aux((select(fA(_318),[s(s(0))],_7386),no_attack1([],fA(_318),s(0)),select(fA(_7426),[s(...)|_7386],fA(_7428)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_536)),[_393,_7386,_536])],[dist(fA(_393),[s(s(0))])]]) ? s
   1184  2  Fail: queensPeano:combine([[_31=[s(0)|_3268],cneg_aux((select(fA(_21),[s(s(0))],_3268),no_attack1([],fA(_21),s(0)),select(fA(_3308),[s(...)|_3268],fA(_3310)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],[dist(fA(_31),[s(s(0))])]]) ? 
   116  2  Redo: queensPeano:negate_conj_frontier([((select(_21,[s(0),s(s(0))],[s(0)|_3268]),no_attack(_21,[]),queens1([s(0)|_3268],[_21],_41)),[select(_21,[s(s(0))],_3268),no_attack1([],_21,s(0)),select(_3308,[s(0)|_3268],_3310),no_attack(_3308,[_21]),queens1(_3310,[_3308,_21],_41)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_41)),[no_attack1([],s(0),s(0)),select(_3435,[s(s(0))],_3437),no_attack(_3435,[s(0)]),queens1(_3437,[_3435,s(0)],_41)])],(select(_21,[s(0),s(s(0))],_31),no_attack(_21,[]),queens1(_31,[_21],_41)),[_41],[[_31=[s(0)|_3268],cneg_aux((select(fA(_21),[s(s(0))],_3268),no_attack1([],fA(_21),s(0)),select(fA(_3308),[s(...)|_3268],fA(_3310)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],[dist(fA(_31),[s(s(0))])]]) ? s
   116  2  Exit: queensPeano:negate_conj_frontier([((select(_21,[s(0),s(s(0))],[s(0)|_3268]),no_attack(_21,[]),queens1([s(0)|_3268],[_21],_41)),[select(_21,[s(s(0))],_3268),no_attack1([],_21,s(0)),select(_3308,[s(0)|_3268],_3310),no_attack(_3308,[_21]),queens1(_3310,[_3308,_21],_41)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_41)),[no_attack1([],s(0),s(0)),select(_3435,[s(s(0))],_3437),no_attack(_3435,[s(0)]),queens1(_3437,[_3435,s(0)],_41)])],(select(_21,[s(0),s(s(0))],_31),no_attack(_21,[]),queens1(_31,[_21],_41)),[_41],[[_31=[s(0)|_3268],cneg_aux((select(fA(_21),[s(s(0))],_3268),no_attack1([],fA(_21),s(0)),select(fA(_3308),[s(...)|_3268],fA(_3310)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],[dist(fA(_21),s(0))]]) ? 
   1169  2  Call: queensPeano:combine([[_31=[s(0)|_3268],cneg_aux((select(fA(_21),[s(s(0))],_3268),no_attack1([],fA(_21),s(0)),select(fA(_3308),[s(...)|_3268],fA(_3310)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],[dist(fA(_21),s(0))]]) ? s
   1169  2  Fail: queensPeano:combine([[_31=[s(0)|_3268],cneg_aux((select(fA(_21),[s(s(0))],_3268),no_attack1([],fA(_21),s(0)),select(fA(_3308),[s(...)|_3268],fA(_3310)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],[dist(fA(_21),s(0))]]) ? 
   116  2  Redo: queensPeano:negate_conj_frontier([((select(_21,[s(0),s(s(0))],[s(0)|_3268]),no_attack(_21,[]),queens1([s(0)|_3268],[_21],_41)),[select(_21,[s(s(0))],_3268),no_attack1([],_21,s(0)),select(_3308,[s(0)|_3268],_3310),no_attack(_3308,[_21]),queens1(_3310,[_3308,_21],_41)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_41)),[no_attack1([],s(0),s(0)),select(_3435,[s(s(0))],_3437),no_attack(_3435,[s(0)]),queens1(_3437,[_3435,s(0)],_41)])],(select(_21,[s(0),s(s(0))],_31),no_attack(_21,[]),queens1(_31,[_21],_41)),[_41],[[_31=[s(0)|_3268],cneg_aux((select(fA(_21),[s(s(0))],_3268),no_attack1([],fA(_21),s(0)),select(fA(_3308),[s(...)|_3268],fA(_3310)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],[dist(fA(_21),s(0))]]) ? s
   116  2  Exit: queensPeano:negate_conj_frontier([((select(_21,[s(0),s(s(0))],[s(0)|_3268]),no_attack(_21,[]),queens1([s(0)|_3268],[_21],_41)),[select(_21,[s(s(0))],_3268),no_attack1([],_21,s(0)),select(_3308,[s(0)|_3268],_3310),no_attack(_3308,[_21]),queens1(_3310,[_3308,_21],_41)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_41)),[no_attack1([],s(0),s(0)),select(_3435,[s(s(0))],_3437),no_attack(_3435,[s(0)]),queens1(_3437,[_3435,s(0)],_41)])],(select(_21,[s(0),s(s(0))],_31),no_attack(_21,[]),queens1(_31,[_21],_41)),[_41],[[_31=[s(0)|_3268],cneg_aux((select(fA(_21),[s(s(0))],_3268),no_attack1([],fA(_21),s(0)),select(fA(_3308),[s(...)|_3268],fA(_3310)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],[_31=[s(s(0))],_21=s(0),cneg(no_attack1([],s(0),s(0)))]]) ? 
   1152  2  Call: queensPeano:combine([[_31=[s(0)|_3268],cneg_aux((select(fA(_21),[s(s(0))],_3268),no_attack1([],fA(_21),s(0)),select(fA(_3308),[s(...)|_3268],fA(_3310)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],[_31=[s(s(0))],_21=s(0),cneg(no_attack1([],s(0),s(0)))]]) ? s
   1152  2  Fail: queensPeano:combine([[_31=[s(0)|_3268],cneg_aux((select(fA(_21),[s(s(0))],_3268),no_attack1([],fA(_21),s(0)),select(fA(_3308),[s(...)|_3268],fA(_3310)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],[_31=[s(s(0))],_21=s(0),cneg(no_attack1([],s(0),s(0)))]]) ? 
   116  2  Redo: queensPeano:negate_conj_frontier([((select(_21,[s(0),s(s(0))],[s(0)|_3268]),no_attack(_21,[]),queens1([s(0)|_3268],[_21],_41)),[select(_21,[s(s(0))],_3268),no_attack1([],_21,s(0)),select(_3308,[s(0)|_3268],_3310),no_attack(_3308,[_21]),queens1(_3310,[_3308,_21],_41)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_41)),[no_attack1([],s(0),s(0)),select(_3435,[s(s(0))],_3437),no_attack(_3435,[s(0)]),queens1(_3437,[_3435,s(0)],_41)])],(select(_21,[s(0),s(s(0))],_31),no_attack(_21,[]),queens1(_31,[_21],_41)),[_41],[[_31=[s(0)|_3268],cneg_aux((select(fA(_21),[s(s(0))],_3268),no_attack1([],fA(_21),s(0)),select(fA(_3308),[s(...)|_3268],fA(_3310)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],[_31=[s(s(0))],_21=s(0),cneg(no_attack1([],s(0),s(0)))]]) ? s
   116  2  Exit: queensPeano:negate_conj_frontier([((select(_21,[s(0),s(s(0))],[s(0)|_3268]),no_attack(_21,[]),queens1([s(0)|_3268],[_21],_41)),[select(_21,[s(s(0))],_3268),no_attack1([],_21,s(0)),select(_3308,[s(0)|_3268],_3310),no_attack(_3308,[_21]),queens1(_3310,[_3308,_21],_41)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_41)),[no_attack1([],s(0),s(0)),select(_3435,[s(s(0))],_3437),no_attack(_3435,[s(0)]),queens1(_3437,[_3435,s(0)],_41)])],(select(_21,[s(0),s(s(0))],_31),no_attack(_21,[]),queens1(_31,[_21],_41)),[_41],[[_31=[s(0)|_3268],cneg_aux((select(fA(_21),[s(s(0))],_3268),no_attack1([],fA(_21),s(0)),select(fA(_3308),[s(...)|_3268],fA(_3310)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],[_31=[s(s(0))],_21=s(0),no_attack1([],s(0),s(0)),cneg_aux((select(fA(_3435),[s(...)],fA(_3437)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_21,_31,_41])]]) ? 
   1426  2  Call: queensPeano:combine([[_31=[s(0)|_3268],cneg_aux((select(fA(_21),[s(s(0))],_3268),no_attack1([],fA(_21),s(0)),select(fA(_3308),[s(...)|_3268],fA(_3310)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],[_31=[s(s(0))],_21=s(0),no_attack1([],s(0),s(0)),cneg_aux((select(fA(_3435),[s(...)],fA(_3437)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_21,_31,_41])]]) ? s
   1426  2  Fail: queensPeano:combine([[_31=[s(0)|_3268],cneg_aux((select(fA(_21),[s(s(0))],_3268),no_attack1([],fA(_21),s(0)),select(fA(_3308),[s(...)|_3268],fA(_3310)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],[_31=[s(s(0))],_21=s(0),no_attack1([],s(0),s(0)),cneg_aux((select(fA(_3435),[s(...)],fA(_3437)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_21,_31,_41])]]) ? 
   116  2  Redo: queensPeano:negate_conj_frontier([((select(_21,[s(0),s(s(0))],[s(0)|_3268]),no_attack(_21,[]),queens1([s(0)|_3268],[_21],_41)),[select(_21,[s(s(0))],_3268),no_attack1([],_21,s(0)),select(_3308,[s(0)|_3268],_3310),no_attack(_3308,[_21]),queens1(_3310,[_3308,_21],_41)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_41)),[no_attack1([],s(0),s(0)),select(_3435,[s(s(0))],_3437),no_attack(_3435,[s(0)]),queens1(_3437,[_3435,s(0)],_41)])],(select(_21,[s(0),s(s(0))],_31),no_attack(_21,[]),queens1(_31,[_21],_41)),[_41],[[_31=[s(0)|_3268],cneg_aux((select(fA(_21),[s(s(0))],_3268),no_attack1([],fA(_21),s(0)),select(fA(_3308),[s(...)|_3268],fA(_3310)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_31,_3268,_41])],[_31=[s(s(0))],_21=s(0),no_attack1([],s(0),s(0)),cneg_aux((select(fA(_3435),[s(...)],fA(_3437)),no_attack(fA(...),[...]),queens1(fA(...),[...|...],_41)),[_21,_31,_41])]]) ? s
   116  2  Fail: queensPeano:negate_conj_frontier([((select(_3254,[s(0),s(s(0))],[s(0)|_3268]),no_attack(_3254,[]),queens1([s(0)|_3268],[_3254],_3280)),[select(_3254,[s(s(0))],_3268),no_attack1([],_3254,s(0)),select(_3308,[s(0)|_3268],_3310),no_attack(_3308,[_3254]),queens1(_3310,[_3308,_3254],_3280)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_3419)),[no_attack1([],s(0),s(0)),select(_3435,[s(s(0))],_3437),no_attack(_3435,[s(0)]),queens1(_3437,[_3435,s(0)],_3419)])],(select(_21,[s(0),s(s(0))],_31),no_attack(_21,[]),queens1(_31,[_21],_41)),[_41],_163) ? 
   59  2  Redo: queensPeano:frontier((select(_21,[s(0),s(s(0))],_31),no_attack(_21,[]),queens1(_31,[_21],_41)),[((select(_3254,[s(0),s(s(0))],[s(0)|_3268]),no_attack(_3254,[]),queens1([s(0)|_3268],[_3254],_3280)),[select(_3254,[s(s(0))],_3268),no_attack1([],_3254,s(0)),select(_3308,[s(0)|_3268],_3310),no_attack(_3308,[_3254]),queens1(_3310,[_3308,_3254],_3280)]),((select(s(0),[s(0),s(s(0))],[s(s(0))]),no_attack(s(0),[]),queens1([s(s(0))],[s(0)],_3419)),[no_attack1([],s(0),s(0)),select(_3435,[s(s(0))],_3437),no_attack(_3435,[s(0)]),queens1(_3437,[_3435,s(0)],_3419)])]) ? s
   59  2  Fail: queensPeano:frontier((select(_21,[s(0),s(s(0))],_31),no_attack(_21,[]),queens1(_31,[_21],_41)),_146) ? 
   58  2  Redo: metaterms:varset([_41],[_41]) ? s
   58  2  Fail: metaterms:varset([_41],_131) ? 
   2  2  Redo: queensPeano:rm_fA((select(fA(_21),[s(0),s(s(0))],fA(_31)),no_attack(fA(_21),[]),queens1(fA(_31),[fA(_21)],_41)),(select(_21,[s(0),s(s(0))],_31),no_attack(_21,[]),queens1(_31,[_21],_41))) ? s
   2  2  Fail: queensPeano:rm_fA((select(fA(_21),[s(0),s(s(0))],fA(_31)),no_attack(fA(_21),[]),queens1(fA(_31),[fA(_21)],_41)),_116) ? 
   1  1  Fail: queensPeano:cneg_aux((select(fA(_21),[s(0),s(s(0))],fA(_31)),no_attack(fA(_21),[]),queens1(fA(_31),[fA(_21)],_41)),[_41]) ? 

no
{trace}


?- queens1([s(s(0)),s(0)],[],Qs).

no

?-cneg_aux((select11(fA(A),[s(s(0)),s(0)],fA(B)),
	    no_attack11(fA(A),[]),
	    queens11(fA(B),[fA(A)],Q)),[Qs])
      
no
?- cneg_aux((select11(fA(A),[s(s(0)),s(0)],fA(B)),
	    queens11(fA(B),[fA(A)],Q)),[Qs]).
   
no
?- cneg_aux((select11(s(0),[s(s(0)),s(0)],[s(s(0))]),
	    queens11([s(s(0))],[s(0)],Q)),[Qs]).
   
yes
?- cneg_aux((select11(s(s(0)),[s(s(0)),s(0)],[s(0)]),
	    queens11([s(0)],[s(s(0))],Q)),[Qs]).
   
yes
?- cneg_aux((select11(A,[s(s(0)),s(0)],B),
	    queens11(B,[A],Q)),[Qs]).
   
no



negate_conj_frontier([
((select11(s(0),[s(s(0)),s(0)],[s(s(0))]),queens11([s(s(0))],[s(0)],_4901)),

          [select11(_4909,[s(s(0))],_4911),
	   no_attack11(_4909,[s(0)]),
	   queens11(_4911,[_4909,s(0)],_4901)]),

((select11(s(s(0)),[s(s(0)),s(0)],[s(0)]),queens11([s(0)],[s(s(0))],_5232)),

          [select11(_5240,[s(0)],_5242),
	   no_attack11(_5240,[s(s(0))]),
	   queens11(_5242,[_5240,s(s(0))],_5232)])],

(select11(_178,[s(s(0)),s(0)],_241),queens11(_241,[_178],_306)),
[_329],_685)

 
negate_conj_frontier([
((select11(s(0),[s(s(0)),s(0)],[s(s(0))]),queens11([s(s(0))],[s(0)],_306)),

	[select11(_4909,[s(s(0))],_4911),
	 no_attack11(_4909,[s(0)]),
	 queens11(_4911,[_4909,s(0)],_306)]),

((select11(s(s(0)),[s(s(0)),s(0)],[s(0)]),queens11([s(0)],[s(s(0))],_306)),

	[select11(_5240,[s(0)],_5242),
	 no_attack11(_5240,[s(s(0))]),
	 queens11(_5242,[_5240,s(s(0))],_306)])],

(select11(_178,[s(s(0)),s(0)],_241),queens11(_241,[_178],_306)),
[_329],

%% Soluciones que fallan:
[[dist(fA(_241),[s(s(0))])],[dist(fA(_241),[s(0)])]])
[[dist(fA(_241),[s(s(0))])],[dist(fA(_178),s(s(0)))]]) 
[[dist(fA(_241),[s(s(0))])],[_241=[s(0)],_178=s(s(0)),cneg_aux((select11(fA(_5240),[s(0)],fA(_5242)),no_attack11(fA(_5240),[s(...)]),queens11(fA(_5242),[fA(...),s(...)],fA(_306))),[_178,_241,_329])]])
[[dist(fA(_178),s(0))],[dist(fA(_241),[s(0)])]])
[[dist(fA(_178),s(0))],[dist(fA(_178),s(s(0)))]])
[[dist(fA(_178),s(0))],[_241=[s(0)],_178=s(s(0)),cneg_aux((select11(fA(_5240),[s(0)],fA(_5242)),no_attack11(fA(_5240),[s(...)]),queens11(fA(_5242),[fA(...),s(...)],fA(_306))),[_178,_241,_329])]])

[[_241=[s(s(0))],
  _178=s(0),
  cneg_aux((select11(fA(_4909),[s(s(...))],fA(_4911)),
            no_attack11(fA(_4909),[s(...)]),
            queens11(fA(_4911),[fA(...),s(...)],fA(_306))),[_178,_241,_329])],
 [dist(fA(_241),[s(0)])]]) 
[[_241=[s(s(0))],_178=s(0),cneg_aux((select11(fA(_4909),[s(s(...))],fA(_4911)),no_attack11(fA(_4909),[s(...)]),queens11(fA(_4911),[fA(...),s(...)],fA(_306))),[_178,_241,_329])],[dist(fA(_178),s(s(0)))]])

%% Solucion que falla y deberia tener exito
[[_241=[s(s(0))],
  _178=s(0),
  cneg_aux((select11(fA(_4909),[s(s(...))],fA(_4911)),
            no_attack11(fA(_4909),[s(...)]),
            queens11(fA(_4911),[fA(...),s(...)],fA(_306))),[_178,_241,_329])],

 [_241=[s(0)],
  _178=s(s(0)),
  cneg_aux((select11(fA(_5240),[s(0)],fA(_5242)),
            no_attack11(fA(_5240),[s(...)]),
            queens11(fA(_5242),[fA(...),s(...)],fA(_306))),[_178,_241,_329])]
])

?- no_queens([],Qs).

no
?- queens(0,Qs).

Qs = [] ? ;

no
?- no_queens(0,Qs).

no



?-  queens1([0],[],Qs).

Qs = [0] ? ;

no
?-  no_queens1([0],[],Qs).

no    %%%%%%% Mal
%% Y sin embargo da bien:
?-  queens1([],[],Qs).

Qs = [] ? ;

no
?-  no_queens1([],[],Qs).

attach_attribute(Qs,formula(Qs,[[Qs/[]]])) ? ;

no
