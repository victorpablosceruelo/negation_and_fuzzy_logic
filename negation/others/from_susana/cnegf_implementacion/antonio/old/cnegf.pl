:- module(cnegf,_).

:- meta_predicate cnegf(goal).

:- data variables/1.

:- use_module(dist,[dist/2,put_attribute/2, actualizar_formula/1,eliminar_repetidas/2, sustituir/4]).
:- use_module(library(lists),[append/3,delete/3,list_insert/2,union/3]).
:- use_module(library(aggregates),[setof/3]).
:- use_module(library(metaterms),[varset/2]).
:- use_module(library(idlists),[memberchk/2]). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    NEGACION DE PREDICADOS CON UN CONJUNTO DE SOLUCIONES FINITO%
%    Negation of predicates with a finite number of solutions   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 

% cnegf(Pred) tiene exito si el predicado Pred falla y 
% ofrece las soluciones posibles en caso de haberlas
% utiliza negacion constructiva negando las soluciones
% del objetivo Pred
%
% cnegf(Pred) is successful if Pred fails and It offers the posible solutions,
% if there are solutions, using the Constructive Negation and negating the 
% solutions of the objetive Pred.

cnegf(Pred):-
        varset(Pred,GVars),
	setof(GVars,Pred,LValores),!,
	obtener_igualdades_de_GoalVars(GVars,LValores,Lista_de_GoalVars_igualadas_aux),
	convertir(Lista_de_GoalVars_igualadas_aux,[],Lista_de_GoalVars_igualadas),
	organizar_conj(LValores,GVars,Lista_a_transformar),
	normalizar(Lista_a_transformar,GVars,LATN),
	eliminar_ig_inutiles(LATN,GVars,LATNF),
        combine_sols(LATNF,Lista_soluciones_negada,GVars,[],[],_LGVars,LNoGVars),
% Este convertir se hace porque la Lista_soluciones_negada tiene elementos simples y otras
% listas, convertir lo unifica todo en una unica lista
	convertir(Lista_soluciones_negada,[],Lista_a_evaluar),
	comprobar_sol_valida(GVars,Lista_a_evaluar),
	
	append(Lista_a_evaluar,Lista_de_GoalVars_igualadas,Lista_a_evaluar_final),
	obtener_Vars_NoDetach(Lista_a_evaluar_final,NoGVarsDetach_aux),
	convertir(NoGVarsDetach_aux,[],NoGVarsDetach_final),
	obtener_Vars_Detach(Lista_a_evaluar_final,NoGVarsDetach_final,NoGVars_final),
        detach_attribute_all(NoGVars_final),
	eliminar_igualdades_inutiles(Lista_a_evaluar_final,LNoGVars,Lista_a_evaluar_final_yes),
% Lista_a_evaluar_final contiene la lista final de soluciones incluidas las igualdades entre
%	Gvars y No_GVars que mantienen la coherencia. De todas formas los atributos de las
%	No_GVars son peligrosos asi que los eliminamos.
	evaluate_neg(Lista_a_evaluar_final_yes).
	

cnegf(_Pred). % Si no ha podido hacer setof es que no hay soluciones

%obtener_igualdades_de_GoalVars(GVars,LValores,Lista_de_GoalVars_igualadas) Mete en 
%           Lista_GoalVars_igualadas todas las igualdades que devuelve el setof, siempre que de
%           la igualdad X=Y la Y sea var. Se hace para que al final del todo se una a la
%           lista solucion negada para que todas las GVars se igualen a las No_GVars.
%           Es necesario porque la negacion del dist de una Goalvar puede implicar a otras GVars.
%           La solucion es una lista de listas, hay que convertirla a una unica lista.
%
%obtener_igualdades_de_GoalVars(GVars,LValores,Lista_de_GoalVars_igualadas) Put into 
%           Lista_GoalVars_Igualadas all the equalities returned by setof, if from the equality
%           X=Y Y is var. This is neccesary because in the end of the predicate, the list of
%           negate solutions has to contain all GVars equaled to No_GVars.
%           The negation of dist(GoalVar) can involve others GVars.
%           The solution is a list of lists which we have to turn into a one list.

obtener_igualdades_de_GoalVars(_GVars,[],[]).
obtener_igualdades_de_GoalVars(GVars,[Conj|RConj],[ConjSol|RConjSol]):-
	obtener_igualdades_aux(GVars,Conj,ConjSol),
	obtener_igualdades_de_GoalVars(GVars,RConj,RConjSol).

obtener_igualdades_aux([],[],[]).
obtener_igualdades_aux([PGV|RGV],[P|R],[(PGV=P)|RSol]):-
	var(P),!,
	obtener_igualdades_aux(RGV,R,RSol).
obtener_igualdades_aux([_PGV|RGV],[_P|R],RSol):-
	obtener_igualdades_aux(RGV,R,RSol).
	


% organizar_conj(LValores,Vars,Lista_sol), LValores es una lista de listas. En Lista_sol
% se devuelve otra lista de listas igual que LValores pero conteniendo igualdades con
% las variables de Vars. EJ organizar_conj([[1,2],[V,6]],[X,Y],[[X=1,Y=2],[X=V,Y=6]]
%
% organizar_conj(LValores,Vars,Lista_sol), LValores is a list of lists. In Lista_sol we have
% another list of lists that is the same than LValores but this contains equalities with   
% the variables of Vars. EJ organizar_conj([[1,2],[V,6]],[X,Y],[[X=1,Y=2],[X=V,Y=6]]

organizar_conj([],_GVars,[]).
organizar_conj([P|Resto],GVars,[SolP|SolRest]):-
	organizar_basico(P,GVars,GVars,SolP),
	organizar_conj(Resto,GVars,SolRest).

organizar_basico([],[],_,[]).
organizar_basico([Elto|Rest_Eltos],[V1|Rvars],GVars,[(V1=Elto)|SolRest]):-
	organizar_basico(Rest_Eltos,Rvars,GVars,SolRest).


% normalizar(ListaC,GV,Sol), realiza todas las operaciones de normalizacion.
%
% normalizar(ListaC,GV,Sol), does all the normalization tasks.

normalizar([],_Gv,[]).
normalizar([C1|Lista],Gv,[CSol|ListaSol]):-
	normalizar_ig(C1,Gv,CSol),
	normalizar(Lista,Gv,ListaSol).

normalizar_ig(C1,Gv,CSol):-
	varset(C1,Vars),
	difference(Vars,Gv,FreeVarsaux),
	minimizar(FreeVarsaux,C1,[],[],FreeVars),
        buscar_igualdades(C1,FreeVars,Gv,[],CSol,si). %el si es el Flag_igualdad

% minimizar(FV,Lista,Sol_aux,Inicio,Sol), obtiene en Sol una lista de variables de FV que
%   aparecen en Lista dos o mas veces en la forma X=_FV siendo X GV y _FV la variable a 
%   aparecer en la lista solucion.
%
% minimizar(FV,Lista,Sol_aux,Inicio,Sol), Sol is the solution which contains a lsit of
%   variables from FV that appear in Lista twice or more times with the form X=_FV 
%   belonging X to GV and _FV is the variable which has to appear in Sol.
	
minimizar(_FVA,[],_SA,Inicio,Sol_Final):- 
	Sol_Final=Inicio.
minimizar(FVA,[(_X=Y)|RC],Sol_aux,Inicio,_Sol_Final):-
	memberchk(Y,FVA),
	\+memberchk(Y,Sol_aux),!,
	append(Sol_aux,[Y],Sol_aux2),
	minimizar(FVA,RC,Sol_aux2,Inicio,_Sol_Final).
minimizar(FVA,[(_X=Y)|RC],Sol_aux,Inicio,_Sol_Final):-
	memberchk(Y,FVA),
	memberchk(Y,Sol_aux),!,
	append(Inicio,[Y],Sol_Final),
	minimizar(FVA,RC,Sol_aux,Sol_Final,_Sol_Final).
minimizar(FVA,[(_X=Y)|RC],Sol_aux,Inicio,_Sol_Final):-
	\+memberchk(Y,FVA),
	minimizar(FVA,RC,Sol_aux,Inicio,_Sol_Final).

	
% buscar_igualdades(Lista1,FV,GV,aux,Sol), analiza la Lista1 sustituye igualdades del tipo
%   (X=_A,Y=_A) por (X=Y), deja X=non_var(Y), deja X=Y ambas GV y deja X=_F si _F tiene atributo 
%
% buscar_igualdades(Lista1,FV,GV,aux,Sol), analises Lista1 substitutes the sort of equalities
%   like (X=_A,Y=_A) for (X=Y), remains X=non_var(Y), remains X=Y both GV and remains X=_F if
%    _F has attribute
	
buscar_igualdades([],_Fv,_Gv,Sol_aux,Sol_Final,_):- Sol_Final=Sol_aux.
buscar_igualdades([(X=Y)|RestoI],FreeVars,Gv,Sol_aux,_Sol_Final,Flag_igualdad):-
	\+var(Y),!,
	append(Sol_aux,[(X=Y)],RSol),
	buscar_igualdades(RestoI,FreeVars,Gv,RSol,_Sol_Final,Flag_igualdad).
buscar_igualdades([(X=Y)|RestoI],FreeVars,Gv,Sol_aux,_Sol_Final,Flag_igualdad):-
	var(Y),
	\+memberchk(Y,FreeVars),!,
	append(Sol_aux,[(X=Y)],RSol),
	buscar_igualdades(RestoI,FreeVars,Gv,RSol,_Sol_Final,Flag_igualdad).
buscar_igualdades([(X=Y)|RestoI],FreeVars,Gv,Sol_aux,_Sol_Final,Flag_igualdad):-
	var(Y),
	memberchk(Y,FreeVars),
	\+get_attribute(Y,_F),!,
	refinar(Y,[(X=Y)|RestoI],Gv,PSol),
	append(Sol_aux,PSol,RSol),
	buscar_igualdades(RestoI,FreeVars,Gv,RSol,_Sol_Final,Flag_igualdad).
buscar_igualdades([(X=Y)|RestoI],FreeVars,Gv,Sol_aux,_Sol_Final,Flag_igualdad):-
	Flag_igualdad=si,
        var(Y),
	memberchk(Y,FreeVars),
	get_attribute(Y,_F),!,
	refinar(Y,[(X=Y)|RestoI],Gv,PSol),
	append(Sol_aux,PSol,RSolaux),
	append(RSolaux,[(X=Y)],RSol),
	buscar_igualdades(RestoI,FreeVars,Gv,RSol,_Sol_Final,no).
buscar_igualdades([(X=Y)|RestoI],FreeVars,Gv,Sol_aux,_Sol_Final,Flag_igualdad):-
	var(Y),
	memberchk(Y,FreeVars),
	get_attribute(Y,_F),!,
	refinar(Y,[(X=Y)|RestoI],Gv,PSol),
	append(Sol_aux,PSol,RSol),
	buscar_igualdades(RestoI,FreeVars,Gv,RSol,_Sol_Final,Flag_igualdad).


% refinar(FV,Lista,GV,Sol), iguala todas las varibles de GV que tengan alguna igualdad en la
%    lista Lista con la variable FV. EJ: (_A,[X=_A,Y=_B,Z=_A],[X,Y,Z], [X=Z]).
%
% refinar(FV,Lista,GV,Sol), equalizes all the variables from GV which have any equialities
%    in Lista with the variable FV. EX: (_A,[X=_A,Y=_B,Z=_A],[X,Y,Z], [X=Z]).

refinar(FV,Lista,Gv,Sol):-
	obtenerLista_V(FV,Lista,Gv,Lista_V),
	igualar_gv(Lista_V,Lista_V,[],Sol).


% obtenerLista_V(FV,Lista,GV,Sol), obtiene todas las variables de GV que tienen alguna igualdad
%     en la Lista con la varible FV. 
%
% obtenerLista_V(FV,Lista,GV,Sol), obtains all the variables form GV which have any equalities
%     in Lista with the variable FV.

obtenerLista_V(_FV,[],_Gv,[]).

obtenerLista_V(FV,[(_X=Y)|R],Gv,RSol):-
	\+Y==FV,!,
	obtenerLista_V(FV,R,Gv,RSol).

obtenerLista_V(FV,[(X=Y)|R],Gv,[X|RSol]):-
	Y==FV,!,
	memberchk(X,Gv),
	obtenerLista_V(FV,R,Gv,RSol).

eliminar_ig_inutiles([],_,[]).
eliminar_ig_inutiles([P|R],GVars,[Sol_Parcial|Sol]):-
	eii_aux(P,GVars,Sol_Parcial),
	eliminar_ig_inutiles(R,GVars,Sol).

eii_aux([],_,[]).
eii_aux([(_X=Y)|R],GVars,Sol):-
	var(Y),
	\+memberchk(Y,GVars),
	\+get_attribute(Y,_F),!,
	eii_aux(R,GVars,Sol).
eii_aux([(X=Y)|R],GVars,[(X=Y)|Sol]):-
	eii_aux(R,GVars,Sol).
	

% combine_sols(Lista_soluciones,Lista_soluciones_negada), Lista_soluciones_negada es una 
%             lista de listas que contiene la Lista_soluciones, negada, es decir, cambiando 
%             los = por / y las conjunciones por disyunciones
%
% combine_sols(Lista_soluciones,Lista_soluciones_negada), Lista_soluciones_negada is a
%             list of lists which contains Lista_soluciones, negated, it means changing the
%             caracter = for / and all the conjuctions for disjunctions.

combine_sols([],[],_GVars,L_GVars_aux, L_NoGVars_aux,L_GVars_Sol,L_NoGVars_Sol):-
	L_GVars_Sol=L_GVars_aux,
	L_NoGVars_Sol=L_NoGVars_aux.
combine_sols([Sol|LSols],[NSol|NLSols],GVars,L_GVars_aux,L_NoGVars_aux,L_GVars_Sol,L_NoGVars_Sol):-
	negate(Sol,NSol,GVars,Lista_GVars_Par,Lista_NoGVars_Par),
	union(Lista_GVars_Par,L_GVars_aux,L_GVars_aux,LGVars),
        union(Lista_NoGVars_Par,L_NoGVars_aux,L_NoGVars_aux,LNoGVars),
	combine_sols(LSols,NLSols,GVars,LGVars,LNoGVars,L_GVars_Sol,L_NoGVars_Sol).

% negate(Sol,Neg, Negate va cogiendo una igualdad de cada solucion para conseguir hacer 
%                  el disy_to_conj pero con backtracking, para ello utiliza el member/3. Cada 
%                  igualdad la niega creando asi una de las soluciones de la negacion.
%
% negate(Sol,Neg), Negate take one equality from each solution to get the same result that
%                  disy_to_conj but using backtracking, for that it uses member/3. Each
%                  equality is negated creating one of the final negation solution.

negate(Sol,Neg,GVars,Lista_GVars,Lista_NoGVars):-
	member(Eq,Sol),
	negate_analize(Eq,Neg,GVars,Lista_GVars,Lista_NoGVars).
        

% negate_analize(Eq,Neg), Cada igualdad X=Y puede tener cuatro casos. X e Y no tienen 
%   atributo. X tiene at pero Y no. Y tiene at pero X no. Ambas tiene atributo. negate_analize 
%   trata estos 4 casos. 
%
% negate_analize(Eq,Neg), Each equality X=Y can be managed with four steps. X and Y dont have 
%   attribute. X has at but not Y. Y has at but not X. Both of them have at. negate_annalize
%   takes in account those four steps.

negate_analize(X=Y,Neg,GVars,Lista_GVars,_Lista_NoGVars):-
	\+get_attribute(X,formula(X,_AttributeX)),
	\+get_attribute(Y,formula(Y,_AttributeY)),
	var(Y),
	memberchk(Y,GVars),!,
	Lista_GVars=[X,Y],
	Neg=(dist(X,Y)).
negate_analize(X=Y,Neg,GVars,Lista_GVars,Lista_NoGVars):-
	\+get_attribute(X,formula(X,_AttributeX)),
	\+get_attribute(Y,formula(Y,_AttributeY)),
	var(Y),
	\+memberchk(Y,GVars),!,
	Lista_GVars=[X],
	Lista_NoGVars=[Y],
	Neg=(X=Y).
negate_analize(X=Y,Neg,GVars,Lista_GVars,Lista_NoGVars):-
	\+get_attribute(X,formula(X,_AttributeX)),
	\+get_attribute(Y,formula(Y,_AttributeY)),
	\+var(Y),!,
	varset((X=Y),Vars),
	intersection(Vars,GVars,[],LGVars_aux),
	Lista_GVars=LGVars_aux,
	difference(Vars,GVars,LNoGVars_aux),
	Lista_NoGVars=LNoGVars_aux,
	Neg=(dist(X,Y)).
negate_analize(X=Y,Neg,GVars,Lista_GVars,Lista_NoGVars):-
	get_attribute(X,formula(X,AttributeX)),
	\+get_attribute(Y,formula(Y,_AttributeY)),!,
	negate_at(AttributeX,Neg,GVars,[],[],LGVarsY,LNoGVarsY),
	Lista_GVars=LGVarsY,
	Lista_NoGVars=LNoGVarsY,
	detach_attribute(X).
negate_analize(X=Y,Neg,GVars,Lista_GVars,Lista_NoGVars):-
	get_attribute(Y,formula(Y,AttributeY)),
	\+get_attribute(X,formula(X,_AttributeX)),!,
	negate_at(AttributeY,Neg_aux,GVars,[],[],LGVarsY,LNoGVarsY),
	append(Neg_aux,[(X=Y)],Neg),
	Lista_GVars=LGVarsY,
	Lista_NoGVars=LNoGVarsY.
negate_analize(X=Y,Neg,GVars,Lista_GVars,Lista_NoGVars):-
	get_attribute(X,formula(X,AttributeX)),
	get_attribute(Y,formula(Y,AttributeY)),!,
	negate_at(AttributeX,NegX,GVars,[],[],LGVarsX,LNoGVarsX),
	negate_at(AttributeY,NegY,GVars,[],[],LGVarsY,LNoGVarsY),
	append(NegX,NegY,Neg_aux),
	append(Neg_aux,[(X=Y)],Neg),
	union(LGVarsX,LGVarsY,LGVarsY,LGVars),
        union(LNoGVarsX,LNoGVarsY,LNoGVarsY,LNoGVars),
	Lista_GVars=LGVars,
        Lista_NoGVars=LNoGVars,
	detach_attribute(Y),
	detach_attribute(X).

% negate_at(Lista_eq,Lista_Sol), Al igual que negate va cogiendo miembro a mienbro de cada 
%     conjuncion para hacer el disy_to_conj pero con BT. Cada miembro es una desigualdad que 
%     afirma.
%
% negate_at(Lista_eq,Lista_Sol), its function is like negate. Each member form the lists is
%     a disequality which is affirmmed.

negate_at([],[],_GVars,Lista_GVars_aux,Lista_NoGVars_aux,LGVars_Sol,LNoGVars_Sol):-
	LGVars_Sol=Lista_GVars_aux,
	LNoGVars_Sol=Lista_NoGVars_aux.
negate_at([Conj|Disy],[NSol|NResto],GVars,Lista_GVars_aux,Lista_NoGVars_aux,LGVars_Sol,LNoGVars_Sol):-
	negate_conj(Conj,NSol,GVars,[],[],LGVars_Par,LNoGVars_Par),
	union(LGVars_Par,Lista_GVars_aux,Lista_GVars_aux,LGVars),
	union(LNoGVars_Par,Lista_NoGVars_aux,Lista_NoGVars_aux,LNoGVars),
	negate_at(Disy,NResto,GVars,LGVars,LNoGVars,LGVars_Sol,LNoGVars_Sol).

% negate_conj(Conj,Sol), Transforma una desigualdad X/Y, perteneciente a Conj, en X=Y. Se utiliza
%      member/3 para aprovechar el BT.
%
% negate_conj(Conj,Sol), Turns a disequiality X/Y, which belongs to Conj, into X=Y. member/3 is 
%      used to take adventage of BT.

negate_conj(Conj,X=Y,GVars,LGVars,LNoGVars,LGVars_Sol,LNoGVars):-
	member(X/Y,Conj),
	var(Y),
	memberchk(Y,GVars),
	memberchk(X,GVars),
	append(LGVars,[X],LGVars_aux),
	append(LGVars_aux,[Y],LGVars_aux2),
	LGVars_Sol=LGVars_aux2.
negate_conj(Conj,X=Y,GVars,LGVars,LNoGVars,[Y|LGVars],[X|LNoGVars]):-
	member(X/Y,Conj),
	var(Y),
	memberchk(Y,GVars),
	\+memberchk(X,GVars).
negate_conj(Conj,X=Y,GVars,LGVars,LNoGVars,[X|LGVars],[Y|LNoGVars]):-
	member(X/Y,Conj),
	var(Y),
	memberchk(X,GVars),
	\+memberchk(Y,GVars).
negate_conj(Conj,X=Y,GVars,LGVars,LNoGVars,LGVars,LNoGVars_Sol):-
	member(X/Y,Conj),
	var(Y),
	append(LNoGVars,[X],LNoGVars_aux),
	append(LNoGVars_aux,[Y],LNoGVars_aux2),
	LNoGVars_Sol=LNoGVars_aux2,
	\+memberchk(X,GVars).
negate_conj(Conj,X=Y,GVars,LGVars,LNoGVars,LGVars,[X|LNoGVars]):-
	member(X/Y,Conj),
	ground(Y),
	\+memberchk(X,GVars).
negate_conj(Conj,X=Y,GVars,LGVars,LNoGVars,[X|LGVars],LNoGVars):-
	member(X/Y,Conj),
	memberchk(X,GVars).

%eliminar_igualdades_inutiles(Lista_igualdades, LNoGVars1, Sol). Si existe un igualdad de 
%                  Lista_igualdades de la forma X=Y, si Y no tiene atributo y no pertenece 
%                  a LNoGVars1 se elimina de Lista_Igualdades, obteniendo al final esa lista 
%                  en Sol
%
%eliminar_igualdades_inutiles(Lista_igualdades, LNoGVars1, Sol). If a equality with form X=Y
%                  from Lista_igualdades exists, if Y hasnt got at and Y doesnt belong to 
%                  LNoGVars1, the equality is removed from Lista_Igualdades, obtaining that
%                  list in Sol.

eliminar_igualdades_inutiles([],_NoGVars,[]).
eliminar_igualdades_inutiles([(_X=Y)|RestoI],NoGVars,Sol):-
	var(Y),
        \+memberchk(Y,NoGVars),
	\+get_attribute(Y,_F),!,
	eliminar_igualdades_inutiles(RestoI,NoGVars,Sol).
eliminar_igualdades_inutiles([(X=Y)|RestoI],NoGVars,[(X=Y)|RestoSol]):-
	eliminar_igualdades_inutiles(RestoI,NoGVars,RestoSol).
eliminar_igualdades_inutiles([dist(X,Y)|RestoI],NoGVars,[dist(X,Y)|RestoSol]):-
	eliminar_igualdades_inutiles(RestoI,NoGVars,RestoSol).
		
% no_gvars_neg(Neg,GVars,No_GVars). Obtiene en NO_Gvars todas las variables que no pertenecen a
%        GVars y que aparecen en la solucion Neg.
%
% no_gvars_neg(Neg,GVars,No_GVars). Obtains in NO_Gvars all the variables that not belong to
%        GVars and that appear in the solution Neg.

no_gvars_neg([],_GVars,[]).
no_gvars_neg([(_X=Y)|Resto],GVars,[Y|Resto_No_GVars]):-
	var(Y),
	\+memberchk(Y,GVars),!,
	no_gvars_neg(Resto,GVars,Resto_No_GVars).
no_gvars_neg([(_X=Y)|Resto],GVars,Resto_No_GVars):-
	var(Y),
	memberchk(Y,GVars),!,
	no_gvars_neg(Resto,GVars,Resto_No_GVars).
no_gvars_neg([_X|Resto],GVars,Resto_No_GVars):-
	no_gvars_neg(Resto,GVars,Resto_No_GVars).
	

% detach_attribute_all(Vars) Aplica detach_attribute a todos los elementos de la lista de variables Vars.

detach_attribute_all([]).
detach_attribute_all([V|Vars]):-
	detach_att_if_needed(V),
	detach_attribute_all(Vars).

% detach_att_if_needed(Var) elimina el atributo de Var si lo tiene.
%
% detach_att_if_needed(Var) removes the attribute from Var, if its posible.

detach_att_if_needed(Var) :-
        get_attribute(Var,_), !,
        detach_attribute(Var).
detach_att_if_needed(_).

% comprobar_solucion_valida(Sol). Evita que una solucion X=_A, Y=_B, Z=_C sea considerada
%         solucion, ya que el resultado es yes y termina la ejecucion del programa.
%
% comprobar_solucion_valida(Sol). It avoids that a solution X=_A, Y=_B, Z=_C is considered
%         a real solution, because the result is yes and the program halts.
%

comprobar_sol_valida(_GVars,[]):- fail.
comprobar_sol_valida(_GVars,[(dist(_X,_Y))|_RSol]).
comprobar_sol_valida(GVars,[(_X=Y)|RSol]):-
	var(Y),
	\+memberchk(Y,GVars),
	\+get_attribute(Y,_),!,
	comprobar_sol_valida(GVars,RSol).
comprobar_sol_valida(_GVars,[(_X=_Y)|_RSol]).
	
% evaluate_neg

evaluate_neg([]).
evaluate_neg([NS|NLSols]):-
	call(NS),
	evaluate_neg(NLSols).

% convertir(Lista,Sol_Par,Sol_Fin). Lista es una lista de listas. El objetivo es que Sol_Fin
%        contenga todos los eltos de Lista pero siendo una unica lista. Sol_Par es una variable
%        auxiliar que va almacenando la solucion hasta el final.
%
% convertir(Lista,Sol_Par,Sol_Fin). Lista is a lsit of lists. The aim is that all the elements of
%        lista are conteined in Sol_Fin, but being Sol_Fin only one list. Sol_Par is an
%        auxiliary  variable which stores the partial solutions until the end. 

convertir([],Sol_Par,Sol_Fin):- Sol_Fin=Sol_Par.
convertir([[]|R],Sol_Par,Sol_Fin):-
	!,
	convertir(R,Sol_Par,Sol_Fin).
convertir([P|R],Sol_Par,Sol_Fin):-
	P=[_Elto|_Rest],!,
	add_list(P,Sol_Par,Sol_Aux),
	convertir(R,Sol_Aux,Sol_Fin).
convertir([P|R],Sol_Par,Sol_Fin):-
	append(Sol_Par,[P],Sol_Aux),
	convertir(R,Sol_Aux,Sol_Fin).


%add_list(Lista,Sol_Par,Sol_Fin). Los eltos de Lista se van insertando en Sol_Par que es una 
%         lista ya existente. Sol_Fin tiene el append de la lista ya existente y Lista.
%
%add_list(Lista,Sol_Par,Sol_Fin). The elements of Lista are inserted in Sol_Par that is a 
%         previous list. Sol_Fin contains append of Sol_Par and Lista.

add_list([],Sol_Par,Sol_Fin):- Sol_Fin=Sol_Par.
add_list([P|R],Sol_P,Sol_Fin):-
	append(Sol_P,[P],Sol_Aux),
	add_list(R,Sol_Aux,Sol_Fin).
	

% desig_to_ig(Lista,Sol), Lista es una lista con desigualdades y Sol es la misma lista pero
% con esas desugualdades pasadas a igualdades.
%
%
%
desig_to_ig([],[]).
desig_to_ig([(X/Y)|Resto],[(X=Y)|RSol]):-
	desig_to_ig(Resto,RSol).

% cuantificacion_universal(Lista1,GoalVars,Lista2), tranforma las variables libres de Lista1
% en fa(Var) en Lista2, siendo Var cualquier variable que no pertenece a GoalVars.

cuantificacion_universal([P|R],FVars,[SolP|RestSol]):-
	c_u_basica(P,FVars,SolP),
	cuantificacion_universal(R,FVars,RestSol).

c_u_basica([Term|RTerm],FVars,[Termuniv|Restuniv]):-
	replace_free_vars(FVars,Term,Termuniv),
	c_u_basica(RTerm,FVars,Restuniv).


%igualar_gv(Lista1,Lista1,Aux,Sol), iguala todos los elementos de Lista1 unos con otros y 
%   almacena las igualdades en Sol. EJ: [X,Y,Z]----->[X=Y,X=Z,Y=Z]
 
igualar_gv([],[],Sol,ListSol):- ListSol=Sol.
igualar_gv([Pv1|Rv],[_Pv2|Rvars],Aux,_ListSol):-
	igualar_gv_2(Pv1,Rvars,Sol1),
	append(Aux,Sol1,ListSol),
	igualar_gv(Rv,Rvars,ListSol,_ListSol).

igualar_gv_2(_V,[],[]).
igualar_gv_2(Var,[V1|Rvars],[(Var=V1)|RSol]):-
	igualar_gv_2(Var,Rvars,RSol).


dc([], [[]]).
dc([L1|Ls], Lds) :- 
        dc(Ls, Lda),
        dc2(L1, Lda, Lds).

dc2([], _, []).
dc2([X|Xs], Lda, Lds) :-
        dc3(Lda, X, Lds, Lds_),
        dc2(Xs, Lda, Lds_).

dc3([], _X, Lds, Lds).
dc3([L|Ls], X, [[X|L]|XLs], Lds_) :- 
        dc3(Ls, X, XLs, Lds_). 

obtain_disy([],[]).
obtain_disy([Conj|L],[X|Disy]):-
	member(X,Conj),
	obtain_disy(L,Disy).

% difference(Vars,NFVars,FreeVars) Devuelve en FreeVars la sublista de elementos de Vars que no 
%            aparecen en NFVars
%
% difference(Vars,NFVars,FreeVars) FreeVars contains the sublist of elements of Vars which dont
%            appear in NFVars

difference([],_NFVars,[]).
difference([Var|Vars],NFVars,FreeVars):-
	memberchk(Var,NFVars),!,
	difference(Vars,NFVars,FreeVars).
difference([Var|Vars],NFVars,[Var|FreeVars]):-
	difference(Vars,NFVars,FreeVars).

union([], _List, List_Almacen, ListS):- ListS=List_Almacen,!.
union([Element|Residue], List, List_Almacen,Union) :-
	\+memberchk(Element,List),
	\+memberchk(Element,List_Almacen),!,
	union(Residue, List, [Element|List_Almacen], Union).
union([_Element|Residue], List,List_Almacen, Union) :-
	union(Residue, List, List_Almacen, Union).

intersection([], _,LInicio,Intersection):- Intersection=LInicio.
intersection([Element|Residue], List, LInicio, Intersection) :-
	not_ison(Element,List),!,
	intersection(Residue, List,[Element|LInicio],Intersection).	
intersection([_Element|Residue], List,LInicio, Intersection) :-
	intersection(Residue, List, LInicio, Intersection).	

not_ison(_,[]):-fail.
not_ison(X,[Y|_Resto]):- X==Y,! .
not_ison(X,[_|Resto]):- not_ison(X,Resto).

%obtener_Vars_NoDetach(L1,L2). Devulve en L2 la lista de variables pertenecientes a L1 que no 
%               necesitan que se elimine su atributo
%
%obtener_Vars_NoDetach(L1,L2). L2 contains the list of variables which belong to L1 and is not
%               neccesary to remove their attribute.

obtener_Vars_NoDetach([],[]).
obtener_Vars_NoDetach([(dist(X,Y))|RestEq],[VarsND|RestoNoGVars]):-
	nonvar(Y),
	varset((X=Y),VarsY),
	difference(VarsY,[X],VarsND),!,
	obtener_Vars_NoDetach(RestEq,RestoNoGVars).
obtener_Vars_NoDetach([(dist(_X,Y))|RestEq],[RestoNoGVars]):-
	var(Y),
	obtener_Vars_NoDetach(RestEq,RestoNoGVars).
obtener_Vars_NoDetach([(_X=Y)|RestEq],[VarsND|RestoNoGVars]):-
	nonvar(Y),
	varset((X=Y),VarsY),
	difference(VarsY,[X],VarsND),!,
	obtener_Vars_NoDetach(RestEq,RestoNoGVars).
obtener_Vars_NoDetach([(_X=_Y)|RestEq],RestoNoGVars):-
	obtener_Vars_NoDetach(RestEq,RestoNoGVars).

% obtener_Vars_Detach(Lista_a_evaluar_final,NoGVars_final)

obtener_Vars_Detach([],_Vars_NoDetach,[]).
obtener_Vars_Detach([(dist(_X,Y))|RestEq],Vars_NoDetach,[Y|RestoNoGVars]):-
	var(Y),
	\+memberchk(Y,Vars_NoDetach),!,
	obtener_Vars_Detach(RestEq,Vars_NoDetach,RestoNoGVars).
obtener_Vars_Detach([(_X=Y)|RestEq],Vars_NoDetach,[Y|RestoNoGVars]):-
	var(Y),
	\+memberchk(Y,Vars_NoDetach),!,
	obtener_Vars_Detach(RestEq,Vars_NoDetach,RestoNoGVars).
obtener_Vars_Detach([(dist(_X,_Y))|RestEq],Vars_NoDetach,RestoNoGVars):-
	obtener_Vars_Detach(RestEq,Vars_NoDetach,RestoNoGVars).
obtener_Vars_Detach([(_X=_Y)|RestEq],Vars_NoDetach,RestoNoGVars):-
	obtener_Vars_Detach(RestEq,Vars_NoDetach,RestoNoGVars).


% replace_free_vars(FreeVars,T,Tf) returns Tf that is T but substituting
% each aparition of a variable X from FreeVars by fA(X)
% There is no repeated variables in FreeVars.

replace_free_vars([],T,T).
replace_free_vars([X|FreeVars],T,Tf):- 
	replace(T,X,fA(X),Tf1), % Free variables will be universally
	replace_free_vars(FreeVars,Tf1,Tf). % quantified at the negation

% replace(Form,Var,Value,Form1) returns Form1 that is the term
% Form but replacing aparitions of Var by Value

replace(Term,Var,Value,Value):-
	Term==Var,!.
replace(Term,Var,_Value,Term):-
	var(Term),
	Term\==Var,!.
replace(Term,Var,Value,Term1):-
	Term=..[Functor|Args],
	replace_all(Args,Var,Value,Args1),
	Term1=..[Functor|Args1].

% replace_all(L,Var,Value,L1) applies replace/4 to all the elements
% of the list L.

replace_all([],_,_,[]).
replace_all([Arg|Rest],Var,Value,[Arg1|Rest1]):-
	replace(Arg,Var,Value,Arg1),
	replace_all(Rest,Var,Value,Rest1).


eliminar_atributos([]).
eliminar_atributos([X|Resto]):-
	detach_attribute(X),
	eliminar_atributos(Resto).


combine_each([]).
combine_each([Sol|Rest]):-
	member(Sol1,[Sol|Rest]),
        call_all(Sol1).
	
% call_all(L) calls all the subgoals of the list L
call_all([]).
call_all([G|L]):-
	call(G),
	call_all(L).

dist_list([]).
dist_list([(X,Y)|L]):-
	dist(X,Y),
	dist_list(L).
no_dist_list(X):- cnegf(dist_list(X)).



