:- module(cnegf,_).

%:- module(cnegf, [cnegf/1,distinto/1,t1/2,t2/2,t3/3,t31/3,refinar/4,minimizar/5]).

:- meta_predicate
	cnegf(goal).


:- data variables/1.
 % It is going to be used to make variables renames
:- use_module(dist,[dist/2,put_attribute/2, actualizar_formula/1, 
	            eliminar_repetidas/2,
		    sustituir/4]).
:- use_module(library(lists),[append/3,delete/3]).
:- use_module(library(aggregates),[setof/3]).
:- use_module(library(metaterms),[varset/2]).
:- use_module(library(idlists),[memberchk/2]). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    NEGACION DE PRED CON UN CONJ DE SOLUCIONES FINITO         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 

% cnegf(Pred) tiene exito si el predicado Pred falla y 
% ofrece las soluciones posibles en caso de haberlas
% utiliza negacion constructiva negando las soluciones
% del objetivo Pred
% Para predicados parcialmente instanciados que no devuelven
% soluciones pero que podrian tenerlas al instanciarse alguna de las
% variables que contienen

cnegf(Pred):-
        varset(Pred,GVars),
	setof(GVars,Pred,LValores),!,
	obtener_igualdades_de_GoalVars(GVars,LValores,Lista_de_GoalVars_igualadas_aux),
	convertir(Lista_de_GoalVars_igualadas_aux,[],Lista_de_GoalVars_igualadas),
	organizar_conj(LValores,GVars,Lista_a_transformar),
	%normalizar(Lista_a_transformar,GVars,Lista_a_t_normalizada),
	%realizar_asignaciones(Lista_a_t_normalizada,GVars,LATN), % ya no es necesario 
						% tenemos obtener_igualdades_de_GoalVars
        normalizar(Lista_a_transformar,GVars,LATN),
        combine_sols(LATN,Lista_soluciones_negada),
% Este convertir se hace porque la Lista_soluciones_negada tiene elementos simples y otras
% listas, convertir lo unifica todo en una unica lista
	convertir(Lista_soluciones_negada,[],Lista_a_evaluar),
	comprobar_sol_valida(GVars,Lista_a_evaluar),
	%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	append(Lista_a_evaluar,Lista_de_GoalVars_igualadas,Lista_a_evaluar_final),
	no_gvars_neg(Lista_a_evaluar_final,GVars,No_GVars),
	detach_attribute_all(No_GVars),
% Lista_a_evaluar_final contiene la lista final de soluciones incluidas las igualdades entre
%	Gvars y No_GVars que mantienen la coherencia. De todas formas los atributos de las
%	No_GVars son peligrosos asi que los eliminamos.
	evaluate_neg(Lista_a_evaluar_final).
	%call_all(Lista_soluciones_negada).

cnegf(_Pred). % Si no ha podido hacer setof es que no hay soluciones

%obtener_igualdades_de_GoalVars(GVars,LValores,Lista_de_GoalVars_igualadas) Mete en 
%           Lista_GoalVars_igualadas todas las igualdades que devuelve el setof, siempre que de
%           la igualdad X=Y la Y sea var. Se hace para que al final del todo se una a la
%           lista solucion negada para que todas las GVars se igualen a las No_GVars.
%           Es necesario porque la negacion del dist de una Goalvar puede implicar a otras GVars.
%           La solucion es una lista de listas, hay que convertirla a una unica lista.

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
	


realizar_asignaciones([],_GV,[]).
realizar_asignaciones([P|R],GV,[Sol1|RSol]):-
	raaux(P,GV,Sol1),
	%append(Sol,Sol1,Sol2),
	realizar_asignaciones(R,GV,RSol).

raaux([],_GV,[]).
raaux([(X=Y)|R],Gvars,RSol):-
	var(Y),
	\+memberchk(Y,Gvars),
	\+get_attribute(Y,_F),!,
	X=Y,
	raaux(R,Gvars,RSol).

raaux([(X=Y)|R],Gvars,[(X=Y)|RSol]):-
	raaux(R,Gvars,RSol).


% organizar_conj(LValores,Vars,Lista_sol), LValores es una lista de listas. En Lista_sol
% se devuelve otra lista de listas igual que LValores pero conteniendo igualdades con
% las variables de Vars. EJ organizar_conj([[1,2],[V,6]],[X,Y],[[X=1,Y=2],[X=V,Y=6]]
organizar_conj([],_GVars,[]).
organizar_conj([P|Resto],GVars,[SolP|SolRest]):-
	organizar_basico(P,GVars,SolP),
	organizar_conj(Resto,GVars,SolRest).

organizar_basico([],[],[]).
organizar_basico([Elto|Rest_Eltos],[V1|Rvars],[(V1=Elto)|SolRest]):-
	organizar_basico(Rest_Eltos,Rvars,SolRest).


% normalizar(ListaC,GV,Sol), realiza todas las operaciones de normalizacion.
normalizar([],_Gv,[]).
normalizar([C1|Lista],Gv,[CSol|ListaSol]):-
	normalizar_ig(C1,Gv,CSol),
	normalizar(Lista,Gv,ListaSol).

normalizar_ig(C1,Gv,CSol):-
	varset(C1,Vars),
	difference(Vars,Gv,FreeVarsaux),
	minimizar(FreeVarsaux,C1,[],[],FreeVars),
        buscar_igualdades(C1,FreeVars,Gv,[],CSol).

% minimizar(FV,Lista,Sol_aux,Inicio,Sol), obtiene en Sol una lista de variables de FV que
%   aparecen en Lista dos o mas veces en la forma X=_FV siendo X GV y _FV la variable a 
%   aparecer en la lista solucion.
	
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
	
buscar_igualdades([],_Fv,_Gv,Sol_aux,Sol_Final):- Sol_Final=Sol_aux.
buscar_igualdades([(X=Y)|RestoI],FreeVars,Gv,Sol_aux,_Sol_Final):-
	\+var(Y),!,
	append(Sol_aux,[(X=Y)],RSol),
	buscar_igualdades(RestoI,FreeVars,Gv,RSol,_Sol_Final).
buscar_igualdades([(X=Y)|RestoI],FreeVars,Gv,Sol_aux,_Sol_Final):-
	var(Y),
	\+memberchk(Y,FreeVars),!,
	append(Sol_aux,[(X=Y)],RSol),
	buscar_igualdades(RestoI,FreeVars,Gv,RSol,_Sol_Final).
buscar_igualdades([(X=Y)|RestoI],FreeVars,Gv,Sol_aux,_Sol_Final):-
	var(Y),
	memberchk(Y,FreeVars),
	\+get_attribute(Y,_F),!,
	refinar(Y,[(X=Y)|RestoI],Gv,PSol),
	append(Sol_aux,PSol,RSol),
	%delete(FreeVars,Y,NewFreeVars),
	buscar_igualdades(RestoI,FreeVars,Gv,RSol,_Sol_Final).
buscar_igualdades([(X=Y)|RestoI],FreeVars,Gv,Sol_aux,_Sol_Final):-
	var(Y),
	memberchk(Y,FreeVars),
	get_attribute(Y,_F),!,
	append(Sol_aux,[(X=Y)],RSol),
	%delete(FreeVars,Y,NewFreeVars),
	buscar_igualdades(RestoI,FreeVars,Gv,RSol,_Sol_Final).

% refinar(FV,Lista,GV,Sol), iguala todas las varibles de GV que tengan alguna igualdad en la
%    lista Lista con la variable FV. EJ: (_A,[X=_A,Y=_B,Z=_A],[X,Y,Z], [X=Z]).
refinar(FV,Lista,Gv,Sol):-
	obtenerLista_V(FV,Lista,Gv,Lista_V),
	igualar_gv(Lista_V,Lista_V,[],Sol).


% obtenerLista_V(FV,Lista,GV,Sol), obtiene todas las variables de GV que tienen alguna igualdad
%     en la Lista con la varible FV. 
obtenerLista_V(_FV,[],_Gv,[]).

obtenerLista_V(FV,[(_X=Y)|R],Gv,RSol):-
	\+Y==FV,!,
	obtenerLista_V(FV,R,Gv,RSol).

obtenerLista_V(FV,[(X=Y)|R],Gv,[X|RSol]):-
	Y==FV,!,
	memberchk(X,Gv),
	obtenerLista_V(FV,R,Gv,RSol).


% combine_sols(Lista_soluciones,Lista_soluciones_negada), Lista_soluciones_negada es una 
%             lista de listas que contiene la Lisata_soluciones, negada, es decir, cambiandos 
%             los = por / y las conjunciones por disyunciones

combine_sols([],[]).
combine_sols([Sol|LSols],[NSol|NLSols]):-
	negate(Sol,NSol),
	combine_sols(LSols,NLSols).

% negate(Sol,Neg). Negate va cogiendo una igualdad de cada solucion para conseguir hacer 
%                  el disy_to_conj pero con backtracking, para ello utiliza el member. Cada 
%                  igualdad la niega creando asi una de las soluciones de la negacion.

negate(Sol,Neg):-
	member(Eq,Sol),
	negate_analize(Eq,Neg).
        %get_attribute(X,formula(X,Attribute)),!,
	%negate_at(Attribute,Neg).
	%detach_attribute(X).
	
%negate(Sol, dist(X,Y)):-
%        member(X=Y,Sol).

% negate_analize(Eq,Neg). Cada igualdad X=Y puede tener cuatro casos. X e Y no tienen 
%   atributo. X tiene at pero Y no. Y tiene at pero X no. Ambas tiene atributo. negate_analize 
%   trata estos 4 casos. 

negate_analize(X=Y,Neg):-
	\+get_attribute(X,formula(X,_AttributeX)),
	\+get_attribute(Y,formula(Y,_AttributeY)),
	var(Y),!,
	Neg=(X=Y).
negate_analize(X=Y,Neg):-
	\+get_attribute(X,formula(X,_AttributeX)),
	\+get_attribute(Y,formula(Y,_AttributeY)),!,
	Neg=(dist(X,Y)).
negate_analize(X=Y,Neg):-
	get_attribute(X,formula(X,AttributeX)),
	\+get_attribute(Y,formula(Y,_AttributeY)),!,
	negate_at(AttributeX,Neg),
	detach_attribute(X).
negate_analize(X=Y,Neg):-
	get_attribute(Y,formula(Y,AttributeY)),
	\+get_attribute(X,formula(X,_AttributeX)),!,
	negate_at(AttributeY,Neg_aux),
	append(Neg_aux,[(X=Y)],Neg),
	detach_attribute(Y).  % creo que ya no hace falta.
negate_analize(X=Y,Neg):-
	get_attribute(X,formula(X,AttributeX)),
	get_attribute(Y,formula(Y,AttributeY)),!,
	negate_at(AttributeX,NegX),
	negate_at(AttributeY,NegY),
	append(NegX,NegY,Neg_aux),
	append(Neg_aux,[(X=Y)],Neg),
	detach_attribute(Y),
	detach_attribute(X).

% negate_at(Lista_eq,Lista_Sol). Al igual que negate va cogiendo miembro a mienbro de cada 
%     conjuncion para hacer el disy_to_conj pero con BT. Cada miembro es una desigualdad que 
%     afirma.

negate_at([],[]).
negate_at([Conj|Disy],[NSol|NResto]):-
	negate_conj(Conj,NSol),
	negate_at(Disy,NResto).

% negate_conj(Conj,Sol). Transforma una desigualdad X/Y, perteneciente a Conj, en X=Y. Se utiliza
%      el member para aprovechar el BT.

negate_conj(Conj,X=Y):-
	member(X/Y,Conj).

% no_gvars_neg(Neg,GVars,No_GVars). Obtiene en NO_Gvars todas las variables que no pertenecen a
%        GVars y que aparecen en la solucion Neg.

no_gvars_neg([],_GVars,[]).
no_gvars_neg([(_X=Y)|Resto],GVars,[Y|Resto_No_GVars]):-
	var(Y),
	\+memberchk(Y,GVars),!,
	%\+memberchk(Y,No_Gvars),
	no_gvars_neg(Resto,GVars,Resto_No_GVars).
no_gvars_neg([(_X=Y)|Resto],GVars,Resto_No_GVars):-
	var(Y),
	memberchk(Y,GVars),!,
	no_gvars_neg(Resto,GVars,Resto_No_GVars).
no_gvars_neg([_X|Resto],GVars,Resto_No_GVars):-
	%non_var(Y),!,
	no_gvars_neg(Resto,GVars,Resto_No_GVars).
%no_gvars_neg([(_X=Y)|Resto],GVars,[Resto_No_GVars]):-
%	var(Y),
%	\+memberchk(Y,GVars),
%	memberchk(Y,No_Gvars),!,
%	no_gvars_neg(Resto,GVars,Resto_No_GVars).	
	

% detach_attribute_all(Vars) applies detach_attribute to all elements
% of the list of variables Vars.
detach_attribute_all([]).
detach_attribute_all([V|Vars]):-
	detach_att_if_needed(V),
	detach_attribute_all(Vars).

% detach_att_if_needed(Var) detaches the attribute of Var if it has it.
detach_att_if_needed(Var) :-
        get_attribute(Var,_), !,
        detach_attribute(Var).
detach_att_if_needed(_).

% comprobar_solucion_valida(Sol). Evita que una solucion X=_A, Y=_B, Z=_C sea considerada
%         solucion, ya que el resultado es yes y termina la ejecucion del programa.

comprobar_sol_valida(_GVars,[]):- fail.
comprobar_sol_valida(_GVars,[(dist(_X,_Y))|_RSol]).
comprobar_sol_valida(GVars,[(_X=Y)|RSol]):-
	var(Y),!,
	\+memberchk(Y,GVars),
%\+get_attribute(Y,formula(Y,_At)), % creo que no hace falta, ene ste punto nada tiene at.
	comprobar_sol_valida(GVars,RSol).
comprobar_sol_valida(_GVars,[(_X=_Y)|_RSol]).
	
	

% evaluate_neg

evaluate_neg([]).
evaluate_neg([NS|NLSols]):-
	%call_all(NS),
        call(NS),
	evaluate_neg(NLSols).

% convertir(Lista,Sol_Par,Sol_Fin). Lista es una lista de listas. El objetivo es que Sol_Fin
%        contega todos los eltos de Lista pero siendo una unica lista. Sol_Par es una variable
%        auxiliarque va almacenando la solucion hasta el final.
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

%add_list(Lista,Sol_Par,Sol_Fin). Los eltos de Lista se vcan insertando en Sol_Par que es una 
%         lista ya existente. Sol_Fin tiene el append de la lista ya existente y Lista.
add_list([],Sol_Par,Sol_Fin):- Sol_Fin=Sol_Par.
add_list([P|R],Sol_P,Sol_Fin):-
	append(Sol_P,[P],Sol_Aux),
	add_list(R,Sol_Aux,Sol_Fin).
	


% desig_to_ig(Lista,Sol), Lisya es un alista con desigualdades y Sol es la misma lista pero
% con esas desugualdades pasadas a igualdades.
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

% disy_to_conj(Ld,Lc), devuelve en Lc el resultado de aplicar la propiedad
% distributiva entre las listas de Ld.
%disy_to_conj(Ld,Lc):-
%	setof(D,obtain_disy(Ld,D),Lc).

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

% difference(Vars,NFVars,FreeVars) retuns in FreeVars the sublist of elements
% of Vars that do not appear in NFVars
difference([],_NFVars,[]).
difference([Var|Vars],NFVars,FreeVars):-
	memberchk(Var,NFVars),!,
	difference(Vars,NFVars,FreeVars).
difference([Var|Vars],NFVars,[Var|FreeVars]):-
	difference(Vars,NFVars,FreeVars).

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
% replace(Term,_Var,_Value,Term):-
% 	ground(Term),!.
% replace(Term,Var,Value,Value):-
% 	var(Term),
% 	Term==Var,!.
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
%	varset(G,GVars),
%	copy_term(G,G1),
%	varset(G1,GVars1),
	call(G),
	call_all(L).







%%% Ejemplos:

%neg_solutions([[1],[3]], [X], [[dist(X,1)],[dist(X,3)])
%neg_solutions([[W],[Z]], [X,Y], [[dist(X,1)],[dist(X,3)])
% pero la Z lleva el atributo Z/3

%?- setof([X],member(X,[1,2]),Sols).
%
%Sols = [[1],[2]] ? ;
%
%no

%?- setof([X,Y],(member(X,[Y,2]),dist(Y,3)),Sols).

%Sols = [[_A,_A],[2,_B]],
%attach_attribute(_B,formula(_B,[[_B/3]])),
%attach_attribute(_A,formula(_A,[[_A/3]])) ? ;

%no

prueba(X,Y,Z):- X=1,Y=_A,Z=_B.
prueba(X,Y,Z):- X=Y,Y=Z,Z=3.

no_es_digito(X):-
	member(Z,[0,1,2,3]),
	dist(X,Z).
es_digito(X):-cnegf(no_es_digito(X)).

p2(X,Y):- 
	dist(q(X,Y),q(Y,2)),  % caso mas sencillo en el que se ve el problema
	dist(X,3).    % No actualiza bien el atributo.Lo hace solo en la X
no_p2(X,Y):- cnegf(p2(X,Y)).

t2(X,Y):- X=s(0),Y=s(1).
no_t2(X,Y):-cnegf(t2(X,Y)).

distinto11(X,_Y):- X=0.
distinto11(_X,Y):- dist(Y,2).
no_distinto11(X,Y):- cnegf(distinto11(X,Y)).

parent(adelaida,heidi).
parent(abuelito,adelaida).
no_parent(X,Y):- cnegf(parent(X,Y)).

t3(X,Y,_Z):- X=[Y,Z1],dist(Z1,1),dist(Y,2).
no_t3(X,Y,Z):-cnegf(t3(X,Y,Z)).

