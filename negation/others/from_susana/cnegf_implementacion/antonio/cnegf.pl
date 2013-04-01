:- module(cnegf,_).

:- meta_predicate cnegf(goal).

:- data variables/1.

:- use_module(dist,[dist/2,put_attribute/2, actualizar_formula/1,eliminar_repetidas/2, sustituir/4]).
:- use_module(library(lists),[append/3,delete/3,list_insert/2,union/3]).
:- use_module(library(terms),[arg/2]).
:- use_module(library(aggregates),[setof/3]).
%:- use_module(library(metaterms),[varset/2]).
:- use_module(library(terms_vars),[varset/2]).
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
	organizar_conj(LValores,GVars,Lista_a_transformar),
	normalizar(Lista_a_transformar,GVars,LATN),
	
% Tratamiento de variables e igualdades al inicio
	obtener_igualdades_de_GoalVars(GVars,LATN,Lista_de_GoalVars_igualadas_aux),
	convertir(Lista_de_GoalVars_igualadas_aux,[],Lista_de_GoalVars_igualadas),
	varset(Lista_de_GoalVars_igualadas, Lista_vars),
	difference(Lista_vars, GVars, V_existenciales_inicio),
	eliminar_ig_inutiles(LATN,GVars,LATNF),

% Tratamiento de la negacion	
        combine_sols(LATNF,Lista_soluciones_negada,GVars,[],VarsDetach,[], V_existenciales),
        convertir(Lista_soluciones_negada,[],Lista_a_evaluar),
	comprobar_sol_valida(GVars,Lista_a_evaluar),
	
% Tratamiento de variables e igualdades al final
	%variables conflicto son las que tiene atributos pero no deben aparecer en la solucion final	
	obtener_vars_conflicto(Lista_de_GoalVars_igualadas, VarsDetach, VarsConflicto),
	convertir(VarsConflicto,[],VarsConflicto_final),
	append(Lista_a_evaluar,Lista_de_GoalVars_igualadas,Lista_a_evaluar_final),
	append(VarsConflicto_final, VarsDetach, VarsDetach_final),
	detach_attribute_all(VarsDetach_final),
	varset(Lista_a_evaluar_final, FreeVars),
	difference(FreeVars,GVars,FreeVars_aux),
	append(V_existenciales, V_existenciales_inicio, V_existenciales_final),
	difference(FreeVars_aux,V_existenciales_final, FreeVars_aux1),
	buscar_en_parte_izq_lista(FreeVars_aux1,Lista_a_evaluar_final,FreeVarsUniversal),
        put_universal(FreeVarsUniversal),

% obtencion de soluciones
	evaluate_neg(Lista_a_evaluar_final).
	

cnegf(_Pred). % Si no ha podido hacer setof es que no hay soluciones

% obtener_vars_conflicto(Lista, VarsDetach , VarsConflicto). Variables conflicto son las que tienen atributo pero
%                        pero no deben apararecer en la solucion final. Una vez encontradas en Lista se almacenan en
%                        VarsConflicto.
%
% obtener_vars_conflicto(Lista, VarsDetach , VarsConflicto). Conflict Variables are variables with attribute but those 
%                        must not appear in the final solution. Once found in Lista these variables are stored in 
%                        VarsConflicto.
obtener_vars_conflicto([], _ , []).
obtener_vars_conflicto([(_X=Y)|Resto], VarsDetach, [Vars_conflicto_aux|RVarsConflicto]):-
	var(Y),
	get_attribute(Y,formula(Y,At)),!,
	varset(At,Vars_at),
	append([Y],Vars_at, Vars_conflicto_aux),
	obtener_vars_conflicto(Resto,VarsDetach,RVarsConflicto).
obtener_vars_conflicto([(_X=Y)|Resto], VarsDetach, [Sol|RVarsConflicto]):-
	nonvar(Y),!,
	varset(Y,Y_aux),
	tratar_nonvar_conflicto(Y_aux, VarsDetach, Sol),
	
obtener_vars_conflicto(Resto,VarsDetach,RVarsConflicto).
obtener_vars_conflicto([(_X=_Y)|Resto], VarsDetach, VarsConflicto):-
	obtener_vars_conflicto(Resto,VarsDetach,VarsConflicto).

% tratar_nonvar_conflicto(Conj,VarsDetach,VarsConflicto). Conj es una tupla (X=Y), cuando Y es una lista o predicado, no una
%                          variable, se obtienen las posibles variables conflicto de Y y se almacenan en VarsConflicto. 
%
% tratar_nonvar_conflicto(Conj,VarsDetach,VarsConflicto). Conj is (X=Y), when Y is a listo or a predicate, not a
%                          variable, the posibles conflict var in Y are obtained and stored in VarsConflicto. 
tratar_nonvar_conflicto([],_VarsDetach,[]).        
tratar_nonvar_conflicto([Var|RVar],VarsDetach,[Var|RVarConflicto]):-
	get_attribute(Var,_At),!,
	tratar_nonvar_conflicto(RVar,VarsDetach,RVarConflicto).
tratar_nonvar_conflicto([_Var|RVar],VarsDetach,VarConflicto):-
	tratar_nonvar_conflicto(RVar,VarsDetach,VarConflicto).

% put_universal(UnivVars) substituye variables de UnivVars por 
% variables cuntiaficadas universalmente fA(_)
%
% put_universal(UnivVars) substitutes variables of UnivVars by 
% universal quantified variables fA(_)
put_universal([]).
put_universal([Var|UnivVars]):-
	var(Var), % the cuantification keeps
	get_attribute(Var,formula(Var,_At)),!,
	put_universal(UnivVars).
put_universal([Var|UnivVars]):-
	var(Var),!, % the cuantification keeps
	Var=fA(_NewVar),
        put_universal(UnivVars). 
put_universal([_Value|UnivVars]):- % the variable has change its quantification
	put_universal(UnivVars).

% put_existencial(Vars) substituye variables de Vars fA(_) por 
% variables cuntiaficadas existencialmente _A
%
% put_existencial(Vars) substitutes variables of Vars fA(_) by 
% existential quantified variables _A
put_existencial(Term, Term_final, Vars_existenciales):-
	%en caso de que sea una lista
	functor(Term,'.',_),!,
	existenciar_lista(Term, Term_final,[],Vars_existenciales).
put_existencial(Term, Term_final, Vars_existenciales):-
	%en caso de que sea un predicado
	functor(Term,Pred,N),
	functor(Term_final,Pred,N),
	existenciar_predicado(1,N,Term,Term_final,[], Vars_existenciales).

% existenciar_lista(Lista,ListaSol,Vars_existenciales_aux, Vars_existenciales) Analiza Lista buscando variables
%                  cuantificadas universalmente para cuantificarlas existencialmente en ListaSol. Ademas esas
%                  variables modificadas quedan almacenadas en Vars_existenciales.
%
% existenciar_lista(Lista,ListaSol,Vars_existenciales_aux, Vars_existenciales) Analize Lista searching universal quantified 
%                  variables to quantify them existentialy in ListaSol. Moreover those variables are stored in 
%                  Vars_existenciales.
existenciar_lista([],[],Vars_existenciales_aux, Vars_existenciales):-
	Vars_existenciales=Vars_existenciales_aux.
existenciar_lista([P|Resto],[Sol|RestoSol],Vars_existenciales_aux, Vars_existenciales):-
	tratar_argumento(P,Sol,Vars_existenciales_aux, Vars_existenciales_par),
	existenciar_lista(Resto,RestoSol,Vars_existenciales_par, Vars_existenciales).

% existenciar_predicado(R1,RN,Term,TermSol,Vars_existenciales_aux, Vars_existenciales) Analiza Term buscando variables
%                  cuantificadas universalmente para cuantificarlas existencialmente en TermSol. Ademas esas
%                  variables modificadas quedan almacenadas en Vars_existenciales. R1 y RN se utilizan para recorrer
%                  los argumentos de Term.
%
% existenciar_predicado(R1,RN,Term,TermSol,Vars_existenciales_aux, Vars_existenciales) Analize Term searching universal 
%                  quantified  variables to quantify them existentialy in TermSol. Moreover those variables are stored in 
%                  Vars_existenciales. R1 and RN are used to go through the arguments of Term. 
existenciar_predicado(N,N,Term, Term_aux, Vars_existenciales_aux, Vars_existenciales):-
	arg(N,Term,Arg_a_tratar),
	tratar_argumento(Arg_a_tratar, Resultado,Vars_existenciales_aux,Vars_existenciales_par),
	arg(N,Term_aux, Resultado),
	Vars_existenciales=Vars_existenciales_par.
existenciar_predicado(P,N,Term, Term_aux, Vars_existenciales_aux, Vars_existenciales):-
	P<N,
	arg(P,Term,Arg_a_tratar),
	tratar_argumento(Arg_a_tratar, Resultado, Vars_existenciales_aux,Vars_existenciales_par),
	arg(P,Term_aux, Resultado),
        P1 is P+1,
        existenciar_predicado(P1,N,Term,Term_aux,Vars_existenciales_par,Vars_existenciales).

%tratar_argumento(Arg,Sol,Vars_existenciales_aux,Vars_existenciales) Si Arg es de la forma fA(X), Sol toma el valor de X 
%                eliminando la cuantificacion universal fA. Si Arg es una variable no hay cambios. Las variables
%                modificadas se almacenan en Vars_existenciales.
%
%tratar_argumento(Arg,Sol,Vars_existenciales_aux,Vars_existenciales) If Arg is something like fA(X), Sol will take the 
%                value of X, eliminating the universal quantification fA. If Arg is a variable there is no change. 
%                The variables modificated are stored in Vars_existenciales.
tratar_argumento(Arg,Sol,Vars_existenciales,Vars_existenciales):- 
	var(Arg),!,
	Sol=Arg.
tratar_argumento(Arg,Sol,Vars_existenciales_aux,Vars_existenciales):- 
	Arg=fA(X),!,
	append([X],Vars_existenciales_aux,Vars_existenciales),
	Sol=X.
tratar_argumento(Arg,Arg,Vars_existenciales,Vars_existenciales):-
	ground(Arg),!.
tratar_argumento(Arg,Sol,Vars_existenciales_aux,Vars_existenciales):-
	% caso en que es una lista
        functor(Arg,'.',_),!,
	existenciar_lista(Arg,X,Vars_existenciales_aux,Vars_existenciales_par),
	Sol=X,
	Vars_existenciales=Vars_existenciales_par.
tratar_argumento(Arg,Sol,Vars_existenciales_aux,Vars_existenciales):-
	% caso de predicado
	functor(Arg,Pred,N),!,
	functor(Term_final,Pred,N),
	existenciar_predicado(1,N,Arg,Term_final,Vars_existenciales_aux, Vars_existenciales_par),
	Sol=Term_final,
	Vars_existenciales=Vars_existenciales_par.

buscar_en_parte_izq_lista([],_,[]).
buscar_en_parte_izq_lista([X|RestoV], Lista_a_evaluar, Sol):-
	buscar_en_parte_izq(X,Lista_a_evaluar),!,
	buscar_en_parte_izq_lista(RestoV, Lista_a_evaluar,Sol).
buscar_en_parte_izq_lista([X|Resto], Lista_a_evaluar, [X|Sol]):-
	buscar_en_parte_izq_lista(Resto,Lista_a_evaluar,Sol).

% buscar_en_parte_izq(Var,Lista) Lista es una lista cuyos elementos son de la forma (X=Y), el predicado tiene exito cuando
%                                Var=X en, al menos, una de las igualdades de Lista.
%
%buscar_en_parte_izq(Var,Lista) Lista is a list which elements are in the form (X=Y), this predicate is successful when
%                                Var=X in, atleast, one of the equalities of List.
buscar_en_parte_izq(_,[]):-fail,!.
buscar_en_parte_izq(Var,[(X=_Y)|_Resto]):-
	memberchk(Var,[X]),!.
buscar_en_parte_izq(Var,[_X|Resto]):-
	buscar_en_parte_izq(Var,Resto).

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

obtener_igualdades_aux(_GVars,[],[]).
obtener_igualdades_aux(GVars,[(X=Y)|R],[(X=Y)|RSol]):-
	var(Y),
	memberchk(X,GVars),
	\+memberchk(Y,GVars),!,
	obtener_igualdades_aux(GVars,R,RSol).
obtener_igualdades_aux(GVars,[_P|R],RSol):-
	obtener_igualdades_aux(GVars,R,RSol).
	


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

combine_sols([],[],_, VDetach, VDetach_final, V_existenciales, V_existenciales_final):- 
	VDetach_final= VDetach,
	V_existenciales_final=V_existenciales.
combine_sols([Sol|LSols],[NSol|NLSols],GVars,VDetach_aux, VDetach_final, V_existenciales_aux, V_existenciales_final):-
	negate(Sol,NSol,GVars,VDetach_par,V_existenciales_par),
	union(VDetach_par,VDetach_aux,VDetach_aux,VDetach),
	union(V_existenciales_par,V_existenciales_aux,V_existenciales_aux,V_existenciales),
	combine_sols(LSols,NLSols,GVars,VDetach, VDetach_final, V_existenciales, V_existenciales_final).

% negate(Sol,Neg, Negate va cogiendo una igualdad de cada solucion para conseguir hacer 
%                  el disy_to_conj pero con backtracking, para ello utiliza el member/3. Cada 
%                  igualdad la niega creando asi una de las soluciones de la negacion.
%
% negate(Sol,Neg), Negate take one equality from each solution to get the same result that
%                  disy_to_conj but using backtracking, for that it uses member/3. Each
%                  equality is negated creating one of the final negation solution.

negate(Sol,Neg,GVars, VDetach,V_existenciales):-
	member(Eq,Sol),
	negate_analize(Eq,Neg,GVars, VDetach,V_existenciales).
        

% negate_analize(Eq,Neg), Cada igualdad X=Y puede tener cuatro casos. X e Y no tienen 
%   atributo. X tiene at pero Y no. Y tiene at pero X no. Ambas tiene atributo. negate_analize 
%   trata estos 4 casos. 
%
% negate_analize(Eq,Neg), Each equality X=Y can be managed with four steps. X and Y dont have 
%   attribute. X has at but not Y. Y has at but not X. Both of them have at. negate_annalize
%   takes in account those four steps.

negate_analize(X=Y,Neg,_GVars, _VDetach,_V_existenciales):-
	var(Y),
        \+get_attribute(X,formula(X,_AttributeX)),
	\+get_attribute(Y,formula(Y,_AttributeY)),!,
	Neg=(dist(X,Y)).
negate_analize(X=Y,Neg,_GVars, _VDetach,_V_existenciales):-
	ground(Y),!,
        Neg=(dist(X,Y)).
negate_analize(X=Y,Neg,_GVars, _VDetach,V_existenciales):-
	nonvar(Y),!,
	put_existencial(Y,Y_final,V_existenciales),
	Neg=(dist(X,Y_final)).
negate_analize(X=Y,Neg,_GVars, VDetach,V_existenciales):-
	get_attribute(X,formula(X,AttributeX)),
	\+get_attribute(Y,formula(Y,_AttributeY)),!,
	negate_at(AttributeX,Neg,_GVars,[],VDetach,[],V_existenciales).
negate_analize(X=Y,Neg,_GVars, VDetach,V_existenciales):-
	get_attribute(Y,formula(Y,AttributeY)),
	\+get_attribute(X,formula(X,_AttributeX)),!,
	negate_at(AttributeY,Neg_aux,_GVars,[], VDetach,[],V_existenciales),
	append(Neg_aux,[(X=Y)],Neg).
negate_analize(X=Y,Neg,_GVars, VDetach, V_existenciales):-
	get_attribute(X,formula(X,AttributeX)),
	get_attribute(Y,formula(Y,AttributeY)),!,
	negate_at(AttributeX,NegX,_GVars,[],VDetachX,[],V_existencialesX),
	negate_at(AttributeY,NegY,_GVars,[],VDetachY,[],V_existencialesY),
	append(VDetachX, VDetachY,VDetach),
	append(V_existencialesX, V_existencialesY,V_existenciales),
	append(NegX,NegY,Neg_aux),
	append(Neg_aux,[(X=Y)],Neg).

% negate_at(Lista_eq,Lista_Sol), Al igual que negate va cogiendo miembro a mienbro de cada 
%     conjuncion para hacer el disy_to_conj pero con BT. Cada miembro es una desigualdad que 
%     afirma.
%
% negate_at(Lista_eq,Lista_Sol), its function is like negate. Each member form the lists is
%     a disequality which is affirmmed.

negate_at([],[],_,VDetach, VDetach_final,V_existenciales,V_existenciales_final):- 
	VDetach_final=VDetach, 
	V_existenciales_final=V_existenciales.
negate_at([Conj|Disy],[NSol|NResto],GVars, VDetach_aux, VDetach_final,V_existenciales_aux,V_existenciales_final):-
	negate_conj(Conj,NSol,GVars,[], VDetach_par,[],V_existenciales_par),
	union(V_existenciales_par,V_existenciales_aux,V_existenciales_aux,V_existenciales),
	union(VDetach_par,VDetach_aux,VDetach_aux,VDetach),
	negate_at(Disy,NResto,GVars,VDetach,VDetach_final,V_existenciales,V_existenciales_final).

% negate_conj(Conj,Sol), Transforma una desigualdad X/Y, perteneciente a Conj, en X=Y. Se utiliza
%      member/3 para aprovechar el BT.
%
% negate_conj(Conj,Sol), Turns a disequiality X/Y, which belongs to Conj, into X=Y. member/3 is 
%      used to take adventage of BT.
negate_conj(Conj,Sol,_, VDetach_aux, VDetach,V_existenciales_aux,V_existenciales):-
	member(Term,Conj),
	negate_conj_aux(Term, Sol,_, VDetach_aux, VDetach, V_existenciales_par),
	append(V_existenciales_aux, V_existenciales_par, V_existenciales).

negate_conj_aux(X/Y,X=Y,_GVars, VDetach_aux, VDetach,_V_existenciales):-
	var(Y),!,
	append(VDetach_aux,[X,Y],VDetach_aux2),
	VDetach=VDetach_aux2.
negate_conj_aux(X/Y,X=Y,_GVars, _VDetach_aux, _VDetach,_V_existenciales):-
	ground(Y),!.
negate_conj_aux(X/Y,X=Y_final,_GVars, VDetach_aux, VDetach,V_existenciales):-
		% Aqui no hay que diferenciar entre listas y predicados
        nonvar(Y),!,
	put_existencial(Y,Y_final,V_existenciales),
	varset(Y_final,V_totales),
	difference(V_totales,V_existenciales,V_universales),
	append(VDetach_aux,[X],VDetach_aux2),
	append(VDetach_aux2,V_universales,VDetach_aux3),
	VDetach=VDetach_aux3.

%eliminar_igualdades_inutiles(Lista_igualdades, LNoGVars1, Sol). Si existe un igualdad de 
%                  Lista_igualdades de la forma X=Y, si Y no tiene atributo y no pertenece 
%                  a LNoGVars1 se elimina de Lista_Igualdades, obteniendo al final esa lista 
%                  en Sol
%
%eliminar_igualdades_inutiles(Lista_igualdades, LNoGVars1, Sol). If a equality with form X=Y
%                  from Lista_igualdades exists, if Y hasnt got at and Y doesnt belong to 
%                  LNoGVars1, the equality is removed from Lista_Igualdades, obtaining that
%                  list in Sol.

eliminar_igualdades_inutiles([],_GVars,[]).
eliminar_igualdades_inutiles([(_X=Y)|RestoI],GVars,Sol):-
	var(Y),
        \+memberchk(Y,GVars),
	\+get_attribute(Y,_F),!,
	eliminar_igualdades_inutiles(RestoI,GVars,Sol).
eliminar_igualdades_inutiles([(X=Y)|RestoI],GVars,[(X=Y)|RestoSol]):-
	eliminar_igualdades_inutiles(RestoI,GVars,RestoSol).
eliminar_igualdades_inutiles([dist(X,Y)|RestoI],GVars,[dist(X,Y)|RestoSol]):-
	eliminar_igualdades_inutiles(RestoI,GVars,RestoSol).
		
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
% con esas desigualdades pasadas a igualdades.
%
%
%
desig_to_ig([],[]).
desig_to_ig([(X/Y)|Resto],[(X=Y)|RSol]):-
	desig_to_ig(Resto,RSol).

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


combine_each([]).
combine_each([Sol|Rest]):-
	member(Sol1,[Sol|Rest]),
        call_all(Sol1).
	
% call_all(L) calls all the subgoals of the list L
call_all([]).
call_all([G|L]):-
	call(G),
	call_all(L).







