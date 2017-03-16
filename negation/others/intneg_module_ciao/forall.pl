%:- module(forall, [forall/4],[]).
:- module(forall,_,[]).
%:- module(forall,_,[.(intneg)]).

% To be able to call Pred from forall
:- meta_predicate forall(_,goal).
:- meta_predicate forall(_,goal,_,_).
:- use_module(library('intneg/dist'),[dist/2]).
:- use_module(library(lists),[append/3,length/2]).
%:- use_module(library(metaterms),[varset/2]).
:- use_module(library(terms_vars),[varset/2]).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            IMPLEMENTACION DEL CUANTIFICADOR UNIVERSAL               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

forall(Vars,Pred):- forall(Vars,Pred,7,_,true).
%forall(Vars,Pred,P,R):- forall(Vars,Pred,P,_,R).

% A las variables no instanciadas en elementos instanciados del dominio 
% se les asigna una constante distinta de todas las del programa.
% Esa constante sera de la forma $(n) siendo n un numero natural.
% No se adopta una sola constante debido a que entonces puede inferirse
% una igualdad entre variables no instanciadas que no seria correcta

% forall(LVars,Pred) tiene exito si encuentra un recubrimiento 
% (del dominio de valores de la lista de variables LVar) para el que 
% Pred tenga exito con todas las soluciones que compongan dicho 
% recubrimiento. 
% Si encuentra una solucion que haga fallar a Pred acaba.
% Si no encontrara ni una cosa ni otra, no acaba.
% El grado de instanciacion de los recubrimientos que se van generando
% va en aumento en el proceso de generacion de sucesivos recubrimientos
forall(LVars,Pred,Prof,linear,Res):- % profundidad,estrategia,resultado 1/6/01
        length(LVars,N),
        solucion_inicial(N,Solucion),
        forall3(LVars,Pred,[Solucion],Prof,Res),!. % si aux quitar linear
forall(LVars,Pred,Prof,Estr,Res):-
	length(LVars,N),
        solucion_inicial(N,Solucion),
	forall3aux([cov([Solucion],1)],LVars,Pred,Prof,Estr,Res),!.


forall3(LVars,Pred,LRecViejo,Prof,Res):- % prof. estr. res. 1/6/01
        verifica_recubrimiento(LRecViejo,LVars,Pred,X,LRec),
        (X==indeterminado ->
	 Prof1 is Prof-1,
	 (Prof1==0 ->
	  Res=X
	 ;
	  nuevo_recubrimiento(LRec,LRecNuevo),
	  forall3(LVars,Pred,LRecNuevo,Prof1,Res) % si aux quitar linear
	  %Res=Y
	 )
	;
	 Res=X
	).

forall3aux([],_LVars,_Pred,_Prof,_Estr,indeterminado).
forall3aux([Cov|RC],LVars,Pred,Prof,Estr,Res):-
	Cov=cov(C,ProfC),
	verifica_recubrimiento(C,LVars,Pred,X,_LRec),
	(X==indeterminado ->
	 (ProfC==Prof ->
	  forall3aux(RC,LVars,Pred,Prof,Estr,Res)
	 ;
	  hijos(C,ProfC,CH),
	  (Estr==depth ->
	   append(CH,RC,NewCoverings)
	  ;
	   append(RC,CH,NewCoverings)   
	  ),
	  forall3aux(NewCoverings,LVars,Pred,Prof,Estr,Res)
	 )
	;
	 Res=X
	).

% sólo tiene hijo de primera unif del cov
% hacer q tenga de todas las unifs: hijos 1ª unif append con RC y
% pegar 1ª unif a cada hijo de RC
%hijos(cov([X|_],Prof),Solucion):- 
%	llamarhijos([],X,Prof,Sol),
%	anyadirprof(Sol,Prof,Solucion).

hijos(C,Prof,Solucion):-
	llamarhijos([],C,Sol),
	anyadirprof(Sol,Prof,Solucion).

%llamarhijos(_Antes,[],_Prof,[]).
%llamarhijos(Antes,[U|RU],Prof,[Lista1|Lista2]):-
%	hijosunif(U,Prof,Hijos1),
%	append(Antes,Hijos1,Sol1), %NO, no se pone cabeza, se hace appendeach
	%desdobla_valor(Hijos1,Antes,Sol1),
%	append(Antes,[U],Antes2),	
%	llamarhijos(Antes2,RU,Prof,Lista2),
%	append(Sol1,RU,Lista1).

llamarhijos(_Antes,[],[]).
llamarhijos(Antes,[U|RU],Hijos):-
	hijosunif(U,Hijos1),
	pega_tras(Hijos1,RU,HijosTras),
	pega_antes(HijosTras,Antes,AntesHijosTras),
	append(Antes,[U],Antes2),	
	llamarhijos(Antes2,RU,Lista2),
	append(AntesHijosTras,Lista2,Hijos).

pega_tras([],_Cov,[]).
pega_tras([C|RC],Cov,[NC|NRC]):-
	append(C,Cov,NC),
	pega_tras(RC,Cov,NRC).

pega_antes([],_Cov,[]).
pega_antes([C|RC],Cov,[NC|NRC]):-
	append(Cov,C,NC),
	pega_antes(RC,Cov,NRC).

hijosunif([],[]).
hijosunif(Unificacion,Solucion):- 
	lista_dolar(Unificacion,Listadolar),
	covering_aux(Listadolar,Unificacion,Solucion).

anyadirprof([],_,[]).
anyadirprof([X|Resto],Prof,[cov(X,I)|Lista2]):-
	I is Prof+1,
	anyadirprof(Resto,Prof,Lista2).

% hay q sacar la máxima N de la unif y dársela a solución_básica para crear una
% lista de ctes+cres instanciados en M distintas y mayores que N	
covering_aux([],_,[]).
covering_aux([X|Resto],Unificacion,[Lista1|Lista2]):- 
	%sustituye(Unificacion,X,[0,s(X)],Lista1),
	cte_maxima(Unificacion,N),
	solucion_basica(List,N),
	sustituye(Unificacion,X,List,Lista1),
	covering_aux(Resto,Unificacion,Lista2).

sustituye(_,_,[],[]).
sustituye(Unificacion,X,[Y|Ys],[Lista1|Lista2]):- 
	sustituir_args_skol(Unificacion,X,Y,Lista1),
	sustituye(Unificacion,X,Ys,Lista2).

%sustituye([$(1),p($(2))],$(2),[0,S($(2))],Salida)
%Salida = [[$(1),p(0)],[$(1),p(s($(2)))]] ?

% Sustituir_dolar(Term,Dolar,Valor,Term1), devuelve en Term1 el predicado Term en elque se % sustituye el término Dolar (si existe) por Valor.

%eje.-) sustituir_dolar((p(X,s($(2))),$(2),5,Salida).
%salida=p(X,s(5)); 
sustituir_skol(Term,Skol,Valor,Valor):-
	Term==Skol,!.
sustituir_skol(Term,Skol,Valor,Term1):-
        Term=..[Functor|Args],
        sustituir_args_skol(Args,Skol,Valor,Args1),!,
        Term1=..[Functor|Args1].
sustituir_skol(Term,Skol,_Valor,Term):-
	Term\==Skol,!.

sustituir_args_skol([],_Skol,_Valor,[]).
sustituir_args_skol([Arg|Resto],Skol,Valor,[Arg1|Resto1]):-
        sustituir_skol(Arg,Skol,Valor,Arg1),
        sustituir_args_skol(Resto,Skol,Valor,Resto1).

lista_dolar([],[]).
lista_dolar([Term|Resto],Lista):- 
	lista_termino(Term,Lista1),
	lista_dolar(Resto,Lista2),	
	append(Lista1,Lista2,Lista).

lista_termino(Term,[]):- 
	es_ground(Term),!.
lista_termino(Term,[]):-
	var(Term),!.
lista_termino($(N),[$(N)]):- !.
lista_termino(Term,Lista):-
	Term=..[_Functor|Args],
	lista_dolar(Args,Lista).


% solucion_inicial(N,Solucion) devuelve en Solucion la solucion mas general
% que se puede obtener para N variables. Estara formada por la lista de N 
% constantes (que no tienen por que ser iguales) y que no seran constantes
% del dominio.
solucion_inicial(1,[$(1)]).
solucion_inicial(N,Solucion):-
        N>1,
        N1 is N-1,
        solucion_inicial(N1,Solucion1),
        append(Solucion1,[$(N)],Solucion).


% verifica_recubrimiento(LRecViejo,LVars,Pred,X,LRec) comprueba que el predicado
% Pred tiene exito para todos los elementos de un recubrimiento de soluciones
% de la lista de variables LVars.
% Devolvera en X un: exito         si Pred tiene exito en todo el recubrimiento
%                    fallo         si Pred falla para alguna solucion ground 
%                    indeterminado si Pred falla para alguna solucion
%                    no ground
% En LRec devuelve el mismo recubrimiento LRecViejo solo que los
% elementos que ya se sabe que verifican el predicado se dejan al
% final para expandir en futuras llamadas a nuevo_recubrimiento/2 los
% que realmente estan indeterminados
verifica_recubrimiento(LRecViejo,LVars,Pred,X,LRec):-
	Pred=..[;,Pred1,PredElse],
	nonvar(Pred1),
	Pred1=..[->,PredCond,PredThen],!,
	verifica_recubrimiento(LRecViejo,LVars,PredCond,XCond,LRecCond),
	(XCond=true ->
	    verifica_recubrimiento(LRecCond,LVars,PredThen,X,LRec)
	;
	    (XCond=fail ->
		verifica_recubrimiento(LRecCond,LVars,PredElse,X,LRec)
	    ;
	        X=XCond, % Es decir: X=indeterminado
		LRec=LRecCond
	    )
	).
verifica_recubrimiento([],_LVars,_Pred,true,_LRec):-!.
verifica_recubrimiento([LSolucion|Resto],LVars,Pred,X,LRec):-
	renombrar_resto_variables(Pred,LVars,Pred1),
        sustituir_predicado(LVars,LSolucion,Pred1,Pred2),
        call(Pred2),
        verifica_recubrimiento(Resto,LVars,Pred,X,RestoRec),!,
	append(RestoRec,[LSolucion],LRec).
verifica_recubrimiento([LSolucion|_Resto],_LVars,_Pred,fail,_LRec):-
        es_ground(LSolucion),!.
verifica_recubrimiento([LSol|Resto],LVars,Pred,indeterminado,[LSol|Resto]):-
	varset(Pred,VarsPred),% Si Pred tiene alguna var de
	member(X,LVars),	      % LVars entonces es indeterminado
	member(X,VarsPred),!.
% Si Pred no tiene ninguna var de LVars entonces es fallo.
verifica_recubrimiento([_LSol|_Resto],_LVars,_Pred,fail,_LRec).


% renombrar_resto_variables(Pred,LVars,Pred1) devuelve en Pred1 el
% predicado resultante de renombrar el el predicado Pred todas las
% variables que no estan en la lista LVars
renombrar_resto_variables(GroundTerm,_LVars,GroundTerm):-
	ground(GroundTerm),!.
renombrar_resto_variables(Var,LVars,Var):-
	var(Var),
	esta_variable(Var,LVars),!.
renombrar_resto_variables(Var,_LVars,_OtraVar):-
	var(Var),!.
renombrar_resto_variables(Term,LVars,Term1):-
	Term=..[Functor|Args],
	renombrar_argumentos(Args,LVars,Args1),
	Term1=..[Functor|Args1].


% esta_variable(ListaVars,Var) tiene exito si Var es una variable de
% la lista ListaVars sin instanciar Var en ningun momento
esta_variable(Var,[Var1|_Resto]):-
	Var==Var1,!.
esta_variable(Var,[_Var1|Resto]):-
	esta_variable(Var,Resto).


% renombrar_argumentos(Args,LVars,Args1) devuelve en Args1 la lista de
% terminos resultante de aplicar el predicado
% renombrar_resto_variables/3 a todos los terminos de la lista Arg
renombrar_argumentos([],_LVars,[]).
renombrar_argumentos([Arg|Resto],LVars,[Arg1|Resto1]):-
	renombrar_resto_variables(Arg,LVars,Arg1),
	renombrar_argumentos(Resto,LVars,Resto1).


% sustituir_predicado(LVars,LSolucion,Pred,Pred1) Pred1 es el predicado
% obtenido de sustituir en el predicado Pred las variables de LVars con
% los correspondientes valores de LSolucion
sustituir_predicado([],[],Pred,Pred).
sustituir_predicado([Var|LVars],[Sol|LSoluciones],Pred,Pred2):-
        sustituir_variable(Pred,Var,Sol,Pred1),
        sustituir_predicado(LVars,LSoluciones,Pred1,Pred2).


% sustituir_variable(Term,Var,Valor,Term1) devuelve en Term1 el predicado 
% resultante de sustituir el valor Valor por la variable Var en Term
sustituir_variable(Term,Var,Valor,Valor):-
        var(Term),
        Term==Var,!.
sustituir_variable(Term,Var,_Valor,Term):-
        var(Term),
        Term\==Var,!.
sustituir_variable(Term,Var,Valor,Term1):-
        Term=..[Functor|Args],
        sustituir_args(Args,Var,Valor,Args1),!,
        Term1=..[Functor|Args1].


% sustituir_args(Args,Var,Valor,Args1) hace lo mismo que el predicado
% anterior solo que no en un solo termino, sino en una lista de ellos
sustituir_args([],_Var,_Valor,[]).
sustituir_args([Arg|Resto],Var,Valor,[Arg1|Resto1]):-
        sustituir_variable(Arg,Var,Valor,Arg1),
        sustituir_args(Resto,Var,Valor,Resto1).


% nuevo_recubrimiento(LRecViejo,LRecNuevo) obtiene la lista de soluciones
% que forman el recubrimiento LRecNuevo a partir del antiguo LRecViejo
% a¤adiendole por detras el conjunto de soluciones obtenidas de expandir
% el primer elemento de delante de LRecViejo. Este elemento se elimina
% por estar ya totalmente representado por las soluciones generadas al
% instanciarle
nuevo_recubrimiento([Solucion|Resto],LRecNuevo):-
	es_ground(Solucion),!,
	append(Resto,[Solucion],LRecViejo),
	nuevo_recubrimiento(LRecViejo,LRecNuevo).
nuevo_recubrimiento([Solucion|Resto],LRecNuevo):-
        expandir_solucion(Solucion,ListaSoluciones),
        append(ListaSoluciones,Resto,LRecNuevo).


% expandir_solucion(Solucion,ListaSoluciones) devuelve en ListaSoluciones
% el conjunto de soluciones que se obtienen al expandir el primer 
% subtermino (de izda a dcha) no instanciado completamente de la lista
% de valores Solucion. Cuando se llama a este predicado se hace con
% una lista Solucion que tiene al menos un elemento de tipo $(n).
% Solucion es una lista y ListaSoluciones es una lista de listas  
expandir_solucion([Valor|Resto],ListaSoluciones):-
        es_ground(Valor),!,
        expandir_solucion(Resto,ListaSolucionesResto),
        desdobla_valor(ListaSolucionesResto,Valor,ListaSoluciones).
expandir_solucion([Term|Resto],ListaSoluciones):-
        cte_maxima([Term|Resto],N),
        expandir_termino(Term,N,ListaTerm),
        desdobla_lista(ListaTerm,Resto,ListaSoluciones).

% es_ground(Term) tiene exito si el termino Term esta totalmente instanciado,
% es decir si no contiene singun elemento del tipo $(n) (antes descrito)
es_ground(Term):-
        sustituir_cte(Term,Term1),
        ground(Term1).

% sustituir_cte(Term,Cte,Var,Term1). 
% Term1 es el predicado resultante de sustituir la cte 
% Cte por la variable Var en el predicado Term
sustituir_cte($(_N),_Var):-!.
sustituir_cte(Cte,Cte):-
        Cte=..[Cte],!.
sustituir_cte(Term,Term1):-
        Term=..[Functor|Args],!,
        sustituir_cte_argumentos(Args,Args1),
        Term1=..[Functor|Args1].

% sustituir_cte_argumentos(Args,Args1) devuelve en Arg1 la lista de
% terminos resultante de aplicar la funcion sustituir_cte/2 a la lista
% de terminos de la lista Args
sustituir_cte_argumentos([],[]).
sustituir_cte_argumentos([Arg|Resto],[Arg1|Resto1]):-
        sustituir_cte(Arg,Arg1),
        sustituir_cte_argumentos(Resto,Resto1).
 
% desdobla_valor(ListaSolucionesResto,Valor,ListaSoluciones) devuelve en
% ListaSoluciones la lista obtenida de concatenar por delante a cada lista 
% de ListaSolucionesResto el elemento Valor 
desdobla_valor([],_Valor,[]).
desdobla_valor([Solucion|Resto],Valor,[[Valor|Solucion]|Lista1]):-
        desdobla_valor(Resto,Valor,Lista1).


% desdobla_lista(ListaTerm,Resto,ListaSoluciones) devuelve en ListaSoluciones
% la lista obtenida de concatenar por delante cada elemento de la lista 
% ListaTerm a la lista Resto
desdobla_lista([],_Resto,[]).
desdobla_lista([Elemento|Lista],Resto,[[Elemento|Resto]|Lista1]):-
        desdobla_lista(Lista,Resto,Lista1).


% cte_maxima(Solucion,N) devuelve en N el numero maximo de constante 
% universal que tiene en alguno de sus valores la lista Solucion. 
% Es decir de todos los subterminos de la forma $(Ni) que tengan sus
% elementos, N sera el mayor de ellos
cte_maxima([],0).
cte_maxima([Term|Resto],N):-
        cte_maxima_term(Term,N1),
        cte_maxima(Resto,N2),
        (N1>N2 ->
                N is N1
                ;
                N is N2
        ).


% cte_maxima_term(Term,N) devuelve en N el munero maximo de constande
% universal que tiene el termino Term
cte_maxima_term(Term,0):-
        es_ground(Term),!.
cte_maxima_term($(N),N):- !. 
cte_maxima_term(Term,N):-
        functor(Term,_Functor,Aridad),
        maxima_cte(Aridad,Term,N).  % sobraba 2º parámetro Aridad 1/6/01


% maxima_cte(I,Aridad,Term,N) devuelve en N el numero maximo de constante
% universal que se encuentra en el termino Term hasta el argumento I-esimo
% de este, que tiene Aridad argumentos.
maxima_cte(0,_Term,0):-!. % sobraba 2º parámetro Aridad 1/6/01
maxima_cte(I,Term,N):-
        arg(I,Term,Argi),
        es_ground(Argi),!,
        I1 is I-1,
        maxima_cte(I1,Term,N).
maxima_cte(I,Term,N):-
        arg(I,Term,Argi),
        cte_maxima_term(Argi,Ni),
        I1 is I-1,
        maxima_cte(I1,Term,N1),
        (Ni>N1 ->
                N=Ni
                ;
                N=N1
        ).


% expandir_termino(Term,N,ListaTerm) ListaTerm es la lista de terminos   
% obtenidos a partir de Term instanciando en todas sus posibilidades el
% primer elemento $(n) con que nos encontremos de izda a dcha
expandir_termino($(_N),Nmax,ListaTerm):-
        solucion_basica(ListaTerm,Nmax).
expandir_termino(Term,Nmax,ListaTerm):-
        expandir1(1,Nmax,Term,ListaTerm).


% solucion_basica(ListaTerm,Nmax) ListaTerm es la lista de todas las 
% constantes y constructores del dominio con argumentos no instanciados
% de firma que todas las variables que contengan sean de la forma $(N)
% siendo todas las N distintas y mayores que Nmax
solucion_basica(ListaTerm,N):-
        obtener_ctes(LCtes),
        obtener_constructores(LConstructores),
        numera_ctes_constructores(LConstructores,N,LConst),
        append(LCtes,LConst,ListaTerm).


% separar_soluciones(L1,L2). Devuelve en L2 la lista formada por las
% sublistas de un elemento que componen los elementos de L1. Es decir
% separar_soluciones([1,2,3],[[1],[2],[3]]) seria cierto
% separar_soluciones([],[]).
% separar_soluciones([Elem|Resto],[[Elem]|Resto1]):-
% 	separar_soluciones(Resto,Resto1).


% numera_ctes_constructores(LConstructores,N,LConst) LConst es la lista de 
% constructores LConstructores en la que se ha sustituido todas las 
% variables por elementos $(Ni) siendo los Ni distintos y mayores que N
numera_ctes_constructores([],_N,[]).
numera_ctes_constructores([Fun|Resto],N,[Fun1|Resto1]):-
        numera_ctes(Fun,N,N1,Fun1),
        numera_ctes_constructores(Resto,N1,Resto1).

% numera_ctes(Fun,N,N1,Fun1) Func1 es el resultado de sustituir las 
% variables del termino Fun por simbolos universales $(Ni) siendo los
% Ni numeros correlativos desde N+1 hasta N1
% El termino Fun solo tendra variables como argumentos
numera_ctes(Fun,N,Nmax,Fun1):-
        functor(Fun,Functor,Aridad),
        functor(Fun1,Functor,Aridad),
        numera1(Aridad,N,Fun,Fun1,Nmax). % sobraba 2º parámetro Aridad 1/6/01


% numera1(I,Aridad,N,Fun,Fun1,Nmax) hace lo mismo que la funcion anterior
% en el argumento I-esimo de Fun devolviendo el maximo N utilizado
numera1(0,N,_Fun,_Fun1,N):-!. % sobraba 2º parámetro Aridad 1/6/01
numera1(I,N,Fun,Fun1,Nmax):-
        N1 is N+1,
        arg(I,Fun1,$(N1)),
        I1 is I-1,
        numera1(I1,N1,Fun,Fun1,Nmax).


% expandir1(I,Nmax,Term,ListaTerm) ListaTerm es la lista de terminos   
% obtenidos a partir de Term instanciando en todas sus posibilidades el
% primer elemento $(n) con que nos encontremos de izda a dcha y en esta
% llamada se mira el I-esimo argumento para buscar ese elemento
expandir1(N,Nmax,Term,ListaTerm):-
        arg(N,Term,Arg),
        es_ground(Arg),!,
        N1 is N+1,
        expandir1(N1,Nmax,Term,ListaTerm).
expandir1(N,Nmax,Term,ListaTerm):-
        arg(N,Term,Arg),
        expandir_termino(Arg,Nmax,LArg),
        desdoblar_termino(LArg,N,Term,ListaTerm).


% desdoblar_termino(LArg,N,Term,ListaTerm) ListaTerm es la lista de 
% instancias de Term obtenidas al sustituir su N_esimo argumento por 
% cada uno de los posibles valores que puede tomar y que estan en LArg
desdoblar_termino([],_N,_Term,[]).
desdoblar_termino([Arg|LArg],N,Term,[Term1|ListaTerm]):-
        obtener_termino(Term,N,Term1),
        arg(N,Term1,Arg),
        desdoblar_termino(LArg,N,Term,ListaTerm).


% obtener_termino(Term,N,Term) devuelve en Term1 un termino identico a 
% Term excepto en el argumento N-esimo que se deja sin instanciar
obtener_termino(Term,N,Term1):-
        functor(Term,Functor,Aridad),
        functor(Term1,Functor,Aridad),
        asignar(Aridad,N,Term,Term1). % sobraba 2º parámetro Aridad 1/6/01

% asignar(I,Aridad,N,Term,Term1) devuelve en el I-esimo argumento de Term1 
% lo que hubiera en el I-esimo argumento de Term y nada si el el argumento
% numero N-esimo
asignar(0,_N,_Term,_Term1):-!. % sobraba 2º parámetro Aridad 1/6/01
asignar(N,N,Term,Term1):-!,
	I1 is N-1,
        asignar(I1,N,Term,Term1).
asignar(I,N,Term,Term1):-
        arg(I,Term,Argi),
        arg(I,Term1,Argi),
        I1 is I-1,
        asignar(I1,N,Term,Term1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%            PRUEBAS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

obtener_ctes([0]).
obtener_constructores([s(_)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
not__member(_X,[]).
not__member(X,[Y|L]):-
        dist(X,Y),
        not__member(X,L).

impar1(s(0)).
impar1(s(s(X))):-
        impar1(X).

impar2(X):- X=s(0).
impar2(X):-
        X=s(s(Y)),
        impar2(Y).

numero(0).
numero(s(X)):-
        numero(X).

menor(0,s(_X)).
menor(s(X),s(Y)):-
        menor(X,Y).

member11(X,[X|_Xs]).
member11(X,[_Y|L]):-
        member11(X,L).

member2(X,[Y|_L]):-
        X=Y.
member2(X,[_Y|L]):-
        member2(X,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p(0).
p(s(0)).
p(s(s(_X))).

q(0).
q(s(s(_X))).

a(X):-b(C).
b(x2).

suma(0,X,X).
suma(X,0,X).
suma(s(X),Y,s(Z)):-
	suma(X,Y,Z).

par(0).
par(s(s(X))):-
        par(X).

num1(0).
num1(s(_X)).

num2(0,_).
num2(s(0),_).
num2(s(s(_)),_).

disjunto([],_).
disjunto([X|Xs],Ys):-
	no_incluye(Ys,X),
	disjunto(Xs,Ys).

no_incluye([],_).
no_incluye([Y|Ys],X):-
	dist(Y,X),
	no_incluye(Ys,X).

a(a):- b(Y1).
b(X2):- c(Y2).
c(X3):- d(a).
d(X4):- e(X4).
e(X5):- f(X5).
f(X6):- g(X6).
g(X7):- h(X7).
h(salva).
h(s(_)).

pru(Y,true).
pru(X,false).

int(a(X1),R2):-forall([X],int(b(X),R2),1,R2).
int(b(0),true).
int(b(s(_)),fail).
