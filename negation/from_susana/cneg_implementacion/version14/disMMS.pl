%:- module(dist, [dist/2,              
% 	         detach_att_if_needed/1,    
%	         put_attribute/2,           
% 		 actualizar_formula/1,          
%		 eliminar_repetidas/2,          
%		 sustituir/4,                   
%		 conjuncion/3],[]).             

% MMS Last updated on 4/3/01        
:- module(dist,_).                      % MMS Para pruebas exportar todos

:- use_module(library(lists),[append/3]).
:- use_module(library(idlists),[memberchk/2]).
% :- use_module(library(metaterms),[varset/2]). % OJO YA NO SE USA VARSET
% :- use_module(formulas,[term_vars/2]). % MMS obsoleto no existe term_vars/2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%      MODULO FORMULAS      %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Modulo que implementa mediante variables con atributo
% la desigualdad entre terminos y como expresarlo a
% traves de disyuncion de conjunciones de desigualdades
% entre terminos que debe satisfacer cada variable.
% Esta implementacion sirve para variables con dominios
% de valores finitos.

% El atributo formula representa true,fail o una disyuncion de
% conjunciones de desigualdades entre terminos.
% :- attribute formula/1.

% put_attribute(Var,Atributo) liga el atributo Atributo a la
% variable Var si lo tenia lo actualiza y si no tenia lo crea
% nuevo y se lo asigna
put_attribute(Var,Formula) :- 
        get_attribute(Var,_FormulaOld),!, % Si la variable tiene atributo
        update_attribute(Var,Formula).
put_attribute(Var,Formula) :- 
        attach_attribute(Var,Formula).

% La implementacion de formula sera una lista cuyos elementos
% seran las disyunciones de la formula.
% Cada una de las disyunciones se implementan como listas cuyos
% elementos seran las desigualdades entre terminos.
% Las desigualdades entre terminos se implementan con el
% predicado / de aridad 2.
% Ej: ((A1 /\ A2 /\ A3) \/ (B1 /\ B2) \/ C1 ) seria
% [ [A1,A2,A3], [B1,B2], [C1] ] siendo cada uno de los
% elementos A1,A2,A3,B1,B2 y C1 desigualdades con la forma
% Termino1/Termino2

% Se introduciran directamente restricciones tipo formula a una
% variable con el predicado cumple(X,Formula)

% verify_attributes(Atri,Valor) asigna el valor Valor que no es
% una variable a la variable de atributo Atri y hace las
% operaciones necesarias de verificación
%:- multifile verify_attribute/2. RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRESTAURAR
% Esta version esta bien pero no funciona en Ciao de momento
%verify_attribute(formula(Var,FormVar),Otra):-
%        cumple_cond(Var,Otra, FormVar),
%        sustituir(FormVar,Var,Otra,FormVarUnif),
%        simpl_formula1(FormVarUnif,FormFinal),
%        varset(FormVar,Vars),
%        varset(FormFinal,VarsFinal),
%        agregar_final(Vars,VarsFinal,FormFinal),
%        (get_attribute(Var,_FormulaOld) -> % Si la variable tiene atributo
%	         detach_attribute(Var), % Se elimina el atributo
%		 Var=Otra
%	; % Si no tiene atributo no se elimina (claro)
%	         Var=Otra),
%	simpl_formula1(FormFinal,FormSimpl), % Para evitar repetidos
%	agregar_formula(FormSimpl).
% Versión que funciona en Ciao de momento
verify_attribute(formula(Var,FormVar),Otra):- % ?NECES. RENAME TRAS CADA SIMPLFMLA1
        cumple_cond(Var,Otra,FormVar),
        sustituir(FormVar,Var,Otra,FormVarUnif),
        simpl_formula1(FormVarUnif,FormSimp), % rename fA
	renombrar_universales(FormSimp,FormFinal), % rename fA(Var) -> fA(_New)
        %varset(FormVar,Vars), % MMS sólo agregar a las variables libres
        %varset(FormFinal,VarsFinal), % MMS
	var_libres(FormVar,Vars), % MMS ahora sólo se agrega a var. libres
	var_libres(FormFinal,VarsFinal), % MMS
        agregar_final(Vars,VarsFinal,FormFinal),

        detach_att_if_needed(Var),

        Var=Otra,
        simpl_formula1(FormFinal,FormSimpl), % Para evitar repetidos
        agregar_formula(FormSimpl).

detach_att_if_needed(Var) :-
        get_attribute(Var,_), !,
        detach_attribute(Var).
detach_att_if_needed(_).

% combine_attributes(Atri1,Atri2) gestiona la unificación entre
%  dos variables de atributos Atri1 y Atri2 respectivamente
%:- multifile combine_attributes/2. RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRESTAURAR
combine_attributes(formula(Var,FormVar),formula(Otra,FormOtra)):-
        sustituir(FormVar,Var,Otra,FormVarUnif), 
        sustituir(FormOtra,Var,Otra,FormOtraUnif),
        conjuncion(FormOtraUnif,FormVarUnif,FormConj),
        simpl_formula1(FormConj,FormSimp), % rename fA
	renombrar_universales(FormSimp,FormFinal), % MMS rename fA
        FormFinal \== fail,
        %varset(FormConj,Variables), % MMS se actualiza sólo atributo de var libres
	var_libres(FormConj,Variables), % MMS sólo var libres
        detach_attribute(Var),
        detach_attribute(Otra),
        Var=Otra,
        actualizar2(Variables,FormFinal).

% Comprueba que la formula del tercer argumento es valida o
% se hace verdadera asignandole a la variable del primer
% argumento el valor, ya instanciado, del segundo argumento
cumple_cond(Var,Valor, Form):-
        sustituir(Form,Var,Valor,Form1),
        simpl_formula1(Form1,FormFinal),
        FormFinal\==fail.

% sustituir(Form,Var,Valor,Form1). Form1 es el termino
% resultante de sustituir el valor (o variable) Valor por
% variable Var en el termino Form
sustituir(Term,Var,Valor,Valor):-
     var(Term),
     Term==Var,!.
sustituir(Term,Var,_Valor,Term):-
     var(Term),
     Term\==Var,!.
sustituir(Term,_Var,_Valor,Term):- % MMS para no sustituir vars. cuantificadas
     Term=fA(V),
     var(V),!.
sustituir(Term,Var,Valor,Term1):-
     Term=..[Functor|Args],
     sustituir_argumentos(Args,Var,Valor,Args1),
     Term1=..[Functor|Args1].

sustituir_argumentos([],_,_,[]).
sustituir_argumentos([Arg|Resto],Var,Valor,[Arg1|Resto1]):-
     sustituir(Arg,Var,Valor,Arg1),
     sustituir_argumentos(Resto,Var,Valor,Resto1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% COMENTARIO DE SICSTUS  %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% attribute_goal(Var, Atributo)
%attribute_goal(Var, cumple(Var,Form1)) :-
%       get_attribute(Var, formula(Form)),
%       legible(Form,Form1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% SIN IMPLEMENTAR %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- multifile portray_attribute/2. 
:- import(write, [print/1]).

% portray_attribute(Atributo) indica como se representan
% los atributos
%portray_attribute(cumple(Var,Form1),_) :-
%       get_attribute(Var, formula(Form)),
%       legible(Form,Form1),
%       print(Form1).

%:- multifile portray/1.
% 
%portray_attribute(cumple(Var,Form1)) :-
%       get_attribute(Var, formula(Form)),
%       legible(Form,Form1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Da la representacion legible de una formula
legible(true,true).
legible(fail,fail).
legible([Conj],Conj1):- !,
                legible_conj(Conj,Conj1).
legible([Conj|Resto],(Conj1;Resto1)):-
                legible_conj(Conj,Conj1),
                legible(Resto,Resto1).

legible_conj([Desig],Desig). % MMS ¿Aquí convendría cortar pq no indiza?
				% ¿y comprobar :- legible_desig(Desig,Desig1)?
legible_conj([Desig|Resto],(Desig1,Resto1) ):-
                legible_desig(Desig,Desig1),
                legible_conj(Resto,Resto1).

legible_desig(X/Y,X/Y).

% La variable Var debe satisfacer la formula Form que
% tiene el formato antes especificado en su definicion
cumple(Var,Form):-
                var(Form),!,
                get_attribute(Var,formula(Var,Form)).
cumple(Var,Form):-
                simpl_formula1(Form,FormSimp), % rename fA
		renombrar_universales(FormSimp,FormFinal), % MMS rename fA
                % put_attribute(VarAux, formula(VarAux,FormFinal)), % MMS attach
		attach_attribute(VarAux, formula(VarAux,FormFinal)), % MMS 23/11/00
                Var=VarAux.  

% conjuncion(F1,F2,F3). F3 es la conjuncion de F1 y F2
% siendo F1,F2 y F3 formulas.
conjuncion(F1,F2,F2):-
                var(F1),!.
conjuncion(F1,F2,F1):-
                var(F2),!.
conjuncion([],_F2,[]):-!.
conjuncion(true,F2,F2):-!.
conjuncion(fail,_F2,fail):-!.
conjuncion(F1,true,F1):-!.
conjuncion(_F1,fail,fail):-!.
conjuncion([Conj|F1],F2,F5):-
        distributiva(F2,Conj,F3),
        conjuncion(F1,F2,F4),
        append(F3,F4,F5).

% distributiva(L1,E,L2). L2 es la lista resultante de aplicarle 
% la propiedad distributiva con respecto a la conjuncion a la la
% lista L1 con respecto al elemento E.
% L1 y L2 son formulas, E es una conjuncion.
distributiva([],_E,[]).
distributiva([E1|L1],E2,[E3|L2]):-
        append(E1,E2,E3),
        distributiva(L1,E2,L2).

% simpl_formula1(F1,F2). F2 es la formula equivalente a F1
% pero simplificada y sin repeticiones
simpl_formula1([],fail):-!.
simpl_formula1(F1,F5):-
        simpl_formula(F1,F2),
        eliminar_repetidos_conj(F2,F3),
        eliminar_repetidas(F3,F4),
	simpl_fA(F4,F5). % MMS 6/2/01 ahora se incluye simplificación de forall
	%renombrar_universales(F5,L,F6).

% El segundo argumento es la formula equivalente a la formula
% del primer argumento pero simplificada
simpl_formula(true,true).
simpl_formula(fail,fail).
simpl_formula([],fail).
simpl_formula([Conj|RestoF1],F2):-
        simpl_conj(Conj,ConjF2),    
        simpl_formula1(RestoF1,RestoF2), 
        disyuncion(ConjF2,RestoF2,F2).

% El segundo argumento es la formula equivalente a la
% conjuncion del primer argumento pero simplificada
simpl_conj(true,true).
simpl_conj(fail,fail).
simpl_conj([],true).
simpl_conj([D1|C1],F1):-
        simpl_desig(D1,DSimpl),
        simpl_conj(C1,CSimpl),
        conjuncion(DSimpl,CSimpl,F1).

% disyuncion(F1,F2,F3). F3 es la disyuncion de F1 y F2
% Siendo F1, F2 y F3 formulas
disyuncion(F1,F2,F2):-var(F1),!.
disyuncion(F1,F2,F1):- var(F2),!.
disyuncion([],_F2,[]):-!.
disyuncion(true,_F2,true):-!.
disyuncion(fail,F2,F2):-!.
disyuncion(_F1,true,true):-!.
disyuncion(F1,fail,F1):-!.
disyuncion(F1,F2,F3):- append(F1,F2,F3).

% El segundo argumento es la desigualdad equivalente a la
% desigualdad del primer argumento pero simplificada
simpl_desig(Term1/_Term2,fail):-
        ground(Term1),                % Cambio el 7 Mayo 2000
        Term1=($(N)),
        number(N),!.                  % Cambio el 23 marzo 2000
simpl_desig(_Term1/Term2,fail):-
        ground(Term2),
        Term2=($(N)),
        number(N),!.
simpl_desig(Term1/Term2,fail):-  % Si los dos son ground y son iguales
        atomic(Term1),
        atomic(Term2),
        Term1==Term2,!.
simpl_desig(Term1/Term2,true):-  % Si los dos son ground y distintos
        atomic(Term1),
        atomic(Term2),
        Term1\==Term2,!.
simpl_desig(Term1/Term2,fail):-  % Si los dos son la misma variable
        var(Term1),                                     
        var(Term2),
        Term1==Term2,!.
simpl_desig(Term1/Term2,[[Term1/Term2]]):- % Si son variables distintas
        var(Term1),
        var(Term2),!.
simpl_desig(Term1/Term2,fail):- % Si ambos son fA(var) con la misma ó distinta var.
	nonvar(Term1), % MMS 23/11/00 este caso no debería aparecer NUNCA ¿no?
	nonvar(Term2),
	Term1=(fA(V)),
	Term2=(fA(W)),
	var(V),
	var(W),!.
simpl_desig(Term1/Term2,Des):- % Si el prim. es fA(var) y el seg. no
	nonvar(Term1), % MMS 23/11/00
	Term1=(fA(V)),
	var(V),!,
	simpl(Term2,V,Des).	 
simpl_desig(Term1/Term2,Des):- % Si el seg. es fA(var) y el prim. no
	nonvar(Term2), % MMS 23/11/00
	Term2=(fA(V)),
	var(V),!, 
	simpl(Term1,V,Des).
simpl_desig(Term1/Term2,true):-  % Si el segundo termino contiene al
        var(Term1),              % primero que es una variable
	var_libres([[Term2]],Lvars),
        %varset(Term2,Lvars),
        memberchk(Term1,Lvars),!.
simpl_desig(Term1/Term2,true):-  % Si el primer termino contiene al
        var(Term2),              % segundo que es una variable
	var_libres([[Term1]],Lvars), % CUIDADO AQUÍ CON EL FORMATO VARLIBRES
        %varset(Term1,Lvars),
        memberchk(Term2,Lvars),!.
simpl_desig(Term1/Term2,[[Term1/Term2]]):- % Si uno es variable y el
                                           % otro no
        var(Term1),
        nonvar(Term2),!.
simpl_desig(Term1/Term2,[[Term2/Term1]]):-
        var(Term2),
        nonvar(Term1),!.
simpl_desig(Term1/Term2,Desig):-  % Son predicados que pueden unificar
        functor(Term1,Functor,Aridad),
        functor(Term2,Functor,Aridad),!,
        Term1=..[Functor|Argumentos1],
        Term2=..[Functor|Argumentos2],
        or(Argumentos1,Argumentos2,TodasDesig),
        (se_cumple(TodasDesig) ->
                Desig=true
        ;
                Desig=TodasDesig
        ).
simpl_desig(Term1/Term2,true):-  % Son predicados no unificables 
        functor(Term1,_Functor1,_Aridad1),
        functor(Term2,_Functor2,_Aridad2),!.

% or(L1, L2, Desig) devuelve en Desig la formula de las desigualdades
% obtenidas de simplificar las desigualdades de los elementos de
% L1 Y L2 que estan situados en la misma posicion de orden
or([],[],[]).
or([Arg1|Resto1],[Arg2|Resto2],Desig):-
        simpl_desig(Arg1/Arg2,DesigArg),
        or(Resto1,Resto2,DesigResto),
        disyuncion(DesigArg,DesigResto,Desig).

% se_cumple(TodasDesig, Desig) tiene exito si TodasDesig tiene mas
% de una desigualdad de una variable dada con terminos ground y 
% distintos entre si con lo cual la disyuncion es siempre cierta.
% TodasDesig es una disyuncion de conjunciones de desigualdades.
% Pero las conjunciones son de un solo elemento.
% No hay disyunciones repetidas
se_cumple([[V/T]|Resto]):-
        ground(T),
        hay_otra(Resto,V/T),!.
se_cumple([[_V/_T]|Resto]):-
        se_cumple(Resto).

hay_otra([[V1/T1]|_Resto],V/T):-
        ground(T1),
        V==V1,
        T\==T1,!.
hay_otra([[_V1/_T1]|Resto],V/T):-
        hay_otra(Resto,V/T).

% hay_otra_conj(Conj,Formula) tiene exito si en la Formula hay
% una conjuncion equivalente a Conj
hay_otra_conj(Conj,Formula):-
        member(Conj1,Formula),
        permutacion(Conj,Conj1),!.

% permutacion(L,L1) tiene exito si una lista es la 
% permutacion de los elementos de la otra
permutacion([],[]).
permutacion([X|Xs],L):-
        quitar(L,X,Ls),
        quitar(Xs,X,Xs1),
        permutacion(Xs1,Ls).

% quitar(L,X,L1) tiene exito si L1 es equivalente a L quitandole
% todas las ocurrencias de X o equivalentes a X (args. en orden intercambiado)
quitar([],_X,[]).
quitar([Y|L],X,L1):-
	%Y==X,!, MMS 18/2/01
	equivalente(X,Y),!, % MMS para desigualdades con args. en dist. posic.
        quitar(L,X,L1).
quitar([Y|L],X,[Y|L1]):-
        quitar(L,X,L1).

% equivalente(X,Y) es cierto si X es idéntico a Y o si X e Y son el mismo functor
% con los dos últimos argumentos idénticos pero en orden intercambiado
equivalente(X,Y):- % MMS 18/2/01
	X==Y,!.
equivalente(X,Y):-
	functor(X,Fun,Ari),
	functor(Y,Fun,Ari),
	arg(Ari,X,X2),
	arg(Ari,Y,Y2),
	Ari1 is Ari-1,
	arg(Ari1,X,X1),
	arg(Ari1,Y,Y1),
	X2==Y1,
	X1==Y2.

% eliminar_repetidas(F1,F2). F2 es la formula equivalente a F1
% sin las conjunciones repetidas dentro de la formula F1
eliminar_repetidas(true,true).
eliminar_repetidas(fail,fail).
eliminar_repetidas([],[]).
eliminar_repetidas([Conj|Resto],Resto1):-
        hay_otra_conj(Conj,Resto),!,
        eliminar_repetidas(Resto,Resto1).
eliminar_repetidas([Conj|Resto],[Conj|Resto1]):-
        eliminar_repetidas(Resto,Resto1).

% is_list(Term) es cierto si Term es una lista
is_list([]).
is_list([_X|_Y]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% remove_duplicates no se usa SE PUEDE BORRAR  %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % remove_duplicates(L1, L2) L2 tiene los elementos de L1 sin repetidos 
% remove_duplicates(Lista,ListaSin):-
%       remove_duplicates2(Lista,[],ListaSin).
% 
% % remove_duplicates2(L1, L2,L3) L3 tiene los elementos de L2  mas los 
% % elementos de L1 que no están ya en L2
% remove_duplicates2([], List2, List2):- !.
% remove_duplicates2([Element|Residue], List, ListaSin) :-
%       memberchk(Element, List), !,
%       remove_duplicates2(Residue, List, Lista),
%       append(List,Lista,ListaSin).
% remove_duplicates2([Element|Residue], List, [Element|ListaSin]) :-
%       remove_duplicates2(Residue, List, Lista),
%       append(List,Lista,ListaSin).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% eliminar equivalentes es un predicado de Oscar que
%%%%% mejora la simplificacion en atributos(13/4/2000)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

member_desig(Elemento,[Cabeza|Cola]):-
        member_desig1(Cola,Cabeza,Elemento).

member_desig1(_,Cabeza,Elemento):-
        functor(Cabeza,Functor1,_Aridad),
        Cabeza=..[Functor1|Argumentos1],
        functor(Elemento,Functor2,_Aridad2),
        Elemento=..[Functor2|Argumentos2],
        permutacion(Argumentos1,Argumentos2),!. 
member_desig1([Cabeza|Cola],_OtroElemento,Elemento):-
        member_desig1(Cola,Cabeza,Elemento).
 
eliminar_equivalentes([],[]):- !.
eliminar_equivalentes([A],[A]) :- !.
eliminar_equivalentes([Primero|Resto],F) :-
     member_desig(Primero,Resto),!,
     eliminar_equivalentes(Resto,F).
eliminar_equivalentes([Primero|Resto],[Primero|F]):-
	eliminar_equivalentes(Resto,F).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% eliminar_repetidos_conj(F1,F2). F2 es la formula equivalente a F1
% sin las desigualdades repetidas dentro de cada conjuncion
eliminar_repetidos_conj(true,true).
eliminar_repetidos_conj(fail,fail).
eliminar_repetidos_conj([],[]).
eliminar_repetidos_conj([Conj|Resto],[Conj1|Resto1]):-
        (is_list(Conj) ->
                % remove_duplicates(Conj,Conj1)
                 eliminar_equivalentes(Conj,Conj1)  %%%%%%% CAMBIO Oscar (13/4/2000)
        ;
                 Conj1=Conj
        ),
        eliminar_repetidos_conj(Resto,Resto1).

% Actualiza el atributo de todas las variables que contiene 
% la formula por ella misma
% (Si la formula es fail falla)
actualizar_formula(Formula):-
        %varset(Formula,Variables),  MMS sólo se actualiza el de las var. libres
	var_libres(Formula,Variables), % MMS sólo las libres 
        actualizar2(Variables,Formula).

actualizar2([],_).
actualizar2([Var|Resto],Formula):-
        actualizar3(Var,Formula),
        actualizar2(Resto,Formula).

actualizar3(T1,Formula):-
        var(T1),!,
	actualizar4(T1,Formula).
actualizar3(T1,Form1):-
        nonvar(T1),
        functor(T1,Functor,_Aridad),!,
        T1=..[Functor|Argumentos],
        actualizar2(Argumentos,Form1).
actualizar3(_T1,_Form1).

actualizar4(T1,true):-
	detach_att_if_needed(T1). % Si la formula es true se deja sin atributo
actualizar4(T1,Formula):-
        (esta(Formula,T1) ->
                 put_attribute(T1,formula(T1,Formula))
        ;
                 detach_att_if_needed(T1)
        ).

% esta(F,V). La variable V esta en la formula F
esta(Formula,Var):-
        % varset(Formula,Variables), MMS sólo interesa saber si figura libre
	var_libres(Formula,Variables), % MMS se obtienen las var libres de fórmula
        memberchk(Var,Variables).

% Añade la formula al atributo de las variables implicadas
% Y lo borra de las variables de Vars que no están implicadas
agregar_final([],_Vars,_Formula).
agregar_final([Var|VarsFinal],Vars,Formula):-
	memberchk(Var,Vars),!, % Si la variable esta se añade la formula.
	agregar3(Var,Formula),
	agregar_final(VarsFinal,Vars,Formula).
agregar_final([Var|VarsFinal],Vars,Formula):- 
        get_attribute(Var,_FormulaOld),!, % Si tiene un atributo
	detach_attribute(Var), % Como el atributo que se quiere poner es cierto 
	% actualizar3(Var,true), % se quita el que tenia
	agregar_final(VarsFinal,Vars,Formula). % Si la variable no estaba en la formula.
agregar_final([_Var|VarsFinal],Vars,Formula):- % Si no tiene un atributo
	agregar_final(VarsFinal,Vars,Formula). % Si la variable no estaba en la formula.

% An~ade la formula al atributo de las variables implicadas
agregar_formula(true):-!.
agregar_formula(fail):-!,
        fail.
agregar_formula(Formula):-
        %varset(Formula,Variables), MMS sólo se añade a los atrib. de var. libres
	var_libres(Formula,Variables), % MMS se obtienen las var libres
        agregar2(Variables,Formula).

agregar2([],_).
agregar2([Var|Resto],Formula):-
        agregar3(Var,Formula),
        agregar2(Resto,Formula).

agregar3(T1,Form1):-
        var(T1),
        esta(Form1,T1),!,
        (get_attribute(T1,formula(T1,Form2)) ->
                conjuncion(Form1,Form2,Form3),
                simpl_formula1(Form3,FormFinal)
        ;
                FormFinal=Form1
        ),
        put_attribute(T1,formula(T1,FormFinal)).
agregar3(T1,true):-
        var(T1),!,
        (get_attribute(T1,formula(T1,Form2)) ->
                 simpl_formula1(Form2,FormFinal)
        ;
             FormFinal=true
        ),
        put_attribute(T1,formula(T1,FormFinal)).
agregar3(T1,fail):-
        var(T1),!,
        put_attribute(T1,formula(T1,fail)).
agregar3(T1,Form1):-
        nonvar(T1),
        functor(T1,Functor,_Aridad),!,
        T1=..[Functor|Argumentos],
        agregar2(Argumentos,Form1).
agregar3(_T1,_Form1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% varset que no incluye las cuantificadas universalmente
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% var_libres(F,L). La lista L contiene las variables de la fórmula F que no están
% cuantificadas universalmente.
var_libres(true,[]). 
var_libres(fail,[]). % más robusto
var_libres(F,L):-
	var_libres_f(F,[],L).

% var_libres_f(F,L1,L2). La lista L2 contiene las variables de la lista L1 más las
% de la fórmula F que no están cuantificadas universalmente y que no están en L1.
var_libres_f([],L,L).
var_libres_f([C|Cs],LI,LOO):-
	var_libres_c(C,LI,LO),
	var_libres_f(Cs,LO,LOO).

% var_libres_c(C,L1,L2). La lista L2 contiene las variables de la lista L1 más las
% de la conjunción C que no están cuantificadas universalmente y no están en L1. 
var_libres_c([],L,L).
var_libres_c([D|Ds],LI,LOO):-
	var_libres_d(D,LI,LO),
	var_libres_c(Ds,LO,LOO).

% var_libres_d(D,L1,L2). La lista L2 contiene las variables de la lista L1 más las
% de la desigualdad D que no están cuantificadas universalmente y no están en L1.
var_libres_d(D,L,LO):-
	functor(D,_Fun,Ari), 
	extrae_var_libres(D,Ari,L,LO).

% extrae_var_libres(D,Ari,L1,L2). La lista L2 es la lista L1 más las variables no 
% cuantificadas universalmente de la desigualdad D que no están en L1. 
% Ari es la aridad del functor desigualdad D.  
extrae_var_libres(_D,0,L,L):-!.
extrae_var_libres(D,Ari,L,LO):-
	arg(Ari,D,Arg),
	var(Arg),
	noesta(L,Arg),!,
	Ari1 is Ari-1,
	extrae_var_libres(D,Ari1,[Arg|L],LO).
extrae_var_libres(D,Ari,L,LO):-
	arg(Ari,D,Arg),
	nonvar(Arg),
	Arg=fA(V),
	var(V),!,
	Ari1 is Ari-1,
	extrae_var_libres(D,Ari1,L,LO).
extrae_var_libres(D,Ari,L,LOO):-
	arg(Ari,D,Arg),
	functor(Arg,_Fun,Ar),
	Ar>0,!,
	extrae_var_libres(Arg,Ar,L,LO),
	Ari1 is Ari-1,
	extrae_var_libres(D,Ari1,LO,LOO).
extrae_var_libres(D,Ari,L,LO):- 
	Ari1 is Ari-1, 
	extrae_var_libres(D,Ari1,L,LO).

% noesta(L,V). La variable V no es elemento de la lista L.
noesta([],_).
noesta([X|Xs],Y):-
	X\==Y,
	noesta(Xs,Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% simplificar los forall
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% simpl_fA(F1,F2). La fórmula F2 es el resultado de simplificar las desigualdades
% de la fórmula F1 del tipo variable cuantificada universalmente (fA) con término
% totalmente instanciado (ground). 
simpl_fA(true,true).
simpl_fA(fail,fail).
simpl_fA([],fail).
simpl_fA([C|CS],FSimpl):-
	simpl_fA_conj(C,Csimpl),
	simpl_fA(CS,CSsimpl),
	disyuncion(Csimpl,CSsimpl,FSimpl).

% simpl_fA_conj(C,F). La fórmula F es el resultado de simplificar las desigualdades
% de la conjunción C del tipo variable cuantificada universalmente (fA) con término
% totalmente instanciado (ground).
simpl_fA_conj([],[[]]). % fmla true, true; fmla fail, fail; fmla vacía, true
simpl_fA_conj([D|DS],CSimpl):- 
	simpl_fA_desig(D,Dsimpl),
	simpl_fA_conj(DS,DSsimpl),
	conjuncion(Dsimpl,DSsimpl,CSimpl).

% simpl_fA_desig(D,F). La fórmula F es el resultado de simplificar la desigualdad
% del tipo variable cuantificada universalmente (fA) con término totalmente 
% instanciado (ground) en la desigualdad D.
simpl_fA_desig(D,DS):-
	functor(D,_Fun,Ari),
	fallar_fA(D,Ari,DS).

% fallar_fA(D,Ari,F). La fórmula F es fail si D es una desigualdad del tipo 
% variable cuantificada universalmente (fA) con término totalmente instanciado
% (ground). En caso contrario F es la fórmula correspondiente a la desigualdad D.
fallar_fA(D,Ari,fail):- % fA a la derecha del functor desigualdad '/'
	arg(Ari,D,Arg),
	nonvar(Arg),
	Arg=fA(V),
	var(V),
	Ari1 is Ari-1,
	arg(Ari1,D,Arg1),
	ground(Arg1),!.
fallar_fA(D,_Ari,[[D]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                     PREDICADO   DISTINTO                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Predicado que implementa mediante variables con atributo
% la desigualdad entre terminos y como expresarlo a
% traves de disyuncion de conjunciones de desigualdades
% entre terminos que debe satisfacer cada variable.
% Esta implementacion sirve para variables con dominios
% de valores finitos.

% Incluye una desigualdad en las formulas de las 
% variables implicadas

dist(T1,T2):-
        simpl_desig(T1/T2,Formula),
	simpl_formula1(Formula,FormSimp), % MMS renombrar
	renombrar_universales(FormSimp,FormFinal), % MMS renombrar fA
        agregar_formula(FormFinal).

% eq(X,Y) unify X and Y
eq(X,Y):-
        X=Y.

% renombrar fA(X) a fA(_) contando con las repes. Fmla, lista X,AnonForX, FmlaOut

% renombrar_universales(F1,F2). F2 es la fórmula F1 con las variables cuantificadas
% universalmente renombradas a nuevas variables.
renombrar_universales(Form,NForm):-
	renombrar(Form,[],_LSust,NForm).

%renomb(Gnd,L,L,Gnd):-
%	ground(Gnd),!.
% renombrar(T1,L1,L2,T2). Si T1 es variable cuantificada universalmente, entonces T2
% es el renombrado de T1 con un nuevo nombre de variable y la lista L2 contiene los 
% renombrados que figuran en la lista L1 más el realizado a T1 en T2.
renombrar(Var,LSust,LSust,Var):-
	var(Var),!.
renombrar(Universal,LSust,NLSust,NUniversal):-
	Universal=fA(V),
	var(V),!,
	(member_sust(s(V,NombreV),LSust)->
	 NUniversal=fA(NombreV),
	 NLSust=LSust
	;
	 NUniversal=fA(_Nueva),
	 NLSust=[s(V,_Nueva)|LSust]
	).
renombrar(Term,LSust,NLSust,NTerm):-
	Term=..[F|Args],
	renombrar_args(Args,LSust,NLSust,NArgs),
	NTerm=..[F|NArgs].

renombrar_args([],LSust,LSust,[]).
renombrar_args([Arg|RArgs],LSust,NNLSust,[NArg|NRArgs]):-
	renombrar(Arg,LSust,NLSust,NArg),
	renombrar_args(RArgs,NLSust,NNLSust,NRArgs).

% member_sust(s(X,NX),L). Es cierto si en la lista L existe un elemento s(Y,NY) con
% Y un nombre de variable idéntico a X y sus renombrados respectivos NY y NX
% unifican.
member_sust(s(Univ1,V),[s(Univ2,V)|_RLSust]):-
	Univ1==Univ2,!.
member_sust(Sust,[_OtraSust|RLSust]):-
	member_sust(Sust,RLSust).

% variable, térm 2 y resultado Des
simpl(Term,V,Des):-
	nonvar(Term),
	functor(Term,_Fun,Ari),
	Ari>0,
	lista_univs(Term,Luniv),
	(Luniv\==[] -> !,
	 (noesta(Luniv,V) ->
	  Des=fail
	 ;
	  Des=true
	 )
	).
simpl(Term,V,Des):-
	Des=[[Term/fA(V)]].

% saca var univs
lista_univs(Term,Luniv):-
	lista_aux(Term,[],Luniv).

lista_aux(Term,L,LO):-
	nonvar(Term),
	Term=fA(V),
	var(V),!,
	LO=[V|L].
lista_aux(Term,L,LO):-
	nonvar(Term),
	functor(Term,_Fun,Ari),!,
	lista_arg(Term,L,LO,Ari).
lista_aux(_Term,L,L).

lista_arg(_Term,L,L,0):-!.
lista_arg(Term,L,LO,Ari):-
	arg(Ari,Term,Arg),
	lista_aux(Arg,L,LI),
	Ari1 is Ari-1,
	lista_arg(Term,LI,LO,Ari1).

