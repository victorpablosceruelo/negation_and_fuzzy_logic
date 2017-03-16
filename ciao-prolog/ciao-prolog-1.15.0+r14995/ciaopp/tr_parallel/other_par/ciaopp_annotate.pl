:- module(ciaopp_annotate,
	[
	    audg/3,
%	    escribir_grafo/1,
	    exp_audg/4,
	    fallo_par/3,
	    sub_audgs/4,
	    audg_merge/8
	],
	[]).

:- use_module(annotate, 
	[
	    vertex/4,
	    vertex_goal/3,
	    vertex_goals/3
	]).

:- use_module(library(lists), 
	[
	    append/3,
	    delete/3,
	    list_concat/2,
	    sublist/2
	]).

:- use_module(siap_annotate, 
	[
	    p_dep/5,
	    sub_udgs/4,
	    merge_dep/2,
	    udg_merge/4,
	    parallelize/2,
	    project_edges/3,
	    parallelize_nodes/3,
	    full_udg/4,
	    form_par_exp/2,
	    length_of_lists/2
	]).

:- use_module(library(idlists), [subtract/3]).

:- use_module(library(sort)).

:- include(library(hlc)).


:- op( 950, xfy,[(&),(\&)]).

%------------------------------------------------------------------------
% audg(+,+,-)
% audg(UDG,Dict,Exp)
% audg/3 - converts an unconditional dep graph into an Ciaopp expression
% UDG is graph(Vertices,Edges) and Edges is a list [(Vx,Vy)|...] where
% Dict can be used to recover a goal from the corresponding vertex
%========================================================================
%  (1) This program assumes that the given (directed) graph is closed 
%      under transitivity.
%  (2) This program doesn't take care of the case which occurs in 
%      Lemma 1 (page 6, MCC TR ACT-ST-233-89). This has to be rectified.
%   -  This has now been rectified (PBC)
%------------------------------------------------------------------------

% AUDGs for graphs with =< 3 vertices is quite straightforward... equals UDG

audg(graph([X],[]),Dict,G) :-
	vertex_goal(X,Dict,G).
audg(graph([X,Y],[]),Dict,(G1 & G2)) :-
	vertex_goals([X,Y],Dict,[G1,G2]).
audg(graph([_,_],[(X,Y)]),Dict,(G1,G2)) :-
	vertex_goals([X,Y],Dict,[G1,G2]).
audg(graph([X,Y,Z],[]),Dict,(G1 & G2 & G3)) :-
	vertex_goals([X,Y,Z],Dict,[G1,G2,G3]).
audg(graph([X,Y,Z],[(P,Q)]),Dict,Exp) :-
	( ((X \== P),(X \== Q)) ->
	    vertex_goals([P,Q,X],Dict,[G1,G2,G3])
	; ((Y \== P),(Y \== Q)) ->
	    vertex_goals([P,Q,Y],Dict,[G1,G2,G3])
	; ((Z \== P),(Z \== Q)) ->
	    vertex_goals([P,Q,Z],Dict,[G1,G2,G3])
	),
	Exp = ((G1,G2) & G3).
audg(graph([_,_,_],[(W,X),(Y,Z)]),Dict,Exp) :-
	( W == Y ->
	  vertex_goals([W,X,Z],Dict,[G1,G2,G3]),
	  Exp = (G1,(G2 & G3))
        ; X == Z ->
	  vertex_goals([W,Y,Z],Dict,[G1,G2,G3]),
	  Exp = ((G1 & G2),G3)
        ).
audg(graph([_,_,_],[(A,B),(C,D),(E,F)]),Dict,Exp) :-
	( (A == C) -> %% E and F are the same as B and D
	   vertex_goals([A,E,F],Dict,[G1,G2,G3])
	; (A == E) -> %% B and F are the same as C and D
	   vertex_goals([A,C,D],Dict,[G1,G2,G3])
	; (C == E) -> %% A and B are the same as D and F
	   vertex_goals([C,A,B],Dict,[G1,G2,G3])
	),
	Exp = (G1,G2,G3).

% A-AUDGs for graphs with >3 vertices...

audg(graph(V,[]),Dict,Exp) :-
	parallelize_nodes(V,Dict,Exp).
audg(graph(V,E),Dict,Exp) :-
%	escribir_grafo(E), % escribe el grafo correspondiente
	p_dep(E,V,P_dep,Q_edges,P_zero), 
	exp_audg(P_dep,Q_edges,Dict,Exp1), % Forma la expresion sin nodos aislados
	full_udg(P_zero,Dict,Exp1,Exp). % Añade los nodos aislados

%------------------------------------------------------------------------
% escribir_grafo(+)
% escribir_grafo(Edges)
% escribir_grafo/1
% Escribe los pares de vertices que forman el grafo
%------------------------------------------------------------------------

% escribir_grafo([]).
% escribir_grafo([X|Body]) :-
% 	X=(Y,Z),
%         Y=vertex(A,_),
%         Z=vertex(B,_),
%         inform_user(['1.- ',A]),
% 	inform_user(['2.- ',B]),
%         escribir_grafo(Body).

% escribe_grafo([],_).
% escribe_grafo([X|Body],GoalDict) :-
% 	X=edge(Y,Z,Cond),
%         Y=vertex(A,info(_,I1)),
%         Z=vertex(B,info(_,I2)),
% 	vertex_goal(Y,GoalDict,A1),
% 	vertex_goal(Z,GoalDict,B1),
%         inform_user([A,'.- ',A1,' Condic: ',I1]),
% 	inform_user([B,'.- ',B1,' Condic: ',I2]),
%         inform_user(['Cond: ',Cond]),
%         escribe_grafo(Body,GoalDict).

%------------------------------------------------------------------------
% exp_audg(+,+,+,-)
% exp_audg(P_dep,Q_edges,Dict,Exp)
%------------------------------------------------------------------------

%% Added merge_intersecting_dep/2 to handle non-void-intersecting Sets of 
%%       dependencies (PBC)
%% This deals with conditions under which Lemma 1 does not hold
%%       length_of_lists/2 creates the (singleton) Sets
%%       merge_intersecting_dep/2 groups them in such a way that Lemma 1
%%             holds thereafter

exp_audg(P_dep,Q_edges,Dict,Exp) :-
	length_of_lists(P_dep,Dep_plus_lengths), % Lista con numero de nodos p
	sort(Dep_plus_lengths,Temp1), % Ordena la lista de menor a mayor nº nodos
	merge_dep(Temp1,Temp2), % Une los pares de un mismo P con la misma longitud
	sort(Temp2,Temp3), % ordena la lista
	fallo_par(Temp3,Q_edges,L_aux), % Lista de perdidas de paralelizacion
	list_concat(L_aux,L_fallos),
	(L_fallos == [] -> % Si no hay perdida entonces algoritmo UDG
		sub_udgs(Temp3,Q_edges,Dict,Sub_udgs), 
		udg_merge(Sub_udgs,[],Dict,Udg_merge), 
		form_par_exp(Udg_merge,Exp)
	; % Si no, nuevo algoritmo AUDG
		sub_audgs(Temp3,Q_edges,Dict,Sub_udgs), 
		audg_merge(Sub_udgs,[],Dict,[],L_fallos,Udg_merge,Q_edges,[]), 
		form_par_exp(Udg_merge,Exp)
	).


%------------------------------------------------------------------------
% fallo_par(+,+,-)
% fallo_par(Lista,Q_edges,LRes)
% fallo_par/3
% Escribe una lista con todas las posibles perdidas de paralelizacion
% Lista es la lista ordenada con [(N,P,QDep)|Resto] donde N es el numero
% de elementos P que se encuentran en la lista P, y QDep son los nodos de Q
% que dependen de P.
% LRes es una lista con [(QValor,QPerd)] donde QPerd es la lista de elemntos de Q
% que esperan por un determinado QValor. QPerd no debe esperar por QValor.
%------------------------------------------------------------------------

fallo_par([(_,_,_)],_,_). % Con un elemento no pierde
fallo_par([(_,P_list,Q_list)|Rest],Q_edges,L_Fallo) :-
	fallo_par2(P_list,Q_list,Rest,Q_edges,L_Fallos1),
	( L_Fallos1 == [] ->
		fallo_par(Rest,Q_edges,L_Fallo)
	;
		list_concat(L_Fallos1,L_Fallo2),
		L_Fallo = [L_Fallo2|R_fallos],
		fallo_par(Rest,Q_edges,R_fallos)
	).

fallo_par2(_,_,[],_,_).
fallo_par2(P1_list,Q1_list,[(_,P2_list,Q2_list)|Rest],Q_edges,L_fallos) :-
	( sublist(P1_list,P2_list) ->
		revisa_dep(Q1_list,Q2_list,Q_edges,L_fallos1),
		( L_fallos1 == [] ->
			fallo_par2(P1_list,Q1_list,Rest,Q_edges,L_fallos)
		;
			L_fallos = [L_fallos1|Resto],
			fallo_par2(P1_list,Q1_list,Rest,Q_edges,Resto)
		)
	;
		fallo_par2(P1_list,Q1_list,Rest,Q_edges,L_fallos)
	).

revisa_dep([],_,_,_).
revisa_dep([Q1|R1],Q2_list,Q_edges,L_fallos) :-
	recorre_lista(Q1,Q2_list,Q_edges,L_fallos_Q1),
	( var(L_fallos_Q1) ->
		revisa_dep(R1,Q2_list,Q_edges,L_fallos)
	;
		L_fallos = [(Q1,L_fallos_Q1)|Resto],
		revisa_dep(R1,Q2_list,Q_edges,Resto)
	).
	
recorre_lista(_,[],_,_).
recorre_lista(Q1,[Q2|R2],Q_edges,L_fallos_Q1) :-
	( sublist([(Q1,Q2)],Q_edges) ->
		recorre_lista(Q1,R2,Q_edges,L_fallos_Q1)
	;
		L_fallos_Q1 = [Q2|Resto],
		recorre_lista(Q1,R2,Q_edges,Resto)
	).

%------------------------------------------------------------------------
% sub_audgs(+,+,+,-)
% sub_audgs(Deps,Q_edges,Dict,Sub_udgs)
%------------------------------------------------------------------------

sub_audgs([(N,P_list,Q_list)|Rest],Q_edges,Dict,[(N,P_list,Sub_udg)|Rest_udg]):-
	project_edges(Q_edges,Q_list,Edges),
	audg(graph(Q_list,Edges),Dict,Sub_udg),
	subtract(Q_edges,Edges,Rest_edges),
	sub_audgs(Rest,Rest_edges,Dict,Rest_udg).
sub_audgs([],_,_,[]).

%------------------------------------------------------------------------
% audg_merge(+,+,+,+,+,-,+,+)
% audg_merge(DepUdgs,IntDepUdgs,Dict,LVar,LFallos,MergedUdgs,Q_edges,LCorr)
% LVar es la lista de variables con las que se ha lanzado un thread y se 
% tiene que esperar.
% LFallos es la lista de perdidas de paralelizacion
% LCorr es una lista de elementos que no se han podido corregir en su momento
% y se almacenan para su posterior correccion.
%------------------------------------------------------------------------

audg_merge([(N,P_list,Sub_udg)|Rest],Sub_udgs,Dict,L_var,L_fallos,Udg_merge,Q_edges,LCorr) :-
	merge_exp2((N,P_list,Sub_udg),P_list,Sub_udgs,[],Dict,L_var,L_fallos,L_var2,L_fallos2,Int_udgs,Q_edges,LCorr,LCorr2),
	audg_merge(Rest,Int_udgs,Dict,L_var2,L_fallos2,Udg_merge,Q_edges,LCorr2).
audg_merge([(N,P_list,Sub_udg)|Rest],[],Dict,L_var,L_fallos,Udg_merge,Q_edges,LCorr) :-
	parallelize_nodes(P_list,Dict,Par_exp),
	audg_merge(Rest,[(N,P_list,(Par_exp,Sub_udg))],Dict,L_var,L_fallos,Udg_merge,Q_edges,LCorr).
audg_merge([],Sub_udgs,_,_,_,Sub_udgs,_,_).

merge_exp2((N1,P1_list,Sub_udg1),[],Sub_udgs,Sub_exps,Dict,L_var,L_fallos,L_var2,L_fallos,Int_udgs,_,LCorr,LCorr2) :-
	mete_thread(Sub_exps,L_var,Sub_udg1,L_var2,Exp,LCorr,LCorr2,L_fallos,Dict),
	Int_udgs = [(N1,P1_list,Exp)|Sub_udgs].
merge_exp2((N1,P1_list,Sub_udg1),List,[(N1,P2_list,Sub_udg2)|Rest],
	   Sub_exps,Dict,L_var,L_fallos,L_var2,L_fallos2,[(N1,P2_list,Sub_udg2)|Int_udgs],Q_edges,LCorr,LCorr2) :-
	/* P2_list cannot be a subset of P1_list */
	merge_exp2((N1,P1_list,Sub_udg1),List,Rest,Sub_exps,Dict,L_var,L_fallos,L_var2,L_fallos2,Int_udgs,Q_edges,LCorr,LCorr2).
%% While P2_list is a subset of P1_list, we have to reduce P1_list, and
%% this is done through List, so... (PBC)
merge_exp2((N1,P1_list,Sub_udg1),List,[(N2,P2_list,Sub_udg2)|Rest],
	   Exp,Dict,L_var,L_fallos,L_var2,L_fallos2,Int_udgs,Q_edges,LCorr,LCorr2) :-
	N1 > N2,
	( sublist(P2_list,List) ->
	    subtract(List,P2_list,New_list),
	    merge_exp2((N1,P1_list,Sub_udg1),New_list,Rest,
	              [Sub_udg2|Exp],Dict,L_var,L_fallos,L_var2,L_fallos2,Int_udgs,Q_edges,LCorr,LCorr2)
	; /* P2_list is not a subset of P1_list */
            Int_udgs = [(N2,P2_list,Sub_udg2)|Int_udg1],
            merge_exp2((N1,P1_list,Sub_udg1),List,Rest,Exp,Dict,L_var,L_fallos,L_var2,L_fallos2,Int_udg1,Q_edges,LCorr,LCorr2)
	).
merge_exp2((N1,P1_list,Sub_udg1),List,[(N2,P2_list,Sub_udg2)|Rest],
	   Exp,Dict,L_var,L_fallos,L_var2,L_fallos2,Int_udgs,Q_edges,LCorr,LCorr2) :-
	N1 > N2,
	( sublist(P2_list,List) ->
	    subtract(List,P2_list,New_list),
	    merge_exp2((N1,P1_list,Sub_udg1),New_list,Rest,
	              [Sub_udg2|Exp],Dict,L_var,L_fallos,L_var2,L_fallos2,Int_udgs,Q_edges,LCorr,LCorr2)
	; /* P2_list is not a subset of P1_list or ... */
          /* it could have been, but a previous sub_udg was selected */
            Int_udgs = [(N2,P2_list,Sub_udg2)|Int_udg1],
            merge_exp2((N1,P1_list,Sub_udg1),List,Rest,Exp,Dict,L_var,L_fallos,L_var2,L_fallos2,Int_udg1,Q_edges,LCorr,LCorr2)
	).
merge_exp2((N1,P1_list,Sub_udg1),List,[],Exp,Dict,L_var,L_fallos,L_var2,L_fallos2,Int_udg,Q_edges,LCorr,LCorr2) :-
	parallelize_nodes(List,Dict,Par_exp),
	( Exp = [] ->
            /* List = P1_list */
            resuelve_udg(N1,P1_list,Par_exp,Sub_udg1,L_var,L_fallos,Dict,L_fallos2,L_var2,Int_udg,Q_edges,LCorr,LCorr2)
	; mete_thread([Par_exp|Exp],L_var,Sub_udg1,L_var2,Par_exp1,LCorr,LCorr2,L_fallos,Dict),
	  Int_udg = [(N1,P1_list,Par_exp1)]
        ).

resuelve_udg(N1,P1_list,Par_exp,Sub_udg1,L_var,L_fallos,Dict,L_fallos2,L_var2,Int_udg,Q_edges,LCorr,LCorr2):-
	meto_disparo(Sub_udg1,Par_exp,L_var,L_fallos,Dict,L_fallos2,L_var2,Salida,Q_edges,LCorr,LCorr2),
	Int_udg = [(N1,P1_list,Salida)].

% meto_disparo: introduce una nueva espera de un hilo lanzado
meto_disparo(Sub_udg1,Par_exp,L_var,[(Q1,LQ1)|Resto],Dict,Resto,L_var2,
	Resultado,Q_edges,LCorr,LCorr2):-
	vertex_goal(Q1,Dict,G1), 
	subtermino(G1,Sub_udg1),
	(en_q_edges(LQ1,Q_edges,Valor) ->
		(Valor == [] -> % Si no hay nada en Q_edges
			Resultado = (Par_exp, Sub_udg1 & (H1 <&&, Otro)),
			LCorr2 = LCorr
		;
			listaendos(Valor,Sub_udg1,Dict,LSi,LNo),
                        % Devuelve el resultado dependiendo de la lista
			(LSi == [] ->
				append(LCorr,LNo,LCorr2),
				project_edges(Q_edges,Valor,Edges),
				audg(graph(Valor,Edges),Dict,Valor2),
				dame_resultado(G1,Sub_udg1,Par_exp,Valor2,Resultado,H1,Otro)
			;
				append(LCorr,LNo,LCorr2),
				project_edges(Q_edges,LSi,Edges),
				audg(graph(LSi,Edges),Dict,Valor2),
				dame_resultado(G1,Sub_udg1,Par_exp,Valor2,Resultado,H1,Otro)
			)
		)
	;
		Resultado = (Par_exp, Sub_udg1 & (H1 <&&, Otro)),
		LCorr2 = LCorr
	),
	inserta_lista(L_var,(H1,Otro,LQ1),L_var2). % Añada la variable metida
meto_disparo(Sub_udg1,Par_exp,L_var,[(Q1,LQ1)|Resto],Dict,[(Q1,LQ1)|Sigue],L_var2,Salida,Q_edges,LCorr,LCorr2):-
	meto_disparo(Sub_udg1,Par_exp,L_var,Resto,Dict,Sigue,L_var2,Salida,Q_edges,LCorr,LCorr2).
meto_disparo(Sub_udg1,Par_exp,L_var,[],_,[],L_var,(Par_exp,Sub_udg1),_,LCorr,LCorr).
	
% listaendos: divide una lista en dos: los elementos de la lista que se
% encuentran en Sub_udg1, y los que no.
listaendos([],_,_,[],[]).
listaendos([X|Resto],Subudg1,Dict,Listasi,Listano):-
	vertex_goal(X,Dict,Goal),
        (subtermino(Goal,Subudg1) ->
                Listasi = [X|RSi],
                listaendos(Resto,Subudg1,Dict,RSi,Listano)
        ;
                Listano = [Goal|RNo],
                listaendos(Resto,Subudg1,Dict,Listasi,RNo)
        ). 

% Devuelve el resultado dependiendo de las paralelizaciones que tenga
dame_resultado(_,Sub_udg1,Par_exp,Valor2,(Par_exp, L & (Valor2,H1 <&&, Otro)),H1,Otro) :-
	saca_valor(Sub_udg1,X&Valor2&Y,L,X&Y).
dame_resultado(_,Sub_udg1,Par_exp,Valor2,(Par_exp, L & (Valor2,H1 <&&, Otro)),H1,Otro) :-
	saca_valor(Sub_udg1,(X&Valor2)&Y,L,X&Y).
dame_resultado(_,Sub_udg1,Par_exp,Valor2,(Par_exp, L & (Valor2,H1 <&&, Otro)),H1,Otro) :-
	saca_valor(Sub_udg1,X&Valor2,L,X).
dame_resultado(_,Sub_udg1,Par_exp,Valor2,(Par_exp, L & (Valor2,H1 <&&, Otro)),H1,Otro) :-
	saca_valor(Sub_udg1,Valor2&Y,L,Y).
dame_resultado(_,Sub_udg1,Par_exp,_,(Par_exp, Sub_udg1 & (H1 <&&, Otro)),H1,Otro).

% subtermino dice si un elemento pertenece a un termino
subtermino(Sub,Term) :- Sub == Term.
subtermino(Sub,Term) :-
	nonvar(Term),
	functor(Term,_,N),
	subtermino3(N,Sub,Term).

subtermino3(N,Sub,Term):-
	arg(N,Term,Arg),
	subtermino(Sub,Arg).
subtermino3(N,Sub,Term):-
	N>1,
	N1 is N-1,
	subtermino3(N1,Sub,Term).

% en_q_edges devuelve una lista con los elementos de la lista de fallos que
% se encuentran en Q_edges.
en_q_edges([X|_],Q_edges,Valor) :-
        en_q_edges2(X,Q_edges,Valor).
 
en_q_edges2(X,[(Q1,X)],[Q1]).
en_q_edges2(X,[(Q1,X)|Resto],[Q1|Sigue]):-
	en_q_edges2(X,Resto,Sigue).
en_q_edges2(X,[(_,_)|Resto],Valor) :-
        en_q_edges2(X,Resto,Valor).
en_q_edges2(_,[(_,_)],[]).

% saca_valor devuelve una expresion X en un formato Y segun una regla R 
% partiendo del termino T
saca_valor(T,T,X,X) :-
        nonvar(T).
saca_valor(T,R,X,Y) :-
        T =.. [F|Xs],
        pone(Xs,V1,Ys),
	pone(Ys,V2,_),
        saca_valor(V2,R,Y1,Y),
        X =.. [F|[V1,Y1]].

pone([X|Xs],X,Xs).

% mete_thread: introduce un hilo respecto a las variables que corresponden con
% las esperas realizadas 
mete_thread([X|Xs],[(H1,V1,[_|_])|Resto],Sub_udg1,Resto,(X &&> H1,Par_exp),[],[],_,_):-
	V1 = Sub_udg1,
	parallelize(Xs,Par_exp).
mete_thread([X|Xs],[(H1,V1,[_Q1|_])|Resto],Sub_udg1,LVAR,(X &&> H1,Res),LCorr,LCorr2,LFallos,Dict):-
	corrige2(X,LCorr,LPC,LC), % Lista de LCorr dividida en 2: LPC y LC
	(LC == [] -> % Si no se corrige ningun elemento con la primera expresion
		LCorr2 = LPC,
		corrige_resultado(Xs,LCorr,LVar,Res), % Se corrige con el resto
                (LVar == [] -> % Si no se ha corregido nada
			V1 = Sub_udg1
		; % Si se ha corregido algo, se hacen esperas para los threads lanzados.
			introduce_esperas(LVar,Exp),
			V1 = (Exp,Sub_udg1)
		),
		LVAR = Resto
	; % Si se ha corregido algun elemento
		busca_lista(LFallos,Sub_udg1,Dict,Elem,L2),
                (LFallos == [] ->
			V1 = Sub_udg1,
			parallelize(Xs,Res),
			LCorr2 = LPC,
			LVAR = Resto
		;
			(var(Elem) ->
				lpcEnLista(LPC,Xs,Valor,LCorr2),
                                (var(Valor) ->
					V1 = Sub_udg1,
					parallelize(Xs,Res),
					LVAR = Resto
				;
					delete(Xs,Valor,LRes),
					V1 = (H2 <&&,Sub_udg1),
					Res = (Valor &&> H2, ResAux),
					parallelize(LRes,ResAux),
					LVAR = Resto
				)
			;
				saca_valor(Sub_udg1,(J,Elem),L,J),
				V1 = (L,Elem & (H2 <&&, V2)),
				parallelize(Xs,Res),
				LCorr2 = LPC,
				LVAR = [(H2,V2,L2)|Resto]
			)
		)
	).
mete_thread([X|Xs],[],Sub_udg1,[],(X &&> H1,Par_exp & (H1 <&&, Sub_udg1)),LCorr,LCorr,_,_) :-
	parallelize(Xs,Par_exp).

% lpcEnLista: Si el elemento X de la lista a corregir, se encuentra en la lista
% de expresiones, devuelve el termino que contiene dicho elemento y una nueva
% lista con los elementos que faltan por corregir.
% Si no se encuentra en las expresiones, sigue buscando con el siguiente
% elemento. Si no se encuentra ninguno, se devuelve la misma lista a corregir
% de entrada, y el termino que iba a contener uno de dichos elementos se queda
% sin instanciar.
lpcEnLista([],_,_,[]).
lpcEnLista([X|Xs],ListaExps,Valor,LPCAux):-
	enListaExps(X,ListaExps,V),
	(var(V) ->
		LPCAux = [X|Resto],
		lpcEnLista(Xs,ListaExps,Valor,Resto)
	;
		Valor = V,
		LPCAux = Xs
	).

% enListaExps: devuelve el termino que contiene el elemento X en la lista de
% expresiones.
enListaExps(_,[],_).
enListaExps(X,[Y|_],Y) :-
	subtermino(X,Y).
enListaExps(X,[_|Ys],N):-
	enListaExps(X,Ys,N).

% busca_lista: se devuelve el elemento y la lista de elementos que esperan por
% el primero y no deben esperar.
busca_lista([],_,_,_,[]).
busca_lista([(Q1,LQ1)|_],Sub_udg1,Dict,G1,LQ1):-
	vertex_goal(Q1,Dict,G1),
	subtermino(G1,Sub_udg1).
busca_lista([_|R],Sub_udg1,Dict,G1,LQ1):-
	busca_lista(R,Sub_udg1,Dict,G1,LQ1).

% introduce_esperas: realiza las esperas de los threads lanzados
introduce_esperas([(1,X)|Xs],(X <&&)) :-
	var(Xs).
introduce_esperas([(1,X)|Xs],(X <&&,Res2)):-
	introduce_esperas(Xs,Res2).
	
% corrige_resultado: corrige los elementos a corregir devolviendo una lista
% de variables para realizar las esperas y el resultado de la expresion
corrige_resultado([X],_,[],X).
corrige_resultado([X|Xs],LCorr,LVar,Res):-
	corrige2(X,LCorr,LPCAux,LCAux),
	(LCAux == [] ->
		Res = (X & Res3),
		corrige_resultado(Xs,LPCAux,LVar,Res3)
	;
		LVar = [(1,H2)|LVar2],
		(LPCAux == [] ->
			Res = (X &&> H2,Par_exp),
			parallelize(Xs,Par_exp)
		;
			Res = (X &&> H2,Res2),
			corrige_resultado(Xs,LPCAux,LVar2,Res2)
		)
	).

% corrige2: devuelve la lista a corregir dividida en 2 partes: la lista de
% elementos que se pueden corregir en este momento(subtermino de las expresiones)
% y la lista de los elementos que no pueden corregirse ahora.
corrige2(_,[],[],[]).
corrige2(X,[Y|Resto],LPorCorr,LCorr):-
	(subtermino(Y,X) ->
		LCorr = [Y|Ys],
		corrige2(X,Resto,LPorCorr,Ys)
	;
		LPorCorr = [Y|Ys],
		corrige2(X,Resto,Ys,LCorr)
	).

% inserta_lista: introduce un elemento en una lista
inserta_lista([],H1,[H1]).
inserta_lista([X|Rest],H1,[X|Sigue]) :-
	inserta_lista(Rest,H1,Sigue).

