%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Module for being loaded by intneg to add the complemented
% predicates to the predicates of the module that is being
% compiled. It uses intensional negation to obtain them.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(intneg_tr,_,[]).
%:- module(intneg_tr,[comp_pred/3],[]).

% :- use_module(library(data_facts),[retract_fact/1]).
:- use_module(library(lists),[append/3]).
:- use_module(library(aggregates),[findall/4]).
%:- use_module(library(metaterms),[ask/2]). %Para Susana
:- use_module(library(terms_check),[ask/2]).  %Para versiones posteriores
:- use_module(library(idlists),[memberchk/2]).
%:- use_module(library(term_basic),[functor/3]).

:- use_module(library(write),[write/1,write/2]). % For debugging

% dynamic predicates to store clauses that are going to be
% expanded. It is used to expand them in a continous way
:- data pre_intneg/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% deep_forall(D) returns the level of deep that we are going to
% consider for expanding the coverings of the forall
deep_forall(7).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Debug (de)activation.
debug_on('Yes'). 
% debug_on('No'). 

% Debugging output file.
debug_intneg_output([]) :-
	debug_aux('DBG OUTPUT: ', '--------------------------', ''), nl.
debug_intneg_output([Line|Txt]) :-
	debug_aux('DBG OUTPUT: ', Line, ''),
	debug_intneg_output(Txt).

debug_intneg_input([]) :-
	debug_aux('DBG INPUT: ', '--------------------------', ''), nl.
debug_intneg_input([Line|Txt]) :-
	debug_aux('DBG INPUT: ', Line, ''),
	debug_intneg_input(Txt).

debug_on_file(File, Txt) :-
	open(File,write,Stream),
	debug_on_file_aux(Txt, Stream),
	close(Stream).

debug_on_file_aux([], _Stream).
debug_on_file_aux([Line|Txt], Stream):-
	write(Stream,Line),
	write(Stream,'\n'),
	debug_on_file_aux(Txt, Stream).

debug_intneg_clauses(Pre, []) :-
	debug_intneg(Pre, '[]').
debug_intneg_clauses(Pre, [Cl]) :-
	debug_intneg(Pre, Cl).
debug_intneg_clauses(Pre, [Cl|Cls]) :-
	debug_intneg(Pre, Cl),
	debug_intneg_clauses(Pre, Cls).	

debug_intneg(Pre, Txt) :-
	debug_aux('DEBUG: ', Pre, Txt).

debug_aux(Msg, Pre, Txt) :-
	debug_on('Yes'), 
	write(Msg), write(Pre), write(Txt), nl.
debug_aux(_Msg, _Pre, _Txt) :- debug_on('No').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% free_list(N,L) returns a list L of N free variables
free_list(0,[]):- !.
free_list(N,[_|L]):-
 N1 is N-1,
 free_list(N1,L).

% conj_to_list(Conj,List) provides in List the elements of
% the conjunction Conj
conj_to_list((A,B),[A|ListB]):-
 !,
 conj_to_list(B,ListB).
conj_to_list(A,[A]).


%Falta añadir:
%intneg(G,fail):- ground(G), call(G), !.
%intneg(G,true):- ground(G), \+ call(G), !.


% intneg_impl(L) returns L that is the list of additional 
% clauses that complete the implementation of intneg/2
intneg_impl([
	(intneg((G1;G2)):- intneg(G1),intneg(G2)),
	 (intneg((G1,G2)):- intneg(G1)),
	  (intneg((G1,G2)):- intneg(G2)),
	   (intneg(intneg(G)):- call(G)),
	    end_of_file]).

% comp_pred(Sentence,SentList,Module) sustitutes Sentence in
% the program Module that is being compilated by the list of
% sentences SentList. The result clauses are continous
comp_pred(end_of_file,OutCls,_):-
	!,
	debug_intneg('', 'comp_pred(end_of_file,OutCls,_)'),
	findall(CL,(retract_fact(pre_intneg(CL))),Cls,[]),
	debug_intneg_input(Cls),
	get_complement(Cls,NegCls),
	append(Cls,NegCls,OutCls1),
	intneg_impl(L),
	append(OutCls1,L,OutCls),
	debug_intneg_output(OutCls).
comp_pred(Fact_Cl, [], _):-  % As facts as clauses
	assertz_fact(pre_intneg(Fact_Cl)).

% get_general_head(Head,GenHead) obtain the same subgoal than Head but
% with all arguments as variables. I.e. get_general_head(p(1,s(X)),p(A,B)).
get_general_head(Head,Pred):-
	functor(Head,Name,Arity),
	free_list(Arity,Args),
	Pred=..[Name|Args].

% get_complement(Cls,NegCls) receives a list of clauses of the form
% [H1:-B1, H2:-B2,...,HN:-BN,F1,F2,end_of_file]
% and return the clauses of the complemented program of the form
% [ intneg(PH1):- NB1,...,intneg(PHn):- NBn]
% where the predicate intneg/1 will be the complement of all
% predicates of the initial list and Fi are facts.
get_complement(Cls,NegCls):-
	get_heads(Cls,Heads),
	remove_duplicates(Heads,Preds),
	intensional(Preds,Cls,NegCls).

% get_heads(Cls,Heads) returns in Heads all the heads of Cls
get_heads([],[]):- !.
get_heads([(Head :- _Body) | Cls],[GHead|Heads]):- !,
	get_general_head(Head,GHead),
	get_heads(Cls,Heads).
get_heads([ Fact | Cls],[GFact|Heads]):- !,
	get_general_head(Fact,GFact),
	get_heads(Cls,Heads).

% remove_duplicates(L1, L2) L2 tiene los elementos de L1 sin repetidos
remove_duplicates([],[]).
remove_duplicates([X|L1],L2):-
	member(X,L1),!, % different variables are treated as the same
	remove_duplicates(L1,L2).
remove_duplicates([X|L1],[X|L2]):-
	remove_duplicates(L1,L2).

% intensional(Preds,Cls,NegCls) returns in NegCls the complemented code
% of the predicates Preds whose positive code is in Cls
intensional([],_Cls,[]).
intensional([end_of_file],_Cls,[]):-!.
intensional([Pred|Preds],Cls,NegCls):-
	intensional_pred(Pred,Cls,NegPred),
	intensional(Preds,Cls,NegPreds),
	append(NegPred,NegPreds,NegCls).

% intensional_pred(Pred,Cls,NegPred) returns NegPred that is the set
% of clauses that are the complement of the clauses of Pred that are
% in Pred.
intensional_pred(Pred,Cls,NegPred):-  
	debug_intneg('intensional_pred(Pred,Cls,NegPred)', ''),
	debug_intneg('Pred -> ', Pred),
	get_clauses(Cls,Pred,Clauses), 
	debug_intneg_clauses('Clauses -> ', Clauses),
	negate_clauses(Clauses,Pred,NegPred),
	nl, debug_intneg_clauses('NegPred -> ', NegPred), nl.

% get_clauses(Cls,Pred,Clauses) returns in Clauses the clauses of Cls
% that unify with Pred
get_clauses([],_Pred,[]):- !.
get_clauses([(Pred1 :- Body)|Cls],Pred,[(Pred1 :- Body)|Clauses]):-
	ask(Pred1,Pred),!,
	get_clauses(Cls,Pred,Clauses).
get_clauses([Pred1|Cls],Pred,[Pred1|Clauses]):-
	ask(Pred1,Pred),!,
	get_clauses(Cls,Pred,Clauses).
get_clauses([_Cl|Cls],Pred,Clauses):- !, % It doesn't match
	get_clauses(Cls,Pred,Clauses).

% negate_clauses(Clauses,Pred,NegPred)returns NegPred that is the set
% of clauses that are the complement of the clauses of Pred that are
% Clauses (with the format (Head,LBody).
% For example:
% negate_clauses([(p(X,s(Y)),[q(X)]),(p(X,0),[r(X,Z),s(Z)])],p(A,B),
%   [(intneg(p(A,B)):- dist((A,B),(fA(X),s(fA(Y)))),
% dist((A,B),fA(X),fA(Y))),
%    (intneg(p(X,s(Y)):- intneg(q(X)),
%    (intneg(p(X,0)):- forall([Z],intneg((r(X,Z),s(Z)))) ]).
negate_clauses(Clauses,Pred,NegClauses):-
	negate_heads(Clauses,Pred,NegHead),
	debug_intneg_clauses('NegHead -> ', NegHead),
	obtain_CP(Clauses,CP_Clauses,NoCP_Clauses),
	debug_intneg_clauses('CP_Clauses -> ', CP_Clauses),
	debug_intneg_clauses('NoCP_Clauses -> ', NoCP_Clauses),
	negate_CP(CP_Clauses,NegCPs),
	debug_intneg_clauses('NegCPs -> ', NegCPs),
	negate_NoCP(NoCP_Clauses,NegNoCPs),
	debug_intneg_clauses('NegNoCPs -> ', NegNoCPs),
	append(NegHead,NegCPs,Neg_Head_CPs),
	append(Neg_Head_CPs,NegNoCPs,NegClauses).

% % negate_heads(Clauses,Pred,NegHead) returns in NegHead one clause that
% % is the negation of the head of all Clauses of Pred
% negate_heads(Clauses,Pred,NegHead):- ...  % Este es el que has hecho

% % que tienes que ampliar con lo de fA y lo que te he dicho hoy y devolver
% % una clausula en vez de una lista.

% %  obtain_CP(Clauses,CP_Clauses,NoCP_Clauses) ya te lo explicaré.
% obtain_CP(Clauses,CP_Clauses,NoCP_Clauses).

% % negate_CP(CP_Clauses,NegCPs) ya te lo explicaré.
% negate_CP(CP_Clauses,NegCPs).

% % negate_NoCP(NoCP_Clauses,NegNoCPs) returns the clauses of intneg/1

% % obtained negating the bodies of the clauses of NoCP_Clauses
% negate_NoCP(NoCP_Clauses,NegNoCPs):- ... % Ten en cuenta los forall
% % cuando hay variables libres

%---------------------------- Respecto al Negate_heads Version 5
% --------------------------------

% Lista de predicados utilizados por negate_heads/3:
%
%          neg_cab_aux/3,
%          get_head/2,
%          get_neg_head/4,
%          get_diseq_head/3,
%          transform_args/2,
%          pred/2,
%          list_to_conj/2,
%          args_all_vars/1,
%          args_all_vars_aux/1,
%          all_var/1.


% Recibe una lista de clausulas, y un predicado general de estas,
% produciendo una lista
% que contiene el predicado llamado intneg, la cual es la negacion de
% las cabeceras de las
% clausulas, que consiste en una sucesion de distintos (entre los
% argumentos de las cabeceras
% de cada una de las clausulas, y los argumentos del predicado general
% de estas).
% Receive a clauses's list, and a general predicate of this, produce a
% list that have the
% predicate called intneg, which is the negation of the head of the
% clauses, that consist in a
% dist succesion ( between the heads arguments of each one of the
% clauses, and the general
% predicate arguments of this ).
negate_heads([],_Pred,[]).
negate_heads(Clauses,_Pred,[]):-
	args_all_vars(Clauses),!.
negate_heads(Clauses,Pred, [(intneg(Pred):- NClauses)]):-
        Pred=..[_Functor|Args],
        neg_cab_aux(Clauses,Args,NClauses).

% Predicado auxiliar de neg_cab, utilizado para generar la conjuncion
% de distintos (Dist). En la
% cual recibe una lista de clausulas, los argumentos del predicado
% general de estas y devuelve la
% conjuncion de distintos entre los argumentos del predicado general y
% los argumentos de cada una
% de las cabeceras de las clausulas.
% Auxiliary function of neg_cab, used to generate the conjuntion of
% diferent (Dist). In which,
% receive a list of clauses, the arguments of the general predicate
% and return the conjuntion
% of different between the arguments of the general prediate and the
% argument of each head of
% clauses.
% neg_cab_aux2([(Head :- _Body)],Args_Pred,dist(Conj_Args_Pred,Args_Clause) ):-
% 	Head=.. [_Functor|Args],
% 	transform_args(Args,Args_Trans),
% 	list_to_conj(Args_Trans,Args_Clause),
%         list_to_conj(Args_Pred,Conj_Args_Pred),!.
% neg_cab_aux2([ (Head :- _Body) |Cls],Args_Pred, List_Dist ):-
% 	Head=.. [_Functor|Args],
% 	all_var(Args),
%         neg_cab_aux(Cls,Args_Pred,List_Dist),!.
% neg_cab_aux2([ (Head :- _Body) |Cls],Args_Pred,
% 	                                dist(Conj_Args_Pred,Args_Clause) ):-
%         Head=.. [_Functor|Args],
% 	transform_args(Args,Args_Trans),
% 	list_to_conj(Args_Trans,Args_Clause),
%         list_to_conj(Args_Pred,Conj_Args_Pred),
% 	args_all_vars(Cls),!.
% neg_cab_aux2([ (Head :- _Body) |Cls],Args_Pred,
% 	                     (dist(Conj_Args_Pred,Args_Clause),List_Dist) ):-
%         Head=.. [_Functor|Args],
% 	transform_args(Args,Args_Trans),
% 	list_to_conj(Args_Trans,Args_Clause),
%         list_to_conj(Args_Pred,Conj_Args_Pred),
%         neg_cab_aux2(Cls,Args_Pred,List_Dist).

neg_cab_aux([(Head :- _Body)], Args_Pred, Diseq):- !, % Only one clause
	get_diseq_head(Head, Args_Pred, Diseq).
neg_cab_aux([Fact], Args_Pred, Diseq):- !,  % Only one fact
	get_diseq_head(Fact, Args_Pred, Diseq).
neg_cab_aux([Cl | Cls], Args_Pred, List):- !, 
	get_head(Cl,Head),
	get_neg_head(Head, Cls, Args_Pred, List).

% get_head(Cls,Head) Head is the head of Cls if Cls is a clause and
% is Cls if it is a Fact
get_head( (Head :- _Body), Head):- !.
get_head( Fact, Fact).

% get_neg_head(Head, Cls, Args_Pred, List) List is the list of disequalities
% of negate Head from one clause and the list of clauses Cls for the 
% variables Args_Pred. It one of the terms is true is removed
get_neg_head(Head, Cls, Args_Pred, List):- 
        get_diseq_head(Head, Args_Pred, Diseq),
	neg_cab_aux(Cls,Args_Pred,List_Dist),
	(Diseq == true ->
	   List = List_Dist
	;
	    (List_Dist == true ->
	        List = Diseq
	    ;
		List = (Diseq,List_Dist) 
	    )
	).       

% get_diseq_head(Head, Args_Pred, Diseq) returns the disequality Diseq 
% between the args of Head and the variables of Args_Pred.
% If all args of Head are variables Diseq will be true
get_diseq_head(Head, _Args_Pred, true):-
	Head=.. [_Functor|Args],
	all_var(Args),!.
get_diseq_head(Head,Args_Pred,dist(Conj_Args_Pred,Args_Clause)):-
	Head=.. [_Functor|Args],
	transform_args(Args,Args_Trans),
	list_to_conj(Args_Trans,Args_Clause),
        list_to_conj(Args_Pred,Conj_Args_Pred).	

% Transforma los elementos de la lista de entrada, en una lista de
% salida. En la que las
% variables estaran cuantificadas con un forall (fA).
% Transform the list's elements of input, in a outside list. In which
% the varibles will be
% quantificate with a forall (fA).
transform_args([],[]).
transform_args([X|Xs],[X_t|Xs_t]):-
	pred(X,X_t),
	transform_args(Xs,Xs_t).

% Recibe un elemento, y devuelve las variables cuantificadas
% universalmente de dicho elemento.
% Receives a element, and returns the variables that universaly
% quantificate variables of this
% element.
pred(X,X):-
	ground(X),!.
pred(X,fA(X)):-
	var(X),!.
pred(X,Y):-
	X=.. [FunctorX|ArgsX],
	transform_args(ArgsX,Args1),
	Y=.. [FunctorX|Args1].

% Transforma una lista de elementos, en una conjuncion de los
% elementos de la lista.
% Transform a element's list, in a conjuntion of elements from the
% list.
list_to_conj([A|ListB],(A,B)):-
	list_to_conj(ListB,B),!.
list_to_conj([A],A).

% Recibe una lista de argumentos, y comprueba que todos los elementos
% de los argumentos
% son variables.
% Receive a list of arguments, and check as all the elements of the
% arguments are variables.
args_all_vars([]).
args_all_vars([Arg|Args]):-
	args_all_vars_aux(Arg),
	args_all_vars(Args).

% Comprueba que todos los argumentos de la cabecera de una clausula
% son variables.
% Check all the head's arguments of clause are variable.
args_all_vars_aux((Head:-_Body)):-!,
	Head=..[_Functor|Args],
	all_var(Args).
args_all_vars_aux(Fact):-
	Fact=..[_Functor|Args],
	all_var(Args).

% Recibe una lista de elementos, y comprueba que todos son variables.
% Receive a list of elements, and check that all are variables.
all_var([]).
all_var([X|Xs]):-
	var(X),
	all_var(Xs).

%---------------------------- Respecto al Negate_NoCP Version 3
% --------------------------------

% Lista de predicados utilizados por negate_NoCP/2:
%
%          neg1_NoCP/2,
%          get_list_var/2,
%          get_var/2,
%          conj_to_list/2,         (Esta en la primera parte)
%          difference/3,
%          remove_duplicates1/2,
%          remove_duplicates2/3.


% Recibe una lista de clausulas sin pares criticos, y devuelve la
% lista de negaciones de estas
% clausulas.
% Recieve a clauses's list isn't critical pair, and return the list of
% negations of this clauses.
negate_NoCP([],[]).
negate_NoCP([Cl|Cls],[NCl|NCls]):-
	neg1_NoCP(Cl,NCl),
	negate_NoCP(Cls,NCls).

% Predicado auxiliar de neg_NoCP, para negar una clausula sin pares
% criticos.
% Predicate auxiliar of neg_NoCP, to negate one clauses isn't critical
% pair.
neg1_NoCP(( Head :- Body ), (intneg(Head):-intneg(Body)) ):-
	Head=.. [_Functor|Args],
        conj_to_list(Body,List_Body),
	get_list_var(List_Body,VarBody),
	get_var(Args,Vars_Args),
	difference(VarBody,Vars_Args,[]),!.
neg1_NoCP(( Head :- Body ),(intneg(Head):-forall(GoalVar,intneg(Body))) ):-
	Head=.. [_Functor|Args],
        conj_to_list(Body,List_Body),
	get_list_var(List_Body,VarBody),
	get_var(Args,Vars_Args),
	difference(VarBody,Vars_Args,List_Vars),
	remove_duplicates1(List_Vars,GoalVar).

% Obtiene de la lista de predicados, la lista de variables de los
% argumentos de dichos
% predicados.
% Get from the list of predicates, the list of variables of the
% argument of each predicates.
get_list_var([],[]).
get_list_var([Pred|Preds],List_Vars):-
	Pred=.. [_Functor|Args],
	get_var(Args,Var),
	append(Var,Vars,List_Vars),
	get_list_var(Preds,Vars).

% Obtiene las variables de la lista de entrada y las pone en la lista
% de salida.
% Get the variables of the input list and put it in the output list.
get_var([],[]).
get_var([X|Xs],[X|Ys]):-
	var(X),
	get_var(Xs,Ys),!.
get_var([X|Xs],Ys):-
	ground(X),
	get_var(Xs,Ys),!.
%get_var([[X]|Xs],Ys):-
	%!,get_var([X|Xs],Ys).
get_var([X|Xs],Ys):-
	X=..[_FunctorX|ArgsX],
	append(ArgsX,Xs,New_var),
	get_var(New_var,Ys).


% % Transforma una conjuncion de elementos, en una lista de los
% elementos de la conjuncion.
% % Transform a element's conjuntion, in a list of elements from the
% conjuntion.
% conj_to_list((A,B),[A|ListB]):-
%  conj_to_list(B,ListB),!.
% conj_to_list(A,[A]).

% difference(Vars,NFVars,FreeVars) devuelve en FreeVars la sublista de
% elementos de Vars que no
% aparecen en NFVars.
% difference(Vars,NFVars,FreeVars) retuns in FreeVars the sublist of
% elements of Vars that do
% not appear in NFVars.
difference([],_NFVars,[]).
difference([Var|Vars],NFVars,FreeVars):-
	memberchk(Var,NFVars),!,
	difference(Vars,NFVars,FreeVars).
difference([Var|Vars],NFVars,[Var|FreeVars]):-
	difference(Vars,NFVars,FreeVars).

% remove_duplicates1(L1, L2) L2 tiene los elementos de L1 sin
% repetidos
% remove_duplicates1(L1, L2) L2 have the elements of L1 are not
% repeating.
remove_duplicates1(Lista,ListaSin):-
         remove_duplicates2(Lista,[],ListaSin).

% remove_duplicates2(L1, L2,L3) L3 tiene los elementos de L2  mas los
% elementos de L1 que no
% estan ya en L2.
% remove_duplicates2(L1, L2,L3) L3 have the elements of L2 more the
% elements of L1 that are not
% in L2.
remove_duplicates2([], List2, List2):- !.
remove_duplicates2([Element|Residue], List, ListaSin) :-
         memberchk(Element, List),!,
         remove_duplicates2(Residue, List, ListaSin).
remove_duplicates2([Element|Residue], List, ListaSin) :-
         remove_duplicates2(Residue, [Element|List], ListaSin).

%---------------------------- Respecto al Mgu Version 6
%--------------------------------

% Lista de predicados utilizados por mgu/2:
%
%          unificate_list/2,
%          obtain_unif/2,
%          obtain_unif_aux/2,
%          get_especific/2,
%          get_especific_aux/3,
%          especific_term/3,
%          unificate2/2,
%          obtain_unif2/2,
%          traspuesta/2,
%          traspuesta_aux/3.


% mgu(L1,Mgu_L1) devuelve en MGU_L1 el unificador general maximo (mgu) de todos los elementos de
% L1 que son lista de terminos.
% mgu(L1,MGU_L1) returns in MGU_L1 the maximun general unifier (mgu) 
% of all elements of L1 that are list of terms
% For example mgu([[0],[X]],[0])
%             mgu([[s(Y)],[X]],[s(Y)])
%             mgu([[0,X],[Y,0]],[0,0])
%             mgu([[0,X],[Y,s(Z)]],[0,s(Z)])
%             mgu([[0,X],[Z,s(Z)]],[0,s(0)])
%             mgu([[0,X],[Y,0],[s(X),0]],Sol)  fail
%             mgu([[Y,X],[A,B]],[A,B])
%             mgu([[s(W),X],[Y,s(Z)]],[s(W),s(Z)])
mgu([],[]).
mgu(Matrix,Especific_T):-
	copy_term(Matrix,Matrix1), % Results in a new copy of Matrix, but with fresh variables.
	traspuesta(Matrix1,Matrix_T), % Ponemos los args de la misma posicion en la misma fila.
	get_especific(Matrix_T,Especific_T),
        unificate_list(Matrix_T,Especific_T).

% Obtiene una lista de terminos, en la cual cada elemento de la lista consta de una lista de dos
% elementos. El primer elemento es mas especifico que el segundo. Por lo que se unifica el 
% termino general con el especifico (Segundo con primero). 
% Obtain a list of terms, in which each element of the list, constain a list of two eleemnts. The
% first element is more especific than the second. In conclusion, unificate the general term with
% the specific term. (Second with first).
unificate_list(_,[]).
unificate_list([[]|Matrix],[_Term|Terms]):-
	unificate_list(Matrix,Terms).
%unificate_list([[_X|_Xs]|Matrix],[[_T|_Ts]|Terms]):-
	%unificate_list(Matrix,Terms),!.
unificate_list([[X|Xs]|Matrix],[Term|Terms]):-
	ask(Term,X),!,
	obtain_unif(Term,X),
%	X=Term,
	unificate_list([Xs|Matrix],[Term|Terms]).
%unificate_list([[_X|Xs]|Matrix],[Term|Terms]):-
	%unificate_list([Xs|Matrix],[Term|Terms]).

% Obtiene para cada par de terminos (El general y su especifico), la unificacion entre el mas 
% general y su especifico.
% Obtain from each pair of terms (the general and his especific), lhe unification between the 
% most generaly and his especific.
obtain_unif(T,G):-
	nonvar(G),
	nonvar(T),
	ask(T,G),!,
	G=..[Functor|Args2],
	T=..[Functor|Args1],
	obtain_unif_aux(Args1,Args2).
obtain_unif(T,G):-
	nonvar(T),
	var(G),
	non_vareq(T,G),!,
        G=T.
obtain_unif(T,G):-
	var(G),
	var(T),
	T==G,!.
obtain_unif(T,G):-
	var(G),
	var(T),!,
	G=T.
obtain_unif([Y],X):-
	var(X),
	non_vareq([Y],X),!,
	X=[Y].
obtain_unif([Y|Ys],X):-
	var(X),
	non_vareq([Y|Ys],X),
	X=[Y|Ys].

% No tienen que tener la misma variable.
non_vareq(V1,V2):-
	var(V1),
	var(V2),!,
	\+ V1==V2.
non_vareq(N,_V):-
	ground(N),!.
non_vareq(N,V):-
	var(V),
	nonvar(N),!,
	N=..[_Functor|Args],
	non_vareq_aux(Args,V).
non_vareq([Y],X):-
	var(X),!,
	non_vareq(Y,X).
non_vareq([Y|Ys],X):-
	!,non_vareq(Y,X),
	non_vareq(Ys,X).

% Para recorrer los argumentos de un termino que sea non_var.
non_vareq_aux([],_).
non_vareq_aux([T1|Terms],V):-
	non_vareq(T1,V),
	non_vareq(Terms,V).

% Predicado auxiliar de obtain_unif para recorrer los argumentos de un termino que es nonvar.
% Auxiliar predicate of obtain_unif to look over the arguments of a term that is nonvar.
obtain_unif_aux(_,[]).
obtain_unif_aux([T|Ts],[G|Gs]):-
	obtain_unif(T,G),
        obtain_unif_aux(Ts,Gs).

% Obtiene los terminos mas especificos de cada fila de la matriz ( cada lista ), y devuelve 
% una lista de terminos especificos.
% Get the terms more especific of the file from matrix ( each list ), and return a list of
% especific's terms.
get_especific([],[]).
get_especific([[X|Xs]|Matrix],[File1_T|Matrix_T]):-
	get_especific_aux(Xs,X,File1_T),
	get_especific(Matrix,Matrix_T).

% Obtiene el termino mas especifico de una lista de terminos.
% Get a especific term of a terms list.
get_especific_aux([],X,X).
get_especific_aux([X|Xs],Y,Z):-
	especific_term(X,Y,W),
	get_especific_aux(Xs,W,Z).

% Devuelve el termino mas especifico entre dos que se pueden unificar. 
% Return the term more especific between two terms as can it unificate.
especific_term(X,Y,Y):-
	var(X),!.
especific_term(X,Y,X):-
	var(Y),!.
%especific_term([X],[Y],[X]):-
	%obtain_unif(X,Y).
especific_term([X|Xs],[Y|Ys],[X|Xs]):-
	%nonvar([X|Xs]),
	%nonvar([Y|Ys]),!,
%%%% Lo de nonvar no vale para nada
%        list([X|Xs]),
%        list([Y|Ys]),!,
	!,unificate2([X|Xs],[Y|Ys]).
especific_term(X,Y,X):-
        ask(X,Y),!,
        Y=X.
especific_term(X,Y,Y):-
 	ask(Y,X),
	X=Y.

% Unifica los terminos de dos listas de terminos, obteniendo al terminar la unificacion dos 
% listas de terminos iguales.
% FALTA EN INGLES.
%unificate2([],[]).
%%%% NUEVO
unificate2(X,Y):-
	var(X),
	var(Y),!,
	obtain_unif(X,Y).
unificate2([X|Xs],[Y|Ys]):-
	ask(X,Y),
	ask(Y,X),!,
	obtain_unif2(X,Y),
	obtain_unif2(Y,X),
	unificate2(Xs,Ys).
unificate2([X|Xs],[Y|Ys]):-
	ask(X,Y),!,
	obtain_unif2(X,Y),
	unificate2(Xs,Ys).
unificate2([X|Xs],[Y|Ys]):-
	ask(Y,X),!,
	obtain_unif2(Y,X),
	unificate2(Xs,Ys).
unificate2([X],[Y]):-
	var(X),
	var(Y),!,
	obtain_unif(X,Y).
unificate2([X],[Y]):-
	var(X),
        nonvar(Y),!,
	obtain_unif(Y,X).
unificate2([X],[Y]):-
        nonvar(X),
	var(Y),!,
	obtain_unif(X,Y).
unificate2([X],[Y]):-
	nonvar(X),
	nonvar(Y),!,
	%list(X),
	%list(Y),!,
	obtain_unif2(X,Y).
unificate2([],[]).

% Unifica dos terminos que no son listas, o por el contrario, realiza una llamada a unificate2,
% (en el caso de que los dos terminos sean listas), para unificar estas.
% FALTA EN INGLES.
obtain_unif2(X,Y):-
	nonvar(X),
	nonvar(Y),
	list(X),
	list(Y),!,
	unificate2(X,Y).
obtain_unif2(X,Y):-
	obtain_unif(X,Y).

% Recibe una lista de listas ( una matriz de elementos), y devuelve la traspuesta de 
% dicha matriz.
% Receive a list of lists ( a matrix of elements ), and return the traspuesta of each matrix.
traspuesta([[]|_Matrix],[]).
traspuesta(Matrix,[File_T|Files_T]):-
	traspuesta_aux(Matrix,File_T,Matrix_aux),
	traspuesta(Matrix_aux,Files_T).

% Recibe una matriz, devuelve la primera fila traspuesta y el resto de la matrix.
% Receive a matrix, return the first traspuesta file and the matrix's rest.
traspuesta_aux([],[],[]).
traspuesta_aux([[X|Xs]|Matrix],[X|List],[Xs|Matrix_Res]):-
	traspuesta_aux(Matrix,List,Matrix_Res).

%---------------------------- Respecto al Obtain_CP Version 4
% --------------------------------

% Lista de predicados utilizados por obtain_CP/3:
%
%          obtain_ClsCP/3,
%          obtain_ClsCP_aux/4,
%          remove_sublist/2,
%          remove_subl_aux/3,
%          obtain_NoCls_CP/3,
%          get_allcls/2,
%          mgu/2,                 (En la parte Mgu)
%          remove_fact/2,
%          difference/3,          (En la parte Negate_NoCP)
%          remove_duplicates1/2,   (En la parte Negate_NoCP)
%          remove_duplicates2/3.  (En la parte Negate_NoCP)

% Obtiene a partir de una lista de clausulas, una lista de clausulas que tienen pares criticos 
% (con su par critico correspondiente) y una lista con las clausulas sin pares criticos.
% Obtain to a list of clauses, a list of clauses that have critical pair ( with his critical pair
% correspond ) and a list with clauses without ciritical pair. 
obtain_CP([],[],[]).
obtain_CP(List,CPClauses_sinR,No_ClausesCP):-
	remove_fact(List,List_SinFact),
	ordenar(List_SinFact,Lista),
	obtain_ClsCP(Lista,[],CPClauses),
        remove_sublist(CPClauses,CPClauses_sinR),
	obtain_NoCls_CP(CPClauses_sinR,List_SinFact,No_ClausesCP).

%
ordenar([],[]).
ordenar([First|Rest],[First|[First|Rest]]).

% Recibe una lista de clausulas, y devuelve una lista en la que sus elementos estan formados
% por una lista de clausulas y el Mgu que tienen en comun todas esas clausulas.
% Receive a list of clauses, and return a list in which element are share with a list of clauses
% and Mgu that have in common all this clauses.

obtain_ClsCP([],[],[]). % No hay eltos en las listas. Fin.
obtain_ClsCP([_Cl1],_,[]). % Solo queda 1 elto en la 1a lista. Fin.
obtain_ClsCP([_Cl1|[Cl2|Clauses]],[],Resto):- % Siguiente clausula de la 1a lista.
	obtain_ClsCP([Cl2|Clauses],Clauses,Resto),!.
obtain_ClsCP([(Head1:-Body1)|Cls1],[(Head2:-Body2)|Cls2],Lista):-
	Head1=..[_Functor1|Args1],
	Head2=..[_Functor2|Args2],
	mgu([Args1,Args2],Mgu),!, % Hay MGU entre ellos. 
	obtain_ClsCP_aux(Cls2,Mgu,[(Head2:-Body2),(Head1:-Body1)],List_aux), % Quien mas comparte mgu?
	append(List_aux,Resto,Lista),
	obtain_ClsCP([(Head1:-Body1)|Cls1],Cls2,Resto). % Sigue buscando mgus para el 1er elto 1a lista.
obtain_ClsCP(Clauses,[_Cl2|Cls2],Resto):- % Siguiente clausula de la 2a lista
	obtain_ClsCP(Clauses,Cls2,Resto).

% Recibe un Mgu y la lista de clausulas, y devuelve una lista con todas las clausulas que tengan 
% ese Mgu en comun.
% Receive a Mgu and the clauses list, and return a list with all the clauses that have this Mgu
% in common.
obtain_ClsCP_aux([],Mgu,List,[(Mgu,List)]).
obtain_ClsCP_aux([(Head:-Body)|Cls],Mgu,List,List3):-
	Head=..[_Functor|Args],
	mgu([Mgu,Args],Mgu1),!, % Hay MGU entre ellos.
	append(List1,List2,List3),
	obtain_ClsCP_aux(Cls,Mgu1,[(Head:-Body)|List],List1),
	obtain_ClsCP_aux(Cls,Mgu,List,List2),!. 
obtain_ClsCP_aux([_Cl1|Cls],Mgu,List,List4):-
	obtain_ClsCP_aux(Cls,Mgu,List,List4).

% Recibe una lista de tuplas, cada una de estas compuesta por una lista de clausulas y un Mgu 
% que tienen en comun las clausulas de la lista, y devuelve la lista inicial sin mgu repetidos.
% Las sublistas con el mismo mgu, tienen mayor longitud (mas clausulas) cuando mas pronto del 
% principio de la lista se encuentren.
% Receive a list of two element, each one of this formed to a list of clauses and a Mgu that have
% in common the clauses of the list, and return a initial list without mgu repeat.
% The sublists with the same mgu, have more long (more clauses) when as soon of the beginning of
% the list appear.
remove_sublist([],[]).
remove_sublist([(Mgu,List)|Resto1],[(Mgu,List)|Resto2]):-
	remove_subl_aux(Mgu,Resto1,List2),
	remove_sublist(List2,Resto2).

% Elimina la lista que tenga el Mgu igual al que se le pasa como parametro.
% Remove the list that have the Mgu equal to pass as parameter. 
remove_subl_aux(_Mgu,[],[]).
remove_subl_aux(Mgu,[(Mgu2,_List)|Resto],List_sinR):-
	ask(Mgu,Mgu2),
	ask(Mgu2,Mgu),!,
	remove_subl_aux(Mgu,Resto,List_sinR).
remove_subl_aux(Mgu,[Elem1|Resto1],[Elem1|Resto2]):-
	remove_subl_aux(Mgu,Resto1,Resto2).

% Recibe la lista de todas las clausulas y la lista de clausulas con pares criticos, y devuelve 
% otra lista con las clausulas que no estan en la lista de clausulas con pares criticos.
% Receive a list of all clauses and the list of clauses with critical pair, and retirn other 
% list with the clauses that aren't in the list of clauses with critital pair. 
obtain_NoCls_CP([],[],[]):-!.
obtain_NoCls_CP(List_CP,Cls,List_ClsSinCP):-
	get_allcls(List_CP,List_CP_all),
	remove_duplicates1(List_CP_all,List_sinR),
        difference(Cls,List_sinR,List_ClsSinCP).

% Obtiene todas las clausulas existentes en la lista de clausulas de pares criticos. En esta 
% lista las clausulas pueden aparecer repetidas.
% Get all the clauses that is in the list of clauses of critical pair. In this list the clauses
% can belong repeat.  
get_allcls([],[]).
get_allcls([(_Mgu,List)|Resto],List_all):-
	append(List,List2,List_all),
	get_allcls(Resto,List2).

% Elimina los hechos de una lista de clausulas.
% Remove the fact of a cluases list.
remove_fact([],[]).
remove_fact([(Head :- Body)|Cls],[(Head :- Body)|RCls]):- !,
	remove_fact(Cls,RCls).
remove_fact([_Fact|Cls],RCls):- !,
	remove_fact(Cls,RCls).


%---------------------------- Respecto al negate_CP Version 3
%--------------------------------

% Lista de predicados utilizados por negate_CP/2:
%
%          negate_1CP/3,
%          common_cp/4,
%          dist_CP/3,
%          unificate_mgu_cls/2,
%          unificate_aux/2,
%          all_var_body/3,
%          list_bodys/2,
%          list_to_disy/2,
%          get_diseq_head/3,      (En la parte negate_heads)
%          difference/3,          (En la parte Negate_NoCP)
%          remove_duplicates1/2,  (En la parte Negate_NoCP)
%          remove_duplicates2/3.  (En la parte Negate_NoCP)


% Recibe una lista en la que cada elemento de esta esta compuesta por
% una tupla, cuyo primer
% elemento es el Mgu del segundo elemento de la tupla que es una lista
% de clausulas. Devuelve
% una lista con los negados correspondientes a cada lista de clausulas
% con pares criticos.
% Receive a list in each element of this is compose for a tupla, which
% first element is the Mgu
% of the second tuple element that is a list of clauses. Return a list
% with the negates
% corresponding to each list of clauses with critical pair.
negate_CP([],[]).
negate_CP([(Mgu,ListCls)|CP_Clauses],Neg_all_CPs):-
	copy_term(ListCls,ListCls_copy),
	negate_1CP(Mgu,ListCls_copy,NegCP_Cl),
	negate_CP(CP_Clauses,NegCPs),
	append(NegCP_Cl,NegCPs,Neg_all_CPs).

% Niega una clausula con pares criticos. Osea un Mgu y su lista de
% pares criticos asociadas.
% Negate a clause with critical pair. So a Mgu and list of critical
% pair associate.
negate_1CP(_Mgu,[],[]).
negate_1CP(Mgu,Clauses,NegCP):-
	common_cp(Mgu,Clauses,Common),
	dist_CP(Mgu,Clauses,ListNegCl),
        append(Common,ListNegCl,NegCP).

% Obtiene la negacion del elemento comun de la lista de clausulas de
% pares criticos.
% Obtain the negation of the common element of the clauses list of
% critical pair.
%common_cp(Mgu,Clauses,[(Head :- _Body)|_Rest],[(intneg(Head_Mgu,R):-
%	(intneg(Disy_bodys,R)))]):-
%        Head=..[Functor|Args],
%	ask(Args,Mgu),
%	ask(Mgu,Args),
%	all_var_body(Clauses,[],[]),!,
%	copy_term(Clauses,Clauses_copy),
%	Head_Mgu=..[Functor|Mgu],
%        unificate_mgu_cls(Mgu,Clauses_copy),
%	list_bodys(Clauses_copy,Disy_bodys).
%common_cp(Mgu,Clauses,[(Head :- _Body)|_Rest],[(intneg(Head_Mgu,R1):-
%	(forall(ExpVars,intneg(Disy_bodys,_R2),Deep,R1)))]):-
%        Head=..[Functor|Args],
%	ask(Args,Mgu),
%	ask(Mgu,Args),
%        copy_term(Clauses,Clauses_copy),
%	all_var_body(Clauses_copy,[],ExpVars),!,
%	deep_forall(Deep),
%	Head_Mgu=..[Functor|Mgu],
%        unificate_mgu_cls(Mgu,Clauses_copy),
%	list_bodys(Clauses_copy,Disy_bodys).
%common_cp(Mgu,Clauses,[_Cl1|Cls],Common):-
%	common_cp(Mgu,Clauses,Cls,Common).
common_cp(Mgu,[(Head :- Body)|Clauses],[(intneg(Head_Mgu):-(intneg(Disy_bodys)))]):-
	copy_term([(Head :- Body)|Clauses],Clauses_copy),
	all_var_body(Clauses_copy,[],[]),!,
        Head=..[Functor|_Args],
	Head_Mgu=..[Functor|Mgu],
        unificate_mgu_cls(Mgu,Clauses_copy),
	list_bodys(Clauses_copy,Disy_bodys).
common_cp(Mgu,[(Head :- Body)|Clauses],[(intneg(Head_Mgu):-
	(forall(ExpVars,intneg(Disy_bodys))))]):-
        copy_term([(Head :- Body)|Clauses],Clauses_copy),
	all_var_body(Clauses_copy,[],ExpVars),
        Head=..[Functor|_Args],
	Head_Mgu=..[Functor|Mgu],
        unificate_mgu_cls(Mgu,Clauses_copy),
	list_bodys(Clauses_copy,Disy_bodys).

% Obtiene los demas terminos negados de todas las clausulas menos la
% comun y la que se
% corresponde con el Mgu.
% Obtain the negate terms of the all clauses less the common and the
% clause correspondig with
% the Mgu.
dist_CP(_Mgu,[],[]).
dist_CP(Mgu,[(Head :- _Body)|Clauses],List_negCP):-
	Head=..[_Functor|Args],
	ask(Args,Mgu),
	ask(Mgu,Args),!,
	dist_CP(Mgu,Clauses,List_negCP).
dist_CP(Mgu,[(Head :-Body)|Clauses],[(intneg(Head):-(Elem_dist,intneg(Body)))|Rest]):-
        Head=..[Functor|Args],
	all_var_body([(Head :- Body)],[],[]),!,
	Head_Mgu=..[Functor|Mgu],
	get_diseq_head(Head_Mgu,Args,Elem_dist),
        dist_CP(Mgu,Clauses,Rest).
dist_CP(Mgu,[(Head :-Body)|Clauses],[(intneg(Head):-(Elem_dist,forall(ExpVars,
	intneg(Body))))|Rest]):-
        Head=..[Functor|Args],
	Head_Mgu=..[Functor|Mgu],
        all_var_body([(Head :- Body)],[],ExpVars),
	get_diseq_head(Head_Mgu,Args,Elem_dist),
        dist_CP(Mgu,Clauses,Rest).

% Unifica el Mgu con los argumentos de las cabeceras de una lista de
% clausulas.
% Unificate the Mgu with the arguemnt of the clauses of a list of
% clauses.
unificate_mgu_cls(_Mgu,[]).
unificate_mgu_cls(Mgu,[(Head:-_Body)|Rest]):-
	Head=..[_Functor|Args],
	unificate_aux(Mgu,Args),
	unificate_mgu_cls(Mgu,Rest).

% Recibe dos listas y unifica cada elemento de la segunda lista, con
% el de la primera lista, la
% cual es el Mgu.
% Receive two lists and unificate each element of th second list, with
% it of the first list,
% which is the Mgu.
unificate_aux([],[]).
unificate_aux([Mgu|Mgus],[Arg|Args]):-
	Arg=Mgu,
        unificate_aux(Mgus,Args).

% Devuelve una lista con todas las variables libres de todos los
% cuerpos de una lista de
% clausulas.
% Return a list with all the free variables of all the bodys of a
% clauses list.
all_var_body([],Lista,Lista).
all_var_body([(Head :- Body)|Clauses],Antigua,Nueva):-
	Head=.. [_Functor|Args],
        conj_to_list(Body,List_Body),
	get_list_var(List_Body,VarBody),
	get_var(Args,Vars_Args),
	difference(VarBody,Vars_Args,List_Vars),
	remove_duplicates1(List_Vars,ExpVar),
	append(ExpVar,Antigua,Todas),
	all_var_body(Clauses,Todas,Nueva).

% Recibe una lista de clausulas y devuelve la disyuncion de los
% cuerpos de cada clausula.
% Osea que los cuerpos de cada clausula se quedan igual.
% Receive a list of clauses and return the alternative of the bodys of
% each clause.
% So the bodys of each clause to be left same.
list_bodys([( _Head :- Body )],Body).
list_bodys([( _Head :- Body )|Cls],(Body;Resto)):-
        list_bodys(Cls,Resto).

% Transforma una conjuncion de elementos, en una lista de los
% elementos de la conjuncion.
% Transform a element's conjuntion, in a list of elements from the
% conjuntion.
list_to_disy([A|ListB],(A;B)):-
	list_to_disy(ListB,B),!.
list_to_disy([A],A).


%% Local Variables: 
%% mode: CIAO
%% update-version-comments: "off"
%% End:

