:- module(try, [try/2,
	        add_elems/3],[]).

% To be able to call Pred from neg
:- meta_predicate
        try(goal,_).

:- use_module(library(lists),[append/3]).
:- use_module(library(metaterms),[varset/2]). 
:- use_module(library(aggregates),[setof/3]).
:- use_module(engine(internals),[term_to_meta/2]).

:- use_module(neg,[is_negation/2]).
:- use_module(cneg,[cneg_tried/3,
	            combine/2]).
:- use_module(stored,[obtener_ctes/1,
	              obtener_constructores/1,
		      stored_clause/2]).
:- use_module(stored_builtin,[stored_builtin_clause/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% Programa Try %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% try(Meta,Disjuction) return in Disjunction the disjuction of
% conjunction of subgoals that you can evaluate to make success 
% the goal Pred. Disjunction is a list of lists that represent 
% a disjunction of conjunctions. Meta is a meta_predicate

try(Meta,Disjunction):-
	term_to_meta(Pred,Meta),
	try1(Pred,Disjunction).

%Ver_verifica_recubrimiento para lo del => y tambien poner lo del ; y ,

% try1(Pred,Disjunction) is the same that try/2 but with a predicate
% instead a meta_predicate
try1(true,[]):- !.

% supongo que aqui no se puede llegar nunca porque se simplifica en neg.pl
% los casos de negacion y de objetivos compuestos pero se contempla ahora
% tambien por si acaso
try1(Pred,Disjunction):-
	Pred=(\+(Goal)),!, % builtin naf of a predicate
	term_to_meta(Goal,Meta),
	try(Meta,Tried),
	invert(Tried,InvTried),
	introduce_disj(\+,InvTried,Disjunction).
try1(Pred,Disjunction):-
	Pred='naf:naf'(Goal),!, % naf of a predicate
	term_to_meta(Goal,Meta),
	try(Meta,Tried),
	invert(Tried,InvTried),
	introduce_disj(\+,InvTried,Disjunction).
try1(Pred,Disjunction):- % negation of a predicate (not naf)
	is_negation(Pred,Goal),
	varset(Goal,Vars),
	term_to_meta(Goal,Meta),
	try(Meta,Tried),
	cneg_tried(Tried,Vars,NSols), %% the conjuction 
	combine(NSols, Disjunction).
try1(Pred,Disjunction):-
	Pred=(PredCond -> PredThen ; PredElse),!, % if-then-else
	% Probablemente no llegue nunca a estos casos de simplificacion 
	% porque esto ya se simplifica en el neg. Pero en caso de hacerlo 
	% probablemente daria problemas el \+ si no esta en stored_builtin
	Pred1=((PredCond,PredThen);(\+(PredCond),PredElse)),
        try1(Pred1,Disjunction).
try1(Pred,Disjunction):- % disjuction of predicates
	Pred=(Pred1;Pred2),!,
	term_to_meta(Pred1,MPred1),
	term_to_meta(Pred2,MPred2),
	try(MPred1,Disj1),
	try(MPred2,Disj2),
	combine([Disj1,Disj2],Disjunction).
try1(Pred,Disjunction):-
	Pred=(Pred1,Pred2),!, % conjuction of predicates
	term_to_meta(Pred1,MPred1), 
	term_to_meta(Pred2,MPred2), 
	try(MPred1,Disj1),          
	try(MPred2,Disj2),         
	append(Disj1,Disj2,Disjunction).
% General case for common predicates
try1(Pred,Disjunction):- % a common predicate whose clauses are stored
	% We use a predicate instead of a meta_predicate for the setof/3
	% because with meta_predicates it doesn't work properly
 	setof((Pred,Body),stored_clause(Pred,Body),Clauses),!,
 	add_head_body(Clauses,Pred,Disjunction). 

% General case for builtin predicates
try1(Pred,Disjunction):- % a builtin predicate whose clauses are stored
 	setof((Pred,Body),stored_builtin_clause(Pred,Body),Clauses),!,
 	add_head_body(Clauses,Pred,Disjunction).

%try(Pred,[[no_try(Pred)]]). % It is a predefined operator
% If apears no_try/1 in any moment it is because there are a mistake
% because the predicate that we are going to negate is not in 
% stored clause. It is an omission because every predicate
% must be there or in stored_builtin if it is a builtin

% add_head_body(Clauses,Pred,Disjunction) return a Disjunction (like
% the described below) from the clauses of a predicate instantiated
% with the goal Pred. The format of Clauses is a list of pairs with
% the information of every clause: (Head,Body) where Body is the 
% list of every subgoal of the body of the clause.
add_head_body([],_Pred,[]). 
add_head_body([(Head,Body)|Clauses],Pred,[Conjunction|Disjunction]):-
	Pred=..[Functor|ArgsPred],
	Head=..[Functor|ArgsHead],
	get_head(ArgsHead,ArgsPred,Conj),
	append(Conj,Body,Conjunction),
	add_head_body(Clauses,Pred,Disjunction).

% get_head(ArgsHead,ArgsPred,Conj) retuns in Conj the list of subgoals
% that represent the equalities that must be done between variables of
% the goal Pred whose arguments are ArgsPred (a list of them) and the 
% patterns in the head of a clause Head whose arguments are ArgsHead
get_head([],[],[]).
get_head([ArgHead|ArgsHead],[ArgPred|ArgsPred],Conjunction):-
	identify(ArgHead,ArgPred,Terms),
	get_head(ArgsHead,ArgsPred,Conj),
	append(Terms,Conj,Conjunction).
	
% identify(ArgHead,ArgPred,Conj) retuns in Conj the list of subgoals
% that represent the equalities that must be done between variables of
% the argument ArgPred and the patterns in the argument ArgPred
identify(_ArgHead,ArgPred,[]):-
	ground(ArgPred),!. % ArgHead==ArgPred
identify(ArgHead,ArgPred,[]):-
	var(ArgHead),
	var(ArgPred),!,
	ArgHead=ArgPred. % PROBLEMA: une los atributos sin simplificar
identify(ArgHead,ArgPred,['dist:eq'(ArgPred,ArgHead)]):-
	var(ArgHead),!. % nonvar(argPred)
identify(ArgHead,ArgPred,['dist:eq'(ArgPred,ArgHead)]):-
	var(ArgPred),!. % it never happends
identify(ArgHead,ArgPred,Terms):-
	ArgHead=..[Functor|ArgsHead],
	ArgPred=..[Functor|ArgsPred],
	get_head(ArgsHead,ArgsPred,Terms).


% invert(Disjunction,InvDisjunction) returns a list of lists obtained of
% negating the disjunction of conjunction that represent the list of
% list Disjunction 
invert([],[]).
invert([Conj|Disj],InvDisjunction):-
	invert(Disj,InvDisj),
	comb_elems(Conj,InvDisj,InvDisjunction).

comb_elems([],_InvDisj,[]):-!.
comb_elems([Elem|Conj],[],[[Elem]|Disj]):- !,
        comb_elems(Conj,[],Disj).
comb_elems([Elem|Conj],InvDisj,InvDisjunction):-
	add_elems(InvDisj,Elem,Firsts),
	comb_elems(Conj,InvDisj,Rest),
	append(Firsts,Rest,InvDisjunction).

add_elems([],_Elem,[]).
add_elems([Conj|Disj],Elem,[[Elem|Conj]|Rest]):-
	add_elems(Disj,Elem,Rest).

% introduce_disj(DisjIn,Pred,DisjOut) retuns in DisjOut the same disjunction of
% conjuctions of DisjIn replacing every element in the conjuctions Elem
% for a call to Pred(Elem)
introduce_disj([],_Pred,[]).
introduce_disj([ConjIn|DisjIn],Pred,[ConjOut|DisjOut]):-
	introduce_conj(ConjIn,Pred,ConjOut),
	introduce_disj(DisjIn,Pred,DisjOut).


% introduce_conj(ConjIn,Pred,ConjOut) retuns in ConjOut the same 
% conjuction of ConjIn replacing every element in the conjuctions Elem
% for a call to Pred(Elem)
introduce_conj([],_Pred,[]).
introduce_conj([Elem|ConjIn],Pred,[New|ConjOut]):-
	New =.. [Pred,Elem], 
	introduce_conj(ConjIn,Pred,ConjOut).

	

