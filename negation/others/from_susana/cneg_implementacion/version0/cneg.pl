%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Package  CNEG for including constructive negation in a program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% To be able to call Pred from cneg
:- meta_predicate
        cneg(goal). 

:- use_module(library(metaterms),[varset/2]). 
:- use_module(library(lists),[append/3]).
:- use_module(library(idlists),[memberchk/2]). 
:- use_module(library(aggregates),[setof/3]).
:- use_module(engine(internals),[term_to_meta/2]).

%:- use_module('/home/susana/tesis/micodigo/ciao/actual/dist.pl',[dist/2]).
:- use_module(dist,[dist/2]).
% stored_clause/2 is going to be a dynamic predicate in the
% programs that are going to include this package

:- data stored_clause/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cneg(Goal) makes the constructive negation of the goal Goal.
cneg(Goal):-  
	varset(Goal,GoalVars),
	cneg_aux(Goal,GoalVars).
 
% cneg_aux(Goal,GoalVars)makes the constructive negation of the goal
% Goal considering the set of variables GoalVars as the variables
% of the goal.
cneg_aux(Goal,GoalVars):-
	frontier(Goal,Frontier),
	negate_conj_frontier(Frontier,Goal,GoalVars,LSolutions),
	combine(LSolutions).

% frontier(Goal,Frontier) obtains in Frontier the frontier of the 
% goal Goal.
% It is a list of list that represent the disjunction of its
% elements where each element is a conjunction of subgoals.
% !!!!!!!!!!!! Tengo que añadir el desglose de negaciones de 
% conjunciones y disjunciones.
frontier(Meta,Frontier):-
	term_to_meta(Goal,Meta),
	setof((Goal,Body),stored_clause(Goal,Body),Frontier).

% negate_conj_frontier(Frontier,Goal,LSolutions) returns in LSolutions
% a list with the same number of elements of the list Frontier.
% Each element of LSolutions will be one solution of negating the 
% conjuction that is in the same position in Frontier. The predicate 
% is a "map" where the fuction that is applied to each element of
% Frontier (each conjunction) is the negation.
% Frontier is the frontier of subgoals of deep 1 of Goal and we need
% it to keep the variables of the Goal and obtain the unifications
negate_conj_frontier([],_Goal,_GoalVars,[]).
negate_conj_frontier([Conj|Frontier],Goal,GoalVars,[SolConj|LSolutions]):-
	negate_conj(Conj,Goal,GoalVars,SolConj),
	negate_conj_frontier(Frontier,Goal,GoalVars,LSolutions).

% negate_conj(Conj,Goal,GoalVars,SolConj) returns in SolConj a solution
% of the negation of the conjunction of subgoals Conj of the goal Goal.
% A conjunction is a pair (H,B) with the head of the clause in H and
% the list of subgoals of the body in B.
% It fails if the negation of the conjunction has no solutions
negate_conj(C,G,GoalVars,SolC):-
	organization_conj(C,G,GoalVars,I,D,R),
	normalization_conj(I,D,R,GoalVars,In,Dn,ImpVars,ExpVars), 
	negation_conj(In,Dn,R,GoalVars,ImpVars,ExpVars,SolC).
	
% organization_conj(C,G,I,D,R) returns in I the values that are asigned
% to the variables of GoalVars in the equalities, in D the list of the
% disequialities and in R the rest of subgoals of C
organization_conj((Head,BodyList),G,GoalVars,I,D,R):-
	setof(GoalVars,(G=Head),[Values]),
	obtain_eq_renamed(GoalVars,Values,Ih),
	organization_body(BodyList,Ib,D,R),
	append(Ih,Ib,I).

%% rename_vars(GoalVars,Values) the elements of Values that are variables
%% are renamed with the same name that the variable in the same place
%% in GoalVars.
%rename_vars([],[]).
%rename_vars([Var|GoalVars],[Value|Values]):-
%	var(Value),!,
%	Var=Value,
%	rename_vars(GoalVars,Values).
%rename_vars([_Var|GoalVars],[_Value|Values]):-
%	rename_vars(GoalVars,Values).

			
% obtain_eq(GoalVars,Values,I) retuns the list I where the element of the
% position i is the subgoal Vari=Valuei where Vari is the element
% of GoalVars in the position i and Valuei is the element of Values
% in the position i.
% If an element of Values is a variable then it is renamed with the same
% name that in GoalVars and the equiality isn't stored in I.
obtain_eq_renamed([],[],[]).
obtain_eq_renamed([Var|GoalVars],[Value|U],I):-
	var(Value),!,
	Var=Value,
	obtain_eq_renamed(GoalVars,U,I).
obtain_eq_renamed([Var|GoalVars],[Value|U],[(Var=Value)|I]):-
	obtain_eq_renamed(GoalVars,U,I).

% organization_body(BodyList,Ib,D,R) returns in Ib the equalities,
% in D the disequalities and in R the rest of subgoals of BodyList
organization_body([],[],[],[]).
organization_body([T1=T2|BodyList],I,D,R):- !,
	varset(T1,T1Vars),
	varset(T2,T2Vars),
	append(T1Vars,T2Vars,T12Vars),
	remove_duplicates(T12Vars,Vars),
	setof(Vars,(T1=T2),Values),
	obtain_eq_renamed(Vars,Values,Ih),
	organization_body(BodyList,Ib,D,R),
	append(Ih,Ib,I).
organization_body([dist(T1,T2)|BodyList],I,D,R):- !,
	varset(T1,T1Vars),
	varset(T2,T2Vars),
	append(T1Vars,T2Vars,T12Vars),
	remove_duplicates(T12Vars,Vars),
	setof(Vars,dist(T1,T2),[Values]),
	(Values=[]->
	    D=[]
	;
	    Values=[Value|_],
	    Vars=Values, % To unify the renamed variables of setof
	    obtain_diseq(Value,Dh),
	    organization_body(BodyList,I,Db,R),
	    append(Dh,Db,D)
	).
organization_body([Other|BodyList],I,D,[Other|R]):- 
	organization_body(BodyList,I,D,R).


% remove_duplicates(L1, L2) L2 tiene los elementos de L1 sin repetidos 
remove_duplicates([],[]).
remove_duplicates([X|L1],L2):-
	memberchk(X,L1),!,
	remove_duplicates(L1,L2).
remove_duplicates([X|L1],[X|L2]):-
	remove_duplicates(L1,L2).

% obtain_diseq(Value,D) retuns the list D where the element of the
% position i is a list of disequalities of the form X/Y that are in a 
% solution obteined from the atributes of the corresponding variables 
% in Values. The attributes of all variables in Values will be the same.
obtain_diseq(Value,D):-
	get_attribute(Value,formula(Value,Form)),
	member(D,Form). % It will generate all the solutions

% normalization_conj(I,D,R,GoalVars,In,Dn,ImpVars,ExVars) 
% returns In and Dn that are the equalities of I and the disequalities
% of D but after normalizating them.
normalization_conj(I,D,R,GoalVars,I1,D1,ImpVars,ExpVars):-
	varset(R,RVars),
	difference(RVars,GoalVars,RelVars),
	varset(I,IVars),
	difference(IVars,GoalVars,ImpVars),
	difference(RVars,ImpVars,ExpVars),
	eliminate_redundant_var(I,GoalVars,Iv),  
	eliminate_repeated_eq(Iv,I1),
	eliminate_irrelevant_disequalities(D,ImpVars,RelVars,D1). 

% difference(Vars,NFVars,FreeVars) retuns in FreeVars the sublist of elements
% of Vars that do not appear in NFVars
difference([],_NFVars,[]).
difference([Var|Vars],NFVars,FreeVars):-
	memberchk(Var,NFVars),!,
	difference(Vars,NFVars,FreeVars).
difference([Var|Vars],NFVars,[Var|FreeVars]):-
	difference(Vars,NFVars,FreeVars).

% eliminate_redundant_var(I,GoalVars,Iv) eliminates from I the equalities 
% that contain a variable that is not in GoalVars and makes the unification
eliminate_redundant_var([],_GoalVars,[]).
eliminate_redundant_var([(Var=Value)|I],GoalVars,Iv):-
	varset((Var=Value),Vars),
	member(V,Vars),
	\+ memberchk(V,GoalVars),!,
	Var=Value,
	eliminate_redundant_var(I,GoalVars,Iv).
eliminate_redundant_var([(Var=Value)|I],GoalVars,[(Var=Value)|Iv]):-
	eliminate_redundant_var(I,GoalVars,Iv).


% eliminate_repeated_eq(I,I1) eliminates from I the repeated equalities
eliminate_repeated_eq([],[]).
eliminate_repeated_eq([X=Y|L1],L2):-
	memberchk(X=Y,L1),!,
	eliminate_repeated_eq(L1,L2).
eliminate_repeated_eq([X=Y|L1],L2):-
	memberchk(Y=X,L1),!,
	eliminate_repeated_eq(L1,L2).
eliminate_repeated_eq([E|L1],[E|L2]):-
	eliminate_repeated_eq(L1,L2).

% eliminate_irrelevant_disequalities(D,ImpVars,RelVars,D1) returns D1
% that is D but without disequalities that contains any variable that
% is not in ImpVars neither RelVars
eliminate_irrelevant_disequalities([],_ImpVars,_RelVars,[]).
eliminate_irrelevant_disequalities([Diseq|D],ImpVars,RelVars,D1):-
	varset(Diseq,Vars),
	member(V,Vars),
	\+ memberchk(V,ImpVars),
	\+ memberchk(V,RelVars),!,
	eliminate_irrelevant_disequalities(D,ImpVars,RelVars,D1).
eliminate_irrelevant_disequalities([Diseq|D],ImpVars,RelVars,[Diseq|D1]):-
	eliminate_irrelevant_disequalities(D,ImpVars,RelVars,D1).

% negation_conj(I,D,R,GoalVars,ImpVars,ExpVars,SolC) returns SolC
% that is one of the solutions of the conjunction that is divided in 
% I, D and R (equalities, disequalities and rest of subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables 
negation_conj(I,D,R,GoalVars,ImpVars,ExpVars,SolC):-
	divide_formula(D,ExpVars,Dimp,Dexp),
	divide_formula(R,ExpVars,Rimp,Rexp),
	negation_formula(I,Dimp,Dexp,Rimp,Rexp,GoalVars,ImpVars,ExpVars,SolC).

% divide_formula(F,ExpVars,Fimp,Fexp) divide F between Fimp and Fexp.
% In Fexp are the elements of F with any variables of ExpVars and
% the rest of elements of F will be in Fimp
divide_formula([],_ExpVars,[],[]).
divide_formula([Term|F],ExpVars,Fimp,[Term|Fexp]):-
	varset(Term,Vars),
	member(V,Vars),
	memberchk(V,ExpVars),!,
	divide_formula(F,ExpVars,Fimp,Fexp).
divide_formula([Term|F],ExpVars,[Term|Fimp],Fexp):-
	divide_formula(F,ExpVars,Fimp,Fexp).


% negation_formula(I,Dimp,Dexp,Rimp,Rexp,GoalVars,ImpVars,ExpVars,SolC)
% returns SolC that is one of the solutions of the conjunction that is divided
% I, Dimp,Dexp,Rimp and Rexp (equalities, disequalities and rest of subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables 
negation_formula(I,_Dimp,_Dexp,_Rimp,_Rexp,GoalVars,_ImpV,_ExpV,SolC):-
	negate_I(I,GoalVars,SolC).
negation_formula(I,Dimp,_Dexp,_Rimp,_Rexp,_GoalV,_ImpV,_ExpV,SolC):-
	negate_Dimp(Dimp,Sol),
	append(I,Sol,SolC).
negation_formula(I,Dimp,Dexp,Rimp,Rexp,_GoalV,ImpVars,ExpsVars,SolC):-
	append(I,Dimp,I_Dimp),
	negation_formula1(I_Dimp,Dexp,Rimp,Rexp,ImpVars,ExpsVars,SolC).


% negation_formula1(I_Dimp,Dexp,Rimp,Rexp,ImpVars,ExpVars,SolC)
% returns SolC that is one of the solutions of the conjunction that is divided
% I_Dimp,Dexp,Rimp and Rexp (equalities and disequalities and rest subgoals).
% GoalVars, ImpVars and ExpVars are set of useful variables.
% Only returns the solution of negating the rest of subgoals. 
negation_formula1(I_Dimp,_Dexp,Rimp,_Rexp,_ImpsV,_ExpsV,SolC):-
	negate_Rimp(Rimp,Sol),
	append(I_Dimp,Sol,SolC).
negation_formula1(I_Dimp,Dexp,Rimp,Rexp,ImpVars,ExpVars,SolC):-
	append(I_Dimp,Rimp,I_Dimp_Rimp),
	append(Dexp,Rexp,DRexp),
	negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,Sol),
	append(I_Dimp_Rimp,Sol,SolC). 

% negate_I(I,GoalVars,Sol) obtains in Sol a solution of negating I.
negate_I(I,GoalVars,Sol):-
	member(Eq,I),
	negate_eq(Eq,GoalVars,Sol).

% negate_eq(Eq,GoalVars,Sol) returns in Sol a solution of the negation
% of the equality Eq.
negate_eq(T1=T2,GoalVars,dist(T1f,T2f)):-
	varset(T1=T2,Vars),
	difference(Vars,GoalVars,FreeVars),
	replace_free_vars(FreeVars,T1,T1f),
	replace_free_vars(FreeVars,T2,T2f).

% replace_free_vars(FreeVars,T,Tf) returns Tf that is T but substituting
% each aparition of a variable X from FreeVars by fA(X)
% No puede haber variables repetidas en freevars o habrá problemas
replace_free_vars([],T,T).
replace_free_vars([X|FreeVars],T,Tf):- 
	replace(T,X,fA(X),Tf1), % Free variables will be universally
	replace_free_vars(FreeVars,Tf1,Tf). % quantified at the negation

% replace(Form,Var,Value,Form1) returns Form1 that is the term
% Form but replacing aparitions of Var by Value
replace(Term,Var,Value,Value):-
     var(Term),
     Term==Var,!.
replace(Term,Var,_Value,Term):-
     var(Term),
     Term\==Var,!.
replace(Term,Var,Value,Term1):-
     Term=..[Functor|Args],
     replace_args(Args,Var,Value,Args1),
     Term1=..[Functor|Args1].

replace_args([],_,_,[]).
replace_args([Arg|Rest],Var,Value,[Arg1|Rest1]):-
     replace(Arg,Var,Value,Arg1),
     replace_args(Rest,Var,Value,Rest1).

% negate_Dimp(Dimp,SolC) obtain in SolC a solution of negating Dimp.
negate_Dimp([D|_Dimp],[(T1=T2)]):-
	replace(D,fA(X),X,(T1/T2)). %% Quantifications change at the negation
negate_Dimp([D|Dimp],[D|SolC]):-
	negate_Dimp(Dimp,SolC).

% negate_Rimp(Rimp,SolC) obtain in SolC a solution of negating Rimp.
negate_Rimp([R|_Rimp],[cneg(R)]).
negate_Rimp([R|Rimp],[R|SolC]):-
	negate_Rimp(Rimp,SolC).

% negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,SolC) obtain in
% SolC a solution of negating Dexp y Rexp juntos.
negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,[cneg_aux(DR,ImpVars)]):-
	replace_free_vars(ExpVars,DRexp,DR).

% combine(LSolutions) combines all its elements that are the solutions
% of the negation of the conjunctions of the Frontier to obtain a 
% solution of the negation of the main Goal. The rest of solutions will
% be obtained by backtracking.
combine([]).
combine([Sol|Rest]):-
	call(Sol),
	combine(Rest).

%%% If this gives us problems could be because of metaterms and them

%combine([Sol|Rest]):-
%	term_to_meta(Sol,MSol),
%	call(MSol),
%	combine(Rest).

% and at the begining:
% :- use_module(engine(internals),[term_to_meta/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cneg_tr contains predicates to obtain stored_clause/2 facts
% that we need to aply constructive negation
%:- load_compilation_module(library('cneg/cneg_tr')). CUANDO SEA LIBRERIA
:- load_compilation_module(.('cneg_tr')).


% flat_dpred/3 adds a stored_clause/2 fact for each clause or
% fact from the program that includes package cneg.pl but
% it does it discontinously
%:- add_sentence_trans(flat_dpred/3).

% flat_cpred/3 adds a stored_clause/2 fact for each clause or
% fact from the program that includes package cneg.pl but
% it does it continously
:- add_sentence_trans(flat_cpred/3).

