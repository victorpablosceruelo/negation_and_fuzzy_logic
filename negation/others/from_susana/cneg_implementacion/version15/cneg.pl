%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Package  CNEG for including constructive in a program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:- module(cneg,_).
% To be able to call Pred from cneg

:- meta_predicate cneg(goal).
:- meta_predicate stored_pred(goal,?). 

:- use_module(library(lists),[append/3]).
:- use_module(library(idlists),[memberchk/2]).
:- use_module(library(aggregates),[setof/3]).
:- use_module(engine(internals),[term_to_meta/2]).
:- use_module(library(metaterms),[varset/2,varsbag/3]).
% ask/2 is too in metaterms
:- use_module(dist,[dist/2]). 

%:- use_module('/home/susana/tesis/micodigo/ciao/actual/dist.pl',[dist/2]).
%:- use_module(library(dist),[dist/2]).
%% Esta linea la podremos incluir 
% sin problemas cuando list sea una libreria.

%:- reexport(dist,[dist/2]).  

% stored_clause/2 is going to be a dynamic predicate in the
% programs that are going to include this package
:- data stored_clause/2.

% stored_pred/2 is going to be a dynamic predicate in the
% programs that are going to include this package
:- data stored_pred/2.
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cneg(Meta) makes the constructive negation of the goal Meta.
cneg(true):- 
	!,
	fail.
cneg(fail):-
	!.	
cneg(Meta):-  
	term_to_meta(Goal,Meta),
	compound_to_term(Goal,Term), 
	varset(Term,GoalVars),
	cneg_auxiliar(Term,GoalVars,[]). 
 
% cneg_auxiliar(Goal,GoalVars,UnivVars) makes the constructive negation 
% of the Goal considering the set of variables GoalVars as the variables
% of the goal and UnivVars the universal quantified variable of it.
cneg_auxiliar(Goal,GoalVarsI,UnivVars):-
	cneg_aux(Goal,GoalVarsI,UnivVars),
	put_universal(UnivVars).

% cneg_aux(Goal,GoalVars,UnivVars) makes the constructive negation of 
% the Goal considering the set of variables GoalVars as the variables
% of the goal and UnivVars the universal quantified variable of it.
cneg_aux(Goal,GoalVarsI,UnivVars):-
	varset(GoalVarsI,GoalVars), 
	frontier(Goal,Frontier),
	negate_conj_frontier(Frontier,Goal,GoalVars,UnivVars,LSolutions),
%	rm_cons_fA(UnivVars,Univs),
	no_unifications(UnivVars,UnivVars),

%parece que no unifications devuelve valores por backtracking cuando solo debería chequear cosas.
	combine(LSolutions),
	varset(LSolutions,VarsSol),
	no_unifications(VarsSol,UnivVars).         
% 	difference(VarsSol,GoalVars,NewVars), 
%	difference(NewVars,UnivVars,FreeVars),
%	put_universal(FreeVars).


% rm_cons_fA(UnivVars,Univs) removes the constructor fA from the args
% of UnivVars and fails if there is something different than a variable
% of a fA variable
rm_cons_fA([],[]).
rm_cons_fA([V|Vars],[V|Vars1]):-
	var(V),!,
	rm_cons_fA(Vars,Vars1).
rm_cons_fA([fA(V)|Vars],[V|Vars1]):-
	!,
	rm_cons_fA(Vars,Vars1).

% put_universal(UnivVars) substitutes variables of UnivVars by 
% universal quantified variables fA(_)
put_universal([]).
put_universal([Var|UnivVars]):-
	var(Var),!, % the cuantification keeps
	Var=fA(_NewVar),
	put_universal(UnivVars).
put_universal([_Value|UnivVars]):- % the variable has change its quantification
	put_universal(UnivVars).

% compound_to_term(G,T) obtains the possibly compound term T
% fron the term G that comes from a metaterm and contains the module
% qualification
compound_to_term((M1;M2),(G1;G2)):- !,
	compound_to_term(M1,G1),
	compound_to_term(M2,G2).
compound_to_term((M1,M2),(G1,G2)):- !,
	compound_to_term(M1,G1),
	compound_to_term(M2,G2).
compound_to_term(T,T):- 
	is_eq_diseq(T),!.
compound_to_term(T,G):- % It's a simple term
	term_to_meta(T,M),
	stored_pred(M,G),!.

% is_eq_diseq(T) success if T is dist(_,_) or _=_ or _\==_ or _==_ 
is_eq_diseq(dist(_,_)).
is_eq_diseq((_=_)). 
%is_eq_diseq((_==_)). 
%is_eq_diseq((_\==_)). 

 
% frontier(Goal,Frontier) obtains in Frontier the frontier of the 
% goal Goal.
% It is a list of list that represent the disjunction of its
% elements where each element is a conjunction of subgoals.
frontier((G1;G2),Frontier):- !,
	frontier(G1,F1),
	frontier(G2,F2),
	append(F1,F2,Front),
	filter_frontier(Front,(G1;G2),Frontier).
frontier((G1,G2),Frontier):- !,
	frontier(G1,F1),
	frontier(G2,F2),
	conjunction(F1,F2,Front),
	filter_frontier(Front,(G1,G2),Frontier).
frontier(dist(X,Y),[(dist(X,Y),[dist(X,Y)])]):- !.
frontier((X=_Y),[(X=X,[])]):- !.
%frontier((X==Y),[(X==Y,[X==Y])]):- !.
%frontier((X\==Y),[(X\==Y,[X\==Y])]):- !.
frontier(Goal,Frontier):-
	setof((Goal,Body),stored_clause(Goal,Body),Frontier),!.
frontier(_Goal,[]).

% filter_frontier(Front,G,Frontier) returns in Frontier the list
% with the elementos of Front whose head unifies with G.
filter_frontier([],_G,[]).
filter_frontier([(H,B)|Front],G,[(H1,B1)|Frontier]):-
	copy_term((H,B),(H1,B1)), % Using the copy we propagate
	copy_term(G,G1),          % repeated variables
	G1=H1,!,
 	filter_frontier(Front,G,Frontier).
% filter_frontier([(H,B)|Front],G,[(H,B)|Frontier]):-
% 	ask(H,G),!,
% 	filter_frontier(Front,G,Frontier).
% filter_frontier([(H,B)|Front],G,[(H,B)|Frontier]):-
% 	ask(G,H),!,
% 	filter_frontier(Front,G,Frontier).
filter_frontier([(_H,_B)|Front],G,Frontier):- 
	filter_frontier(Front,G,Frontier).

% conjuntcion(F1,F2,F3) returns F3 that is the conjunction of the list 
% of lists F1 and F2 after using the distributive rule
conjunction([],_F2,[]):-!.
conjunction([Conj|F1],F2,F5):-
        distributive(F2,Conj,F3),
        conjunction(F1,F2,F4),
        append(F3,F4,F5).

% distributive(L1,E,L2) returns L2 that is the list obtained after 
% using the distributive rule to the list of list L1 with respect
% the list E. 
% L1 and L2 are disjunction of conjunctions and E is a conjunction.
distributive([],_Conj,[]).
distributive([Conj2|L],Conj1,[Conj3|L3]):-
        conj_clauses(Conj1,Conj2,Conj3),
        distributive(L,Conj1,L3).

% conj_clauses(C1,C2,C3) joins in C3 clauses C1 and C2
conj_clauses((H1,B1),(H2,B2),((H1,H2),B3)):-
	append(B1,B2,B3).
 
% negate_conj_frontier(Frontier,Goal,LSolutions) returns in LSolutions
% a list with the same number of elements of the list Frontier.
% Each element of LSolutions will be one solution of negating the 
% conjuction that is in the same position in Frontier. The predicate 
% is a "map" where the fuction that is applied to each element of
% Frontier (each conjunction) is the negation.
% Frontier is the frontier of subgoals of deep 1 of Goal and we need
% it to keep the variables of the Goal and obtain the unifications
negate_conj_frontier([],_Goal,_GoalVars,_UnivVars,[]).
negate_conj_frontier([Conj|Frontier],Goal,GoalVars,UnivVars,[SolConj|LSolutions]):-
	negate_conj(Conj,Goal,GoalVars,UnivVars,SolConj),
	negate_conj_frontier(Frontier,Goal,GoalVars,UnivVars,LSolutions).
 
% negate_conj(Conj,Goal,GoalVars,SolConj) returns in SolConj a solution
% of the negation of the conjunction of subgoals Conj of the goal Goal.
% A conjunction is a pair (H,B) with the head of the clause in H and
% the list of subgoals of the body in B.
% It fails if the negation of the conjunction has no solutions
negate_conj(C,G,GoalVars,_UnivVars,[]):- % The clause fails, the negation is true
	organization_conj(C,G,GoalVars,[fail],_D,_R),!.
negate_conj(C,G,GoalVars,UnivVars,SolC):-
	organization_conj(C,G,GoalVars,I,D,R),
	((I=[],D=[],R=[]) ->
	    fail  % Its always true and the negation fails
	;
	    varset(G,NowGoalVars),
	    normalization_conj(I,D,R,GoalVars,NowGoalVars,In,Dn,Rn,ImpVars,ExpVars), 
	    negation_conj(In,Dn,Rn,GoalVars,ImpVars,ExpVars,UnivVars,SolC)
	).
	
% varset_order(Term,Vars) returns in Vars the variables of Term
% without duplicates and in the same order that appear in Term
varset_order(Term,Vars):-
	varsbag(Term,VarsDup,[]),
	remove_duplicates(VarsDup,Vars).

% organization_conj(C,G,I,D,R) returns in I the values that are asigned
% to the variables of GoalVars in the equalities, in D the list of the
% disequialities and in R the rest of subgoals of C
organization_conj((Head,BodyList),G,_GoalVars,I,D,R):-
	copy_term(G,G1),
	copy_term(Head,Head1),
	varset_order(G1,GoalVars1), 
	varset_order(G,GVars),
	varset_order(Head,HeadVars),
	varset_order(Head1,HeadVars1), 
	setof(GoalVars1,(G1=Head1),[Values]),!, % Si hay unificaciones en cabeza
	unifications_nonGoalvars(HeadVars1,HeadVars),
	(Values=[] ->
	     organization_body(BodyList,I,D,R)
	;
	    replace_list(HeadVars1,HeadVars,Values,Values1),
	    get_eq_head(GVars,Values1,Ih),
	    organization_body(BodyList,Ib,D,R),
	    ( Ib==[fail] ->
	        I=Ib  %% The body fails
	    ;
		append(Ih,Ib,I)
	    )
	).
organization_conj((_Head,BodyList),_G,_GoalVars,I,D,R):-
	% Si no hay unificaciones en la cabeza
	organization_body(BodyList,I,D,R).

% unifications_nonGoalvars(HeadVars1,HeadVars) unifies variables
% of HeadVars with the corresponding values of HeadVars1
% when the values are not variables.
unifications_nonGoalvars([],[]).
unifications_nonGoalvars([Value|HeadVars1],[_Var|HeadVars]):-
	var(Value),!,
	unifications_nonGoalvars(HeadVars1,HeadVars).
unifications_nonGoalvars([Value|HeadVars1],[Var|HeadVars]):-
	Var=Value,  % It is not a variable, then we unify
	unifications_nonGoalvars(HeadVars1,HeadVars).

% get_eq_head(GoalVars,Values,Ih) retuns in Ih the list of equalities
% where Values is a list with one only list of values inside
get_eq_head([],[],[]).
get_eq_head([Var|GoalVars],[X|Vs],[(Var=X)|I]):-
	get_eq_head(GoalVars,Vs,I). 

% organization_body(BodyList,Ib,D,R) returns in Ib the equalities,
% in D the disequalities and in R the rest of subgoals of BodyList
organization_body([],[],[],[]).
organization_body([T1=T2|BodyList],I,D,R):- 
	varset_order(T1,T1Vars),
	varset_order(T2,T2Vars),
	append(T1Vars,T2Vars,T12Vars),
	remove_duplicates(T12Vars,Vars),
	setof(Vars,(T1=T2),[Values]),!,  %%% Problems????????
	(Values=[] ->
	    organization_body(BodyList,I,D,R)
	;
	    get_eq_head(Vars,Values,Ih),
	    organization_body(BodyList,Ib,D,R),
	    append(Ih,Ib,I)
	).
organization_body([_T1=_T2|_BodyList],[fail],_D,_R):- !.
organization_body([dist(T1,T2)|BodyList],I,D,R):- 
	varset_order(dist(T1,T2),VarsI),
	copy_term(dist(T1,T2),DNew),
	varset_order(DNew,VarsNew),
        detach_attribute_all(VarsNew),
        remove_duplicates(VarsNew,Vars),
%	varset(T1,T1Vars),
%	varset(T2,T2Vars),
%	append(T1Vars,T2Vars,T12Vars),
%	remove_duplicates(T12Vars,Vars),
	setof(VarsNew,DNew,Values),!,
	(Values=[[]] -> % The disequality is true
	    organization_body(BodyList,I,D,R)
	;
	    organization_body_diseq(Values,Vars,BodyList,I1,D1,R1),
	    replace_list(VarsNew,VarsI,(I1,D1,R1),(I,D,R))
	).
organization_body([dist(_T1,_T2)|_BodyList],[fail],_D,_R):-!. 
organization_body([Other|BodyList],I,D,[Other|R]):- 
	organization_body(BodyList,I,D,R).

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

% organization_body_diseq_aux(ValueVars,Vars,BodyList,I,D,R) is the same
% that organization_body/4 but for the organization of the disequalities
organization_body_diseq([],_Vars,BodyList,I,D,R):- !, %% The disequality is true then the negation
	organization_body(BodyList,I,D,R). %% is fail and this solution is eliminated
organization_body_diseq(Values,Vars,BodyList,I,D,R):-
	member(ValueVars,Values),
	organization_body_diseq_aux(ValueVars,Vars,BodyList,I,D,R).

% organization_body_diseq_aux(ValueVars,Vars,BodyList,I,D,R) it is the same
% that organization_body_diseq/6 but with a list of values instead with
% a list of lists of values
organization_body_diseq_aux([V|ValueVars],Vars,BodyList,I,D,R):-
	% We take one value V because the atribute is the same 
        % in all of them and we take the first one e.g.
	obtain_diseq(V,D1),
	replace_list([V|ValueVars],Vars,D1,Dh),
	replace_dist(Dh,Daux),
	organization_body(BodyList,I,Db,R),
	append(Daux,Db,D).

% replace_dist(Form,Form1) returns Form1 that is the term
% Form but replacing aparitions of / /2 by dist/2
replace_dist([],[]).
replace_dist([X/Y|List],[dist(X,Y)|ListD]):-
	replace_dist(List,ListD).

% remove_duplicates(L1, L2) L2 tiene los elementos de L1 sin repetidos 
remove_duplicates([],[]).
remove_duplicates([X|L1],L2):-
	memberchk(X,L1),!,
	remove_duplicates(L1,L2).
remove_duplicates([X|L1],[X|L2]):-
	remove_duplicates(L1,L2).

% obtain_diseq(V,D1) returns the list D where the element of the
% position i is a list of disequalities of the form X/Y that are in a 
% solution obteined from the atributes of the corresponding variables 
% in Values. The attributes of all variables in Values will be the same.
obtain_diseq(Value,D):-
	get_attribute(Value,formula(Value,Form)),
	member(D,Form). % It will generate all the solutions

% normalization_conj(I,D,R,GoalVars,NowGoalVars,In,Dn,Rn,ImpVars,ExVars) 
% returns In and Dn that are the equalities of I and the disequalities
% of D but after normalizating them.
normalization_conj(I,D,R,GoalVars,_NowGoalVars,I1,D1,Rv,ImpVars,ExpVars):-
	eliminate_redundant_var(I,D,R,GoalVars,Iv,Dv,Rv),  
	eliminate_repeated_eq(Iv,I1),
	varset(I1,IVars),
	union(IVars,GoalVars,ImpVars),
	varset(Rv,RVars),
	difference(RVars,ImpVars,ExpVars),
	difference(RVars,GoalVars,RelVars),
	eliminate_irrelevant_disequalities(Dv,ImpVars,RelVars,D1). 
 
% difference(Vars,NFVars,FreeVars) retuns in FreeVars the sublist of elements
% of Vars that do not appear in NFVars
difference([],_NFVars,[]).
difference([Var|Vars],NFVars,FreeVars):-
	memberchk(Var,NFVars),!,
	difference(Vars,NFVars,FreeVars).
difference([Var|Vars],NFVars,[Var|FreeVars]):-
	difference(Vars,NFVars,FreeVars).

% union(Vars1,Vars2,Vars) retuns in Vars the union of elements
% of Vars1 and Vars2 
union([],Vars2,Vars2).
union([Var|Vars1],Vars2,Vars):-
	memberchk(Var,Vars2),!,
	union(Vars1,Vars2,Vars).
union([Var|Vars1],Vars2,[Var|Vars]):-
	union(Vars1,Vars2,Vars).

% eliminate_redundant_var(I,D,R,GoalVars,Iv,Dv,Rv) eliminates from 
% I the equalities 
% X=Y that contain a variable, X or Y,that is not in GoalVars and makes 
% the unification of the corresponding value in D and R
eliminate_redundant_var([],D,R,_GoalVars,[],D,R).
eliminate_redundant_var([(Var=Value)|I],D,R,GoalVars,Iv,Dv,Rv):-
	var(Var),
	\+ memberchk(Var,GoalVars),!,
	replace((I,D,R),Var,Value,(I1,D1,R1)),
	eliminate_redundant_var(I1,D1,R1,GoalVars,Iv,Dv,Rv).
eliminate_redundant_var([(Var=Value)|I],D,R,GoalVars,Iv,Dv,Rv):-
	var(Value),
	\+ memberchk(Value,GoalVars),!,
	replace((I,D,R),Value,Var,(I1,D1,R1)),
	eliminate_redundant_var(I1,D1,R1,GoalVars,Iv,Dv,Rv).
eliminate_redundant_var([(Var=Value)|I],D,R,GoalVars,[(Var=Value)|Iv],Dv,Rv):-
	eliminate_redundant_var(I,D,R,GoalVars,Iv,Dv,Rv).

% % eliminate_redundant_var(I,GoalVars,Iv) eliminates from I the equalities 
% % X=Y that contain a variable, X or Y,that is not in GoalVars and makes 
% % the unification.
% eliminate_redundant_var([],_GoalVars,_NowGoalVars,[]).
% eliminate_redundant_var([(Var=Value)|I],GoalVars,NowGoalVars,Iv):-
% 	var(Var),
% %	var(Value),
% 	\+ memberchk(Var,GoalVars),
% 	\+ memberchk(Var,NowGoalVars),!, % There isn't in the actual goal
% 	Var=Value,
% 	eliminate_redundant_var(I,GoalVars,NowGoalVars,Iv).
% eliminate_redundant_var([(Var=Value)|I],GoalVars,NowGoalVars,Iv):-
% 	var(Value),
% %	var(Var),
% 	\+ memberchk(Value,GoalVars),
% 	\+ memberchk(Value,NowGoalVars),!,
% 	Var=Value,
% 	eliminate_redundant_var(I,GoalVars,NowGoalVars,Iv).
% eliminate_redundant_var([(Var=Value)|I],GoalVars,NowGoalVars,[(Var=Value)|Iv]):-
% 	eliminate_redundant_var(I,GoalVars,NowGoalVars,Iv).

% eliminate_repeated_eq(I,I1) eliminates from I the repeated equalities
% or the equalities between the same variable, X=X.
eliminate_repeated_eq([],[]).
eliminate_repeated_eq([X=Y|L1],L2):-
	X==Y,!, 
	eliminate_repeated_eq(L1,L2).
eliminate_repeated_eq([X=Y|L1],L2):-
	memberchk(X=Y,L1),!,
	eliminate_repeated_eq(L1,L2).
eliminate_repeated_eq([X=Y|L1],L2):-
	memberchk(Y=X,L1),!, %% Different order
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
negation_conj(I,D,R,GoalVars,ImpVars,ExpVars,UnivVars,SolC):-
	divide_formula(D,ExpVars,Dimp,Dexp),
	divide_formula(R,ExpVars,Rimp,Rexp),
	copy_term(UnivVars,ExistVars),
	replace_list(UnivVars,ExistVars,(Dexp,Rexp),(Dexp1,Rexp1)),
	varset((Dimp,Dexp1,Rimp,Rexp1),VarsRest),
	union(VarsRest,GoalVars,VarsImportant),
	rm_redundant_eqs(I,VarsImportant,I1),
	negation_formula(I1,Dimp,Dexp1,GoalVars,Rimp,Rexp1,ImpVars,ExpVars,SolC).

% rm_redundant_eqs(I,VarsRest,I1) returns I1 that is I without the
% equalities that don't appears in VarsRest
rm_redundant_eqs([],_VarsRest,[]).
rm_redundant_eqs([Eq|I],VarsRest,[Eq|I1]):-
	eq_useful(Eq,VarsRest),!,
	rm_redundant_eqs(I,VarsRest,I1).
rm_redundant_eqs([_Eq|I],VarsRest,I1):-
	rm_redundant_eqs(I,VarsRest,I1).

% eq_useful(Eq,VarsRest) checks that variables of Eq are in VarsRest
eq_useful(X=Y,VarsRest):-
	memberchk(X,VarsRest),
	memberchk(Y,VarsRest). 
eq_useful(Eq,VarsRest):-
	copy_term((Eq,VarsRest),(Eq1,VarsRest1)),
	varset(Eq1,Vars),
	setof(Vars,Eq1,[Values]),
	asigned_vars(Vars,Values,VarsAsig),
	member(Var,VarsAsig),
	memberchk(Var,VarsRest1).

% asigned_vars(Vars,Values,VarsAsig) returns in VarsAsig the subset of Vars
% whose corresponding values in Values are significant (no variables)
asigned_vars([],_Values,[]).
asigned_vars([_Var|Vars],[Value|Values],VarsAsig):-
	var(Value),!,
	asigned_vars(Vars,Values,VarsAsig).
asigned_vars([Var|Vars],[_Value|Values],[Var|VarsAsig]):-
	asigned_vars(Vars,Values,VarsAsig).

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
negation_formula(I,_Dimp,_Dexp,GoalVars,_Rimp,_Rexp,_ImpV,_ExpV,[Sol]):-
	negate_I(I,GoalVars,Sol). 
negation_formula(I,Dimp,_Dexp,_GoalVars,_Rimp,_Rexp,_ImpV,_ExpV,SolC):-
	negate_Dimp(Dimp,Sol),
	append(I,Sol,SolC).
negation_formula(I,Dimp,Dexp,_GoalVars,Rimp,Rexp,ImpVars,ExpsVars,SolC):-
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

% negate_I(I,GoalVars,Sol) obtains in Sol a solution of negating I
negate_I(I,GoalVars,Sol):-
	member(Eq,I),
	negate_eq(Eq,GoalVars,Sol).

% negate_eq(Eq,GoalVars,Sol) returns in Sol a solution of the 
% negation of the equality Eq 
negate_eq(T1=T2,GoalVars,dist(T1,T2)):-
 	varset(T1=T2,Vars),         
 	difference(Vars,GoalVars,FreeVars), 
	put_universal(FreeVars).

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

% replace_list(LVars,LValues,Term1,Term2) returns in Term2 the same term
% Term1 but substituting each variable Vi from the position i of LVars
% by the value Valuei from the position i of LValues
replace_list([],[],Term,Term).
replace_list([Var|LVars],[Value|LValues],Term1,Term2):-
	var(Var),!,
	replace(Term1,Var,Value,TermAux),
	replace_list(LVars,LValues,TermAux,Term2).
replace_list([_Var|LVars],[_Value|LValues],Term1,Term2):-
	replace_list(LVars,LValues,Term1,Term2).

% negate_Dimp(Dimp,SolC) obtain in SolC a solution of negating Dimp.
negate_Dimp([dist(T1,T2)|_Dimp],[(T1=T2)]). 
negate_Dimp([D|Dimp],[D|SolC]):-
	negate_Dimp(Dimp,SolC).

% negate_Rimp(Rimp,SolC) obtain in SolC a solution of negating Rimp.
negate_Rimp([R|_Rimp],[cneg(R)]).
negate_Rimp([R|Rimp],[R|SolC]):-
	negate_Rimp(Rimp,SolC).

% negate_Dexp_Rexp(DRexp,ImpVars,ExpVars,SolC) obtain in
% SolC a solution of negating Dexp y Rexp juntos.
negate_Dexp_Rexp([],_,_,_):- !,fail.
negate_Dexp_Rexp(DRexp,ImpVs,ExpVs,[cneg_aux(DR,ImpVs,ExpVs)]):-
	get_conjunction(DRexp,DR).

% get_conjunction(List,Conj) obtain the conjunction Conj with the
% same elements that List
get_conjunction([X],X).
get_conjunction([X|L],(X,C)):-
	get_conjunction(L,C).

% combine(LSolutions) combines all its elements that are the solutions
% of the negation of the conjunctions of the Frontier to obtain a 
% solution of the negation of the main Goal. The rest of solutions will
% be obtained by backtracking.
combine([]).
combine([Sol|Rest]):-
	call_all(Sol),
	combine(Rest).

% call_all(L) calls all the subgoals of the list L
call_all([]).
call_all([G|L]):-
	call(G),
	call_all(L).

% no_unifications(UnivVars,UniversalVars) checks there is no unifications 
% of variables of UnivVars and that if they have an attribute is simplify 
% to true.
no_unifications([],_UnivVars).
no_unifications([Var|Vars],UnivVars):-
	no_unify(Var,UnivVars),
	no_unifications(Vars,UnivVars).

% no_unify(Va,UnivVarsr) checks there is no unifications of variable Var
% and that if it has an attribute is simplify to true.
no_unify(Var,UnivVars):-
	var(Var),
	get_attribute(Var,Attribute),!,
	check_is_true(Attribute,UnivVars).
no_unify(Var,_UnivVars):-
	var(Var),!.

% check_is_true(Attribute,UnivVars) checks the Attribute is a formula
% equivalent to true (it has the structure described in dist.pl)
% Este predicado habrá que ponerlo en dist.pl mejor.
check_is_true(formula(_Var,true),_UnivVars):-
	!.
check_is_true(formula(_Var,fail),_UnivVars):-
	!,
	fail.
check_is_true(formula(_Var,[]),_UnivVars):-
	!.
check_is_true(formula(_Var,[Conj]),UnivVars):- % Only a conjunction with 
	varset(Conj,Vars),                     % universal variables
	member(Var1,Vars), % Var1 is a variable of Conj
	memberchk(Var1,UnivVars), % and is universal
	!,
	fail.
check_is_true(formula(_Var,[Conj]),_UnivVars):- % Only a conjunction with fA 
	member(Diseq,Conj),
	%check_desig(Diseq,_VarfA,_T),
	appears_fA(Diseq),
	!,
	fail.
check_is_true(formula(_Var,[_Conj]),_UnivVars):- % Only a conj. without fA 
	!.
check_is_true(formula(_Var,F),UnivVars):-  % F = [Conj | Rest]
	disy_to_conj(F,Lc),
	check_all_true(Lc,UnivVars).

% check_all_true(Lc) checks that any list of Lc is simplified to true.
% Lc is a conjunction of disjunctions of disequalities.
check_all_true([],_UnivVars).
check_all_true([Disj|Lc],UnivVars):-
	simple_disj_true(Disj,UnivVars),
	check_all_true(Lc,UnivVars).

% simple_disj_true(Disj,UnivVars) checks the disjunction of 
% disequalities Disj is equivalent to true with the list of
% universal variables UnivVars
simple_disj_true(Disj,UnivVars):-
	skolem(Disj,SkDisj,UnivVars),
	\+ unify_neg_disj(SkDisj).

% skolem(Disj,SkDisj) obtain the same list Disj but substituting
% existencial quantified variables by skolem constants $ in SkDisj
% and removes universal quantification fA
skolem([],[],_UnivVars).
skolem([Diseq|Disj],[SkDiseq|SkDisj],UnivVars):-
	skolem_term(Diseq,SkDiseq,UnivVars),
	skolem(Disj,SkDisj,UnivVars).

% skolem_term(Diseq,SkDiseq,UnivVars) obtain the same term Diseq but
% substituting existencial quantified variables by skolem constants $ 
% in SkDiseq and keeps universal quantified variables
skolem_term(Term,Term,_UnivVars):-
	ground(Term),!.
skolem_term(Var,Var,UnivVars):-
	var(Var),
	memberchk(Var,UnivVars),!.
skolem_term(Term,$,_UnivVars):-
	var(Term),!. 
skolem_term(fA(Var),Var,_UnivVars):-
	var(Var),!. % It is not necessary to check it 
skolem_term(Term,Term1,UnivVars):-
	Term=..[Functor|Args],
	skolem(Args,Args1,UnivVars),
	Term1=..[Functor|Args1].

% unify_neg_disj(Disj) negates the disjunction of disequalities Disj.
% Therefore it unifies the disequalies of Disj and operates the conjunction
% of the results of it.
unify_neg_disj([]).
unify_neg_disj([(T1/T2) | DisjExist]):-
	T1=T2,
	unify_neg_disj(DisjExist).

% disy_to_conj(Ld,Lc) applies distributive property to obtain Lc from Ld
% if Ld is a disjunction of conjunctions then Lc is a conjunction of
% disyunctions and viceversa.
disy_to_conj(Ld,Lc):-
	setof(D,obtain_disy(Ld,D),Lc).

% obtain_disy(L,D) D is a list with an element from each list of L
% that is a list of lists. 
obtain_disy([],[]).
obtain_disy([Conj|L],[X|Disy]):-
	member(X,Conj),
	obtain_disy(L,Disy). 

% % check_desig(D,Var,T) checks that D is a disequality between the 
% % universally quantified variable Var and the term T
% check_desig(T1/T2,Var,T2):-
% 	nonvar(T1),
% 	T1=fA(Var).
% check_desig(T1/T2,Var,T1):-
% 	nonvar(T2), 
% 	T2=fA(Var).

% appears_fA(Term) successes if a subterm of the form fA(_Var) appears in Term
appears_fA(Term):-
	var(Term),!,
	fail.
appears_fA(Term):-
	ground(Term),!,
	fail.
appears_fA(Term):-
	Term =.. [fA,_Var],!.
appears_fA(Term):-
	Term =.. [_Functor|Args],
	member(Arg,Args),
	appears_fA(Arg).


 
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


