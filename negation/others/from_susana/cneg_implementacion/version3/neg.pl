:- module(neg, [neg/1,
	        is_negation/2
	       ],[]).

% To be able to call a goal from neg
:- meta_predicate
        neg(goal).

:- use_module(intneg,[call_not/2]).
:- use_module(cnegf,[cnegf/1]).
:- use_module(cneg,[cneg/1]).
:- use_module(naf,[naf/1]).

:- use_module(engine(internals),[term_to_meta/2]).
:- use_module(library(lists),[length/2]).
:- use_module(library(aggregates),[findnsols/4]).
:- use_module(library(metaterms),[varset/2]). 
:- use_module(library(idlists),[memberchk/2]). 

%:- use_module(library(strings)). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    NEGACION DE PRED CON ESTRATEGIA SELECTIVA DE TECNICA      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% is_negation(Pred,Goal) return the Goal that can be call if
% Pred is a negation of a positive goal 
is_negation(Pred,Goal):-
	Pred='neg:neg'(Goal). % negation 
is_negation(Pred,Goal):-
	Pred='naf:naf'(Goal). % naf 
is_negation(Pred,MGoal):-
        Pred=(\+(Goal)),  % predefined naf 
        term_to_meta(Goal,MGoal).
is_negation(Pred,Goal):-
	Pred='cneg:cneg'(Goal). % constructive negation 
is_negation(Pred,Goal):-
	Pred='cnegf:cnegf'(Goal). % finite constructive negation 
is_negation(Pred,Goal):-
	Pred='intneg:call_not'(Goal,_End). % intensional negation 

% is_negation_or_compound(Pred) cheks that negation is a negation
% or a compound goal and makes its negation
is_negation_or_compound(Pred):-
	is_negation(Pred,Goal),
	call(Goal).
is_negation_or_compound(Pred):-
	is_compound(Pred).

% is_compound(Pred) if Pred is a compound goal divides it in simple goals
% ATENCION: esto hay que revisarlo porque hay que tener en cuenta las
% cuantificaciones. Los objetivos compuestos pueden tener fA y al negar
% se convierten en existenciales y viceversa.
% Revisar  la implicacion y la disjuncion.
% En principio creo que no deberia haber en este punto cuantificaciones
% universales del tipo fA en los objetivos negados.
is_compound(Pred):-
	Pred=(PredCond -> PredThen ; PredElse),!, % if-then-else
	Pred1=((PredCond,PredThen);(\+(PredCond),PredElse)),
        term_to_meta(Pred1,MPred1),
	neg(MPred1).
is_compound(Pred):-
	Pred=(Pred1;Pred2),!,   % disjuction of predicates
        term_to_meta(Pred1,MPred1),
        term_to_meta(Pred2,MPred2),
	neg(MPred1),
	neg(MPred2).
is_compound(Pred):-
	Pred=(Pred1,Pred2),!, % conjuction of predicates
        term_to_meta(Pred1,MPred1),
        term_to_meta(Pred2,MPred2),
	(neg(MPred1);neg(MPred2)). 

% % Esto esta mal teoricamente: MAL
% is_compound(Pred):-
% 	Pred=(Pred1,Pred2),!, % conjuction of predicates
% 	varset(Pred1,Vars),
%         term_to_meta(Pred1,MPred1),
% 	eliminate_fA(Pred1,Vars,Pred1New),
% 	eliminate_fA(Pred2,Vars,Pred2New),
%         term_to_meta(Pred1New,MPred1New),
%         term_to_meta(Pred2New,MPred2New),
% 	(neg(MPred1);(MPred1New,neg(MPred2New))). % la negacion de Chan para dos

% % eliminate_fA(Pred,Vars,PredNew) returns in PredNew the same term
% % than Pred but with all apearances of variables of Vars existencially
% % cuantified, i.e. eliminate de fA of the vars of Pred that are in Vars
% eliminate_fA(Pred,_Vars,PredNew):-
% 	ground(Pred),!,
% 	PredNew=Pred.
% eliminate_fA(Pred,_Vars,PredNew):-
% 	var(Pred),!,
% 	PredNew=Pred.
% eliminate_fA(Pred,Vars,PredNew):-
% 	Pred=fA(Var), % Var was universally quantified
% 	memberchk(Var,Vars),!, % Var was in the before subgoal
% 	PredNew=Var. % now is existentially quantified
% eliminate_fA(Pred,_Vars,PredNew):-
% 	Pred=fA(_Var),!, % Var was universally quantified
% 	PredNew=Pred. % Var was not in the before subgoal
% eliminate_fA(Pred,Vars,PredNew):-
% 	Pred=..[Functor|Args],
%         eliminate_fA_Args(Args,Vars,ArgsNew),
%         PredNew=..[Functor|ArgsNew].

% % eliminate_fA_Args(Args,Vars,ArgsNew) return in ArgsNew the list obtained
% % from the map of Args aplying eliminate_fA to all the arguments
% eliminate_fA_Args([],_Vars,[]).
% eliminate_fA_Args([Arg|Args],Vars,[Arg1|ArgsNew]):-
% 	eliminate_fA(Arg,Vars,Arg1),
% 	eliminate_fA_Args(Args,Vars,ArgsNew).

% finite(Pred) successes if Pred has a finite number of solutions
% Actually the aproximation is to obtain 50 solutions and check if
% the number of solution is 50 we suposse the number of solutions 
% is not finite.
finite(Pred):- 
	varset(Pred,Vars),
	findnsols(50,Vars,Pred,L),
	length(L,N),
	N < 50.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% neg(Pred) return the negation of calling goal Pred

% Negation of negation is to call the positive goal
neg(Meta):-
	term_to_meta(Pred,Meta),
	is_negation_or_compound(Pred). % No se pone ! para obtener
                                       % todas las soluciones

neg(Meta):-
	term_to_meta(Pred,Meta),
	\+ is_negation_or_compound(Pred),
	neg1(Meta).

% For ground call is used negation as failure (naf)
neg1(Meta):-
  	ground(Meta),!,
  	\+(Meta). % naf(Meta) but it will be more inefficient

% For ground call is used negation as failure (naf)
% This is for ground metapredicates
neg1( (last_module_exp(Pred,_,_,_,Var),call(Var)) ) :-
  	ground(Pred),!,
  	\+(Pred). % naf(Pred) but it will be more inefficient

% For non-ground calls that give fail then the negation successes
neg1(Meta):-
	\+(call(Meta)),!.

% The strategy is followed:

% For calls with a finite number of solutions is used 
% finite constructive negation
neg1(Pred):-
 	finite(Pred),!,
 	cnegf(Pred).

% Try intensional negation checking of adequate result
neg1(Pred):-
 	call_not(Pred,S),
 	(S==success ->
	 !,true    % Success
	;
	     (S==failure ->
	      !,fail % Failure
	     )
	).  % Otherwise uncertain for this tecnique

% Otherwise full constructive negation
neg1(Pred):- 
 	cneg(Pred).

% If it is unable to negate de predicate with general
% constructive negation then show an error message
% and finish (to avoid an infinite loop).
% This possibility must be imposible if the program 
% runs correctly (all predicates are in stored.pl)
% Actually it fails for predefined predicates that
% must be considered here as particular cases like
% \+/2 or the negations

%It is unneeded if we have stored_builtin.pl
%neg(Meta):- 
%	term_to_meta(Pred,Meta),
%	Pred='neg:no_try'(Goal),!,
%	neg(Goal). % it is supposed that it is imposible
%                   % to cause an infinite loop at last version

% no_try(Pred) calls goal Pred
% no_try(Pred):-
%  	call(Pred).









