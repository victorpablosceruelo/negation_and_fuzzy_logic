:- module(freeze, [freeze/2, frozen/2],[assertions,isomodes]).

:- doc(title,"Delaying predicates (freeze)").
:- doc(author,"Manuel Carro").
:- doc(author,"Daniel Cabeza").

:- doc(module,"This library offers a simple implementation of
   @pred{freeze/2}, @pred{frozen/2},
   etc. @cite{Prologii,MU-Prolog,naish:nu-prolog,Carlsson} based on
   the use of attributed variables
   @cite{holzbaur-plilp92,holzbaur-phd}.").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Implementation based on Holzbauer's examples
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(engine(internals)).
:- use_module(engine(attributes)).

%% :- ensure_loaded(library(attrdecl)).

:- pred freeze(X, Goal) : callable(Goal) 

# "If @var{X} is free delay @var{Goal} until @var{X} is
   non-variable.".

:- meta_predicate freeze(?, primitive(goal)).
:- meta_predicate frozen(?, primitive(goal)).

freeze(X, Goal) :-
        attach_attribute( V, '$frozen_goals'(V,Goal)),
        X = V.

:- doc(hide,verify_attribute/2).

:- multifile verify_attribute/2.

:- use_module(engine(hiord_rt), ['$meta_call'/1]).

verify_attribute('$frozen_goals'(Var, Goal), Value):-
        detach_attribute(Var),
        Var = Value, 
        '$meta_call'(Goal).

:- doc(hide,combine_attributes/2).

:- multifile combine_attributes/2.

combine_attributes('$frozen_goals'(V1, G1), '$frozen_goals'(V2, G2)):-
        detach_attribute(V1),
        detach_attribute(V2),
        V1 = V2,
        attach_attribute(V1, '$frozen_goals'(V1, 'basiccontrol:,'(G1,G2))).


:- pred frozen(X, Goal) => callable(Goal) # "@var{Goal} is currently delayed
   until variable @var{X} becomes bound.".

frozen(Var, Goal):-
        get_attribute(Var, '$frozen_goals'(_, Goal)).
