:- module(_, [], [pure, compiler(complang)]).

:- use_module(engine(basiccontrol)).
:- use_module(engine(basic_props)).
:- use_module(engine(term_basic)).

:- public assertion_body/7.
assertion_body(Pred,Compat,Call,Succ,Comp,Comm,
	      (Pred::Compat:Call=>Succ+Comp#Comm)).
