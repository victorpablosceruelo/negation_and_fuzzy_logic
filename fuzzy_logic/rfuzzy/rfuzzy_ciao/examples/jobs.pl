:- module(jobs,_,[rfuzzy, clpr]).

:- set_prop job_offer/1 => position.
(job_offer(J) cred (min,0.8)) :~ prod interest(J), distance(J), salary(J), future_development(J).

position(consultant).
position(systems_analyst).
position(developer).
position(programmer).
position(teacher).

:- set_prop interest/1 => position.
:- default(interest/1, 0.1).
interest(consultant) value 0.6 .
interest(systems_analyst) value 0.8 .
interest(developer) value 0.6 .
interest(programmer) value 0.4.
interest(teacher) value 0.4.

:- set_prop distance/1 => position.
:- default(distance/1, 0.1).
distance(consultant) value 0.4.
distance(systems_analyst) value 0.1 .
distance(developer) value 0.5 .
distance(programmer) value 0.5.
distance(teacher) value 0.85.

:- set_prop salary/1 => position.
:- default(salary/1, 0.1).
salary(consultant) value 0.8.
salary(systems_analyst) value 0.9 .
salary(developer) value 0.6 .
salary(programmer) value 0.5.
salary(teacher) value 0.3.

:- set_prop future_development/1 => position.
:- default(future_development/1, 0.1).
future_development(consultant) value 0.5.
future_development(systems_analyst) value 0.3 .
future_development(developer) value 0.8 .
future_development(programmer) value 0.7.
future_development(teacher) value 0.5.